tm_p_swimlane <- function(label = "Swimlane", dataname, time_var, subject_var, value_var, event_var, plot_height = 700) {
  module(
    label = label,
    ui = ui_p_swimlane,
    server = srv_p_swimlane,
    datanames = "all",
    ui_args = list(height = plot_height),
    server_args = list(
      dataname = dataname,
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var
    )
  )
}

ui_p_swimlane <- function(id, height) {
  ns <- NS(id)
  tagList(
    fluidRow(
      class = "simple-card",
      h4("Swim Lane - Duration of Tx"),
      sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, height, width = "100%"),
      plotly::plotlyOutput(ns("plot"), height = "100%")
    ),
    fluidRow(
      column(
        6,
        class = "simple-card",
        tagList(
          h4("Multiple Myeloma Response"),
          ui_t_reactable(ns("mm_response"))
        )
      ),
      column(
        6,
        class = "simple-card",
        tagList(
          h4("Study Tx Listing"),
          ui_t_reactable(ns("tx_listing"))
        )
      )
    )
  )
}
srv_p_swimlane <- function(id, 
                              data, 
                              dataname,
                              time_var,
                              subject_var,
                              value_var,
                              event_var,
                              filter_panel_api, 
                              plot_height = 600) {
  moduleServer(id, function(input, output, session) {
    plotly_q <- reactive({
      data() |>
        within(
          dataname = str2lang(dataname),
          dataname_filtered = str2lang(sprintf("%s_filtered", dataname)),
          time_var = str2lang(time_var),
          subject_var = str2lang(subject_var),
          value_var = str2lang(value_var),
          event_var = str2lang(event_var),
          subject_var_char = subject_var,
          height = input$plot_height,
          {
            dataname <- dataname |>
              mutate(
                subject_var_char := forcats::fct_reorder(as.factor(subject_var), time_var, .fun = max),
                tooltip = case_when(
                  event_var == "study_drug_administration" ~ paste(
                    "Subject:", subject_var,
                    "<br>Study Day:", time_var,
                    "<br>Administration:", value_var
                  ),
                  event_var == "response_assessment" ~ paste(
                    "Subject:", subject_var,
                    "<br>Study Day:", time_var,
                    "<br>Response Assessment:", value_var
                  ),
                  event_var == "disposition" ~ paste(
                    "Subject:", subject_var,
                    "<br>Study Day:", time_var,
                    "<br>Disposition:", value_var
                  ),
                  TRUE ~ NA_character_
                )
              )
            
            dataname <- dataname |>
              group_by(subject_var, time_var) |>
              mutate(tooltip = paste(unique(tooltip), collapse = "<br>")) |>
              ungroup() |>
              mutate(tooltip = stringr::str_remove_all(tooltip, "<br>Subject: [0-9]+ <br>Study Day: [0-9]+"))
            
            
            disposition <- dataname |>
              filter(!is.na(time_var)) |>
              filter(event_var == "disposition") |>
              mutate(subject_var, event_var, catagory = value_var, study_day = time_var, tooltip, .keep = "none")
            
            response_assessment <- swimlane_ds |>
              filter(!is.na(event_study_day)) |>
              filter(event_type == "response_assessment") |>
              mutate(subject_var, event_var, catagory = value_var, study_day = time_var, tooltip, .keep = "none")
            
            study_drug_administration <- swimlane_ds |>
              filter(!is.na(event_study_day)) |>
              filter(event_type == "study_drug_administration") |>
              mutate(subject_var, event_var, catagory = value_var, study_day = time_var, tooltip, .keep = "none")
            
            max_subject_day <- swimlane_ds |>
              group_by(subject_var) |>
              summarise(study_day = max(time_var)) |>
              bind_rows(tibble(subject_var_char := unique(dataname[[subject_var_char]]), study_day = 0))
            
            p <- plotly::plot_ly(
              source = "swimlane",
              colors = c(
                "DEATH" = "black",
                "WITHDRAWAL BY SUBJECT" = "grey",
                "PD (Progressive Disease)" = "red",
                "SD (Stable Disease)" = "darkorchid4",
                "MR (Minimal/Minor Response)" = "sienna4",
                "PR (Partial Response)" = "maroon",
                "VGPR (Very Good Partial Response)" = "chartreuse4",
                "CR (Complete Response)" = "#3a41fc",
                "SCR (Stringent Complete Response)" = "midnightblue",
                "X Administration Injection" = "goldenrod",
                "Y Administration Infusion" = "deepskyblue3",
                "Z Administration Infusion" = "darkorchid"
              ),
              symbols = c(
                "DEATH" = "circle",
                "WITHDRAWAL BY SUBJECT" = "square",
                "PD (Progressive Disease)" = "circle",
                "SD (Stable Disease)" = "square-open",
                "MR (Minimal/Minor Response)" = "star-open",
                "PR (Partial Response)" = "star-open",
                "VGPR (Very Good Partial Response)" = "star-open",
                "CR (Complete Response)" = "star-open",
                "SCR (Stringent Complete Response)" = "star-open",
                "X Administration Injection" = "line-ns-open",
                "Y Administration Infusion" = "line-ns-open",
                "Z Administration Infusion" = "line-ns-open"
              ),
              height = height
            ) |>
              plotly::add_markers(
                x = ~study_day, y = ~subject_var, color = ~catagory, symbol = ~catagory,
                text = ~tooltip,
                hoverinfo = "text",
                data = study_drug_administration
              ) |>
              plotly::add_markers(
                x = ~study_day, y = ~subject_var, color = ~catagory, symbol = ~catagory,
                text = ~tooltip,
                hoverinfo = "text",
                data = response_assessment
              ) |>
              plotly::add_markers(
                x = ~study_day, y = ~subject_var, color = ~catagory, symbol = ~catagory,
                text = ~tooltip,
                hoverinfo = "text",
                data = disposition
              ) |>
              plotly::add_segments(
                x = ~0, xend = ~study_day, y = ~subject_var, yend = ~subject_var,
                data = max_subject_day,
                line = list(width = 1, color = "grey"),
                showlegend = FALSE
              ) |>
              plotly::layout(
                xaxis = list(title = "Study Day"), yaxis = list(title = "Subject")
              ) |>
              plotly::layout(dragmode = "select") |>
              plotly::config(displaylogo = FALSE)
          },
          height = input$plot_height
        )
    })
    
    output$plot <- plotly::renderPlotly(plotly::event_register(plotly_q()$p, "plotly_selected"))
    
    plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "swimlane"))
    
    plotly_selected_q <- reactive({
      req(plotly_selected())
      within(
        plotly_q(),
        dataname = str2lang(dataname),
        time_var = str2lang(time_var),
        subject_var = subject_var,
        value_var = str2lang(value_var),
        time_vals = plotly_selected()$x,
        value_vals = plotly_selected()$y,
        expr = {
          brushed_subjects <- dplyr::filter(
            dataname, time_var %in% time_vals, value_var %in% value_vals
          )[[subject_var]]
        }
      )
    })
    
    mm_response_vars <- c(
      "subject", "visit_name", "visit_date", "form_name", "source_system_url_link",
      "rspdn", "rspd", "rspd_study_day", "orsp", "bma", "bmb", "comnts"
    )
    
    tx_listing_vars <- c(
      "site_name", "subject", "visit_name", "visit_date", "form_name", "source_system_url_link", "txnam",
      "txrec", "txrecrs", "txd_study_day", "date_administered", "cydly", "cydlyrs", "cydlyae", "txdly", 
      "txdlyrs", "txdlyae", "txpdos", "txpdosu", "frqdv", "txrte", "txform", "txdmod", "txrmod",
      "txdmae", "txad", "txadu", "txd", "txstm", "txstmu", "txed", "txetm", "txetmu", "txtm", "txtmu",
      "txed_study_day", "infrt", "infrtu", "tximod", "txirmod", "tximae"
    )
    
    mm_response_q <- reactive({
      within(
        plotly_selected_q(),
        dataname = str2lang(dataname),
        time_var = str2lang(time_var),
        subject_var = str2lang(subject_var),
        time_vals = plotly_selected()$x,
        subject_vals = plotly_selected()$y,
        col_defs = mm_response_vars,
        expr = {
          mm_response <- dataname |>
            filter(time_var %in% time_vals, subject_var %in% subject_vals) |>
            select(all_of(col_defs))
        }
      )
    
    })
    
    tx_listing_q <- reactive({
      within(
        plotly_selected_q(),
        dataname = str2lang(dataname),
        time_var = str2lang(time_var),
        subject_var = str2lang(subject_var),
        time_vals = plotly_selected()$x,
        subject_vals = plotly_selected()$y,
        col_defs = tx_listing_vars,
        expr = {
          tx_listing <- dataname |>
            filter(time_var %in% time_vals, subject_var %in% subject_vals) |>
            select(all_of(col_defs))
        }
      )
      
    })
    
    mm_reactable_q <- srv_t_reactable("mm_response", data = mm_response_q, dataname = "mm_response", selection = NULL)
    tx_reactable_q <- srv_t_reactable("tx_listing", data = tx_listing_q, dataname = "tx_listing", selection = NULL)    

  })
}