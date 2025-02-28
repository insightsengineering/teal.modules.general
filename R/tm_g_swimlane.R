tm_g_swimlane <- function(label = "Swimlane", 
                          dataname, 
                          time_var, 
                          subject_var, 
                          value_var, 
                          event_var, 
                          value_var_color,
                          value_var_symbol,
                          plot_height = 700) {
  module(
    label = label,
    ui = ui_g_swimlane,
    server = srv_g_swimlane,
    datanames = "all",
    ui_args = list(height = plot_height),
    server_args = list(
      dataname = dataname,
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var,
      value_var_color = value_var_color,
      value_var_symbol = value_var_symbol
    )
  )
}

ui_g_swimlane <- function(id, height) {
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
srv_g_swimlane <- function(id, 
                              data, 
                              dataname,
                              time_var,
                              subject_var,
                              value_var,
                              event_var,
                              value_var_color,
                              value_var_symbol,
                              filter_panel_api, 
                              plot_height = 600) {
  moduleServer(id, function(input, output, session) {
    plotly_q <- reactive({
      req(data())
      adjusted_colors <- .adjust_colors(
        x = unique(data()[[dataname]][[value_var]]),
        predefined = value_var_color
      )
      data() |>
        within(
          dataname = str2lang(dataname),
          dataname_filtered = str2lang(sprintf("%s_filtered", dataname)),
          time_var = str2lang(time_var),
          subject_var = str2lang(subject_var),
          value_var = str2lang(value_var),
          event_var = str2lang(event_var),
          colors = adjusted_colors,
          symbols = value_var_symbol,
          height = input$plot_height,
          filtered_events = c("disposition","response_assessment", "study_drug_administration"),
          xaxis_label = "Study Day",
          yaxis_label = "Subject",
          {
            dataname <- dataname |>
              mutate(subject_var_ordered = forcats::fct_reorder(as.factor(subject_var), time_var, .fun = max)) |>
              group_by(subject_var, time_var) |>
              mutate(
                tooltip = paste(
                  "Subject:", subject_var,
                  "<br>Study Day:", time_var,
                  paste(
                    unique(
                      sprintf("<br>%s: %s", tools::toTitleCase(gsub("[^0-9A-Za-z]+", " ", event_var)), value_var)
                    ),
                    collapse = ""
                  )
                )
              )
          
            
            p <- dataname |> 
              dplyr::filter(
                event_var %in% filtered_events,
                !is.na(time_var)
              ) |>
              plotly::plot_ly(
              source = "swimlane",
              colors = colors,
              symbols = symbols,
              height = height
            ) |>
              plotly::add_markers(
                x = ~time_var, y = ~subject_var_ordered, color = ~value_var, symbol = ~value_var,
                text = ~tooltip,
                hoverinfo = "text"
              ) |>
              plotly::add_segments(
                x = ~0, xend = ~study_day, y = ~subject_var_ordered, yend = ~subject_var_ordered,
                data = dataname |> group_by(subject_var_ordered) |> summarise(study_day = max(time_var)),
                line = list(width = 1, color = "grey"),
                showlegend = FALSE
              ) |>
              plotly::layout(
                xaxis = list(title = xaxis_label), yaxis = list(title = yaxis_label)
              ) |>
              plotly::layout(dragmode = "select") |>
              plotly::config(displaylogo = FALSE)
          }
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

.adjust_colors <- function(x, predefined)  {
  p <- predefined[names(predefined) %in% x]
  p_rgb_num <- col2rgb(p)
  p_hex <- rgb(p_rgb_num[1,]/255, p_rgb_num[2,]/255, p_rgb_num[3,]/255)
  p <- setNames(p_hex, names(p))
  missing_x <- setdiff(x, names(p))
  N <- length(x)
  n <- length(p)
  m <- N - n
  adjusted_colors <- if (m & n) {
    current_space <- rgb2hsv(col2rgb(p))
    optimal_color_space <- colorspace::qualitative_hcl(N)
    color_distances <- dist(t(cbind(current_space, rgb2hsv(col2rgb(optimal_color_space)))))
    optimal_to_current_dist <- as.matrix(color_distances)[seq_len(n), -seq_len(n)]
    furthest_neighbours_idx <- order(apply(optimal_to_current_dist, 2, min), decreasing = TRUE)
    missing_colors <- optimal_color_space[furthest_neighbours_idx][seq_len(m)]
    missing_colors <- setNames(missing_colors, missing_x)
    p <- c(p, missing_colors)
  } else if (n) {
    # todo: generate color palette
    hsv(
      h = seq(0, by = 1/N, length.out = N + 1), 
      s = 1, 
      v = 1
    )
  } else {
    p
  }        
}

