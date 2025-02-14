tm_p_spiderplot <- function(label = "Spiderplot",
                            time_var,
                            subject_var,
                            value_var,
                            plot_height = 600) {
  module(
    label = label,
    ui = ui_p_spiderplot,
    server = srv_p_spiderplot,
    ui_args = list(height = plot_height),
    server_args = list(
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var
    ),
    datanames = "all",
  )
}


ui_p_spiderplot <- function(id, height) {
  ns <- NS(id)
  tagList(
    div(
      style = "display: flex; justify-content: center; align-items: center; gap: 30px;",
      div(
        selectInput(NS(id, "event_type"), "Select Y Axis", NULL)
      ),
      div(sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, height))
    ),
    div(
      style = "display: flex",
      div(
        class = "simple-card",
        style = "width: 50%",
        tagList(
          h4("Most Recent Resp and Best Resp"),
          ui_t_reactable(ns("recent_resp"))
        )
      ),
      div(
        class = "simple-card",
        style = "width: 50%",
        plotly::plotlyOutput(ns("plot"), height = "100%")
      )
    ),
    div(
      style = "display: flex",
      div(
        style = "width: 50%",
        div(
          class = "simple-card",
          h4("Disease Assessment - SFLC"),
          ui_t_reactable(ns("sflc_listing"))
        ),
        div(
          class = "simple-card",
          h4("Disease Assessment - SPEP"),
          ui_t_reactable(ns("spep_listing"))
        )
      ),
      div(
        class = "simple-card",
        style = "width: 50%",
        h4("Multiple Myeloma Response"),
        ui_t_reactable(ns("all_resp"))
      )
    )
  )
}

srv_p_spiderplot <- function(id,
                             data,
                             time_var,
                             subject_var,
                             value_var,
                             filter_panel_api,
                             plot_height = 600) {
  moduleServer(id, function(input, output, session) {
    dataname <- "spiderplot_ds"
    spiderplot_ds <- reactive(data()[[dataname]])
    observeEvent(spiderplot_ds(), {
      event_types <- unique(spiderplot_ds()$event_type)
      updateSelectInput(
        inputId = "event_type",
        choices = event_types[!event_types %in% c("response_assessment", "latest_response_assessment")]
      )
    })
    plotly_q <- reactive({
      data() |>
        within(
          dataname = str2lang(dataname),
          dataname_filtered = str2lang(sprintf("%s_filtered", dataname)),
          time_var = str2lang(time_var),
          subject_var = str2lang(subject_var),
          value_var = str2lang(value_var),
          selected_event = input$event_type,
          height = input$plot_height,
          expr = {
            y_title <- selected_event
            dataname_filtered <- filter(dataname, event_type == selected_event)

            p <- plotly::plot_ly(source = "spiderplot", height = height) |>
              plotly::add_markers(
                x = ~time_var, y = ~value_var, color = ~subject_var,
                data = dataname_filtered
              ) |>
              plotly::add_lines(
                x = ~time_var, y = ~value_var, color = ~subject_var,
                data = dataname_filtered,
                showlegend = FALSE
              ) |>
              plotly::layout(
                xaxis = list(title = "Collection Date Study Day", zeroline = FALSE),
                yaxis = list(title = ~y_title),
                title = ~ paste0(y_title, " Over Time")
              ) |>
              plotly::layout(dragmode = "select") |>
              plotly::config(displaylogo = FALSE)
          }
        )
    })

    output$plot <- plotly::renderPlotly({
      plotly::event_register(plotly_q()$p, "plotly_selected")
    })

    plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "spiderplot"))


    resp_cols <- c(
      "subject", "raise_query", "visit_name", "rspdn", "rspd", "rspd_study_day",
      "orsp", "bma", "bmb", "comnts"
    )
    spep_cols <- c(
      "subject", "visit_name", "visit_date", "form_name", "source_system_url_link",
      "rspdn", "rspd", "rspd_study_day", "orsp", "bma", "bmb", "comnts",
      "asmntdn", "blq", "coldr", "cold_study_day", "coltm", "coltmu", "lrspep1",
      "mprte_raw", "mprtec"
    )
    sflc_cols <- c(
      "subject", "visit_name", "visit_date", "form_name", "source_system_url_link", "rspdn", "rspd",
      "rspd_study_day", "orsp", "bma", "bmb", "comnts", "asmntdn", "blq", "coldr", "cold_study_day",
      "coltm", "coltmu", "lchfrc", "lchfr_raw", "klchf_raw", "llchf_raw",
      "klchp_raw", "mprte_raw", "mprtec"
    )

    plotly_selected_q <- reactive({
      req(plotly_selected())
      within(
        plotly_q(),
        dataname = str2lang(dataname),  # todo: replace with argument
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

    recent_resp_q <- reactive({
      req(plotly_selected_q())
      within(
        plotly_selected_q(),
        dataname = str2lang(dataname),
        subject_var = str2lang(subject_var),
        resp_cols = resp_cols,
        expr = {
          recent_resp <- dplyr::filter(
            dataname,
            event_type == "latest_response_assessment",
            subject_var %in% brushed_subjects # todo: figure this out
          ) |>
            select(all_of(resp_cols))
        }
      )
    })
    
    recent_resp_selected_q <- srv_t_reactable(
      "recent_resp", data = recent_resp_q, dataname = "recent_resp", selection = "single"
    )
  

    all_resp_q <- reactive({
      req(nrow(recent_resp_selected_q()[["recent_resp_selected"]]))
      within(
        recent_resp_selected_q(),
        dataname = str2lang(dataname),
        subject_var = str2lang(subject_var),
        subject_var_char = subject_var,
        resp_cols = resp_cols,
        expr = {
          all_resp <- dplyr::filter(
            dataname, 
            event_type == "response_assessment",
            subject_var %in% unique(recent_resp_selected[[subject_var_char]])
          ) |>
            select(all_of(resp_cols))
        }
      )
    })
    spep_q <- reactive({
      req(nrow(recent_resp_selected_q()[["recent_resp_selected"]]))
      within(
        recent_resp_selected_q(),
        dataname = str2lang(dataname),
        subject_var = str2lang(subject_var),
        subject_var_char = subject_var,
        spep_cols = spep_cols,
        expr = {
          spep <- dplyr::filter(
            dataname,
            event_type == "Serum M-protein",
            subject_var %in% unique(recent_resp_selected[[subject_var_char]])
          ) |>
            select(all_of(spep_cols))
        }
      )
    })
    sflc_q <- reactive({
      req(nrow(recent_resp_selected_q()[["recent_resp_selected"]]))
      within(
        recent_resp_selected_q(),
        dataname = str2lang(dataname),
        subject_var = str2lang(subject_var),
        subject_var_char = subject_var,
        sflc_cols = sflc_cols,
        expr = {
          sflc <- dplyr::filter(
            dataname,
            event_type %in% c(
              "Kappa free light chain quantity",
              "Lambda free light chain quantity",
              "Kappa-Lambda free light chain ratio"
            ),
            subject_var %in% unique(recent_resp_selected[[subject_var_char]])
          ) |>
            select(all_of(sflc_cols))
        }
      )
    })
  
    #todo: show all_resp only if recent_resp is selected  
    all_resp_selected_q <- srv_t_reactable("all_resp", data = all_resp_q, dataname = "all_resp") 
    spep_selected_d <- srv_t_reactable("spep_listing", data = spep_q, dataname = "spep")
    sflc_selected_d <- srv_t_reactable("sflc_listing", data = sflc_q, dataname = "sflc")
  })
}


.with_tooltips <- function(...) {
  args <- list(...)
  lapply(args, function(col) {
    col$header <- tags$span(col$name, title = col$name)
    return(col)
  })
}
