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
          reactableOutput(ns("recent_resp"))
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
    spiderplot_ds <- reactive(data()[["spiderplot_ds"]])
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
          selected_event = input$event_type,
          height = input$plot_height,
          time_var = str2lang(time_var),
          subject_var = str2lang(subject_var),
          value_var = str2lang(value_var),
          expr = {
            y_title <- selected_event
            spiderplot_ds_filtered <- spiderplot_ds |>
              filter(event_type == selected_event)

            p <- plotly::plot_ly(source = "spiderplot", height = height) |>
              plotly::add_markers(
                x = ~time_var, y = ~value_var, color = ~subject_var,
                data = spiderplot_ds_filtered
              ) |>
              plotly::add_lines(
                x = ~time_var, y = ~value_var, color = ~subject_var,
                data = spiderplot_ds_filtered,
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
      plotly::event_register(
        plotly_q()$p,
        "plotly_selected"
      )
    })

    plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "spiderplot"))


    resp_cols <- c(
      "subject", "raise_query", "visit_name", "rspdn", "rspd", "rspd_study_day",
      "orsp", "bma", "bmb", "comnts"
    )

    plotly_selected_subjects <- reactive({
      data()[["spiderplot_ds"]] |>
        filter(event_study_day %in% plotly_selected()$x, event_result %in% plotly_selected()$y) |>
        pull(subject)
    })

    recent_resp_ds <- reactive({
      data()[["spiderplot_ds"]] |>
        filter(event_type == "latest_response_assessment") |>
        filter(subject %in% plotly_selected_subjects()) |>
        select(all_of(resp_cols))
    })

    output$recent_resp <- renderReactable({
      req(plotly_selected_subjects())
      reactable(
        recent_resp_ds(),
        # columns = resp_cols,
        selection = "single",
        onClick = "select",
        defaultPageSize = 15,
        wrap = FALSE,
        rowClass = JS("
            function(rowInfo) {
            console.log(rowInfo);
              if (rowInfo.selected) {
                return 'selected-row';
              }
            }
          ")
      )
    })

    table_selected_subjects <- reactive({
      selected_row <- getReactableState("recent_resp", "selected")
      if (!is.null(selected_row)) {
        recent_resp_ds()[selected_row, ]$subject
      } else {
        unique(recent_resp_ds()$subject)
      }
    })

    all_resp <- reactive({
      data()[["spiderplot_ds"]] |>
        filter(event_type == "response_assessment") |>
        select(all_of(resp_cols)) |>
        filter(subject %in% plotly_selected_subjects()) |>
        filter(subject %in% table_selected_subjects())
    })

    output$all_resp <- renderReactable({
      if (nrow(all_resp()) == 0) {
        return()
      }

      reactable(
        all_resp(),
        # columns = resp_cols,
        defaultPageSize = 15,
        wrap = FALSE
      )
    })

    spep_cols <- c(
      "subject", "visit_name", "visit_date", "form_name", "source_system_url_link",
      "rspdn", "rspd", "rspd_study_day", "orsp", "bma", "bmb", "comnts",
      "asmntdn", "blq", "coldr", "cold_study_day", "coltm", "coltmu", "lrspep1",
      "mprte_raw", "mprtec"
    )

    spep <- reactive({
      data()[["spiderplot_ds"]] |>
        filter(event_type == "Serum M-protein") |>
        filter(subject %in% table_selected_subjects()) |>
        select(all_of(spep_cols))
    })

    output$spep_listing <- renderReactable({
      if (nrow(spep()) == 0) {
        return()
      }

      reactable(
        spep(),
        # columns = spep_cols,
        defaultPageSize = 5,
        wrap = FALSE
      )
    })


    sflc_cols <- c(
      "subject", "visit_name", "visit_date", "form_name", "source_system_url_link", "rspdn", "rspd",
      "rspd_study_day", "orsp", "bma", "bmb", "comnts", "asmntdn", "blq", "coldr", "cold_study_day",
      "coltm", "coltmu", "lchfrc", "lchfr_raw", "klchf_raw", "llchf_raw",
      "klchp_raw", "mprte_raw", "mprtec"
    )

    sflc <- reactive({
      data()[["spiderplot_ds"]] |>
        filter(
          event_type %in% c(
            "Kappa free light chain quantity",
            "Lambda free light chain quantity",
            "Kappa-Lambda free light chain ratio"
          )
        ) |>
        filter(subject %in% table_selected_subjects()) |>
        select(all_of(sflc_cols))
    })

    output$sflc_listing <- renderReactable({
      if (nrow(sflc()) == 0) {
        return()
      }

      reactable(
        sflc(),
        # columns = sflc_cols,
        defaultPageSize = 5,
        wrap = FALSE
      )
    })
  })
}


.with_tooltips <- function(...) {
  args <- list(...)
  lapply(args, function(col) {
    col$header <- tags$span(col$name, title = col$name)
    return(col)
  })
}
