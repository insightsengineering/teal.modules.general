tm_p_spiderplot <- function(label = "Spiderplot",
                            time_var,
                            subject_var,
                            value_var,
                            event_var,
                            table_cols,
                            plot_height = 600) {
  module(
    label = label,
    ui = ui_p_spiderplot,
    server = srv_p_spiderplot,
    ui_args = list(height = plot_height),
    server_args = list(
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var,
      table_cols = table__cols
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
        selectInput(ns("select_event"), "Select Y Axis", NULL)
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
    )
  )
}

srv_p_spiderplot <- function(id,
                             data,
                             time_var,
                             subject_var,
                             value_var,
                             event_var,
                             table_cols,
                             filter_panel_api,
                             plot_height = 600) {
  moduleServer(id, function(input, output, session) {
    dataname <- "spiderplot_ds"
    excl_events <- c("response_assessment", "latest_response_assessment")
    spiderplot_ds <- reactive(data()[[dataname]])
    observeEvent(spiderplot_ds(), {
      event_levels <- setdiff(unique(spiderplot_ds()[[event_var]]), excl_events)
      updateSelectInput(inputId = "select_event",  choices = event_levels)
    })
    
    plotly_q <- reactive({
      within(
        data(),
        dataname = str2lang(dataname),
        dataname_filtered = str2lang(sprintf("%s_filtered", dataname)),
        time_var = str2lang(time_var),
        subject_var = str2lang(subject_var),
        value_var = str2lang(value_var),
        selected_event = input$select_event,
        height = input$plot_height,
        event_var = str2lang(event_var),
        expr = {
          y_title <- selected_event
          dataname_filtered <- filter(dataname, event_var == selected_event)

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

    output$plot <- plotly::renderPlotly(plotly::event_register(plotly_q()$p, "plotly_selected"))

    plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "spiderplot"))


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
        table_cols = table_cols,
        event_var = str2lang(event_var),
        expr = {
          recent_resp <- dplyr::filter(
            dataname,
            event_var == "latest_response_assessment",
            subject_var %in% brushed_subjects # todo: figure this out
          ) |>
            select(all_of(table_cols))
        }
      )
    })
    
    srv_t_reactable(
      "recent_resp", data = recent_resp_q, dataname = "recent_resp", selection = "single"
    )
  })
}


.with_tooltips <- function(...) {
  args <- list(...)
  lapply(args, function(col) {
    col$header <- tags$span(col$name, title = col$name)
    return(col)
  })
}
