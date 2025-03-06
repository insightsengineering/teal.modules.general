#' @export
tm_g_spiderplot <- function(label = "Spiderplot",
                            time_var,
                            subject_var,
                            value_var,
                            event_var,
                            plot_height = 600,
                            transformator = transformator) {
  module(
    label = label,
    ui = ui_g_spiderplot,
    server = srv_g_spiderplot,
    ui_args = list(height = plot_height),
    server_args = list(
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var
    ),
    datanames = "all",
  )
}


ui_g_spiderplot <- function(id, height) {
  ns <- NS(id)
  div(
    div(
      class = "simple-card",
      div(
        class = "row",
        column(
          width = 6,
          selectInput(ns("select_event"), "Select Y Axis", NULL)
        ),
        column(
          width = 6,
          sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, height)
        )        
      ),
      plotly::plotlyOutput(ns("plot"), height = "100%")
    )
  )
}

srv_g_spiderplot <- function(id,
                             data,
                             dataname,
                             time_var,
                             subject_var,
                             value_var,
                             event_var,
                             filter_panel_api,
                             plot_height = 600) {
  moduleServer(id, function(input, output, session) {
    event_levels <- reactive({
      req(data())
      unique(data()[[dataname]][[event_var]])
    })
    observeEvent(event_levels(), {
      updateSelectInput(inputId = "select_event",  choices = event_levels(), selected = event_levels()[1])
    })
    
    plotly_q <- reactive({
      # todo: tooltip!
      req(input$select_event)
      within(
        data(),
        dataname = str2lang(dataname),
        time_var = str2lang(time_var),
        subject_var = str2lang(subject_var),
        value_var = str2lang(value_var),
        event_var = str2lang(event_var),
        selected_event = input$select_event,
        height = input$plot_height,
        xaxis_label = attr(data()[[dataname]][[time_var]], "label"),
        yaxis_label = input$select_event,
        title = paste0(input$select_event, " Over Time"),
        expr = {
          p <- dataname |> filter(event_var == selected_event)|> 
            plotly::plot_ly(source = "spiderplot", height = height) |>
            plotly::add_markers(
              x = ~time_var, y = ~value_var, color = ~subject_var
            ) |>
            plotly::add_lines(
              x = ~time_var, y = ~value_var, color = ~subject_var,
              showlegend = FALSE
            ) |>
            plotly::layout(
              xaxis = list(title = xaxis_label, zeroline = FALSE),
              yaxis = list(title = yaxis_label),
              title = title,
              dragmode = "select"
            ) |>
            plotly::config(displaylogo = FALSE)
        }
      )
    })

    output$plot <- plotly::renderPlotly(plotly::event_register(plotly_q()$p, "plotly_selected"))

    plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "spiderplot"))

    reactive({
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
          plotly_brushed_time <- time_vals
          plotly_brushed_value <- value_vals
        }
      )
    })
  })
}
