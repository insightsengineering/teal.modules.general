tm_p_swimlane2 <- function(label = "Swimlane Plot Module", plotly_specs, title) {
  module(
    label = label,
    ui = ui_p_swimlane2,
    server = srv_p_swimlane2,
    datanames = "all",
    server_args = list(
      plotly_specs = plotly_specs,
      title = title
    )
  )
}


ui_p_swimlane2 <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    plotly::plotlyOutput(ns("plot")),
    verbatimTextOutput(ns("selecting")),
    shinyjs::hidden(tableOutput(ns("table")))
  )
}

srv_p_swimlane2 <- function(id,
                            data,
                            plotly_specs,
                            title = "Swimlane plot",
                            filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    plotly_q <- reactive({
      code <- substitute(
        p <- plotly_specs |> plotly::event_register("plotly_selecting"),
        list(plotly_specs = plotly_specs)
      )
      eval_code(data(), code = code)
    })

    output$plot <- plotly::renderPlotly(plotly_q()$p)

    output$selecting <- renderPrint({
      d <- plotly::event_data("plotly_selecting")
      if (is.null(d)) "Brush points appear here (double-click to clear)" else d
    })
  })
}
