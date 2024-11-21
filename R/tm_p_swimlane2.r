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
      plotly_call <- .make_plotly_call(specs = plotly_specs)
      code <- substitute(
        p <- plotly_call %>% plotly::event_register("plotly_selecting"),
        list(plotly_call = plotly_call)
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



.make_plotly_call <- function(init_call = quote(plotly::plot_ly()), specs) {
  points_calls <- lapply(specs, function(x) {
    which_fun <- c(which(names(x) == "fun"), 1)[1]
    if (is.character(x[[which_fun]])) {
      x[[which_fun]] <- str2lang(x[[which_fun]])
    }
    basic_call <- as.call(
      c(
        list(x[[which_fun]]),
        x[-which_fun]
      )
    )
  })

  rhs <- Reduce(
    x = c(init_call, points_calls),
    f = function(x, y) call("%>%", x, y)
  )
}
