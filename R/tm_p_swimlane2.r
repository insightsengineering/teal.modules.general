tm_p_swimlane2 <- function(
    label = "Swimlane Plot Module", plotly_specs, title,
    colors = c(), symbols = c(), transformers = list(),
    ui_mod = ui_data_table,
    srv_mod = srv_data_table) {
  module(
    label = label,
    ui = ui_p_swimlane2,
    server = srv_p_swimlane2,
    datanames = "all",
    ui_args = list(ui_mod = ui_mod),
    server_args = list(
      plotly_specs = plotly_specs,
      title = title,
      colors = colors,
      symbols = symbols,
      srv_mod = srv_mod
    ),
    transformers = transformers
  )
}


ui_p_swimlane2 <- function(id, ui_mod) {
  ns <- NS(id)
  shiny::tagList(
    sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, 800),
    plotly::plotlyOutput(ns("plot"), height = "100%"),
    ui_mod(ns("brush_tables"))
  )
}

srv_p_swimlane2 <- function(id,
                            data,
                            plotly_specs,
                            title = "Swimlane plot",
                            colors,
                            symbols,
                            srv_mod,
                            filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    plotly_q <- reactive({
      plotly_call <- .make_plotly_call(specs = plotly_specs, colors = colors, symbols = symbols)
      code <- substitute(
        p <- plotly_call,
        list(plotly_call = plotly_call)
      )
      eval_code(data(), code = code)
    })

    output$plot <- plotly::renderPlotly({
      plotly::event_register(
        plotly_q()$p |> layout(height = input$plot_height),
        "plotly_selected"
      )
    })

    plotly_selected <- reactive(plotly::event_data("plotly_selected"))

    observeEvent(plotly_selected(), once = TRUE, {
      srv_mod("brush_tables", data = data, filter_panel_api = filter_panel_api, plotly_selected = plotly_selected)
    })
  })
}



.make_plotly_call <- function(specs, colors = c(), symbols = c()) {
  init_call <- substitute(plotly::plot_ly(colors = colors, symbols = symbols), list(colors = colors, symbols = symbols))
  points_calls <- lapply(specs, function(x) {
    which_fun <- c(which(names(x) == "fun"), 1)[1]
    if (is.character(x[[which_fun]])) {
      x[[which_fun]] <- str2lang(x[[which_fun]])
    }
    as.call(
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
