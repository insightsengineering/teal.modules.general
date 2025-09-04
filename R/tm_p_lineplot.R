#' @export
tm_p_lineplot <- function(label = "Line Plot",
                          plot_dataname,
                          x_var,
                          y_var,
                          group_var,
                          transformators = list()) {
  module(
    label = label,
    ui = ui_p_lineplot,
    server = srv_p_lineplot,
    ui_args = list(),
    server_args = list(
      plot_dataname = plot_dataname,
      x_var = x_var,
      y_var = y_var,
      group_var = group_var
    ),
    transformators = transformators
  )
}

ui_p_lineplot <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    tags$div(
      # trigger_tooltips_deps(),
      plotly::plotlyOutput(ns("plot"), height = "100%")
    )
  )
}

srv_p_lineplot <- function(id, data, plot_dataname, x_var, y_var, group_var) {
  moduleServer(id, function(input, output, session) {
    plotly_q <- reactive({
      df <- data()[[plot_dataname]]

      validate(need(nrow(df) > 0, "No data after applying filters."))

      # TODO: implement the high/low lines with annotations
      y_low_last <- if ("si_low" %in% names(df)) utils::tail(stats::na.omit(df[["si_low"]]), 1) else NA
      y_high_last <- if ("si_high" %in% names(df)) utils::tail(stats::na.omit(df[["si_high"]]), 1) else NA

      p <- plotly::plot_ly(data = df |> dplyr::group_by(!!sym(group_var)), x = df[[x_var]]) |>
        plotly::add_trace(
          y = df[[y_var]],
          mode = "lines+markers", type = "scatter", name = "Lab Result",
          line = list(color = "green"),
          marker = list(color = "green"),
          showlegend = FALSE
        ) |>
        # plotly::add_trace(
        #   y = df[["si_low"]],
        #   mode = "lines",
        #   line = list(color = "red", dash = "dash"),
        #   showlegend = FALSE
        # ) |>
        # plotly::add_annotations(
        #   x = max(df[[x_var]], na.rm = TRUE),
        #   y = y_low_last,
        #   yshift = 15,
        #   text = "Original LLN",
        #   showarrow = FALSE
        # ) |>
        # plotly::add_trace(
        #   y = df[["si_high"]],
        #   mode = "lines",
        #   line = list(color = "red", dash = "solid"),
        #   showlegend = FALSE
        # ) |>
        # plotly::add_annotations(
        #   x = max(df[[x_var]], na.rm = TRUE),
        #   y = y_high_last,
        #   yshift = -15,
        #   text = "Original ULN",
        #   showarrow = FALSE
        # ) |>
        plotly::layout(
          xaxis = list(title = "Study Day of Sample Collection", zeroline = FALSE),
          yaxis = list(title = "Original Result")
        )

      p
    })


    output$plot <- plotly::renderPlotly({
      p <- plotly_q()
      plotly::event_register(p, "plotly_selected")
      p
    })
  })
}
