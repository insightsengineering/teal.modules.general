#' @export
tm_p_bargraph <- function(label = "Bar Plot",
                          plot_dataname,
                          y_var,
                          color_var,
                          count_var,
                          bar_colors = NULL) {
  module(
    label = label,
    ui = ui_p_bargraph,
    server = srv_p_bargraph,
    ui_args = list(),
    server_args = list(
      plot_dataname = plot_dataname,
      y_var = y_var,
      color_var = color_var,
      count_var = count_var,
      bar_colors = bar_colors
    )
  )
}

ui_p_bargraph <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    bslib::card(
      full_screen = TRUE,
      tags$div(
        # trigger_tooltips_deps(),
        plotly::plotlyOutput(ns("plot"), height = "100%")
      )
    )
  )
}

srv_p_bargraph <- function(id, data, plot_dataname, y_var, color_var, count_var, bar_colors) {
  moduleServer(id, function(input, output, session) {
    plotly_q <- reactive({
      df <- data()[[plot_dataname]]
      df[[color_var]] <- as.character(df[[color_var]])

      plot_data <- df %>%
        group_by(!!as.name(y_var), !!as.name(color_var)) %>%
        summarize(count = n_distinct(!!as.name(count_var)), .groups = "drop")

      event_type_order <- plot_data %>%
        group_by(!!as.name(y_var)) %>%
        summarize(total = sum(count)) %>%
        arrange(total) %>%
        pull(!!as.name(y_var))

      plot_data[[y_var]] <- factor(plot_data[[y_var]], levels = event_type_order)

      p <- plot_ly(
        data = plot_data,
        y = as.formula(paste0("~", y_var)),
        x = ~count,
        color = as.formula(paste0("~", color_var)),
        colors = bar_colors,
        type = "bar",
        orientation = "h"
      ) %>%
        layout(
          barmode = "stack",
          xaxis = list(title = "Count"),
          yaxis = list(title = "Adverse Event Type"),
          legend = list(title = list(text = "AE Type"))
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
