#' @export
tm_p_scatterlineplot <- function(label = "Scatter + Line Plot",
                                 plot_dataname,
                                 subject_var,
                                 x_var,
                                 y_var,
                                 color_var,
                                 point_colors = character(0),
                                 transformators = list()) {
  module(
    label = label,
    ui = ui_p_scatterlineplot,
    server = srv_p_scatterlineplot,
    ui_args = list(),
    server_args = list(
      plot_dataname = plot_dataname,
      subject_var = subject_var,
      x_var = x_var,
      y_var = y_var,
      color_var = color_var,
      point_colors = point_colors
    ),
    transformators = transformators
  )
}

ui_p_scatterlineplot <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    ui_p_scatterplot(ns("scatter")),
    ui_p_lineplot(ns("line"))
  )
}

srv_p_scatterlineplot <- function(id,
                                  data,
                                  plot_dataname,
                                  subject_var,
                                  x_var,
                                  y_var,
                                  color_var,
                                  point_colors) {
  moduleServer(id, function(input, output, session) {
    plot_q <- srv_p_scatterplot(
      "scatter",
      data = data,
      plot_dataname = plot_dataname,
      subject_var = subject_var,
      x_var = x_var,
      y_var = y_var,
      color_var = color_var,
      point_colors = point_colors
    )
    srv_p_lineplot(
      "line",
      data = plot_q,
      plot_dataname = plot_dataname,
      x_var = x_var,
      y_var = y_var,
      group_var = subject_var
    )
  })
}
