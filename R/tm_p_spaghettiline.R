#' Spaghetti + Line Plot Module
#'
#' This module creates a combined visualization with both spaghetti plot and line plot views.
#' It displays a spaghetti plot where users can select points, and the selection is reflected
#' in a corresponding line plot below. The spaghetti plot shows individual trajectories for
#' each group over time.
#' 
#' The spaghetti plot connects points within each `group_var` level to show individual trajectories.
#' The line plot uses the same `group_var` for grouping and updates to show only the selected data
#' when brushing occurs in the spaghetti plot.
#' 
#' @param label (`character(1)`) Label shown in the navigation item for the module.
#' @param plot_dataname (`character(1)`) Name of the dataset to be used for plotting.
#' @param group_var (`character(1)`) Name of the grouping variable used for creating individual 
#'   trajectories in the spaghetti plot and grouping in the line plot.
#' @param x_var (`character(1)`) Name of the variable to be used for x-axis in both plots.
#' @param y_var (`character(1)`) Name of the variable to be used for y-axis in both plots.
#' @param color_var (`character(1)`) Name of the variable to be used for coloring points and lines in both plots.
#' @param point_colors (`named character`) Valid color names or hex-colors named by levels of color_var column.
#' @param transformators (`list`) Named list of transformator functions.
#' @param reference_lines (`list` or `NULL`) Reference lines specification for the line plot.
#'
#' @examples
#' data <- teal_data() |>
#'   within({
#'     df <- data.frame(
#'       subject_id = rep(c("S1", "S2", "S3", "S4"), each = 4),
#'       time_point = rep(c(0, 30, 60, 90), 4),
#'       response = rnorm(16, 15, 3),
#'       treatment = rep(c("A", "B", "A", "B"), each = 4)
#'     )
#'   })
#' 
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_p_spaghettiline(
#'       label = "Spaghetti + Line Plot",
#'       plot_dataname = "df",
#'       group_var = "subject_id",
#'       x_var = "time_point",
#'       y_var = "response",
#'       color_var = "treatment"
#'     )
#'   )
#' )
#' 
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_p_spaghettiline <- function(label = "Scatter + Line Plot",
                               plot_dataname,
                               group_var,
                               x_var,
                               y_var,
                               color_var,
                               point_colors = character(0),
                               transformators = list(),
                               reference_lines = NULL) {
  module(
    label = label,
    ui = ui_p_spaghettiline,
    server = srv_p_spaghettiline,
    ui_args = list(),
    server_args = list(
      plot_dataname = plot_dataname,
      group_var = group_var,
      x_var = x_var,
      y_var = y_var,
      color_var = color_var,
      point_colors = point_colors,
      reference_lines = reference_lines
    ),
    transformators = transformators
  )
}

ui_p_spaghettiline <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    ui_p_spaghetti(ns("scatter")),
    ui_p_lineplot(ns("line"))
  )
}

srv_p_spaghettiline <- function(id,
                                data,
                                plot_dataname,
                                group_var,
                                x_var,
                                y_var,
                                color_var,
                                point_colors,
                                reference_lines) {
  moduleServer(id, function(input, output, session) {
    plot_q <- srv_p_spaghetti(
      "scatter",
      data = data,
      plot_dataname = plot_dataname,
      group_var = group_var,
      x_var = x_var,
      y_var = y_var,
      color_var = color_var,
      point_colors = point_colors,
      show_widgets = FALSE
    )

    srv_p_lineplot(
      "line",
      data = plot_q,
      plot_dataname = plot_dataname,
      x_var = x_var,
      y_var = y_var,
      color_var = color_var,
      group_var = group_var,
      colors = point_colors,
      reference_lines = reference_lines,
      activate_on_brushing = TRUE
    )
  })
}
