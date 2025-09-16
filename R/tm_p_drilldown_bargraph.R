#' Drilldown Bar Graph Module
#'
#' This module creates two synchronized interactive bar chart visualizations displayed
#' vertically. The top bar chart allows users to select segments by brushing, and the
#' bottom bar chart automatically updates to show a different categorical breakdown of
#' the selected data. Both charts use the same color coding and count variables but
#' display different categorical variables on their y-axes. This is particularly useful
#' for drill-down analysis and exploring relationships between different categorical dimensions.
#'
#' @param label (`character(1)`) Label shown in the navigation item for the module.
#' @param plot_dataname (`character(1)`) Name of the dataset to be used for plotting.
#' @param y_var (`character(1)`) Name of the categorical variable for the main (top) bar chart y-axis.
#' @param color_var (`character(1)`) Name of the categorical variable used for color coding in both charts.
#' @param count_var (`character(1)`) Name of the variable whose distinct values will be counted for bar heights in both charts.
#' @param secondary_y_var (`character(1)`) Name of the categorical variable for the secondary (bottom) bar chart y-axis.
#' @param bar_colors (`named character` or `NULL`) Valid color names or hex-colors named by levels of color_var column.
#'
#' @examples
#' data <- teal_data() |>
#'   within({
#'     df <- data.frame(
#'       adverse_event = sample(c("Headache", "Nausea", "Fatigue", "Dizziness"),
#'         150,
#'         replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.1)
#'       ),
#'       severity = sample(c("Mild", "Moderate", "Severe"), 150,
#'         replace = TRUE,
#'         prob = c(0.6, 0.3, 0.1)
#'       ),
#'       system_organ_class = sample(c("Nervous System", "Gastrointestinal", "General"),
#'         150,
#'         replace = TRUE, prob = c(0.5, 0.3, 0.2)
#'       ),
#'       subject_id = sample(paste0("S", 1:40), 150, replace = TRUE),
#'       treatment = sample(c("Active", "Placebo"), 150, replace = TRUE)
#'     )
#'
#'     # Add labels
#'     attr(df$adverse_event, "label") <- "Adverse Event Term"
#'     attr(df$severity, "label") <- "Severity Grade"
#'     attr(df$system_organ_class, "label") <- "System Organ Class"
#'     attr(df$subject_id, "label") <- "Subject ID"
#'     attr(df$treatment, "label") <- "Treatment Group"
#'   })
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_p_drilldown_bargraph(
#'       label = "SOC to Term Breakdown",
#'       plot_dataname = "df",
#'       y_var = "system_organ_class",
#'       color_var = "treatment",
#'       count_var = "subject_id",
#'       secondary_y_var = "adverse_event",
#'       tooltip_vars = c("system_organ_class", "adverse_event", "treatment"),
#'       bar_colors = c("Active" = "#E74C3C", "Placebo" = "#3498DB")
#'     )
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_p_drilldown_bargraph <- function(label = "Bar Plot",
                                    plot_dataname,
                                    y_var,
                                    color_var,
                                    count_var,
                                    secondary_y_var,
                                    tooltip_vars = NULL,
                                    bar_colors = NULL) {
  module(
    label = label,
    ui = ui_p_drilldown_bargraph,
    server = srv_p_drilldown_bargraph,
    ui_args = list(),
    server_args = list(
      plot_dataname = plot_dataname,
      y_var = y_var,
      color_var = color_var,
      count_var = count_var,
      secondary_y_var = secondary_y_var,
      tooltip_vars = tooltip_vars,
      bar_colors = bar_colors
    )
  )
}

ui_p_drilldown_bargraph <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    ui_p_bargraph(ns("main_bargraph")),
    ui_p_bargraph(ns("secondary_bargraph"))
  )
}

srv_p_drilldown_bargraph <- function(id,
                                     data,
                                     plot_dataname,
                                     y_var,
                                     color_var,
                                     count_var,
                                     secondary_y_var,
                                     tooltip_vars,
                                     bar_colors) {
  moduleServer(id, function(input, output, session) {
    plot_q <- srv_p_bargraph(
      "main_bargraph",
      data = data,
      plot_dataname = plot_dataname,
      y_var = y_var,
      color_var = color_var,
      count_var = count_var,
      tooltip_vars = tooltip_vars,
      bar_colors = bar_colors
    )

    brushed_q <- reactive({
      req(attr(plot_q(), "has_brushing"))
      plot_q()
    })

    srv_p_bargraph(
      "secondary_bargraph",
      data = brushed_q,
      plot_dataname = plot_dataname,
      y_var = secondary_y_var,
      color_var = color_var,
      count_var = count_var,
      tooltip_vars = tooltip_vars,
      bar_colors = bar_colors
    )
  })
}
