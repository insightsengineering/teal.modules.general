#' `teal` module: Swimlane plot
#'
#' Module visualizes subjects' events in time.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param plot_dataname (`character(1)` or `choices_selected`) name of the dataset which visualization is builded on.
#' @param time_var (`character(1)` or `choices_selected`) name of the `numeric` column
#' in `plot_dataname` to be used as x-axis.
#' @param subject_var (`character(1)` or `choices_selected`) name of the `factor` or `character` column
#' in `plot_dataname` to be used as y-axis.
#' @param color_var (`character(1)` or `choices_selected`) name of the `factor` or `character` column
#' in `plot_dataname` to name and color subject events in time.
#' @param group_var (`character(1)` or `choices_selected`) name of the `factor` or `character` column in `plot_dataname`
#'  to categorize type of event.
#'  (legend is sorted according to this variable, and used in toolip to display type of the event)
#'  todo: this can be fixed by ordering factor levels
#' @param sort_var (`character(1)` or `choices_selected`) name(s) of the column in `plot_dataname` which
#'  value determines order of the subjects displayed on the y-axis.
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created.
#' @param point_size (`numeric(1)` or `named numeric`) Default point size of the points in the plot.
#' If `point_size` is a named numeric vector, it should be named by levels of `color_var` column.
#' @param point_colors (`named character`) valid color names (see [colors()]) or hex-colors named
#'  by levels of `color_var` column.
#' @param point_symbols (`named character`) valid plotly symbol name named  by levels of `color_var` column.
#' @param table_datanames (`character`) Names of the datasets to be displayed in the tables below the plot.
#' @param reactable_args (`list`) Additional arguments passed to the `reactable` function for table customization.
#'
#' @examples
#' data <- teal_data() |>
#'   within({
#'     subjects <- data.frame(
#'       subject_var = c("A", "B", "C"),
#'       AGE = sample(30:100, 3),
#'       ARM = c("Combination", "Combination", "Placebo")
#'     )
#'
#'     swimlane_ds <- data.frame(
#'       subject_var = sample(c("A", "B", "C"), 10, replace = TRUE),
#'       time_var = sample(1:100, 10, replace = TRUE),
#'       color_var = sample(c("CR", "PR", "SD", "PD"), 10, replace = TRUE)
#'     )
#'   })
#' join_keys(data) <- join_keys(
#'   join_key("subjects", "swimlane_ds", keys = c(subject_var = "subject_var"))
#' )
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_p_swimlane_table(
#'       plot_dataname = "swimlane_ds",
#'       table_datanames = "subjects",
#'       time_var = "time_var",
#'       subject_var = "subject_var",
#'       color_var = "color_var",
#'       group_var = "color_var",
#'       sort_var = "time_var",
#'       plot_height = 400,
#'       point_colors = c(
#'         CR = "#FF0000", PR = "#00FF00", SD = "#0000FF", PD = "#FFFF00"
#'       ),
#'       point_symbols = c(
#'         CR = "circle", PR = "square", SD = "triangle-up", PD = "diamond"
#'       )
#'     )
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_p_swimlane_table <- function(label = "Swimlane",
                                plot_dataname,
                                time_var,
                                subject_var,
                                color_var,
                                group_var,
                                sort_var = time_var,
                                tooltip_vars = NULL,
                                point_size = 10,
                                point_colors = character(0),
                                point_symbols = character(0),
                                plot_height = c(700, 400, 1200),
                                table_datanames = character(0),
                                reactable_args = list()) {
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  if (is.character(time_var)) {
    time_var <- choices_selected(choices = time_var, selected = time_var)
  }
  if (is.character(subject_var)) {
    subject_var <- choices_selected(choices = subject_var, selected = subject_var)
  }
  if (is.character(color_var)) {
    color_var <- choices_selected(choices = color_var, selected = color_var)
  }
  if (is.character(group_var)) {
    group_var <- choices_selected(choices = group_var, selected = group_var)
  }
  if (is.character(sort_var)) {
    sort_var <- choices_selected(choices = sort_var, selected = sort_var)
  }
  module(
    label = label,
    ui = ui_p_swimlane_table,
    server = srv_p_swimlane_table,
    datanames = c(plot_dataname, table_datanames),
    ui_args = list(height = plot_height),
    server_args = list(
      plot_dataname = plot_dataname,
      time_var = time_var,
      subject_var = subject_var,
      color_var = color_var,
      group_var = group_var,
      sort_var = sort_var,
      point_size = point_size,
      point_colors = point_colors,
      point_symbols = point_symbols,
      table_datanames = table_datanames,
      reactable_args = reactable_args,
      tooltip_vars = tooltip_vars
    )
  )
}

ui_p_swimlane_table <- function(id, height) {
  ns <- NS(id)
  bslib::page_fluid(
    ui_p_swimlane(ns("swimlane"), height = height),
    ui_t_reactables(ns("subtables"))
  )
}
srv_p_swimlane_table <- function(id,
                                 data,
                                 plot_dataname,
                                 time_var,
                                 subject_var,
                                 color_var,
                                 group_var,
                                 sort_var,
                                 point_size = 10,
                                 point_colors,
                                 point_symbols,
                                 table_datanames,
                                 reactable_args = list(),
                                 tooltip_vars = NULL,
                                 filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    plot_q <- srv_p_swimlane(
      "swimlane",
      data = data,
      plot_dataname = plot_dataname,
      time_var = time_var,
      subject_var = subject_var,
      color_var = color_var,
      group_var = group_var,
      sort_var = sort_var,
      point_size = point_size,
      point_colors = point_colors,
      point_symbols = point_symbols,
      show_widgets = FALSE
    )

    filtered_data_q <- reactive({
      req(plot_q())
      plot_q() |>
        within(
          {
            table_names <- c("recist_listing")
            for (table_name in table_names) {
              current_table <- get(table_name)
              filtered_table <- current_table |>
                dplyr::filter(!!sym(subject_var) %in% plot_dataname[[subject_var]])
              assign(table_name, filtered_table)
            }
          },
          plot_dataname = str2lang(plot_dataname),
          subject_var = subject_var$selected
        )
    })
    srv_t_reactables(
      "subtables",
      data = filtered_data_q,
      filter_panel_api = filter_panel_api,
      datanames = table_datanames,
      reactable_args = reactable_args
    )
  })
}
