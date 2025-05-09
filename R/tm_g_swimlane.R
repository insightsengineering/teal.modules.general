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
#' @param size_var (`character(1)` or `NULL`) If provided, this numeric column from the `plot_dataname`
#' will be used to determine the size of the points. If `NULL`, a fixed size based on the `point_size` is used.
#' @param point_size (`numeric(1)`) Default point size of the points in the plot.
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
#'     tm_g_swimlane(
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
tm_g_swimlane <- function(label = "Swimlane",
                          plot_dataname,
                          time_var,
                          subject_var,
                          color_var,
                          group_var,
                          sort_var = NULL,
                          tooltip_vars = NULL,
                          size_var = NULL,
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
    ui = ui_g_swimlane,
    server = srv_g_swimlane,
    datanames = c(plot_dataname, table_datanames),
    ui_args = list(height = plot_height),
    server_args = list(
      plot_dataname = plot_dataname,
      time_var = time_var,
      subject_var = subject_var,
      color_var = color_var,
      group_var = group_var,
      sort_var = sort_var,
      size_var = size_var,
      point_size = point_size,
      point_colors = point_colors,
      point_symbols = point_symbols,
      table_datanames = table_datanames,
      reactable_args = reactable_args,
      tooltip_vars = tooltip_vars
    )
  )
}

ui_g_swimlane <- function(id, height) {
  ns <- NS(id)
  bslib::page_sidebar(
    sidebar = div(
      selectInput(ns("time_var"), label = "Time variable:", choices = NULL, selected = NULL, multiple = FALSE),
      selectInput(ns("subject_var"), label = "Color by:", choices = NULL, selected = NULL, multiple = FALSE),
      selectInput(ns("color_var"), label = "Color by:", choices = NULL, selected = NULL, multiple = FALSE),
      selectInput(ns("group_var"), label = "Group by:", choices = NULL, selected = NULL, multiple = FALSE),
      selectInput(ns("sort_var"), label = "Sort by:", choices = NULL, selected = NULL, multiple = FALSE),
      colour_picker_ui(ns("colors")),
      sliderInput(ns("plot_height"), "Plot Height (px)", height[2], height[3], height[1])
    ),
    tags$div(
      bslib::card(
        full_screen = TRUE,
        tags$div(
          ui_trigger_tooltips(ns("show_tooltips")),
          plotly::plotlyOutput(ns("plot"), height = "100%")
        )
      ),
      ui_t_reactables(ns("subtables"))
    )
  )
}
srv_g_swimlane <- function(id,
                           data,
                           plot_dataname,
                           time_var,
                           subject_var,
                           color_var,
                           group_var,
                           sort_var = time_var,
                           size_var = NULL,
                           point_size = 10,
                           point_colors,
                           point_symbols,
                           table_datanames,
                           reactable_args = list(),
                           tooltip_vars = NULL,
                           filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    .update_cs_input(inputId = "time_var", data = reactive(data()[[dataname]]), cs = time_var)
    .update_cs_input(inputId = "subject_var", data = reactive(data()[[dataname]]), cs = subject_var)
    .update_cs_input(inputId = "color_var", data = reactive(data()[[dataname]]), cs = color_var)
    .update_cs_input(inputId = "group_var", data = reactive(data()[[dataname]]), cs = group_var)
    .update_cs_input(inputId = "sort_var", data = reactive(data()[[dataname]]), cs = sort_var)

    color_inputs <- colour_picker_srv(
      "colors",
      x = reactive({
        req(input$color_var)
        data()[[plot_dataname]][[input$color_var]]
      }),
      default_colors = point_colors
    )

    plotly_q <- reactive({
      req(data(), input$time_var, input$subject_var, input$color_var, input$group_var, input$sort_var, color_inputs())
      adjusted_symbols <- .shape_palette_discrete(
        levels = unique(data()[[plot_dataname]][[input$color_var]]),
        symbol = point_symbols
      )
      within(
        data(),
        dataname = str2lang(plot_dataname),
        time_var = input$time_var,
        subject_var = input$subject_var,
        color_var = input$color_var,
        group_var = input$group_var,
        sort_var = input$sort_var,
        size_var = size_var,
        point_size = point_size,
        colors = color_inputs(),
        symbols = adjusted_symbols,
        height = input$plot_height,
        tooltip_vars = tooltip_vars,
        expr = {
          p <- swimlanely(
            data = dataname,
            time_var = time_var,
            subject_var = subject_var,
            color_var = color_var,
            group_var = group_var,
            sort_var = sort_var,
            size_var = size_var,
            point_size = point_size,
            colors = colors,
            symbols = symbols,
            height = height,
            tooltip_vars = tooltip_vars
          )
        }
      )
    })

    output$plot <- plotly::renderPlotly(plotly::event_register(plotly_q()$p, "plotly_selected"))

    plotly_selected <- reactive({
      plotly::event_data("plotly_deselect", source = "swimlane") # todo: deselect doesn't work
      plotly::event_data("plotly_selected", source = "swimlane")
    })

    srv_trigger_tooltips("show_tooltips", plotly_selected, session$ns("plot"))

    tables_selected_q <- .plotly_selected_filter_children(
      data = plotly_q,
      plot_dataname = plot_dataname,
      xvar = reactive(input$time_var),
      yvar = reactive(input$subject_var),
      plotly_selected = plotly_selected,
      children_datanames = table_datanames
    )

    srv_t_reactables(
      "subtables",
      data = tables_selected_q,
      datanames = table_datanames,
      reactable_args = reactable_args
    )
  })
}


# todo: export is temporary, this should go to a new package teal.graphs or another bird species
#' @export
swimlanely <- function(
    data, time_var, subject_var, color_var, group_var, sort_var,
    colors, symbols, height, tooltip_vars = NULL, size_var = NULL, point_size = 10) {
  subject_var_label <- .get_column_label(data, subject_var)
  time_var_label <- .get_column_label(data, time_var)

  if (is.null(size_var)) {
    size <- point_size
  } else {
    size <- stats::as.formula(sprintf("~%s", size_var))
  }

  # forcats::fct_reorder doesn't seem to work here
  subject_levels <- data %>%
    dplyr::group_by(!!as.name(subject_var)) %>%
    dplyr::summarize(v = max(!!as.name(sort_var))) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(v) %>%
    dplyr::pull(!!as.name(subject_var))
  data[[subject_var]] <- factor(data[[subject_var]], levels = subject_levels)

  data %>%
    dplyr::mutate(
      !!as.name(color_var) := factor(!!as.name(color_var), levels = names(colors)),
    ) %>%
    dplyr::group_by(!!as.name(subject_var), !!as.name(time_var)) %>%
    dplyr::mutate(
      tooltip = {
        if (is.null(tooltip_vars)) {
          paste(
            unique(
              c(
                paste(subject_var_label, !!as.name(subject_var)),
                paste(time_var_label, !!as.name(time_var)),
                sprintf(
                  "%s: %s",
                  tools::toTitleCase(gsub("[^0-9A-Za-z]+", " ", !!as.name(group_var))),
                  !!as.name(color_var)
                )
              )
            ),
            collapse = "<br>"
          )
        } else {
          .generate_tooltip(.data, tooltip_vars)
        }
      }
    ) %>%
    plotly::plot_ly(
      source = "swimlane",
      colors = colors,
      symbols = symbols,
      height = height
    ) %>%
    plotly::add_markers(
      x = stats::as.formula(sprintf("~%s", time_var)),
      y = stats::as.formula(sprintf("~%s", subject_var)),
      color = stats::as.formula(sprintf("~%s", color_var)),
      symbol = stats::as.formula(sprintf("~%s", color_var)),
      size = size,
      text = ~tooltip,
      hoverinfo = "text"
    ) %>%
    plotly::add_segments(
      x = ~0,
      xend = ~study_day,
      y = stats::as.formula(sprintf("~%s", subject_var)),
      yend = stats::as.formula(sprintf("~%s", subject_var)),
      data = data |>
        dplyr::group_by(!!as.name(subject_var), !!as.name(group_var)) |>
        dplyr::summarise(study_day = max(!!as.name(time_var))),
      line = list(width = 2, color = "grey"),
      showlegend = FALSE
    ) %>%
    plotly::layout(
      xaxis = list(title = time_var_label),
      yaxis = list(title = subject_var_label)
    ) %>%
    plotly::layout(dragmode = "select") %>%
    plotly::config(displaylogo = FALSE)
}
