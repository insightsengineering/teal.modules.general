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
#'     tm_p_swimlane(
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
tm_p_swimlane <- function(label = "Swimlane",
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
    ui = ui_p_swimlane,
    server = srv_p_swimlane,
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

ui_p_swimlane <- function(id, height) {
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
          trigger_tooltips_deps(),
          plotly::plotlyOutput(ns("plot"), height = "100%")
        )
      ),
      ui_t_reactables(ns("subtables"))
    )
  )
}
srv_p_swimlane <- function(id,
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
      print(input$subject_var)
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
        point_size = point_size,
        colors = color_inputs(),
        symbols = adjusted_symbols,
        height = input$plot_height,
        tooltip_vars = tooltip_vars,
        expr = {
          subject_var_label <- attr(dataname[[subject_var]], "label")
          if (!length(subject_var_label)) subject_var_label <- subject_var
          time_var_label <- attr(dataname[[time_var]], "label")
          if (!length(time_var_label)) time_var_label <- time_var
          plot_data <- dataname |>
            dplyr::mutate(customdata = dplyr::row_number())

          # forcats::fct_reorder doesn't seem to work here
          subject_levels <- plot_data %>%
            dplyr::group_by(!!as.name(subject_var)) %>%
            dplyr::summarize(v = max(!!as.name(sort_var))) %>%
            dplyr::ungroup() %>%
            dplyr::arrange(v) %>%
            dplyr::pull(!!as.name(subject_var))
          plot_data[[subject_var]] <- factor(plot_data[[subject_var]], levels = subject_levels)

          min_size <- min(point_size, na.rm = TRUE)

          if (length(point_size) > 1) {
            plot_data <- plot_data %>%
              dplyr::mutate(
                size_var = ifelse(
                  as.character(color_var) %in% names(point_size),
                  point_size[as.character(color_var)],
                  min_size
                )
              )
          } else {
            plot_data <- plot_data %>%
              dplyr::mutate(size_var = point_size)
          }

          p <- plot_data %>%
            dplyr::mutate(
              !!as.name(color_var) := factor(!!as.name(color_var), levels = names(colors)),
            ) %>%
            dplyr::group_by(!!as.name(subject_var), !!as.name(time_var)) %>%
            dplyr::mutate(
              tooltip = {
                default_tip <- paste(
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
                if (is.null(tooltip_vars)) {
                  default_tip
                } else {
                  cur_data <- dplyr::pick(dplyr::everything())
                  cols <- intersect(tooltip_vars, names(cur_data))
                  if (!length(cols)) {
                    default_tip
                  } else {
                    sub <- cur_data[cols]
                    labels <- vapply(cols, function(cn) {
                      lb <- attr(sub[[cn]], "label")
                      if (length(lb) && !is.null(lb) && !is.na(lb)) as.character(lb) else cn
                    }, character(1))
                    values <- lapply(sub, as.character)
                    parts <- Map(function(v, l) paste0(l, ": ", v), values, labels)
                    do.call(paste, c(parts, sep = "<br>"))
                  }
                }
              }
            ) %>%
            plotly::plot_ly(
              source = "swimlane",
              colors = colors,
              symbols = symbols,
              height = height,
              customdata = ~customdata
            ) %>%
            plotly::add_markers(
              x = stats::as.formula(sprintf("~%s", time_var)),
              y = stats::as.formula(sprintf("~%s", subject_var)),
              color = stats::as.formula(sprintf("~%s", color_var)),
              symbol = stats::as.formula(sprintf("~%s", color_var)),
              size = ~size_var,
              text = ~tooltip,
              hoverinfo = "text"
            ) %>%
            plotly::add_segments(
              x = ~0,
              xend = ~study_day,
              y = stats::as.formula(sprintf("~%s", subject_var)),
              yend = stats::as.formula(sprintf("~%s", subject_var)),
              data = plot_data |>
                dplyr::group_by(!!as.name(subject_var), !!as.name(group_var)) |>
                dplyr::summarise(study_day = max(!!as.name(time_var))),
              line = list(width = 2, color = "grey"),
              showlegend = FALSE,
              customdata = NULL
            ) %>%
            plotly::layout(
              xaxis = list(title = time_var_label),
              yaxis = list(title = subject_var_label)
            ) %>%
            plotly::layout(dragmode = "select", title = title) %>%
            plotly::config(displaylogo = FALSE) %>%
            plotly::layout(title = title)
        }
      )
    })

    output$plot <- plotly::renderPlotly(plotly::event_register(
      {
        plotly_q()$p |>
          set_plot_data(session$ns("plot_data")) |>
          setup_trigger_tooltips(session$ns)
      },
      "plotly_selected"
    ))

    plotly_data <- reactive({
      data.frame(
        x = unlist(input$plot_data$x),
        y = unlist(input$plot_data$y),
        customdata = unlist(input$plot_data$customdata),
        curve = unlist(input$plot_data$curveNumber),
        index = unlist(input$plot_data$pointNumber)
      )
    })

    plotly_selected <- reactive({
      plotly::event_data("plotly_deselect", source = "swimlane") # todo: deselect doesn't work
      plotly::event_data("plotly_selected", source = "swimlane")
    })

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
