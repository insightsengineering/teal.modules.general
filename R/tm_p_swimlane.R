#' Swimlane Plot Module
#'
#' This module creates an interactive swimlane plot visualization that displays subjects' events
#' over time. Each subject is represented by a horizontal lane, with events plotted as points
#' along the timeline. The plot supports color coding and symbol differentiation for different
#' event types, customizable sorting of subjects, and interactive tooltips. This visualization
#' is particularly useful for showing temporal sequences of events across multiple subjects.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param plot_dataname (`character(1)`) Name of the dataset to be used for plotting.
#' @param time_var (`character(1)`) Name of the numeric column in `plot_dataname` to be used as x-axis.
#' @param subject_var (`character(1)`) Name of the factor or character column in `plot_dataname`
#' to be used as y-axis (subject lanes).
#' @param color_var (`character(1)`) Name of the factor or character column in `plot_dataname`
#' to name and color subject events in time.
#' @param group_var (`character(1)`) Name of the factor or character column in `plot_dataname`
#' to categorize type of event. Legend is sorted according to this variable.
#' @param sort_var (`character(1)`) Name of the column in `plot_dataname` whose values determine
#' the order of subjects displayed on the y-axis.
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing subject, time, color, and group variables.
#' @param point_size (`numeric(1)` or `named numeric`) Default point size of the points in the plot.
#' If `point_size` is a named numeric vector, it should be named by levels of `color_var` column.
#' @param point_colors (`named character` or `NULL`) Valid color names or hex-colors named by levels of `color_var` column.
#' If `NULL`, default colors will be used.
#' @param point_symbols (`named character` or `NULL`) Valid plotly symbol names named by levels of `color_var` column.
#' If `NULL`, default symbols will be used.
#' @param plot_height (`numeric(3)`) Vector of length 3 with c(default, min, max) plot height values.
#' @param show_widgets (`logical(1)`) Whether to show module widgets.
#'
#' @inherit shared_params return
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
#'       time_var = "time_var",
#'       subject_var = "subject_var",
#'       color_var = "color_var",
#'       group_var = "color_var",
#'       sort_var = "time_var",
#'       plot_height = c(700, 400, 1200),
#'       tooltip_vars = c("subject_var", "color_var"),
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
                          show_widgets = TRUE) {
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
    datanames = c(plot_dataname),
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
      tooltip_vars = tooltip_vars,
      show_widgets = show_widgets
    )
  )
}

ui_p_swimlane <- function(id, height) {
  ns <- NS(id)
  bslib::page_fluid(
    tags$div(
      shinyjs::useShinyjs(),
      tags$div(
        id = ns("top_widgets"),
        style = "display: flex;",
        selectInput(ns("subject_var"), label = "Color by:", choices = NULL, selected = NULL, multiple = FALSE),
        selectInput(ns("color_var"), label = "Color by:", choices = NULL, selected = NULL, multiple = FALSE),
        selectInput(ns("group_var"), label = "Group by:", choices = NULL, selected = NULL, multiple = FALSE),
        selectInput(ns("sort_var"), label = "Sort by:", choices = NULL, selected = NULL, multiple = FALSE),
        colour_picker_ui(ns("colors")),
        sliderInput(ns("plot_height"), "Plot Height (px)", height[2], height[3], height[1])
      ),
      bslib::card(
        full_screen = TRUE,
        tags$div(
          trigger_tooltips_deps(),
          plotly::plotlyOutput(ns("plot"), height = "100%"),
        )
      ),
      tags$div(
        id = ns("bottom_widgets"),
        selectInput(ns("time_var"), label = "Time variable:", choices = NULL, selected = NULL, multiple = FALSE)
      )
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
                           tooltip_vars = NULL,
                           filter_panel_api,
                           show_widgets) {
  moduleServer(id, function(input, output, session) {
    .update_cs_input(inputId = "time_var", data = reactive(data()[[dataname]]), cs = time_var)
    .update_cs_input(inputId = "subject_var", data = reactive(data()[[dataname]]), cs = subject_var)
    .update_cs_input(inputId = "color_var", data = reactive(data()[[dataname]]), cs = color_var)
    .update_cs_input(inputId = "group_var", data = reactive(data()[[dataname]]), cs = group_var)
    .update_cs_input(inputId = "sort_var", data = reactive(data()[[dataname]]), cs = sort_var)

    if (!show_widgets) {
      shinyjs::hide("top_widgets")
      shinyjs::hide("bottom_widgets")
    }

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
      data() |>
        within(
          code,
          code = swimlaneplotly(
            df = plot_dataname,
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
            source = session$ns("swimlane")
          )
        )
    })

    output$plot <- plotly::renderPlotly({
      plotly_q()$p |>
        set_plot_data(session$ns("plot_data")) |>
        setup_trigger_tooltips(session$ns) |>
        plotly::event_register("plotly_selected")
    })
  })
}

#' Generate Swimlane Plotly Code
#'
#' Creates code expression that generates a swimlane plot with tooltips using plotly.
#' This function includes all the data manipulation and plot creation logic
#' from tm_p_swimlane module, including subject sorting, label extraction, tooltip generation,
#' marker and segment creation, and event registration.
#'
#' @param df (`character(1)`) Name of the data frame to plot
#' @param time_var (`character(1)`) Name of the numeric column to be used as x-axis
#' @param subject_var (`character(1)`) Name of the factor or character column to be used as y-axis (subject lanes)
#' @param color_var (`character(1)`) Name of the factor or character column to name and color subject events in time
#' @param group_var (`character(1)`) Name of the factor or character column to categorize type of event
#' @param sort_var (`character(1)`) Name of the column whose values determine the order of subjects displayed on the y-axis
#' @param point_size (`numeric(1)` or `named numeric`) Default point size or named vector of sizes by color_var levels
#' @param colors (`character`) Named vector of colors for color_var levels
#' @param symbols (`character`) Named vector of plotly symbols for color_var levels
#' @param height (`numeric(1)`) Plot height in pixels
#' @param source (`character(1)`) Source identifier for plotly events
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing subject, time, and group variables.
#'
#' @return A code expression that when evaluated creates a plotly plot object
#'
#' @examples
#' # Generate code for a swimlane plot
#' code <- swimlaneplotly(
#'   df = "swimlane_data",
#'   time_var = "time_point",
#'   subject_var = "subject_id",
#'   color_var = "event_type",
#'   group_var = "event_category",
#'   sort_var = "time_point",
#'   point_size = 10,
#'   colors = c("Event1" = "red", "Event2" = "blue"),
#'   symbols = c("Event1" = "circle", "Event2" = "square"),
#'   height = 700,
#'   source = "swimlane",
#'   tooltip_vars = c("subject_id", "event_type")
#' )
#'
swimlaneplotly <- function(df, time_var, subject_var, color_var, group_var, sort_var,
                           point_size = 10, colors, symbols, height = 700, source, tooltip_vars = NULL) {
  substitute(
    {
      subject_var_label <- attr(df_sym[[subject_var_str]], "label")
      if (!length(subject_var_label)) subject_var_label <- subject_var_str
      time_var_label <- attr(df_sym[[time_var_str]], "label")
      if (!length(time_var_label)) time_var_label <- time_var_str

      plot_data <- df_sym |>
        dplyr::mutate(customdata = dplyr::row_number())

      subject_levels <- plot_data %>%
        dplyr::group_by(!!as.name(subject_var_str)) %>%
        dplyr::summarize(v = max(!!as.name(sort_var_str))) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(v) %>%
        dplyr::pull(!!as.name(subject_var_str))
      plot_data[[subject_var_str]] <- factor(plot_data[[subject_var_str]], levels = subject_levels)

      min_size <- min(point_size_sym, na.rm = TRUE)

      if (length(point_size_sym) > 1) {
        plot_data <- plot_data %>%
          dplyr::mutate(
            size_var = ifelse(
              as.character(!!as.name(color_var_str)) %in% names(point_size_sym),
              point_size_sym[as.character(!!as.name(color_var_str))],
              min_size
            )
          )
      } else {
        plot_data <- plot_data %>%
          dplyr::mutate(size_var = point_size_sym)
      }

      p <- plot_data %>%
        dplyr::mutate(
          !!as.name(color_var_str) := {
            original_label <- attr(.data[[color_var_str]], "label")
            new_factor <- factor(.data[[color_var_str]], levels = names(colors_sym))
            attr(new_factor, "label") <- original_label
            new_factor
          }
        ) %>%
        dplyr::group_by(!!as.name(subject_var_str), !!as.name(time_var_str)) %>%
        dplyr::mutate(
          tooltip = {
            default_tip <- paste(
              unique(
                c(
                  paste(subject_var_label, !!as.name(subject_var_str)),
                  paste(time_var_label, !!as.name(time_var_str)),
                  sprintf(
                    "%s: %s",
                    tools::toTitleCase(gsub("[^0-9A-Za-z]+", " ", !!as.name(group_var_str))),
                    !!as.name(color_var_str)
                  )
                )
              ),
              collapse = "<br>"
            )
            if (is.null(tooltip_vars_sym)) {
              default_tip
            } else {
              cur_data <- dplyr::cur_data()
              grouping_vars <- list()
              grouping_vars[[subject_var_str]] <- dplyr::cur_group()[[subject_var_str]]
              grouping_vars[[time_var_str]] <- dplyr::cur_group()[[time_var_str]]
              cur_data <- c(cur_data, grouping_vars)

              cols <- intersect(tooltip_vars_sym, names(cur_data))
              if (!length(cols)) {
                default_tip
              } else {
                sub <- cur_data[cols]
                labels <- vapply(cols, function(cn) {
                  if (cn == subject_var_str) {
                    lb <- subject_var_label
                  } else if (cn == time_var_str) {
                    lb <- time_var_label
                  } else {
                    lb <- attr(sub[[cn]], "label")
                  }
                  if (length(lb) && !is.null(lb) && !is.na(lb)) as.character(lb) else cn
                }, character(1))
                values <- lapply(sub, as.character)
                parts <- Map(function(v, l) paste0(l, ": ", v), values, labels)
                do.call(paste, c(parts, sep = "<br>"))
              }
            }
          }
        ) %>%
        dplyr::ungroup() %>%
        plotly::plot_ly(
          source = source_sym,
          colors = colors_sym,
          symbols = symbols_sym,
          height = height_sym,
          customdata = ~customdata
        ) %>%
        plotly::add_markers(
          x = ~time_var_sym,
          y = ~subject_var_sym,
          color = ~color_var_sym,
          symbol = ~color_var_sym,
          size = ~size_var,
          text = ~tooltip,
          hoverinfo = "text"
        ) %>%
        plotly::add_segments(
          x = ~0,
          xend = ~study_day,
          y = ~subject_var_sym,
          yend = ~subject_var_sym,
          data = plot_data |>
            dplyr::group_by(!!as.name(subject_var_str), !!as.name(group_var_str)) |>
            dplyr::summarise(study_day = max(!!as.name(time_var_str))),
          line = list(width = 2, color = "grey"),
          showlegend = FALSE,
          customdata = NULL
        ) %>%
        plotly::layout(
          xaxis = list(title = time_var_label),
          yaxis = list(title = subject_var_label)
        ) %>%
        plotly::layout(dragmode = "select") %>%
        plotly::config(displaylogo = FALSE)
    },
    list(
      df_sym = str2lang(df),
      time_var_sym = str2lang(time_var),
      subject_var_sym = str2lang(subject_var),
      color_var_sym = str2lang(color_var),
      group_var_sym = str2lang(group_var),
      sort_var_sym = str2lang(sort_var),
      time_var_str = time_var,
      subject_var_str = subject_var,
      color_var_str = color_var,
      group_var_str = group_var,
      sort_var_str = sort_var,
      point_size_sym = point_size,
      colors_sym = colors,
      symbols_sym = symbols,
      height_sym = height,
      source_sym = source,
      tooltip_vars_sym = tooltip_vars
    )
  )
}
