#' Spaghetti Plot Module
#'
#' This module creates an interactive spaghetti plot visualization that shows individual
#' trajectories for each group over time. Each trajectory is represented by connected
#' points and lines, creating a "spaghetti-like" appearance. The plot supports customizable
#' tooltips and color coding by categorical variables. Users can select points by brushing
#' to filter the underlying data.
#'
#' @inheritParams teal::module
#' @param plot_dataname (`character(1)`) Name of the dataset to be used for plotting.
#' @param group_var (`character(1)`) Name of the grouping variable that defines individual trajectories.
#' @param x_var (`character(1)`) Name of the variable to be used for x-axis (typically time).
#' @param y_var (`character(1)`) Name of the variable to be used for y-axis (typically a measurement).
#' @param color_var (`character(1)`) Name of the variable to be used for coloring points and lines.
#' @param point_colors (`named character` or `NULL`) Valid color names or hex-colors named by levels of `color_var` column.
#' If `NULL`, default colors will be used.
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing group, x, y, and color variables.
#' @param transformators (`list`) Named list of transformator functions.
#' @param show_widgets (`logical(1)`) Whether to show module widgets.
#'
#' @inherit shared_params return
#'
#' @examples
#' data <- teal_data() |>
#'   within({
#'     df <- data.frame(
#'       subject_id = rep(paste0("S", 1:10), each = 4),
#'       time_point = rep(c(0, 30, 60, 90), 10),
#'       response = rnorm(40, 15, 3) + rep(c(0, 2, 4, 6), 10),
#'       treatment = rep(c("Active", "Placebo"), each = 20),
#'       age_group = rep(c("Young", "Old"), 20)
#'     )
#'
#'     # Add labels
#'     attr(df$subject_id, "label") <- "Subject ID"
#'     attr(df$time_point, "label") <- "Time Point (days)"
#'     attr(df$response, "label") <- "Response Score"
#'     attr(df$treatment, "label") <- "Treatment Group"
#'   })
#'
#' # Default tooltip example
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_p_spaghetti(
#'       label = "Spaghetti Plot",
#'       plot_dataname = "df",
#'       group_var = "subject_id",
#'       x_var = "time_point",
#'       y_var = "response",
#'       color_var = "treatment",
#'       tooltip_vars = c("subject_id", "treatment")
#'     )
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_p_spaghetti <- function(label = "Spaghetti Plot",
                           plot_dataname,
                           group_var,
                           x_var,
                           y_var,
                           color_var,
                           point_colors = character(0),
                           tooltip_vars = NULL,
                           transformators = list(),
                           show_widgets = TRUE) {
  module(
    label = label,
    ui = ui_p_spaghetti,
    server = srv_p_spaghetti,
    ui_args = list(),
    server_args = list(
      plot_dataname = plot_dataname,
      group_var = group_var,
      x_var = x_var,
      y_var = y_var,
      color_var = color_var,
      point_colors = point_colors,
      tooltip_vars = tooltip_vars,
      show_widgets = show_widgets
    ),
    transformators = transformators
  )
}

ui_p_spaghetti <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    shinyjs::useShinyjs(),
    tags$div(
      tags$span(id = ns("colors_span"), colour_picker_ui(ns("colors"))),
      bslib::card(
        full_screen = TRUE,
        tags$div(
          trigger_tooltips_deps(),
          plotly::plotlyOutput(ns("plot"), height = "100%")
        )
      )
    )
  )
}

srv_p_spaghetti <- function(id,
                            data,
                            plot_dataname,
                            group_var,
                            x_var,
                            y_var,
                            color_var,
                            point_colors,
                            tooltip_vars = NULL,
                            show_widgets) {
  moduleServer(id, function(input, output, session) {
    color_inputs <- colour_picker_srv(
      "colors",
      x = reactive({
        data()[[plot_dataname]][[color_var]]
      }),
      default_colors = point_colors
    )

    if (!show_widgets) {
      shinyjs::hide("colors_span")
    }

    plotly_q <- reactive({
      req(color_inputs())
      data() |>
        within(
          code,
          code = spaghettiplotly(
            df = plot_dataname,
            group_var = group_var,
            x_var = x_var,
            y_var = y_var,
            color_var = color_var,
            colors = color_inputs(),
            source = session$ns("spaghetti"),
            tooltip_vars = tooltip_vars
          )
        )
    })


    output$plot <- plotly::renderPlotly(
      plotly_q()$p |>
        setup_trigger_tooltips(session$ns) |>
        set_plot_data(session$ns("plot_data")) |>
        plotly::event_register("plotly_selected")
    )

    plotly_selected <- reactive(
      plotly::event_data("plotly_selected", source = session$ns("spaghetti"))
    )
    reactive({
      if (is.null(plotly_selected()) || is.null(group_var)) {
        plotly_q()
      } else {
        q <- plotly_q() |>
          within(
            {
              selected_plot_data <- plot_data |>
                dplyr::filter(customdata %in% plotly_selected_customdata)
              df <- df |>
                dplyr::filter(!!as.name(group_var_string) %in% selected_plot_data[[group_var_string]])
            },
            df = str2lang(plot_dataname),
            group_var_string = group_var,
            plotly_selected_customdata = plotly_selected()$customdata
          )
        attr(q, "has_brushing") <- TRUE
        q
      }
    })
  })
}

#' Generate Spaghetti Plotly Code
#'
#' Creates code expression that generates a spaghetti plot with tooltips using plotly.
#' This function includes all the data manipulation and plot creation logic
#' from tm_p_spaghetti module, including label extraction, tooltip generation,
#' line segments creation, and event registration.
#'
#' @param df (`character(1)`) Name of the data frame to plot
#' @param group_var (`character(1)`) Name of the grouping variable that defines individual trajectories
#' @param x_var (`character(1)`) Name of the variable to be used for x-axis (typically time)
#' @param y_var (`character(1)`) Name of the variable to be used for y-axis (typically a measurement)
#' @param color_var (`character(1)`) Name of the variable to be used for coloring points and lines
#' @param colors (`character`) Named vector of colors for color_var levels
#' @param source (`character(1)`) Source identifier for plotly events
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing group, x, y, and color variables.
#'
#' @return A code expression that when evaluated creates a plotly plot object
#'
#' @examples
#' # Generate code for a spaghetti plot
#' code <- spaghettiplotly(
#'   df = "longitudinal_data",
#'   group_var = "subject_id",
#'   x_var = "time_point",
#'   y_var = "response",
#'   color_var = "treatment",
#'   colors = c("Active" = "red", "Placebo" = "blue"),
#'   source = "spaghetti",
#'   tooltip_vars = c("subject_id", "treatment")
#' )
#'
spaghettiplotly <- function(df, group_var, x_var, y_var, color_var, colors, source, tooltip_vars = NULL) {
  substitute(
    {
      group_var_label <- attr(df_sym[[group_var_str]], "label")
      if (!length(group_var_label)) group_var_label <- group_var_str

      x_var_label <- attr(df_sym[[x_var_str]], "label")
      if (!length(x_var_label)) x_var_label <- x_var_str

      y_var_label <- attr(df_sym[[y_var_str]], "label")
      if (!length(y_var_label)) y_var_label <- y_var_str

      color_var_label <- attr(df_sym[[color_var_str]], "label")
      if (!length(color_var_label)) color_var_label <- color_var_str

      plot_data <- df_sym |>
        dplyr::select(!!as.name(group_var_str), !!as.name(x_var_str), !!as.name(y_var_str), !!as.name(color_var_str)) |>
        dplyr::mutate(!!as.name(color_var_str) := factor(!!as.name(color_var_str), levels = names(colors_sym))) %>%
        dplyr::mutate(customdata = dplyr::row_number()) |>
        dplyr::mutate(
          tooltip = {
            if (is.null(tooltip_vars_sym)) {
              paste(
                paste(group_var_label, ":", !!as.name(group_var_str)),
                paste(x_var_label, ":", !!as.name(x_var_str)),
                paste(y_var_label, ":", !!as.name(y_var_str)),
                paste(color_var_label, ":", !!as.name(color_var_str)),
                sep = "<br>"
              )
            } else {
              cur_data <- dplyr::cur_data()
              cols <- intersect(tooltip_vars_sym, names(cur_data))
              if (!length(cols)) {
                paste(
                  paste(group_var_label, ":", !!as.name(group_var_str)),
                  paste(x_var_label, ":", !!as.name(x_var_str)),
                  paste(y_var_label, ":", !!as.name(y_var_str)),
                  paste(color_var_label, ":", !!as.name(color_var_str)),
                  sep = "<br>"
                )
              } else {
                sub <- cur_data[cols]
                labels <- vapply(cols, function(cn) {
                  if (cn == group_var_str) {
                    lb <- group_var_label
                  } else if (cn == x_var_str) {
                    lb <- x_var_label
                  } else if (cn == y_var_str) {
                    lb <- y_var_label
                  } else if (cn == color_var_str) {
                    lb <- color_var_label
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
        )

      segments_df <- plot_data %>%
        dplyr::arrange(!!as.name(group_var_str), !!as.name(x_var_str)) %>%
        dplyr::group_by(!!as.name(group_var_str)) %>%
        dplyr::mutate(
          x = !!as.name(x_var_str),
          y = !!as.name(y_var_str),
          xend = dplyr::lead(!!as.name(x_var_str)),
          yend = dplyr::lead(!!as.name(y_var_str)),
          color_var_seg = dplyr::lead(!!as.name(color_var_str))
        ) %>%
        dplyr::filter(!is.na(xend))

      p <- plotly::plot_ly(
        data = segments_df,
        customdata = ~customdata,
        source = source_sym
      ) %>%
        plotly::add_segments(
          x = ~x, y = ~y,
          xend = ~xend, yend = ~yend,
          color = ~color_var_seg,
          colors = colors_sym,
          showlegend = TRUE
        ) %>%
        plotly::add_markers(
          data = plot_data,
          x = ~x_var_sym,
          y = ~y_var_sym,
          color = ~color_var_sym,
          colors = colors_sym,
          text = ~tooltip,
          hoverinfo = "text"
        ) |>
        plotly::layout(dragmode = "select")
    },
    list(
      df_sym = str2lang(df),
      group_var_sym = str2lang(group_var),
      x_var_sym = str2lang(x_var),
      y_var_sym = str2lang(y_var),
      color_var_sym = str2lang(color_var),
      group_var_str = group_var,
      x_var_str = x_var,
      y_var_str = y_var,
      color_var_str = color_var,
      colors_sym = colors,
      source_sym = source,
      tooltip_vars_sym = tooltip_vars
    )
  )
}
