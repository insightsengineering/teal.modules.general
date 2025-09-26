#' Line Plot Module
#'
#' This module creates an interactive line plot visualization that connects data points
#' within groups to show trends over time. The plot displays both line segments connecting
#' points and individual markers, with support for customizable tooltips and color coding.
#' Optional reference lines can be added to highlight specific values. The plot can be
#' activated by brushing events from other plots when used in combination modules.
#'
#' @inheritParams teal::module
#' @param plot_dataname (`character(1)`) Name of the dataset to be used for plotting.
#' @param x_var (`character(1)`) Name of the variable to be used for x-axis (typically time).
#' @param y_var (`character(1)`) Name of the variable to be used for y-axis (typically a measurement).
#' @param color_var (`character(1)`) Name of the variable to be used for coloring points and lines.
#' @param group_var (`character(1)`) Name of the grouping variable that defines which points to connect with lines.
#' @param colors (`named character` or `NULL`) Valid color names or hex-colors named by levels of `color_var` column.
#' If `NULL`, default colors will be used.
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing group, x, y, and color variables.
#' @param transformators (`list`) Named list of transformator functions.
#' @param reference_lines (`list` or `NULL`) Reference lines specification for adding horizontal reference lines.
#' @param activate_on_brushing (`logical(1)`) Whether to activate the plot only when brushing occurs in another plot.
#'
#' @inherit shared_params return
#'
#' @examples
#' data <- teal_data() |>
#'   within({
#'     df <- data.frame(
#'       subject_id = rep(paste0("S", 1:8), each = 5),
#'       time_week = rep(c(0, 2, 4, 6, 8), 8),
#'       measurement = rnorm(40, 20, 4) + rep(c(0, 1, 2, 3, 4), 8),
#'       treatment = rep(c("Active", "Placebo"), each = 20),
#'       baseline = rep(rnorm(8, 18, 2), each = 5)
#'     )
#'
#'     # Add labels
#'     attr(df$subject_id, "label") <- "Subject ID"
#'     attr(df$time_week, "label") <- "Time (weeks)"
#'     attr(df$measurement, "label") <- "Measurement Value"
#'     attr(df$treatment, "label") <- "Treatment Group"
#'     attr(df$baseline, "label") <- "Baseline Value"
#'   })
#'
#' # Basic line plot example
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_p_lineplot(
#'       label = "Line Plot",
#'       plot_dataname = "df",
#'       x_var = "time_week",
#'       y_var = "measurement",
#'       color_var = "treatment",
#'       group_var = "subject_id",
#'       tooltip_vars = c("subject_id", "time_week")
#'     )
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_p_lineplot <- function(label = "Line Plot",
                          plot_dataname,
                          x_var,
                          y_var,
                          color_var,
                          group_var,
                          colors = NULL,
                          tooltip_vars = NULL,
                          transformators = list(),
                          reference_lines = NULL,
                          activate_on_brushing = FALSE) {
  module(
    label = label,
    ui = ui_p_lineplot,
    server = srv_p_lineplot,
    ui_args = list(),
    server_args = list(
      plot_dataname = plot_dataname,
      x_var = x_var,
      y_var = y_var,
      color_var = color_var,
      colors = colors,
      group_var = group_var,
      tooltip_vars = tooltip_vars,
      reference_lines = reference_lines,
      activate_on_brushing = activate_on_brushing
    ),
    transformators = transformators
  )
}

ui_p_lineplot <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    tags$div(
      trigger_tooltips_deps(),
      plotly::plotlyOutput(ns("plot"), height = "100%")
    )
  )
}

srv_p_lineplot <- function(id,
                           data,
                           plot_dataname,
                           x_var,
                           y_var,
                           color_var,
                           group_var,
                           colors,
                           tooltip_vars = NULL,
                           reference_lines,
                           activate_on_brushing) {
  moduleServer(id, function(input, output, session) {
    plotly_q <- reactive({
      if (activate_on_brushing) {
        req(attr(data(), "has_brushing"))
      }
      data() |>
        within(
          code,
          code = lineplotly(
            df = plot_dataname,
            x_var = x_var,
            y_var = y_var,
            color_var = color_var,
            group_var = group_var,
            colors = colors,
            tooltip_vars = tooltip_vars,
            reference_lines = reference_lines,
            source = session$ns("lineplot")
          )
        )
    })


    output$plot <- plotly::renderPlotly({
      plotly_q()$p %>%
        plotly::event_register("plotly_selected")
    })
  })
}

#' Create Line Plot with Plotly
#'
#' This function generates plotly code for creating interactive line plots
#' that connect data points within groups to show trends over time.
#'
#' @param df (`character(1)`) Name of the data frame containing the plotting data
#' @param x_var (`character(1)`) Name of the x-axis variable
#' @param y_var (`character(1)`) Name of the y-axis variable
#' @param color_var (`character(1)`) Name of the color variable
#' @param group_var (`character(1)`) Name of the grouping variable
#' @param colors (`named character` or `NULL`) Color mapping for groups
#' @param tooltip_vars (`character` or `NULL`) Variables to include in tooltip
#' @param reference_lines (`list` or `NULL`) Reference lines specification
#' @param source (`character(1)`) Plotly source identifier for events
#'
#' @return A substitute expression that creates a plotly object with line segments and markers
#' @keywords internal
lineplotly <- function(df,
                       x_var,
                       y_var,
                       color_var,
                       group_var,
                       colors = NULL,
                       tooltip_vars = NULL,
                       reference_lines = NULL,
                       source = "lineplot") {
  substitute(
    {
      validate(need(nrow(df_sym) > 0, "No data after applying filters."))

      group_var_label <- attr(df_sym[[group_var_str]], "label")
      if (!length(group_var_label)) group_var_label <- group_var_str

      x_var_label <- attr(df_sym[[x_var_str]], "label")
      if (!length(x_var_label)) x_var_label <- x_var_str

      y_var_label <- attr(df_sym[[y_var_str]], "label")
      if (!length(y_var_label)) y_var_label <- y_var_str

      color_var_label <- attr(df_sym[[color_var_str]], "label")
      if (!length(color_var_label)) color_var_label <- color_var_str

      df_sym <- df_sym |>
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

      add_reference_lines <- function(data,
                                      reference_lines,
                                      default_line_color = "red",
                                      default_font_color = "red",
                                      default_font_size = 12) {
        shapes <- list()
        annotations <- list()
        for (i in seq_along(reference_lines)) {
          if (is.character(reference_lines[[i]]) && length(reference_lines[[i]]) == 1) {
            col <- reference_lines[[i]]
            label <- col
            line_mode <- "dash"
          } else if (is.list(reference_lines[[i]])) {
            col <- names(reference_lines)[i]
            if (col == "") next
            label <- if (!is.null(reference_lines[[col]]$label)) reference_lines[[col]]$label else col
            line_mode <- if (!is.null(reference_lines[[col]]$line_mode)) reference_lines[[col]]$line_mode else "dash"
          } else {
            next
          }
          if (length(unique(data[[col]])) != 1) {
            label <- paste0(label, "<br>(mean)")
          }
          y_val <- mean(data[[col]])
          shapes[[length(shapes) + 1]] <- list(
            type = "line",
            x0 = 0, x1 = 1,
            xref = "paper",
            y0 = y_val, y1 = y_val,
            yref = "y",
            line = list(color = default_line_color, dash = line_mode, width = 2)
          )
          annotations[[length(annotations) + 1]] <- list(
            x = 1, xref = "paper",
            y = y_val, yref = "y",
            text = label,
            showarrow = FALSE,
            xanchor = "left",
            font = list(color = default_font_color, size = default_font_size)
          )
        }
        list(shapes = shapes, annotations = annotations)
      }

      segments_df <- df_sym %>%
        dplyr::arrange(!!as.name(group_var_str), !!as.name(x_var_str)) %>%
        dplyr::group_by(!!as.name(group_var_str)) %>%
        dplyr::mutate(
          xend = dplyr::lead(!!as.name(x_var_str)),
          yend = dplyr::lead(!!as.name(y_var_str)),
          color_var_seg = dplyr::lead(!!as.name(color_var_str))
        ) %>%
        dplyr::filter(!is.na(xend))

      p <- plotly::plot_ly(
        data = segments_df,
        source = source_sym,
        height = 600L
      ) %>%
        plotly::add_segments(
          x = ~x_var_sym,
          y = ~y_var_sym,
          xend = ~xend,
          yend = ~yend,
          color = ~color_var_seg,
          colors = colors_sym,
          showlegend = FALSE
        ) %>%
        plotly::add_markers(
          data = df_sym,
          x = ~x_var_sym,
          y = ~y_var_sym,
          color = ~color_var_sym,
          colors = colors_sym,
          text = ~tooltip,
          hoverinfo = "text"
        )

      if (!is.null(reference_lines_sym)) {
        ref_lines <- add_reference_lines(df_sym, reference_lines_sym)
        p <- p %>%
          plotly::layout(
            shapes = ref_lines$shapes,
            annotations = ref_lines$annotations
          )
      }
    },
    list(
      df_sym = str2lang(df),
      x_var_sym = str2lang(x_var),
      y_var_sym = str2lang(y_var),
      color_var_sym = str2lang(color_var),
      group_var_sym = str2lang(group_var),
      x_var_str = x_var,
      y_var_str = y_var,
      color_var_str = color_var,
      group_var_str = group_var,
      colors_sym = colors,
      tooltip_vars_sym = tooltip_vars,
      reference_lines_sym = reference_lines,
      source_sym = source
    )
  )
}
