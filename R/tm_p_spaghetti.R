#' Spaghetti Plot Module
#'
#' This module creates an interactive spaghetti plot visualization that shows individual 
#' trajectories for each group over time. Each trajectory is represented by connected 
#' points and lines, creating a "spaghetti-like" appearance. The plot supports customizable 
#' tooltips and color coding by categorical variables. Users can select points by brushing 
#' to filter the underlying data.
#'
#' @param label (`character(1)`) Label shown in the navigation item for the module.
#' @param plot_dataname (`character(1)`) Name of the dataset to be used for plotting.
#' @param group_var (`character(1)`) Name of the grouping variable that defines individual trajectories.
#' @param x_var (`character(1)`) Name of the variable to be used for x-axis (typically time).
#' @param y_var (`character(1)`) Name of the variable to be used for y-axis (typically a measurement).
#' @param color_var (`character(1)`) Name of the variable to be used for coloring points and lines.
#' @param point_colors (`named character`) Valid color names or hex-colors named by levels of color_var column.
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing group, x, y, and color variables.
#' @param transformators (`list`) Named list of transformator functions.
#' @param show_widgets (`logical(1)`) Whether to show module widgets.
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
tm_p_spaghetti <- function(label = "Scatter Plot",
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
      within(
        data(),
        group_var = group_var,
        x_var = x_var,
        y_var = y_var,
        color_var = color_var,
        colors = color_inputs(),
        source = session$ns("spaghetti"),
        tooltip_vars = tooltip_vars,
        expr = {
          # Get label attributes for variables, fallback to column names
          group_var_label <- attr(df[[group_var]], "label")
          if (!length(group_var_label)) group_var_label <- group_var
          
          x_var_label <- attr(df[[x_var]], "label")
          if (!length(x_var_label)) x_var_label <- x_var
          
          y_var_label <- attr(df[[y_var]], "label")
          if (!length(y_var_label)) y_var_label <- y_var
          
          color_var_label <- attr(df[[color_var]], "label")
          if (!length(color_var_label)) color_var_label <- color_var

          plot_data <- df |>
            dplyr::select(!!as.name(group_var), !!as.name(x_var), !!as.name(y_var), !!as.name(color_var)) |>
            dplyr::mutate(!!as.name(color_var) := factor(!!as.name(color_var), levels = names(colors))) %>%
            dplyr::mutate(customdata = dplyr::row_number()) |>
            dplyr::mutate(
              tooltip = {
                if (is.null(tooltip_vars)) {
                  # Default tooltip: show group, x, y, color variables with labels
                  paste(
                    paste(group_var_label, ":", !!as.name(group_var)),
                    paste(x_var_label, ":", !!as.name(x_var)),
                    paste(y_var_label, ":", !!as.name(y_var)),
                    paste(color_var_label, ":", !!as.name(color_var)),
                    sep = "<br>"
                  )
                } else {
                  # Custom tooltip: show only specified columns
                  cur_data <- dplyr::cur_data()
                  cols <- intersect(tooltip_vars, names(cur_data))
                  if (!length(cols)) {
                    # Fallback to default if no valid columns found
                    paste(
                      paste(group_var_label, ":", !!as.name(group_var)),
                      paste(x_var_label, ":", !!as.name(x_var)),
                      paste(y_var_label, ":", !!as.name(y_var)),
                      paste(color_var_label, ":", !!as.name(color_var)),
                      sep = "<br>"
                    )
                  } else {
                    # Create tooltip from specified columns
                    sub <- cur_data[cols]
                    labels <- vapply(cols, function(cn) {
                      if (cn == group_var) {
                        lb <- group_var_label
                      } else if (cn == x_var) {
                        lb <- x_var_label
                      } else if (cn == y_var) {
                        lb <- y_var_label
                      } else if (cn == color_var) {
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
            dplyr::arrange(!!as.name(group_var), !!as.name(x_var)) %>%
            dplyr::group_by(!!as.name(group_var)) %>%
            dplyr::mutate(
              x = !!as.name(x_var),
              y = !!as.name(y_var),
              xend = lead(!!as.name(x_var)),
              yend = lead(!!as.name(y_var)),
              color_var_seg = lead(!!as.name(color_var))
            ) %>%
            dplyr::filter(!is.na(xend))

          p <- plotly::plot_ly(
            data = segments_df,
            customdata = ~customdata,
            source = source
          ) %>%
            plotly::add_segments(
              x = ~x, y = ~y,
              xend = ~xend, yend = ~yend,
              color = ~color_var_seg,
              colors = colors,
              showlegend = TRUE
            ) %>%
            plotly::add_markers(
              data = plot_data,
              x = stats::as.formula(sprintf("~%s", x_var)),
              y = stats::as.formula(sprintf("~%s", y_var)),
              color = stats::as.formula(sprintf("~%s", color_var)),
              colors = colors,
              text = ~tooltip,
              hoverinfo = "text"
            ) |>
            plotly::layout(dragmode = "select")

          p
        },
        df = str2lang(plot_dataname)
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
