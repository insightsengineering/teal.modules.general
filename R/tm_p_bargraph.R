#' Bar Graph Module
#'
#' This module creates an interactive horizontal stacked bar chart visualization that
#' displays counts of distinct values grouped by categories. The bars are automatically
#' ordered by total count (ascending) and support color coding by a categorical variable.
#' Users can select bar segments by brushing to filter the underlying data. The plot
#' aggregates data by counting distinct values within each group combination.
#'
#' @inheritParams teal::module
#' @param plot_dataname (`character(1)`) Name of the dataset to be used for plotting.
#' @param y_var (`character(1)`) Name of the categorical variable to be displayed on y-axis (bar categories).
#' @param color_var (`character(1)`) Name of the categorical variable used for color coding and stacking segments.
#' @param count_var (`character(1)`) Name of the variable whose distinct values will be counted for bar heights.
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing y, color, and count variables.
#' @param bar_colors (`named character` or `NULL`) Valid color names or hex-colors named by levels of `color_var` column.
#' If `NULL`, default colors will be used.
#'
#' @inherit shared_params return
#'
#' @examples
#' data <- teal_data() |>
#'   within({
#'     df <- data.frame(
#'       adverse_event = sample(
#'         c("Headache", "Nausea", "Fatigue", "Dizziness", "Rash", "Insomnia"),
#'         150,
#'         replace = TRUE,
#'         prob = c(0.25, 0.2, 0.18, 0.15, 0.12, 0.1)
#'       ),
#'       severity = sample(
#'         c("Mild", "Moderate", "Severe"),
#'         150,
#'         replace = TRUE,
#'         prob = c(0.6, 0.3, 0.1)
#'       ),
#'       subject_id = sample(paste0("S", 1:40), 150, replace = TRUE),
#'       treatment = sample(c("Active", "Placebo"), 150, replace = TRUE),
#'       age_group = sample(c("Young", "Middle", "Old"), 150, replace = TRUE),
#'       center = sample(c("Site A", "Site B", "Site C", "Site D"), 150, replace = TRUE),
#'       system_organ_class = sample(
#'         c("Nervous System", "Gastrointestinal", "General", "Skin"),
#'         150,
#'         replace = TRUE
#'       )
#'     )
#'
#'     attr(df$adverse_event, "label") <- "Adverse Event Type"
#'     attr(df$severity, "label") <- "Severity Grade"
#'     attr(df$subject_id, "label") <- "Subject ID"
#'     attr(df$treatment, "label") <- "Treatment Group"
#'     attr(df$age_group, "label") <- "Age Group"
#'     attr(df$center, "label") <- "Study Center"
#'     attr(df$system_organ_class, "label") <- "System Organ Class"
#'   })
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_p_bargraph(
#'       label = "Basic Bar Graph",
#'       plot_dataname = "df",
#'       y_var = "adverse_event",
#'       color_var = "treatment",
#'       count_var = "subject_id"
#'     )
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_p_bargraph(
#'       label = "Advanced Bar Graph with All Features",
#'       plot_dataname = "df",
#'       y_var = "adverse_event",
#'       color_var = "severity",
#'       count_var = "subject_id",
#'       bar_colors = c(
#'         "Mild" = "#90EE90",
#'         "Moderate" = "#FFD700",
#'         "Severe" = "#FF6347"
#'       ),
#'       tooltip_vars = c("adverse_event", "severity", "treatment", "age_group", "center", "system_organ_class")
#'     )
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_p_bargraph <- function(label = "Bar Plot",
                          plot_dataname,
                          y_var,
                          color_var,
                          count_var,
                          bar_colors = NULL,
                          tooltip_vars = NULL,
                          transformators = list()) {
  module(
    label = label,
    ui = ui_p_bargraph,
    server = srv_p_bargraph,
    ui_args = list(),
    server_args = list(
      plot_dataname = plot_dataname,
      y_var = y_var,
      color_var = color_var,
      count_var = count_var,
      tooltip_vars = tooltip_vars,
      bar_colors = bar_colors
    ),
    transformators = transformators
  )
}

ui_p_bargraph <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    bslib::card(
      full_screen = TRUE,
      tags$div(
        trigger_tooltips_deps(),
        plotly::plotlyOutput(ns("plot"), height = "100%")
      )
    )
  )
}

srv_p_bargraph <- function(id,
                           data,
                           plot_dataname,
                           y_var,
                           color_var,
                           count_var,
                           tooltip_vars = NULL,
                           bar_colors) {
  moduleServer(id, function(input, output, session) {
    plotly_q <- reactive({
      data() |>
        within(
          code,
          code = bargraphplotly(
            df = plot_dataname,
            y_var = y_var,
            color_var = color_var,
            count_var = count_var,
            tooltip_vars = tooltip_vars,
            bar_colors = bar_colors,
            source = session$ns("bargraph")
          )
        )
    })


    output$plot <- plotly::renderPlotly({
      plotly_q()$p %>%
        set_plot_data(session$ns("plot_data")) |>
        setup_trigger_tooltips(session$ns) |>
        plotly::event_register("plotly_selected")
    })
  })
}

#' Create Bar Graph with Plotly
#'
#' This function generates plotly code for creating interactive horizontal stacked bar charts
#' that display counts of distinct values grouped by categories.
#'
#' @param df (`character(1)`) Name of the data frame containing the plotting data
#' @param y_var (`character(1)`) Name of the y-axis variable
#' @param color_var (`character(1)`) Name of the color variable
#' @param count_var (`character(1)`) Name of the count variable
#' @param tooltip_vars (`character` or `NULL`) Variables to include in tooltip
#' @param bar_colors (`named character` or `NULL`) Color mapping for groups
#' @param source (`character(1)`) Plotly source identifier for events
#'
#' @return A substitute expression that creates a plotly object with horizontal stacked bars
#' @keywords internal
bargraphplotly <- function(df,
                           y_var,
                           color_var,
                           count_var,
                           tooltip_vars = NULL,
                           bar_colors = NULL,
                           source = "bargraph") {
  substitute(
    {
      df_sym[[color_var_str]] <- as.character(df_sym[[color_var_str]])

      plot_data <- df_sym %>%
        dplyr::group_by(y_var_sym, color_var_sym) %>%
        dplyr::summarize(count = dplyr::n_distinct(count_var_sym), .groups = "drop") %>%
        dplyr::ungroup() %>%
        dplyr::mutate(customdata = dplyr::row_number()) %>%
        dplyr::mutate(
          tooltip = {
            if (is.null(tooltip_vars_sym)) {
              y_var_label <- attr(df_sym[[y_var_str]], "label")
              if (!length(y_var_label)) y_var_label <- y_var_str
              color_var_label <- attr(df_sym[[color_var_str]], "label")
              if (!length(color_var_label)) color_var_label <- color_var_str

              paste(
                paste(y_var_label, ":", y_var_sym),
                paste(color_var_label, ":", color_var_sym),
                paste("Count:", count),
                sep = "<br>"
              )
            } else {
              cur_data <- dplyr::cur_data()

              actual_cols <- character(0)
              for (col in tooltip_vars_sym) {
                if (col == "y_var") {
                  actual_cols <- c(actual_cols, y_var_str)
                } else if (col == "color_var") {
                  actual_cols <- c(actual_cols, color_var_str)
                } else if (col == "count_var") {
                  actual_cols <- c(actual_cols, "count")
                } else {
                  actual_cols <- c(actual_cols, col)
                }
              }

              cols <- intersect(actual_cols, names(cur_data))

              if (!length(cols)) {
                y_var_label <- attr(df_sym[[y_var_str]], "label")
                if (!length(y_var_label)) y_var_label <- y_var_str
                color_var_label <- attr(df_sym[[color_var_str]], "label")
                if (!length(color_var_label)) color_var_label <- color_var_str

                paste(
                  paste(y_var_label, ":", y_var_sym),
                  paste(color_var_label, ":", color_var_sym),
                  paste("Count:", count),
                  sep = "<br>"
                )
              } else {
                sub <- cur_data[cols]
                values <- lapply(sub, as.character)
                parts <- Map(function(v, n) paste0(n, ": ", v), values, names(values))
                do.call(paste, c(parts, sep = "<br>"))
              }
            }
          }
        )

      event_type_order <- plot_data %>%
        dplyr::group_by(y_var_sym) %>%
        dplyr::summarize(total = sum(count), .groups = "drop") %>%
        dplyr::arrange(total) %>%
        dplyr::pull(y_var_sym)

      plot_data[[y_var_str]] <- factor(plot_data[[y_var_str]], levels = event_type_order)

      p <- plotly::plot_ly(
        data = plot_data,
        y = ~y_var_sym,
        x = ~count,
        color = ~color_var_sym,
        colors = bar_colors,
        type = "bar",
        orientation = "h",
        hovertext = ~tooltip,
        hoverinfo = "text",
        customdata = ~customdata,
        source = source
      ) %>%
        plotly::layout(
          barmode = "stack",
          xaxis = list(title = "Count"),
          yaxis = list(title = "Category"),
          legend = list(title = list(text = "Group"))
        ) %>%
        plotly::layout(dragmode = "select")
    },
    list(
      df_sym = str2lang(df),
      y_var_sym = str2lang(y_var),
      color_var_sym = str2lang(color_var),
      count_var_sym = str2lang(count_var),
      y_var_str = y_var,
      color_var_str = color_var,
      count_var_str = count_var,
      tooltip_vars_sym = tooltip_vars,
      bar_colors = bar_colors,
      source = source
    )
  )
}
