#' Bar Graph Module
#'
#' This module creates an interactive horizontal stacked bar chart visualization that
#' displays counts of distinct values grouped by categories. The bars are automatically
#' ordered by total count (ascending) and support color coding by a categorical variable.
#' Users can select bar segments by brushing to filter the underlying data. The plot
#' aggregates data by counting distinct values within each group combination.
#'
#' @param label (`character(1)`) Label shown in the navigation item for the module.
#' @param plot_dataname (`character(1)`) Name of the dataset to be used for plotting.
#' @param y_var (`character(1)`) Name of the categorical variable to be displayed on y-axis (bar categories).
#' @param color_var (`character(1)`) Name of the categorical variable used for color coding and stacking segments.
#' @param count_var (`character(1)`) Name of the variable whose distinct values will be counted for bar heights.
#' @param bar_colors (`named character` or `NULL`) Valid color names or hex-colors named by levels of color_var column.
#'
#' @examples
#' data <- teal_data() |>
#'   within({
#'     df <- data.frame(
#'       adverse_event = sample(c("Headache", "Nausea", "Fatigue", "Dizziness", "Rash"),
#'         100,
#'         replace = TRUE, prob = c(0.3, 0.25, 0.2, 0.15, 0.1)
#'       ),
#'       severity = sample(c("Mild", "Moderate", "Severe"), 100,
#'         replace = TRUE,
#'         prob = c(0.6, 0.3, 0.1)
#'       ),
#'       subject_id = sample(paste0("S", 1:30), 100, replace = TRUE),
#'       treatment = sample(c("Active", "Placebo"), 100, replace = TRUE)
#'     )
#'
#'     # Add labels
#'     attr(df$adverse_event, "label") <- "Adverse Event Type"
#'     attr(df$severity, "label") <- "Severity Grade"
#'     attr(df$subject_id, "label") <- "Subject ID"
#'     attr(df$treatment, "label") <- "Treatment Group"
#'   })
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_p_bargraph(
#'       label = "AE by Treatment",
#'       plot_dataname = "df",
#'       y_var = "adverse_event",
#'       color_var = "treatment",
#'       count_var = "subject_id",
#'       bar_colors = c("Active" = "#FF6B6B", "Placebo" = "#4ECDC4")
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
                          bar_colors = NULL) {
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
      bar_colors = bar_colors
    )
  )
}

ui_p_bargraph <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    bslib::card(
      full_screen = TRUE,
      tags$div(
        # trigger_tooltips_deps(),
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
                           bar_colors) {
  moduleServer(id, function(input, output, session) {
    plotly_q <- reactive({
      data() |>
        within(
          {
            df[[color_var]] <- as.character(df[[color_var]])

            plot_data <- df %>%
              dplyr::group_by(!!as.name(y_var), !!as.name(color_var)) %>%
              dplyr::summarize(count = dplyr::n_distinct(!!as.name(count_var)), .groups = "drop") %>%
              dplyr::mutate(customdata = dplyr::row_number())

            event_type_order <- plot_data %>%
              dplyr::group_by(!!as.name(y_var)) %>%
              dplyr::summarize(total = sum(count), .groups = "drop") %>%
              dplyr::arrange(total) %>%
              dplyr::pull(!!as.name(y_var))

            plot_data[[y_var]] <- factor(plot_data[[y_var]], levels = event_type_order)

            p <- plotly::plot_ly(
              data = plot_data,
              y = as.formula(paste0("~", y_var)),
              x = ~count,
              color = as.formula(paste0("~", color_var)),
              colors = bar_colors,
              type = "bar",
              orientation = "h",
              customdata = ~customdata,
              source = source
            ) %>%
              plotly::layout(
                barmode = "stack",
                xaxis = list(title = "Count"),
                yaxis = list(title = "Adverse Event Type"),
                legend = list(title = list(text = "AE Type"))
              ) %>%
              plotly::layout(dragmode = "select")
          },
          df = str2lang(plot_dataname),
          color_var = color_var,
          y_var = y_var,
          count_var = count_var,
          bar_colors = bar_colors,
          source = session$ns("bargraph")
        )
    })


    output$plot <- plotly::renderPlotly({
      plotly_q()$p %>%
        set_plot_data(session$ns("plot_data")) |>
        plotly::event_register("plotly_selected")
    })
    plotly_selected <- reactive(
      plotly::event_data("plotly_selected", source = session$ns("bargraph"))
    )

    reactive({
      if (is.null(plotly_selected())) {
        plotly_q()
      } else {
        q <- plotly_q() |>
          within(
            {
              selected_plot_data <- plot_data |>
                dplyr::filter(customdata %in% plotly_selected_customdata)
              df <- df |>
                dplyr::filter(
                  !!as.name(y_var_string) %in% selected_plot_data[[y_var_string]],
                  !!as.name(color_var_string) %in% selected_plot_data[[color_var_string]]
                )
            },
            df = str2lang(plot_dataname),
            y_var_string = y_var,
            color_var_string = color_var,
            plotly_selected_customdata = plotly_selected()$customdata
          )
        attr(q, "has_brushing") <- TRUE
        q
      }
    })
  })
}
