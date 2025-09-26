#' Scatterplot Module
#'
#' This module creates an interactive scatter plot visualization with customizable tooltips.
#' Users can select points by brushing to filter the underlying data. The plot supports
#' color coding by categorical variables and displays tooltips on hover that can show
#' default variables (subject, x, y, color) or custom columns specified via `tooltip_vars`.
#'
#' @inheritParams teal::module
#' @param plot_dataname (`character(1)`) Name of the dataset to be used for plotting.
#' @param subject_var (`character(1)`) Name of the subject variable.
#' @param x_var (`character(1)`) Name of the variable to be used for x-axis.
#' @param y_var (`character(1)`) Name of the variable to be used for y-axis.
#' @param color_var (`character(1)`) Name of the variable to be used for coloring points.
#' @param point_colors (`named character` or `NULL`) Valid color names or hex-colors named by levels of `color_var` column.
#' If `NULL`, default colors will be used.
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing subject, x, y, and color variables.
#' @param transformators (`list`) Named list of transformator functions.
#' @param show_widgets (`logical(1)`) Whether to show module widgets.
#'
#' @inherit shared_params return
#'
#' @examples
#' data <- teal_data() |>
#'   within({
#'     df <- data.frame(
#'       subject_id = paste0("S", 1:50),
#'       age = sample(20:80, 50, replace = TRUE),
#'       response = rnorm(50, 15, 3),
#'       treatment = sample(c("Active", "Placebo"), 50, replace = TRUE),
#'       gender = sample(c("M", "F"), 50, replace = TRUE)
#'     )
#'
#'     # Add labels for better tooltips
#'     attr(df$age, "label") <- "Age (years)"
#'     attr(df$response, "label") <- "Response Score"
#'     attr(df$treatment, "label") <- "Treatment Group"
#'   })
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_p_scatterplot(
#'       label = "Scatter Plot with Custom Tooltip",
#'       plot_dataname = "df",
#'       subject_var = "subject_id",
#'       x_var = "age",
#'       y_var = "response",
#'       color_var = "treatment",
#'       tooltip_vars = c("age", "gender")
#'     )
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_p_scatterplot <- function(label = "Scatter Plot",
                             plot_dataname,
                             subject_var,
                             x_var,
                             y_var,
                             color_var,
                             point_colors = character(0),
                             tooltip_vars = NULL,
                             transformators = list(),
                             show_widgets = TRUE) {
  module(
    label = label,
    ui = ui_p_scatterplot,
    server = srv_p_scatterplot,
    ui_args = list(),
    server_args = list(
      plot_dataname = plot_dataname,
      subject_var = subject_var,
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

ui_p_scatterplot <- function(id) {
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

srv_p_scatterplot <- function(id,
                              data,
                              plot_dataname,
                              subject_var,
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
          code = scatterplotly(
            df = plot_dataname,
            x_var = x_var,
            y_var = y_var,
            color_var = color_var,
            subject_var = subject_var,
            colors = color_inputs(),
            source = session$ns("scatterplot"),
            tooltip_vars = tooltip_vars
          )
        )
    })


    output$plot <- plotly::renderPlotly(
      plotly_q()$p |>
        setup_trigger_tooltips(session$ns)
    )
  })
}

#' Generate Scatter Plotly Code
#'
#' Creates code expression that generates a scatter plot with tooltips using plotly.
#' This function includes all the data manipulation and plot creation logic
#' from tm_p_scatterplot module, including label extraction, tooltip generation,
#' and event registration.
#'
#' @param df (`language`) Symbol representing the data frame to plot
#' @param x_var (`character(1)`) Name of the variable to be used for x-axis
#' @param y_var (`character(1)`) Name of the variable to be used for y-axis
#' @param color_var (`character(1)`) Name of the variable to be used for coloring points
#' @param subject_var (`character(1)`) Name of the subject variable
#' @param colors (`character`) Named vector of colors for color_var levels
#' @param source (`character(1)`) Source identifier for plotly events
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing subject, x, y, and color variables.
#'
#' @return A code expression that when evaluated creates a plotly plot object
#'
#' @examples
#' # Generate code for a scatter plot
#' code <- scatterplotly(
#'   df = quote(iris_data),
#'   x_var = "Sepal.Length",
#'   y_var = "Petal.Length",
#'   color_var = "Species",
#'   subject_var = "row_id",
#'   colors = c("setosa" = "red", "versicolor" = "blue", "virginica" = "green"),
#'   source = "scatterplot",
#'   tooltip_vars = c("Sepal.Width", "Petal.Width")
#' )
#'
scatterplotly <- function(df, x_var, y_var, color_var, subject_var, colors, source, tooltip_vars = NULL) {
  substitute(
    {
      subject_var_label <- attr(df_sym[[subject_var_str]], "label")
      if (!length(subject_var_label)) subject_var_label <- subject_var_str

      x_var_label <- attr(df_sym[[x_var_str]], "label")
      if (!length(x_var_label)) x_var_label <- x_var_str

      y_var_label <- attr(df_sym[[y_var_str]], "label")
      if (!length(y_var_label)) y_var_label <- y_var_str

      color_var_label <- attr(df_sym[[color_var_str]], "label")
      if (!length(color_var_label)) color_var_label <- color_var_str

      plot_data <- df_sym |>
        dplyr::mutate(!!as.name(color_var_str) := factor(!!as.name(color_var_str), levels = names(colors_sym))) |>
        dplyr::mutate(customdata = dplyr::row_number()) |>
        dplyr::mutate(
          tooltip = {
            if (is.null(tooltip_vars_sym)) {
              paste(
                paste(subject_var_label, ":", !!as.name(subject_var_str)),
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
                  paste(subject_var_label, ":", !!as.name(subject_var_str)),
                  paste(x_var_label, ":", !!as.name(x_var_str)),
                  paste(y_var_label, ":", !!as.name(y_var_str)),
                  paste(color_var_label, ":", !!as.name(color_var_str)),
                  sep = "<br>"
                )
              } else {
                sub <- cur_data[cols]
                labels <- vapply(cols, function(cn) {
                  if (cn == subject_var_str) {
                    lb <- subject_var_label
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

      p <- plotly::plot_ly(
        data = plot_data,
        source = source_sym,
        colors = colors_sym,
        customdata = ~customdata
      ) |>
        plotly::add_markers(
          x = ~x_var_sym,
          y = ~y_var_sym,
          color = ~color_var_sym,
          text = ~tooltip,
          hoverinfo = "text"
        ) |>
        plotly::layout(dragmode = "select") |>
        plotly::event_register("plotly_selected")
    },
    list(
      df_sym = str2lang(df),
      x_var_sym = str2lang(x_var),
      y_var_sym = str2lang(y_var),
      color_var_sym = str2lang(color_var),
      subject_var_sym = str2lang(subject_var),
      x_var_str = x_var,
      y_var_str = y_var,
      color_var_str = color_var,
      subject_var_str = subject_var,
      colors_sym = colors,
      source_sym = source,
      tooltip_vars_sym = tooltip_vars
    )
  )
}
