#' Scatterplot Module
#'
#' This module creates an interactive scatter plot visualization with customizable tooltips.
#' Users can select points by brushing to filter the underlying data. The plot supports
#' color coding by categorical variables and displays tooltips on hover that can show
#' default variables (subject, x, y, color) or custom columns specified via `tooltip_vars`.
#'
#' @param label (`character(1)`) Label shown in the navigation item for the module.
#' @param plot_dataname (`character(1)`) Name of the dataset to be used for plotting.
#' @param subject_var (`character(1)`) Name of the subject variable.
#' @param x_var (`character(1)`) Name of the variable to be used for x-axis.
#' @param y_var (`character(1)`) Name of the variable to be used for y-axis.
#' @param color_var (`character(1)`) Name of the variable to be used for coloring points.
#' @param point_colors (`named character`) Valid color names or hex-colors named by levels of color_var column.
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing x, y, and color variables.
#' @param transformators (`list`) Named list of transformator functions.
#' @param show_widgets (`logical(1)`) Whether to show module widgets.
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
                             subject_var,
                             x_var,
                             y_var,
                             color_var,
                             point_colors = character(0),
                             tooltip_vars = NULL,
                             transformators = list()) {
  checkmate::assert_string(label)
  checkmate::assert_class(subject_var, "picks")
  checkmate::assert_class(x_var, "picks")
  checkmate::assert_class(y_var, "picks")
  checkmate::assert_class(color_var, "picks")
  checkmate::assert_class(tooltip_vars, "picks", null.ok = TRUE)
  
  args <- as.list(environment())
  module(
    label = label,
    ui = ui_p_scatterplot_module,
    server = srv_p_scatterplot_module,
    ui_args = args[names(args) %in% names(formals(ui_p_scatterplot_module))],
    server_args = args[names(args) %in% names(formals(srv_p_scatterplot_module))],
    transformators = transformators,
    datanames = {
      datanames <- datanames(list(subject_var = subject_var, x_var = x_var, y_var = y_var, color_var = color_var))
      if (length(datanames)) datanames else "all"
    }
  )
}

ui_p_scatterplot_module <- function(id, subject_var, x_var, y_var, color_var) {
  ns <- NS(id)
  bslib::page_sidebar(
    sidebar = div(
      class = "standard-layout encoding-panel",
      teal::teal_nav_item(
        label = tags$strong("Subject Variable:"),
        teal.transform::module_input_ui(id = ns("subject_var"), spec = subject_var)
      ),
      teal::teal_nav_item(
        label = tags$strong("X-axis Variable:"),
        teal.transform::module_input_ui(id = ns("x_var"), spec = x_var)
      ),
      teal::teal_nav_item(
        label = tags$strong("Y-axis Variable:"),
        teal.transform::module_input_ui(id = ns("y_var"), spec = y_var)
      ),
      teal::teal_nav_item(
        label = tags$strong("Color by:"),
        teal.transform::module_input_ui(id = ns("color_var"), spec = color_var),
        colour_picker_ui(ns("colors"))
      )
    ),
    ui_p_scatterplot(ns("output"))
  )
}

srv_p_scatterplot_module <- function(id,
                                      data,
                                      subject_var,
                                      x_var,
                                      y_var,
                                      color_var,
                                      point_colors,
                                      tooltip_vars = NULL) {
  moduleServer(id, function(input, output, session) {
    selectors <- teal.transform::module_input_srv(
      data = data,
      spec = list(subject_var = subject_var, x_var = x_var, y_var = y_var, color_var = color_var, tooltip_vars = tooltip_vars)
    )
    merged_dataname <- "anl"
    merged_q <- reactive({
      req(data(), map_merged(selectors))
      obj <- data()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Scatterplot data preparation")
      qenv_merge_selectors(x = obj, selectors = selectors, output_name = merged_dataname)
    })

    color_inputs <- colour_picker_srv(
      "colors",
      x = reactive({
        selected_color <- req(map_merged(selectors)$color_var)
        merged_q()[[merged_dataname]][[selected_color$variables]]
      }),
      default_colors = point_colors
    )

    srv_p_scatterplot(
      "output",
      data = merged_q,
      dataname = merged_dataname,
      x_var = reactive(map_merged(selectors)$x_var$variables),
      y_var = reactive(map_merged(selectors)$y_var$variables),
      color_var = reactive(map_merged(selectors)$color_var$variables),
      color_inputs = color_inputs,
      subject_var = reactive(map_merged(selectors)$subject_var$variables),
      tooltip_vars = reactive(map_merged(selectors)$tooltip_vars$variables)
    )
  })
}

ui_p_scatterplot <- function(id) {
  ns <- NS(id)
  tags$div(
    class = "standard-layout output-panel",
    shinyjs::useShinyjs(),
    bslib::card(
      full_screen = TRUE,
      tags$div(
        trigger_tooltips_deps(),
        plotly::plotlyOutput(ns("plot"), height = "100%")
      )
    )
  )
}

srv_p_scatterplot <- function(id,
                              data,
                              dataname,
                              subject_var,
                              x_var,
                              y_var,
                              color_var,
                              color_inputs,
                              tooltip_vars = NULL) {
  moduleServer(id, function(input, output, session) {
    plotly_q <- reactive({
      obj <- req(data(), x_var(), y_var(), subject_var(), color_var())
      within(
        data(),
        df = str2lang(dataname),
        x_var = x_var(),
        y_var = y_var(),
        color_var = color_var(),
        subject_var = subject_var(),
        colors = color_inputs(),
        source = session$ns("scatterplot"),
        tooltip_vars = tooltip_vars(),
        expr = {
          # Get label attributes for variables, fallback to column names
          subject_var_label <- attr(df[[subject_var]], "label")
          if (!length(subject_var_label)) subject_var_label <- subject_var

          x_var_label <- attr(df[[x_var]], "label")
          if (!length(x_var_label)) x_var_label <- x_var

          y_var_label <- attr(df[[y_var]], "label")
          if (!length(y_var_label)) y_var_label <- y_var

          color_var_label <- attr(df[[color_var]], "label")
          if (!length(color_var_label)) color_var_label <- color_var

          plot_data <- df |>
            dplyr::mutate(!!as.name(color_var) := factor(!!as.name(color_var), levels = names(colors))) |>
            dplyr::mutate(customdata = dplyr::row_number()) |>
            dplyr::mutate(
              tooltip = {
                if (is.null(tooltip_vars)) {
                  # Default tooltip: show subject, x, y, color variables with labels
                  paste(
                    paste(subject_var_label, ":", !!as.name(subject_var)),
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
                      paste(subject_var_label, ":", !!as.name(subject_var)),
                      paste(x_var_label, ":", !!as.name(x_var)),
                      paste(y_var_label, ":", !!as.name(y_var)),
                      paste(color_var_label, ":", !!as.name(color_var)),
                      sep = "<br>"
                    )
                  } else {
                    # Create tooltip from specified columns
                    sub <- cur_data[cols]
                    labels <- vapply(cols, function(cn) {
                      if (cn == subject_var) {
                        lb <- subject_var_label
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

          plotly::plot_ly(
            data = plot_data,
            source = source,
            colors = colors,
            customdata = ~customdata
          ) |>
            plotly::add_markers(
              x = stats::as.formula(sprintf("~%s", x_var)),
              y = stats::as.formula(sprintf("~%s", y_var)),
              color = stats::as.formula(sprintf("~%s", color_var)),
              text = ~tooltip,
              hoverinfo = "text"
            ) |>
            plotly::layout(dragmode = "select") |>
            plotly::event_register("plotly_selected")
        }
      )
    })

    output$plot <- plotly::renderPlotly({
      req(plotly_q())
      tail(teal.code::get_outputs(plotly_q()), 1)[[1]] |>
        setup_trigger_tooltips(session$ns) |>
        set_plot_data(session$ns("plot_data")) |>
        plotly::event_register("plotly_selected")
    })


    plotly_selected <- reactive(
      plotly::event_data("plotly_selected", source = session$ns("scatterplot"))
    )
    reactive({
      if (is.null(plotly_selected()) || is.null(subject_var())) {
        plotly_q()
      } else {
        q <- plotly_q() |>
          within(
            {
              selected_plot_data <- plot_data |>
                dplyr::filter(customdata %in% plotly_selected_customdata)
              df <- df |>
                dplyr::filter(!!as.name(subject_var_string) %in% selected_plot_data[[subject_var_string]])
            },
            df = str2lang(dataname),
            subject_var_string = subject_var(),
            plotly_selected_customdata = plotly_selected()$customdata
          )
        attr(q, "has_brushing") <- TRUE
        q
      }
    })
  })
}