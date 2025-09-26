#' Waterfall Plot Module
#'
#' This module creates an interactive waterfall plot visualization that displays subjects
#' sorted by their values in a descending waterfall pattern. Each subject is represented
#' by a vertical bar, with the height corresponding to the value variable. The plot supports
#' color coding by categorical variables, optional horizontal reference lines, and customizable
#' tooltips. This visualization is particularly useful for showing ranked responses or changes
#' across subjects.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param plot_dataname (`character(1)`) Name of the dataset to be used for plotting.
#' @param subject_var (`character(1)`) Name of the factor or character column in `plot_dataname`
#' to be used as x-axis (subject identifiers).
#' @param value_var (`character(1)`) Name of the numeric column in `plot_dataname`
#' to be used as y-axis (values determining bar heights).
#' @param sort_var (`character(1)` or `NULL`) Name of the column used for sorting subjects.
#' If `NULL`, defaults to `value_var`.
#' @param color_var (`character(1)` or `NULL`) Name of the factor or character column in `plot_dataname`
#' to be used to differentiate bar colors. If `NULL`, all bars will have the same color.
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing subject, value, and color variables.
#' @param bar_colors (`named character` or `NULL`) Valid color names or hex-colors named by levels of `color_var` column.
#' If `NULL`, default colors will be used.
#' @param value_arbitrary_hlines (`numeric` or `NULL`) Values in the same scale as `value_var` to add
#' horizontal reference lines on the plot.
#' @param plot_title (`character` or `NULL`) Title of the plot. If `NULL`, no title is displayed.
#' @param plot_height (`numeric(3)`) Vector of length 3 with c(default, min, max) plot height values.
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
#'     waterfall_ds <- data.frame(
#'       subject_var = sample(c("A", "B", "C"), 10, replace = TRUE),
#'       value_var = sample(-20:90, 10, replace = TRUE),
#'       color_var = sample(c("CR", "PR", "SD", "PD"), 10, replace = TRUE)
#'     )
#'   })
#' join_keys(data) <- join_keys(
#'   join_key("subjects", "waterfall_ds", keys = c(subject_var = "subject_var"))
#' )
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_p_waterfall(
#'       plot_dataname = "waterfall_ds",
#'       subject_var = "subject_var",
#'       value_var = "value_var",
#'       sort_var = "value_var",
#'       color_var = "color_var",
#'       tooltip_vars = c("value_var", "subjects"),
#'       value_arbitrary_hlines = c(20, -30),
#'       bar_colors = c(
#'         CR = "#FF0000", PR = "#00FF00", SD = "#0000FF", PD = "#FFFF00"
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
tm_p_waterfall <- function(label = "Waterfall",
                           plot_dataname,
                           subject_var,
                           value_var,
                           sort_var = NULL,
                           color_var = NULL,
                           tooltip_vars = NULL,
                           bar_colors = character(0),
                           value_arbitrary_hlines = c(0.2, -0.3),
                           plot_title = "Waterfall plot",
                           plot_height = c(600, 400, 1200)) {
  if (is.character(subject_var)) {
    subject_var <- choices_selected(choices = subject_var, selected = subject_var)
  }
  if (is.character(value_var)) {
    value_var <- choices_selected(choices = value_var, selected = value_var)
  }
  if (is.character(sort_var)) {
    sort_var <- choices_selected(choices = sort_var, selected = sort_var)
  }
  if (is.character(color_var)) {
    color_var <- choices_selected(choices = color_var, selected = color_var)
  }

  module(
    label = label,
    ui = ui_p_waterfall,
    server = srv_p_waterfall,
    datanames = plot_dataname,
    ui_args = list(height = plot_height),
    server_args = list(
      plot_dataname = plot_dataname,
      subject_var = subject_var,
      value_var = value_var,
      sort_var = sort_var,
      color_var = color_var,
      bar_colors = bar_colors,
      value_arbitrary_hlines = value_arbitrary_hlines,
      plot_title = plot_title,
      tooltip_vars = tooltip_vars
    )
  )
}

ui_p_waterfall <- function(id, height) {
  ns <- NS(id)

  bslib::page_fluid(
    div(
      style = "display: flex;",
      selectInput(
        ns("subject_var"),
        label = "Subject variable (x-axis):",
        choices = NULL, selected = NULL, multiple = FALSE
      ),
      selectInput(
        ns("value_var"),
        label = "Value variable (y-axis):",
        choices = NULL, selected = NULL, multiple = FALSE
      ),
      selectInput(ns("sort_var"), label = "Sort by:", choices = NULL, selected = NULL, multiple = FALSE),
      selectInput(ns("color_var"), label = "Color by:", choices = NULL, selected = NULL, multiple = FALSE),
      colour_picker_ui(ns("colors")),
      sliderInput(ns("plot_height"), "Plot Height (px)", height[2], height[3], height[1])
    ),
    tags$div(
      bslib::card(
        full_screen = TRUE,
        tags$div(
          plotly::plotlyOutput(ns("plot"), height = "100%")
        )
      )
    )
  )
}
srv_p_waterfall <- function(id,
                            data,
                            plot_dataname,
                            subject_var,
                            value_var,
                            sort_var,
                            color_var,
                            bar_colors,
                            value_arbitrary_hlines,
                            plot_title,
                            plot_height = c(600, 400, 1200),
                            tooltip_vars,
                            filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    .update_cs_input(inputId = "subject_var", data = reactive(data()[[dataname]]), cs = subject_var)
    .update_cs_input(inputId = "value_var", data = reactive(data()[[dataname]]), cs = value_var)
    .update_cs_input(inputId = "sort_var", data = reactive(data()[[dataname]]), cs = sort_var)
    .update_cs_input(inputId = "color_var", data = reactive(data()[[dataname]]), cs = color_var)

    color_inputs <- colour_picker_srv(
      "colors",
      x = reactive({
        req(data(), input$color_var)
        data()[[plot_dataname]][[input$color_var]]
      }),
      default_colors = bar_colors
    )

    plotly_q <- reactive({
      req(data(), input$subject_var, input$value_var, input$sort_var, input$color_var, color_inputs())

      data() |>
        within(
          code,
          code = waterfallplotly(
            df = plot_dataname,
            subject_var = input$subject_var,
            value_var = input$value_var,
            sort_var = input$sort_var,
            color_var = input$color_var,
            colors = color_inputs(),
            value_arbitrary_hlines = value_arbitrary_hlines,
            height = input$plot_height,
            title = "Waterfall plot",
            tooltip_vars = tooltip_vars,
            source = session$ns("waterfall")
          )
        )
    })

    output$plot <- plotly::renderPlotly(plotly::event_register(plotly_q()$p, "plotly_selected"))
  })
}

#' Generate Waterfall Plotly Code
#'
#' Creates code expression that generates a waterfall plot with tooltips using plotly.
#' This function includes all the data manipulation and plot creation logic
#' from tm_p_waterfall module, including sorting, label extraction, tooltip generation,
#' bar chart creation, and horizontal reference lines.
#'
#' @param df (`character(1)`) Name of the data frame to plot
#' @param subject_var (`character(1)`) Name of the factor or character column to be used as x-axis (subject identifiers)
#' @param value_var (`character(1)`) Name of the numeric column to be used as y-axis (values determining bar heights)
#' @param sort_var (`character(1)` or `NULL`) Name of the column whose values determine sorting order. If `NULL` or same as `value_var`, sorts by value_var descending
#' @param color_var (`character(1)` or `NULL`) Name of the factor or character column to differentiate bar colors. If `NULL`, all bars have same color
#' @param colors (`character`) Named vector of colors for color_var levels
#' @param value_arbitrary_hlines (`numeric` or `NULL`) Values for horizontal reference lines
#' @param height (`numeric(1)`) Plot height in pixels
#' @param title (`character(1)`) Plot title
#' @param source (`character(1)`) Source identifier for plotly events
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing subject, value, and color variables.
#'
#' @return A code expression that when evaluated creates a plotly plot object
#'
#' @examples
#' # Generate code for a waterfall plot
#' code <- waterfallplotly(
#'   df = "waterfall_data",
#'   subject_var = "subject_id",
#'   value_var = "response_value",
#'   sort_var = "response_value",
#'   color_var = "response_category",
#'   colors = c("CR" = "green", "PR" = "blue", "SD" = "yellow", "PD" = "red"),
#'   value_arbitrary_hlines = c(20, -30),
#'   height = 600,
#'   title = "Response Waterfall Plot",
#'   source = "waterfall",
#'   tooltip_vars = c("subject_id", "response_category")
#' )
#'
waterfallplotly <- function(df, subject_var, value_var, sort_var = NULL, color_var = NULL,
                            colors, value_arbitrary_hlines = NULL, height = 600,
                            title = "Waterfall plot", source, tooltip_vars = NULL) {
  substitute(
    {
      subject_var_label <- attr(df_sym[[subject_var_str]], "label")
      if (!length(subject_var_label)) subject_var_label <- subject_var_str
      value_var_label <- attr(df_sym[[value_var_str]], "label")
      if (!length(value_var_label)) value_var_label <- value_var_str
      color_var_label <- attr(df_sym[[color_var_str]], "label")
      if (!length(color_var_label)) color_var_label <- color_var_str

      plot_data <- dplyr::mutate(
        if (identical(sort_var_str, value_var_str) || is.null(sort_var_str)) {
          dplyr::arrange(df_sym, desc(!!as.name(value_var_str)))
        } else {
          dplyr::arrange(df_sym, !!as.name(sort_var_str), desc(!!as.name(value_var_str)))
        },
        !!as.name(subject_var_str) := factor(!!as.name(subject_var_str), levels = unique(!!as.name(subject_var_str))),
        tooltip = {
          default_tip <- sprintf(
            "%s: %s <br>%s: %s%% <br>%s: %s",
            subject_var_label, !!as.name(subject_var_str),
            value_var_label, !!as.name(value_var_str),
            color_var_label, !!as.name(color_var_str)
          )
          if (is.null(tooltip_vars_sym)) {
            default_tip
          } else {
            cur_data <- dplyr::pick(dplyr::everything())
            cols <- intersect(tooltip_vars_sym, names(cur_data))
            if (!length(cols)) {
              default_tip
            } else {
              sub <- cur_data[cols]
              labels <- vapply(cols, function(cn) {
                if (cn == subject_var_str) {
                  lb <- subject_var_label
                } else if (cn == value_var_str) {
                  lb <- value_var_label
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
      ) %>%
        dplyr::filter(!duplicated(!!as.name(subject_var_str))) %>%
        dplyr::mutate(customdata = dplyr::row_number())

      p <- plotly::plot_ly(
        data = plot_data,
        source = source_sym,
        customdata = ~customdata,
        height = height_sym
      ) %>%
        plotly::add_bars(
          x = ~subject_var_sym,
          y = ~value_var_sym,
          color = ~color_var_sym,
          colors = colors_sym,
          text = ~tooltip,
          hoverinfo = "text"
        ) %>%
        plotly::layout(
          shapes = lapply(value_arbitrary_hlines_sym, function(y) {
            list(
              type = "line",
              x0 = 0,
              x1 = 1,
              xref = "paper",
              y0 = y,
              y1 = y,
              line = list(color = "black", dash = "dot")
            )
          }),
          xaxis = list(title = subject_var_label, tickangle = -45),
          yaxis = list(title = value_var_label),
          legend = list(title = list(text = "<b>Color by:</b>")),
          barmode = "relative"
        ) %>%
        plotly::layout(dragmode = "select") %>%
        plotly::config(displaylogo = FALSE) %>%
        plotly::layout(title = title_sym)
    },
    list(
      df_sym = str2lang(df),
      subject_var_sym = str2lang(subject_var),
      value_var_sym = str2lang(value_var),
      sort_var_sym = if (!is.null(sort_var)) str2lang(sort_var) else NULL,
      color_var_sym = if (!is.null(color_var)) str2lang(color_var) else NULL,
      subject_var_str = subject_var,
      value_var_str = value_var,
      sort_var_str = sort_var,
      color_var_str = color_var,
      colors_sym = colors,
      value_arbitrary_hlines_sym = value_arbitrary_hlines,
      height_sym = height,
      title_sym = title,
      source_sym = source,
      tooltip_vars_sym = tooltip_vars
    )
  )
}
