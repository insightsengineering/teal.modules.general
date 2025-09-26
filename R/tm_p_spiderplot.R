#' Spider Plot Module
#'
#' This module creates an interactive spider plot visualization that shows value development
#' over time grouped by subjects. The plot displays individual trajectories as connected
#' lines and points, with support for color coding and symbol differentiation. Optional
#' filtering by categorical variables allows dynamic data subsetting. The plot includes customizable
#' tooltips and point sizing based on data values.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param plot_dataname (`character(1)`) Name of the dataset to be used for plotting.
#' @param time_var (`character(1)`) Name of the numeric column in `plot_dataname` to be used as x-axis.
#' @param value_var (`character(1)`) Name of the numeric column in `plot_dataname` to be used as y-axis.
#' @param id_var (`character(1)`) Name of the factor or character column in `plot_dataname`
#' to be used as grouping variable for displayed lines/points.
#' @param color_var (`character(1)`) Name of the factor or character column in `plot_dataname`
#' to be used to differentiate colors and symbols.
#' @param filter_var (`character(1)`) Name of the factor or character column in `plot_dataname`
#' to be used to filter the data. The plot will be updated with just the filtered data when the user
#' selects a value from the dropdown menu.
#' @param size_var (`character(1)` or `NULL`) If provided, this numeric column from the `plot_dataname`
#' will be used to determine the size of the points. If `NULL`, a fixed size is used.
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing time, value, id, and color variables.
#' @param point_colors (`named character` or `NULL`) Valid color names or hex-colors named by levels of `color_var` column.
#' If `NULL`, default colors will be used.
#' @param point_symbols (`named character` or `NULL`) Valid plotly symbol names named by levels of `color_var` column.
#' If `NULL`, default symbols will be used.
#' @param plot_height (`numeric(3)`) Vector of length 3 with c(default, min, max) plot height values.
#'
#' @inherit shared_params return
#'
#' @examples
#' data <- teal_data() |>
#'   within({
#'     df <- data.frame(
#'       subject_id = rep(paste0("S", 1:8), each = 6),
#'       time_week = rep(c(0, 4, 8, 12, 16, 20), 8),
#'       percent_change = c(
#'         rnorm(24, -10, 15), # First 4 subjects with some improvement
#'         rnorm(24, 5, 10) # Last 4 subjects with less improvement
#'       ),
#'       response_category = rep(c("Complete Response", "Partial Response", "Stable Disease", "Progressive Disease"), 12),
#'       treatment = rep(c("Active", "Placebo"), each = 24),
#'       age_group = rep(c("Young", "Old", "Young", "Old"), 12),
#'       baseline_tumor_size = rep(rnorm(8, 50, 10), each = 6),
#'       center = rep(c("Site A", "Site B", "Site C", "Site D"), 12)
#'     )
#'
#'     # Add labels
#'     attr(df$subject_id, "label") <- "Subject ID"
#'     attr(df$time_week, "label") <- "Time (weeks)"
#'     attr(df$percent_change, "label") <- "Percent Change from Baseline"
#'     attr(df$response_category, "label") <- "Response Category"
#'     attr(df$treatment, "label") <- "Treatment Group"
#'     attr(df$age_group, "label") <- "Age Group"
#'     attr(df$baseline_tumor_size, "label") <- "Baseline Tumor Size (mm)"
#'     attr(df$center, "label") <- "Study Center"
#'   })
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_p_spiderplot(
#'       label = "Basic Spider Plot",
#'       plot_dataname = "df",
#'       time_var = "time_week",
#'       value_var = "percent_change",
#'       id_var = "subject_id",
#'       color_var = "response_category",
#'       filter_var = "treatment"
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
#'     tm_p_spiderplot(
#'       label = "Advanced Spider Plot with All Features",
#'       plot_dataname = "df",
#'       time_var = "time_week",
#'       value_var = "percent_change",
#'       id_var = "subject_id",
#'       color_var = "response_category",
#'       filter_var = "treatment",
#'       size_var = "baseline_tumor_size",
#'       tooltip_vars = c("subject_id", "time_week", "percent_change", "response_category", "treatment", "age_group", "center"),
#'       point_colors = c(
#'         "Complete Response" = "#00FF00",
#'         "Partial Response" = "#FFFF00",
#'         "Stable Disease" = "#FFA500",
#'         "Progressive Disease" = "#FF0000"
#'       ),
#'       point_symbols = c(
#'         "Complete Response" = "circle",
#'         "Partial Response" = "square",
#'         "Stable Disease" = "triangle-up",
#'         "Progressive Disease" = "diamond"
#'       ),
#'       plot_height = c(700, 400, 1000)
#'     )
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
tm_p_spiderplot <- function(label = "Spiderplot",
                            plot_dataname,
                            time_var,
                            value_var,
                            id_var,
                            color_var,
                            filter_var,
                            size_var = NULL,
                            tooltip_vars = NULL,
                            point_colors = character(0),
                            point_symbols = character(0),
                            plot_height = c(600, 400, 1200),
                            transformators = list()) {
  if (is.character(time_var)) {
    time_var <- choices_selected(choices = time_var, selected = time_var)
  }
  if (is.character(value_var)) {
    value_var <- choices_selected(choices = value_var, selected = value_var)
  }
  if (is.character(id_var)) {
    id_var <- choices_selected(choices = id_var, selected = id_var)
  }
  if (is.character(color_var)) {
    color_var <- choices_selected(choices = color_var, selected = color_var)
  }
  if (is.character(filter_var)) {
    filter_var <- choices_selected(choices = filter_var, selected = filter_var)
  }

  module(
    label = label,
    ui = ui_p_spiderplot,
    server = srv_p_spiderplot,
    ui_args = list(height = plot_height),
    server_args = list(
      plot_dataname = plot_dataname,
      time_var = time_var,
      value_var = value_var,
      id_var = id_var,
      filter_var = filter_var,
      color_var = color_var,
      size_var = size_var,
      point_colors = point_colors,
      point_symbols = point_symbols,
      tooltip_vars = tooltip_vars
    ),
    datanames = plot_dataname,
    transformators = transformators
  )
}


ui_p_spiderplot <- function(id, height) {
  ns <- NS(id)
  bslib::page_sidebar(
    sidebar = div(
      selectInput(ns("time_var"), label = "Time variable (x-axis):", choices = NULL, selected = NULL, multiple = FALSE),
      selectInput(
        ns("value_var"),
        label = "Value variable (y-axis):",
        choices = NULL, selected = NULL, multiple = FALSE
      ),
      selectInput(ns("id_var"), label = "ID variable:", choices = NULL, selected = NULL, multiple = FALSE),
      selectInput(ns("color_var"), label = "Color by:", choices = NULL, selected = NULL, multiple = FALSE),
      selectInput(ns("filter_var"), label = "Filter by:", choices = NULL, selected = NULL, multiple = FALSE),
      selectInput(ns("filter_var_level"), label = "Select value:", choices = NULL, selected = NULL, multiple = FALSE),
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
      )
    )
  )
}

srv_p_spiderplot <- function(id,
                             data,
                             plot_dataname,
                             time_var,
                             value_var,
                             id_var,
                             filter_var,
                             color_var,
                             point_colors,
                             point_symbols,
                             size_var = NULL,
                             plot_height = 600,
                             tooltip_vars = NULL,
                             filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    .update_cs_input(inputId = "value_var", data = reactive(data()[[dataname]]), cs = value_var)
    .update_cs_input(inputId = "time_var", data = reactive(data()[[dataname]]), cs = time_var)
    .update_cs_input(inputId = "id_var", data = reactive(data()[[dataname]]), cs = id_var)
    .update_cs_input(inputId = "color_var", data = reactive(data()[[dataname]]), cs = color_var)
    .update_cs_input(inputId = "filter_var", data = reactive(data()[[dataname]]), cs = filter_var)

    filter_var_levels <- reactive({
      req(data(), input$filter_var)
      # comment:
      #  i don't know if it makes sense. I think it will be rare that dataset would have multiple
      #  category variables. There would rather be another dataset (consider responses, interventions etc.)
      unique(data()[[plot_dataname]][[input$filter_var]])
    })
    observeEvent(filter_var_levels(), {
      label <- attr(data()[[plot_dataname]][[input$filter_var]], "label")
      updateSelectInput(
        inputId = "filter_var_level",
        label = sprintf("Select %s:", if (length(label)) label else "value:"),
        choices = filter_var_levels(),
        selected = filter_var_levels()[1]
      )
      if (length(filter_var_levels()) < 2) shinyjs::hide("filter_var_level")
    })

    color_inputs <- colour_picker_srv(
      "colors",
      x = reactive({
        req(input$color_var)
        data()[[plot_dataname]][[input$color_var]]
      }),
      default_colors = point_colors
    )

    plotly_q <- reactive({
      req(
        input$filter_var_level, input$time_var, input$value_var,
        input$id_var, input$filter_var, input$color_var, color_inputs()
      )

      adjusted_symbols <- .shape_palette_discrete(
        levels = unique(data()[[plot_dataname]][[input$color_var]]),
        symbol = point_symbols
      )

      data() |>
        within(
          code,
          code = spiderplotly(
            df = plot_dataname,
            time_var = input$time_var,
            value_var = input$value_var,
            id_var = input$id_var,
            filter_var = input$filter_var,
            selected_value = input$filter_var_level,
            color_var = input$color_var,
            colors = color_inputs(),
            symbols = adjusted_symbols,
            size_var = size_var,
            height = input$plot_height,
            point_size = 10,
            title = sprintf("%s over time", input$filter_var_level),
            tooltip_vars = tooltip_vars,
            source = session$ns("spiderplot")
          )
        )
    })

    output$plot <- output$plot <- plotly::renderPlotly(plotly::event_register(
      {
        plotly_q()$p |>
          set_plot_data(session$ns("plot_data")) |>
          setup_trigger_tooltips(session$ns)
      },
      "plotly_selected"
    ))

    observeEvent(data(), {
      if (class(id_var) == "choices_selected") {
        subject_col <- id_var$selected
      } else {
        subject_col <- id_var
      }
      updateSelectInput(
        inputId = "id_var",
        choices = data()[[plot_dataname]][[subject_col]]
      )
    })
  })
}

#' Generate Spider Plotly Code
#'
#' Creates code expression that generates a spider plot with tooltips using plotly.
#' This function includes all the data manipulation and plot creation logic
#' from tm_p_spiderplot module, including filtering, label extraction, tooltip generation,
#' line segments creation, and event registration.
#'
#' @param df (`character(1)`) Name of the data frame to plot
#' @param time_var (`character(1)`) Name of the numeric column to be used as x-axis
#' @param value_var (`character(1)`) Name of the numeric column to be used as y-axis
#' @param subject_var (`character(1)`) Name of the factor or character column to be used as grouping variable
#' @param filter_var (`character(1)`) Name of the factor or character column to be used to filter the data
#' @param selected_value (`character(1)`) Selected value for filtering
#' @param color_var (`character(1)`) Name of the factor or character column to be used to differentiate colors and symbols
#' @param colors (`character`) Named vector of colors for color_var levels
#' @param symbols (`character`) Named vector of plotly symbols for color_var levels
#' @param size_var (`character(1)` or `NULL`) If provided, this numeric column will determine point size
#' @param height (`numeric(1)`) Plot height in pixels
#' @param point_size (`numeric(1)`) Fixed point size when size_var is NULL
#' @param title (`character(1)`) Plot title
#' @param source (`character(1)`) Source identifier for plotly events
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing time, value, id, and color variables.
#'
#' @return A code expression that when evaluated creates a plotly plot object
#'
#' @examples
#' # Generate code for a spider plot
#' code <- spiderplotly(
#'   df = "spider_data",
#'   time_var = "time_point",
#'   value_var = "response",
#'   id_var = "subject_id",
#'   filter_var = "event_type",
#'   selected_value = "response",
#'   color_var = "treatment",
#'   colors = c("Active" = "red", "Placebo" = "blue"),
#'   symbols = c("Active" = "circle", "Placebo" = "square"),
#'   size_var = NULL,
#'   height = 600,
#'   point_size = 10,
#'   title = "Spider Plot",
#'   source = "spiderplot",
#'   tooltip_vars = c("subject_id", "treatment")
#' )
#'
spiderplotly <- function(df, time_var, value_var, id_var, filter_var, selected_value,
                         color_var, colors, symbols, size_var = NULL, height = 600, point_size = 10,
                         title = "Spider Plot", source, tooltip_vars = NULL) {
  substitute(
    {
      plot_data <- df_sym %>%
        dplyr::filter(filter_var_sym == selected_value) %>%
        dplyr::arrange(id_var_sym, time_var_sym) %>%
        dplyr::group_by(id_var_sym)

      id_var_label <- attr(plot_data[[id_var_str]], "label")
      if (!length(id_var_label)) id_var_label <- id_var_str
      time_var_label <- attr(plot_data[[time_var_str]], "label")
      if (!length(time_var_label)) time_var_label <- time_var_str
      value_var_label <- attr(plot_data[[value_var_str]], "label")
      if (!length(value_var_label)) value_var_label <- value_var_str
      color_var_label <- attr(plot_data[[color_var_str]], "label")
      if (!length(color_var_label)) color_var_label <- color_var_str

      plot_data <- plot_data |>
        dplyr::mutate(customdata = dplyr::row_number())

      if (is.null(size_var)) {
        size <- point_size
      } else {
        size <- ~size_var_sym
      }

      p <- plot_data %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          x = dplyr::lag(time_var_sym, default = 0),
          y = dplyr::lag(value_var_sym, default = 0),
          tooltip = {
            if (is.null(tooltip_vars_sym)) {
              sprintf(
                "%s: %s <br>%s: %s <br>%s: %s%% <br>",
                id_var_label, id_var_sym,
                time_var_label, time_var_sym,
                value_var_label, value_var_sym * 100
              )
            } else {
              cur_data <- dplyr::cur_data()
              cols <- intersect(tooltip_vars_sym, names(cur_data))
              if (!length(cols)) {
                sprintf(
                  "%s: %s <br>%s: %s <br>%s: %s%% <br>",
                  id_var_label, id_var_sym,
                  time_var_label, time_var_sym,
                  value_var_label, value_var_sym * 100
                )
              } else {
                sub <- cur_data[cols]
                labels <- vapply(cols, function(cn) {
                  if (cn == id_var_str) {
                    lb <- id_var_label
                  } else if (cn == time_var_str) {
                    lb <- time_var_label
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
        dplyr::ungroup() %>%
        plotly::plot_ly(
          source = source,
          height = height,
          color = ~color_var_sym,
          colors = colors,
          symbols = symbols
        ) %>%
        plotly::add_segments(
          x = ~x,
          y = ~y,
          xend = ~time_var_sym,
          yend = ~value_var_sym,
          customdata = NULL
        ) %>%
        plotly::add_markers(
          x = ~time_var_sym,
          y = ~value_var_sym,
          symbol = ~color_var_sym,
          size = size,
          text = ~tooltip,
          hoverinfo = "text",
          customdata = ~customdata
        ) %>%
        plotly::layout(
          xaxis = list(title = time_var_label),
          yaxis = list(title = value_var_label),
          title = title,
          dragmode = "select"
        ) %>%
        plotly::config(displaylogo = FALSE) %>%
        plotly::layout(title = title)
    },
    list(
      df_sym = str2lang(df),
      time_var_sym = str2lang(time_var),
      value_var_sym = str2lang(value_var),
      id_var_sym = str2lang(id_var),
      filter_var_sym = str2lang(filter_var),
      color_var_sym = str2lang(color_var),
      size_var_sym = if (!is.null(size_var)) str2lang(size_var) else NULL,
      time_var_str = time_var,
      value_var_str = value_var,
      id_var_str = id_var,
      filter_var_str = filter_var,
      color_var_str = color_var,
      selected_value = selected_value,
      colors = colors,
      symbols = symbols,
      size_var = size_var,
      height = height,
      point_size = point_size,
      title = title,
      source = source,
      tooltip_vars_sym = tooltip_vars
    )
  )
}
