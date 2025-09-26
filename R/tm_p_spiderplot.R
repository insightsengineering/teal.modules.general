#' Spider Plot Module
#'
#' This module creates an interactive spider plot visualization that shows value development
#' over time grouped by subjects. The plot displays individual trajectories as connected
#' lines and points, with support for color coding and symbol differentiation. Optional
#' filtering by event variables allows dynamic data subsetting. The plot includes customizable
#' tooltips and point sizing based on data values.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param plot_dataname (`character(1)`) Name of the dataset to be used for plotting.
#' @param time_var (`character(1)`) Name of the numeric column in `plot_dataname` to be used as x-axis.
#' @param value_var (`character(1)`) Name of the numeric column in `plot_dataname` to be used as y-axis.
#' @param subject_var (`character(1)`) Name of the factor or character column in `plot_dataname`
#' to be used as grouping variable for displayed lines/points.
#' @param color_var (`character(1)`) Name of the factor or character column in `plot_dataname`
#' to be used to differentiate colors and symbols.
#' @param filter_event_var (`character(1)`) Name of the factor or character column in `plot_dataname`
#' to be used to filter the data. The plot will be updated with just the filtered data when the user
#' selects an event from the dropdown menu.
#' @param size_var (`character(1)` or `NULL`) If provided, this numeric column from the `plot_dataname`
#' will be used to determine the size of the points. If `NULL`, a fixed size is used.
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing time, value, subject, and color variables.
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
#'
#'     spiderplot_ds <- data.frame(
#'       subject_var = sample(c("A", "B", "C"), 10, replace = TRUE),
#'       time_var = 1:10,
#'       filter_event_var = "response",
#'       color_var = sample(c("CR", "PR", "SD", "PD"), 10, replace = TRUE),
#'       value_var = sample(-50:100, 10, replace = TRUE)
#'     )
#'
#'     waterfall_ds <- data.frame(
#'       subject_var = sample(c("A", "B", "C"), 10, replace = TRUE),
#'       value_var = sample(-20:90, 10, replace = TRUE),
#'       color_var = sample(c("CR", "PR", "SD", "PD"), 10, replace = TRUE)
#'     )
#'   })
#' join_keys(data) <- join_keys(
#'   join_key("subjects", "spiderplot_ds", keys = c(subject_var = "subject_var"))
#' )
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_p_spiderplot(
#'       plot_dataname = "spiderplot_ds",
#'       time_var = "time_var",
#'       value_var = "value_var",
#'       subject_var = "subject_var",
#'       filter_event_var = "filter_event_var",
#'       color_var = "color_var",
#'       point_colors = c(
#'         CR = "#FF0000", PR = "#00FF00", SD = "#0000FF", PD = "#FFFF00"
#'       ),
#'       point_symbols = c(
#'         CR = "circle", PR = "square", SD = "triangle-up", PD = "diamond"
#'       ),
#'       tooltip_vars = c("subject_var")
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
                            subject_var,
                            color_var,
                            filter_event_var,
                            size_var = NULL,
                            tooltip_vars = NULL,
                            point_colors = character(0),
                            point_symbols = character(0),
                            plot_height = c(600, 400, 1200)) {
  if (is.character(time_var)) {
    time_var <- choices_selected(choices = time_var, selected = time_var)
  }
  if (is.character(value_var)) {
    value_var <- choices_selected(choices = value_var, selected = value_var)
  }
  if (is.character(subject_var)) {
    subject_var <- choices_selected(choices = subject_var, selected = subject_var)
  }
  if (is.character(color_var)) {
    color_var <- choices_selected(choices = color_var, selected = color_var)
  }
  if (is.character(filter_event_var)) {
    filter_event_var <- choices_selected(choices = filter_event_var, selected = filter_event_var)
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
      subject_var = subject_var,
      filter_event_var = filter_event_var,
      color_var = color_var,
      size_var = size_var,
      point_colors = point_colors,
      point_symbols = point_symbols,
      tooltip_vars = tooltip_vars
    ),
    datanames = plot_dataname
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
      selectInput(ns("subject_var"), label = "Subject variable:", choices = NULL, selected = NULL, multiple = FALSE),
      selectInput(ns("color_var"), label = "Color by:", choices = NULL, selected = NULL, multiple = FALSE),
      selectInput(ns("filter_event_var"), label = "Event variable:", choices = NULL, selected = NULL, multiple = FALSE),
      selectInput(ns("filter_event_var_level"), label = "Select an event:", choices = NULL, selected = NULL, multiple = FALSE),
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
                             subject_var,
                             filter_event_var,
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
    .update_cs_input(inputId = "subject_var", data = reactive(data()[[dataname]]), cs = subject_var)
    .update_cs_input(inputId = "color_var", data = reactive(data()[[dataname]]), cs = color_var)
    .update_cs_input(inputId = "filter_event_var", data = reactive(data()[[dataname]]), cs = filter_event_var)

    filter_event_var_levels <- reactive({
      req(data(), input$filter_event_var)
      # comment:
      #  i don't know if it makes sense. I think it will be rare that dataset would have multiple
      #  category variables. There would rather be another dataset (consider responses, interventions etc.)
      unique(data()[[plot_dataname]][[input$filter_event_var]])
    })
    observeEvent(filter_event_var_levels(), {
      label <- attr(data()[[plot_dataname]][[input$filter_event_var]], "label")
      updateSelectInput(
        inputId = "filter_event_var_level",
        label = sprintf("Select %s:", if (length(label)) label else "en event:"),
        choices = filter_event_var_levels(),
        selected = filter_event_var_levels()[1]
      )
      if (length(filter_event_var_levels()) < 2) shinyjs::hide("filter_event_var_level")
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
        input$filter_event_var_level, input$time_var, input$value_var,
        input$subject_var, input$filter_event_var, input$color_var, color_inputs()
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
            subject_var = input$subject_var,
            filter_event_var = input$filter_event_var,
            selected_event = input$filter_event_var_level,
            color_var = input$color_var,
            colors = color_inputs(),
            symbols = adjusted_symbols,
            size_var = size_var,
            height = input$plot_height,
            point_size = 10,
            title = sprintf("%s over time", input$filter_event_var_level),
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
      if (class(subject_var) == "choices_selected") {
        subject_col <- subject_var$selected
      } else {
        subject_col <- subject_var
      }
      updateSelectInput(
        inputId = "subjects",
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
#' @param filter_event_var (`character(1)`) Name of the factor or character column to be used to filter the data
#' @param selected_event (`character(1)`) Selected event value for filtering
#' @param color_var (`character(1)`) Name of the factor or character column to be used to differentiate colors and symbols
#' @param colors (`character`) Named vector of colors for color_var levels
#' @param symbols (`character`) Named vector of plotly symbols for color_var levels
#' @param size_var (`character(1)` or `NULL`) If provided, this numeric column will determine point size
#' @param height (`numeric(1)`) Plot height in pixels
#' @param point_size (`numeric(1)`) Fixed point size when size_var is NULL
#' @param title (`character(1)`) Plot title
#' @param source (`character(1)`) Source identifier for plotly events
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created showing time, value, subject, and color variables.
#'
#' @return A code expression that when evaluated creates a plotly plot object
#'
#' @examples
#' # Generate code for a spider plot
#' code <- spiderplotly(
#'   df = "spider_data",
#'   time_var = "time_point",
#'   value_var = "response",
#'   subject_var = "subject_id",
#'   filter_event_var = "event_type",
#'   selected_event = "response",
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
spiderplotly <- function(df, time_var, value_var, subject_var, filter_event_var, selected_event,
                         color_var, colors, symbols, size_var = NULL, height = 600, point_size = 10,
                         title = "Spider Plot", source, tooltip_vars = NULL) {
  substitute(
    {
      plot_data <- df_sym %>%
        dplyr::filter(!!as.name(filter_event_var_str) == selected_event_sym) %>%
        dplyr::arrange(!!as.name(subject_var_str), !!as.name(time_var_str)) %>%
        dplyr::group_by(!!as.name(subject_var_str))

      subject_var_label <- attr(plot_data[[subject_var_str]], "label")
      if (!length(subject_var_label)) subject_var_label <- subject_var_str
      time_var_label <- attr(plot_data[[time_var_str]], "label")
      if (!length(time_var_label)) time_var_label <- time_var_str
      value_var_label <- attr(plot_data[[value_var_str]], "label")
      if (!length(value_var_label)) value_var_label <- value_var_str
      color_var_label <- attr(plot_data[[color_var_str]], "label")
      if (!length(color_var_label)) color_var_label <- color_var_str

      plot_data <- plot_data |>
        dplyr::mutate(customdata = dplyr::row_number())

      if (is.null(size_var_sym)) {
        size <- point_size_sym
      } else {
        size <- ~size_var_lang
      }

      p <- plot_data %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          x = dplyr::lag(!!as.name(time_var_str), default = 0),
          y = dplyr::lag(!!as.name(value_var_str), default = 0),
          tooltip = {
            if (is.null(tooltip_vars_sym)) {
              sprintf(
                "%s: %s <br>%s: %s <br>%s: %s%% <br>",
                subject_var_label, !!as.name(subject_var_str),
                time_var_label, !!as.name(time_var_str),
                value_var_label, !!as.name(value_var_str) * 100
              )
            } else {
              cur_data <- dplyr::cur_data()
              cols <- intersect(tooltip_vars_sym, names(cur_data))
              if (!length(cols)) {
                sprintf(
                  "%s: %s <br>%s: %s <br>%s: %s%% <br>",
                  subject_var_label, !!as.name(subject_var_str),
                  time_var_label, !!as.name(time_var_str),
                  value_var_label, !!as.name(value_var_str) * 100
                )
              } else {
                sub <- cur_data[cols]
                labels <- vapply(cols, function(cn) {
                  if (cn == subject_var_str) {
                    lb <- subject_var_label
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
          source = source_sym,
          height = height_sym,
          color = ~color_var_sym,
          colors = colors_sym,
          symbols = symbols_sym
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
          title = title_sym,
          dragmode = "select"
        ) %>%
        plotly::config(displaylogo = FALSE) %>%
        plotly::layout(title = title_sym)
    },
    list(
      df_sym = str2lang(df),
      time_var_sym = str2lang(time_var),
      value_var_sym = str2lang(value_var),
      subject_var_sym = str2lang(subject_var),
      color_var_sym = str2lang(color_var),
      size_var_lang = if (!is.null(size_var)) str2lang(size_var) else NULL,
      time_var_str = time_var,
      value_var_str = value_var,
      subject_var_str = subject_var,
      filter_event_var_str = filter_event_var,
      color_var_str = color_var,
      selected_event_sym = selected_event,
      colors_sym = colors,
      symbols_sym = symbols,
      size_var_sym = size_var,
      height_sym = height,
      point_size_sym = point_size,
      title_sym = title,
      source_sym = source,
      tooltip_vars_sym = tooltip_vars
    )
  )
}
