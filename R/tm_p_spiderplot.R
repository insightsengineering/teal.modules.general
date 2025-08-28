#' `teal` module: Spider Plot
#'
#' Module visualizes value development in time grouped by subjects.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param plot_dataname (`character(1)` or `choices_selected`) name of the dataset which visualization is builded on.
#' @param time_var (`character(1)` or `choices_selected`) name of the `numeric` column
#' in `plot_dataname` to be used as x-axis.
#' @param value_var (`character(1)` or `choices_selected`) name of the `numeric` column
#' in `plot_dataname` to be used as y-axis.
#' @param subject_var (`character(1)` or `choices_selected`) name of the `factor` or `character` column
#' in `plot_dataname` to be used as grouping variable for displayed lines/points.
#' @param color_var (`character(1)` or `choices_selected`) name of the `factor` or `character` column in `plot_dataname`
#'  to be used to differentiate colors and symbols.
#' @param filter_event_var (`character(1)` or `choices_selected`) name of the `factor` or `character` column
#' in `plot_dataname` to be used to filter the data.
#' The plot will be updated with just the filtereed data when the user selects an event from the dropdown menu.
#' @param size_var (`character(1)` or `NULL`) If provided, this numeric column from the `plot_dataname`
#' will be used to determine the size of the points. If `NULL`, a fixed size based on the `point_size` is used.
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created.
#' @param point_colors (`named character`) valid color names (see [colors()]) or hex-colors named
#'  by levels of `color_var` column.
#' @param point_symbols (`named character`) valid plotly symbol name named  by levels of `color_var`column.
#' @param table_datanames (`character`) Names of the datasets to be displayed in the tables below the plot.
#' @param reactable_args (`list`) Additional arguments passed to the `reactable` function for table customization.
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
#'       table_datanames = "subjects",
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
                            plot_height = c(600, 400, 1200),
                            table_datanames = character(0),
                            reactable_args = list()) {
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
      table_datanames = table_datanames,
      reactable_args = reactable_args,
      tooltip_vars = tooltip_vars
    ),
    datanames = union(plot_dataname, table_datanames)
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
      sliderInput(ns("plot_height"), "Plot Height (px)", height[2], height[3], height[1]),
      selectInput(ns("subjects"), "Subjects", choices = NULL, selected = NULL, multiple = TRUE),
      actionButton(ns("subject_tooltips"), "Show Subject Tooltips")
    ),
    tags$div(
      bslib::card(
        full_screen = TRUE,
        tags$div(
          trigger_tooltips_deps(),
          plotly::plotlyOutput(ns("plot"), height = "100%")
        )
      ),
      ui_t_reactables(ns("subtables"))
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
                             table_datanames = character(0),
                             reactable_args = list(),
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

      within(
        data(),
        dataname = str2lang(plot_dataname),
        filter_event_var_lang = str2lang(input$filter_event_var),
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
        expr = {
          plot_data <- dataname %>%
            dplyr::filter(filter_event_var_lang == selected_event) %>%
            dplyr::arrange(!!as.name(subject_var), !!as.name(time_var)) %>%
            dplyr::group_by(!!as.name(subject_var))
          subject_var_label <- attr(plot_data[[subject_var]], "label")
          if (!length(subject_var_label)) subject_var_label <- subject_var
          time_var_label <- attr(plot_data[[time_var]], "label")
          if (!length(time_var_label)) time_var_label <- time_var
          value_var_label <- attr(plot_data[[value_var]], "label")
          if (!length(value_var_label)) value_var_label <- value_var
          plot_data <- plot_data |>
            dplyr::mutate(customdata = dplyr::row_number())

          if (is.null(size_var)) {
            size <- point_size
          } else {
            size <- stats::as.formula(sprintf("~%s", size_var))
          }

          p <- plot_data %>%
            dplyr::mutate(
              x = dplyr::lag(!!as.name(time_var), default = 0),
              y = dplyr:::lag(!!as.name(value_var), default = 0),
              tooltip = {
                if (is.null(tooltip_vars)) {
                  sprintf(
                    "%s: %s <br>%s: %s <br>%s: %s%% <br>",
                    subject_var_label, !!as.name(subject_var),
                    time_var_label, !!as.name(time_var),
                    value_var_label, !!as.name(value_var) * 100
                  )
                } else {
                  tooltip_lines <- sapply(tooltip_vars, function(col) {
                    label <- .get_column_label(.data, col)
                    value <- .data[[col]]
                    paste0(label, ": ", value)
                  })
                  if (is.vector(tooltip_lines)) {
                    paste(tooltip_lines, collapse = "<br>")
                  } else {
                    apply(tooltip_lines, 1, function(row) paste(row, collapse = "<br>"))
                  }
                }
              }
            ) %>%
            dplyr::ungroup() %>%
            plotly::plot_ly(
              source = "spiderplot",
              height = height,
              color = stats::as.formula(sprintf("~%s", color_var)),
              colors = colors,
              symbols = symbols
            ) %>%
            plotly::add_segments(
              x = ~x,
              y = ~y,
              xend = stats::as.formula(sprintf("~%s", time_var)),
              yend = stats::as.formula(sprintf("~%s", value_var)),
              customdata = NULL
            ) %>%
            plotly::add_markers(
              x = stats::as.formula(sprintf("~%s", time_var)),
              y = stats::as.formula(sprintf("~%s", value_var)),
              symbol = stats::as.formula(sprintf("~%s", color_var)),
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
        }
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

    plotly_data <- reactive({
      data.frame(
        x = unlist(input$plot_data$x),
        y = unlist(input$plot_data$y),
        customdata = unlist(input$plot_data$customdata),
        curve = unlist(input$plot_data$curveNumber),
        index = unlist(input$plot_data$pointNumber)
      )
    })

    plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "spiderplot"))

    observeEvent(input$subject_tooltips, {
      hovervalues <- data()[[plot_dataname]] |>
        dplyr::mutate(customdata = dplyr::row_number()) |>
        dplyr::filter(!!rlang::sym(input$subject_var) %in% input$subjects) |>
        dplyr::pull(customdata)

      hovertips <- plotly_data() |>
        dplyr::filter(customdata %in% hovervalues)

      session$sendCustomMessage(
        "triggerTooltips",
        list(
          plotID = session$ns("plot"),
          tooltipPoints = jsonlite::toJSON(hovertips)
        )
      )
    })

    tables_selected_q <- .plotly_selected_filter_children(
      data = plotly_q,
      plot_dataname = plot_dataname,
      xvar = reactive(input$time_var),
      yvar = reactive(input$value_var),
      plotly_selected = plotly_selected,
      children_datanames = table_datanames
    )

    srv_t_reactables(
      "subtables",
      data = tables_selected_q,
      datanames = table_datanames,
      reactable_args = reactable_args
    )
  })
}
