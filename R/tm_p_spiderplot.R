#' `teal` module: Spider Plot
#'
#' Module visualizes value development in time grouped by subjects.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param time_var (`character(1)` or `variables`) name of the `numeric` column
#' in `plot_dataname` to be used as x-axis.
#' @param value_var (`character(1)` or `variables`) name of the `numeric` column
#' in `plot_dataname` to be used as y-axis.
#' @param subject_var (`character(1)` or `variables`) name of the `factor` or `character` column
#' in `plot_dataname` to be used as grouping variable for displayed lines/points.
#' @param color_var (`character(1)` or `variables`) name of the `factor` or `character` column in `plot_dataname`
#'  to be used to differentiate colors and symbols.
#' @param size_var (`character(1)` or `variables` or `NULL`) If provided, this numeric column from the `plot_dataname`
#' will be used to determine the size of the points. If `NULL`, a fixed size based on the `point_size` is used.
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created.
#' @param point_colors (`named character`) valid color names (see [colors()]) or hex-colors named
#'  by levels of `color_var` column.
#' @param point_symbols (`named character`) valid plotly symbol name named  by levels of `color_var`column.
#'
#' @examples
#' library(teal.transform)
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
#'       time_var = picks(datasets("spiderplot_ds"), variables("time_var")),
#'       value_var = picks(datasets("spiderplot_ds"), variables("value_var")),
#'       subject_var = picks(datasets("spiderplot_ds"), variables("subject_var")),
#'       color_var = picks(datasets("spiderplot_ds"), variables("color_var")),
#'       transformators = list(
#'         teal_transform_filter(
#'           picks(
#'             datasets("spiderplot_ds"), variables("filter_event_var"), values()
#'           )
#'         )
#'       ),
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
                            time_var,
                            value_var,
                            subject_var,
                            color_var,
                            size_var = NULL,
                            tooltip_vars = NULL,
                            point_colors = character(0),
                            point_symbols = character(0),
                            plot_height = c(600, 400, 1200),
                            transformators = list(),
                            decorators = list()) {
  # todo: filter_event_var shouldn't in arguments as it is not a dimension of the plot
  #       title based on arbitrary filter is not an accepted solution.
  #       additional filters should be passed to trasformers
  checkmate::assert_string(label)
  checkmate::assert_class(time_var, "picks")
  checkmate::assert_class(subject_var, "picks")
  checkmate::assert_class(color_var, "picks")
  checkmate::assert_class(size_var, "picks", null.ok = TRUE)

  args <- as.list(environment())
  module(
    label = label,
    ui = ui_p_spiderplot,
    server = srv_p_spiderplot,
    ui_args = args[names(args) %in% names(formals(ui_p_spiderplot))],
    server_args = args[names(args) %in% names(formals(srv_p_spiderplot))],
    transformators = transformators,
    datanames = {
      datanames <- datanames(
        list(
          time_var = time_var, value_var = value_var, subject_var = subject_var,
          color_var = color_var, size_var = size_var
        )
      )
      if (length(datanames)) datanames else "all"
    }
  )
}

ui_p_spiderplot <- function(id, time_var, value_var, subject_var, color_var, size_var, plot_height, decorators) {
  ns <- NS(id)
  bslib::page_sidebar(
    sidebar = div(
      class = "standard-layout encoding-panel",
      teal::teal_nav_item(
        label = tags$strong("Time variable (x-axis):"),
        teal.transform::module_input_ui(id = ns("time_var"), spec = time_var)
      ),
      teal::teal_nav_item(
        label = tags$strong("Value variable (y-axis):"),
        teal.transform::module_input_ui(id = ns("value_var"), spec = value_var)
      ),
      teal::teal_nav_item(
        label = tags$strong("Subject variable:"),
        teal.transform::module_input_ui(id = ns("subject_var"), spec = subject_var)
      ),
      teal::teal_nav_item(
        label = tags$strong("Color by:"),
        teal.transform::module_input_ui(id = ns("color_var"), spec = color_var)
      ),
      if (!is.null(size_var)) {
        colour_picker_ui(ns("colors"))
      },
      ui_decorate_teal_data(ns("decorator"), decorators = decorators),
      sliderInput(ns("plot_height"), "Plot Height (px)", plot_height[2], plot_height[3], plot_height[1])
    ),
    tags$div(
      class = "standard-layout output-panel",
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
                             time_var,
                             value_var,
                             subject_var,
                             color_var,
                             size_var = NULL,
                             tooltip_vars = NULL,
                             point_colors,
                             point_symbols,
                             plot_height = 600,
                             decorators = list(),
                             filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    selectors <- teal.transform::module_input_srv(
      data = data,
      spec = list(
        time_var = time_var, value_var = value_var, subject_var = subject_var,
        color_var = color_var, size_var = size_var
      )
    )

    color_inputs <- colour_picker_srv(
      "colors",
      x = reactive({
        selected_color <- req(map_merged(selectors)$color_var)
        data()[[selected_color$datasets]][[selected_color$variables]]
      }),
      default_colors = point_colors
    )

    merged_q <- reactive({
      req(data(), map_merged(selectors))
      obj <- data()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Spiderplot data preparation")
      qenv_merge_selectors(x = obj, selectors = selectors, output_name = "anl")
    })

    plot_data_q <- reactive({
      obj <- req(merged_q())
      within(obj,
        {
          anl <- anl %>%
            dplyr::mutate(customdata = dplyr::row_number()) %>%
            dplyr::arrange(subject_var_lang, time_var_lang) %>%
            dplyr::group_by(subject_var_lang)
        },
        subject_var_lang = str2lang(map_merged(selectors)$subject_var$variables),
        time_var_lang = str2lang(map_merged(selectors)$time_var$variables)
      )
    })

    output_q <- reactive({
      obj <- req(plot_data_q())
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Spiderplot Visualization")
      adjusted_symbols <- .shape_palette_discrete(
        levels = unique(obj$anl[[map_merged(selectors)$color_var$variables]]),
        symbol = point_symbols
      )

      within(
        obj,
        dataname = str2lang("anl"),
        time_var_lang = str2lang(map_merged(selectors)$time_var$variables),
        value_var_lang = str2lang(map_merged(selectors)$value_var$variables),
        subject_var_lang = str2lang(map_merged(selectors)$subject_var$variables),
        color_var_lang = str2lang(map_merged(selectors)$color_var$variables),
        time_var = map_merged(selectors)$time_var$variables,
        value_var = map_merged(selectors)$value_var$variables,
        subject_var = map_merged(selectors)$subject_var$variables,
        color_var = map_merged(selectors)$color_var$variables,
        colors = color_inputs(),
        symbols = adjusted_symbols,
        size_var = if (!is.null(size_var)) map_merged(selectors)$size_var$variables,
        height = input$plot_height,
        point_size = 10,
        tooltip_vars = tooltip_vars,
        source = session$ns("spiderplot"),
        expr = {
          subject_var_label <- attr(anl[[subject_var]], "label")
          if (!length(subject_var_label)) subject_var_label <- subject_var

          time_var_label <- attr(anl[[time_var]], "label")
          if (!length(time_var_label)) time_var_label <- time_var

          value_var_label <- attr(anl[[value_var]], "label")
          if (!length(value_var_label)) value_var_label <- value_var
          color_var_label <- attr(plot_data[[color_var]], "label")
          if (!length(color_var_label)) color_var_label <- color_var

          if (is.null(size_var)) {
            size <- point_size
          } else {
            size <- stats::as.formula(sprintf("~%s", size_var))
          }

          plot <- anl %>%
            dplyr::ungroup() %>%
            dplyr::mutate(
              x = dplyr::lag(time_var_lang, default = 0),
              y = dplyr:::lag(value_var_lang, default = 0),
              tooltip = {
                if (is.null(tooltip_vars)) {
                  # Default tooltip: show subject, x, y, color variables with labels
                  sprintf(
                    "%s: %s <br>%s: %s <br>%s: %s%% <br>",
                    subject_var_label, subject_var_lang,
                    time_var_label, time_var_lang,
                    value_var_label, value_var_lang * 100
                  )
                } else {
                  # Custom tooltip: show only specified columns
                  cur_data <- dplyr::cur_data()
                  cols <- intersect(tooltip_vars, names(cur_data))
                  if (!length(cols)) {
                    # Fallback to default if no valid columns found
                    sprintf(
                      "%s: %s <br>%s: %s <br>%s: %s%% <br>",
                      subject_var_label, !!as.name(subject_var),
                      time_var_label, !!as.name(time_var),
                      value_var_label, !!as.name(value_var) * 100
                    )
                  } else {
                    # Create tooltip from specified columns
                    sub <- cur_data[cols]
                    labels <- vapply(cols, function(cn) {
                      if (cn == subject_var) {
                        lb <- subject_var_label
                      } else if (cn == time_var) {
                        lb <- time_var_label
                      } else if (cn == value_var) {
                        lb <- value_var_label
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
            ) %>%
            dplyr::ungroup() %>%
            plotly::plot_ly(
              source = source,
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
              title = "Spiderplot",
              dragmode = "select"
            ) %>%
            plotly::config(displaylogo = FALSE)
        }
      )
    })

    decorated_output_plot_q <- srv_decorate_teal_data(
      id = "decorator",
      data = output_q,
      decorators = decorators,
      expr = quote(plot)
    )

    output$plot <- plotly::renderPlotly(plotly::event_register(
      {
        rev(teal.code::get_outputs(decorated_output_plot_q()))[[1]] |>
          set_plot_data(session$ns("plot_data")) |>
          setup_trigger_tooltips(session$ns)
      },
      "plotly_selected"
    ))

    plotly_data <- reactive({
      data.frame(
        x = unlist(input$plot_data$x),
        y = unlist(input$plot_data$y),
        customdata = unlist(input$plot_data$customdata),
        curve = unlist(input$plot_data$curveNumber),
        index = unlist(input$plot_data$pointNumber)
      )
    })

    plotly_selected <- reactive(
      plotly::event_data("plotly_selected", source = session$ns("spiderplot"))
    )


    reactive({
      req(decorated_output_plot_q())
      if (length(plotly_selected()) && nrow(plotly_selected())) {
        within(
          decorated_output_plot_q(),
          anl <- dplyr::filter(anl, customdata %in% selected),
          selected = unique(plotly_selected()$customdata)
        )
      } else {
        decorated_output_plot_q()
      }
    })
  })
}
