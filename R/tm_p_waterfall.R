#' `teal` module: Waterfall plot
#'
#' Module visualizes subjects sorted decreasingly by y-values.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param plot_dataname (`character(1)`) name of the dataset which visualization is builded on.
#' @param subject_var (`character(1)` or `choices_selected`) name of the `factor` or `character` column
#' in `plot_dataname` to be used as x-axis.
#' @param value_var (`character(1)` or `choices_selected`) name of the `numeric` column
#' in `plot_dataname` to be used as y-axis.
#' @param color_var (`character(1)` or `choices_selected`) name of the `factor` or `character` column in `plot_dataname`
#'  to be used to differentiate bar colors.
#' @param tooltip_vars (`character` or `NULL`) A vector of column names to be displayed in the tooltip.
#' If `NULL`, default tooltip is created.
#' @param bar_colors (`named character`) valid color names (see [colors()]) or hex-colors named
#'  by levels of `color_var` column.
#' @param value_arbitrary_hlines (`numeric`) values in the same scale as `value_var` to horizontal
#'  lines on the plot.
#' @param plot_title (`character`) Title of the plot.
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

      within(
        data(),
        dataname = str2lang(plot_dataname),
        subject_var = input$subject_var,
        value_var = input$value_var,
        sort_var = input$sort_var,
        color_var = input$color_var,
        colors = color_inputs(),
        value_arbitrary_hlines = value_arbitrary_hlines,
        height = input$plot_height,
        title = sprintf("Waterfall plot"),
        tooltip_vars = tooltip_vars,
        source = session$ns("waterfall"),
        expr = {
          subject_var_label <- attr(dataname[[subject_var]], "label")
          if (!length(subject_var_label)) subject_var_label <- subject_var
          value_var_label <- attr(dataname[[value_var]], "label")
          if (!length(value_var_label)) value_var_label <- value_var
          color_var_label <- attr(dataname[[color_var]], "label")
          if (!length(color_var_label)) color_var_label <- color_var


          plot_data <- dplyr::mutate(
            if (identical(sort_var, value_var) || is.null(sort_var)) {
              dplyr::arrange(dataname, desc(!!as.name(value_var)))
            } else {
              dplyr::arrange(dataname, !!as.name(sort_var), desc(!!as.name(value_var)))
            },
            !!as.name(subject_var) := factor(!!as.name(subject_var), levels = unique(!!as.name(subject_var))),
            tooltip = {
              default_tip <- sprintf(
                "%s: %s <br>%s: %s%% <br>%s: %s",
                subject_var_label, !!as.name(subject_var),
                value_var_label, !!as.name(value_var),
                color_var_label, !!as.name(color_var)
              )
              if (is.null(tooltip_vars)) {
                default_tip
              } else {
                cur_data <- dplyr::pick(dplyr::everything())
                cols <- intersect(tooltip_vars, names(cur_data))
                if (!length(cols)) {
                  default_tip
                } else {
                  sub <- cur_data[cols]
                  labels <- vapply(cols, function(cn) {
                    lb <- attr(sub[[cn]], "label")
                    if (length(lb) && !is.null(lb) && !is.na(lb)) as.character(lb) else cn
                  }, character(1))
                  values <- lapply(sub, as.character)
                  parts <- Map(function(v, l) paste0(l, ": ", v), values, labels)
                  do.call(paste, c(parts, sep = "<br>"))
                }
              }
            }
          ) %>%
            dplyr::filter(!duplicated(!!as.name(subject_var))) %>%
            dplyr::mutate(customdata = dplyr::row_number())
          p <- plotly::plot_ly(
            data = plot_data,
            source = source,
            customdata = ~customdata,
            height = height
          ) %>%
            plotly::add_bars(
              x = stats::as.formula(sprintf("~%s", subject_var)),
              y = stats::as.formula(sprintf("~%s", value_var)),
              color = stats::as.formula(sprintf("~%s", color_var)),
              colors = colors,
              text = ~tooltip,
              hoverinfo = "text"
            ) %>%
            plotly::layout(
              shapes = lapply(value_arbitrary_hlines, function(y) {
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
            plotly::layout(title = title)
        },
        height = input$plot_height
      )
    })

    output$plot <- plotly::renderPlotly(plotly::event_register(plotly_q()$p, "plotly_selected"))

    plotly_selected <- reactive(plotly::event_data("plotly_selected", source = session$ns("waterfall")))

    reactive({
      req(plotly_selected())
      plotly_q() |>
        within(
          {
            selected_plot_data <- plot_data |>
              dplyr::filter(customdata %in% plotly_selected_customdata)
            dataname <- dataname |>
              dplyr::filter(!!sym(subject_var) %in% selected_plot_data[[subject_var]])
          },
          dataname = str2lang(plot_dataname),
          subject_var = input$subject_var,
          plotly_selected_customdata = plotly_selected()$customdata
        )
    })
  })
}
