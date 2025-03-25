#' `teal` module: Spider Plot
#'
#' Module visualizes value development in time grouped by subjects.
#' 
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param plot_dataname (`character(1)`) name of the dataset which visualization is builded on.
#' @param time_var (`character(1)`) name of the `numeric` column in `plot_dataname` to be used as x-axis.
#' @param value_var (`character(1)`) name of the `numeric` column in `plot_dataname` to be used as y-axis.
#'  column.
#' @param subject_var (`character(1)`) name of the `factor` or `character` column in `plot_dataname` 
#'  to be used as grouping variable for displayed lines/points.
#' @param color_var (`character(1)`) name of the `factor` or `character` column in `plot_dataname` 
#'  to be used to differentiate colors and symbols.
#' @param point_colors (`named character`) valid color names (see [colors()]) or hex-colors named 
#'  by levels of `color_var` column.
#' @param point_symbols (`named character`) valid plotly symbol name named  by levels of `color_var` 
#'  column.
#' @export
tm_g_spiderplot <- function(label = "Spiderplot",
                            plot_dataname,
                            time_var,
                            value_var,
                            subject_var,
                            event_var,
                            color_var,
                            point_colors,
                            point_symbols,
                            plot_height = 600,
                            table_datanames = character(0),
                            reactable_args =  list(),
                            transformator = transformator) {
  module(
    label = label,
    ui = ui_g_spiderplot,
    server = srv_g_spiderplot,
    ui_args = list(height = plot_height),
    server_args = list(
      plot_dataname = plot_dataname,
      time_var = time_var,
      value_var = value_var,
      subject_var = subject_var,
      event_var = event_var,
      color_var = color_var,
      point_colors = point_colors,
      point_symbols = point_symbols,
      table_datanames = table_datanames,
      reactable_args = reactable_args
    ),
    datanames = union(plot_dataname, table_datanames)
  )
}


ui_g_spiderplot <- function(id, height) {
  ns <- NS(id)
  bslib::page_sidebar(
    sidebar = div(
      selectInput(ns("select_event"), "Select Y Axis", NULL),
      colour_picker_ui(ns("colors")),
      sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, height)
    ),
    bslib::page_fillable(
      plotly::plotlyOutput(ns("plot"), height = "100%"),
      ui_t_reactables(ns("subtables"))      
    )
  )
}

srv_g_spiderplot <- function(id,
                             data,
                             plot_dataname,
                             time_var,
                             value_var,
                             subject_var,
                             event_var,
                             color_var,
                             point_colors,
                             point_symbols,
                             plot_height = 600,
                             table_datanames = character(0),
                             reactable_args = list(),
                             filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    event_levels <- reactive({
      req(data())
      unique(data()[[plot_dataname]][[event_var]])
    })
    observeEvent(event_levels(), {
      updateSelectInput(inputId = "select_event",  choices = event_levels(), selected = event_levels()[1])
    })
    
    color_inputs <- colour_picker_srv(
      "colors", 
      x = reactive(data()[[plot_dataname]][[color_var]]),
      default_colors = point_colors
    )
    
    plotly_q <- reactive({
      req(input$select_event, color_inputs())
  
      adjusted_symbols <- .shape_palette_discrete(
        levels = unique(data()[[plot_dataname]][[color_var]]),
        symbol = point_symbols
      )
      
      within(
        data(),
        dataname = str2lang(plot_dataname),
        event_var_lang = str2lang(event_var),
        time_var = time_var,
        value_var = value_var,
        subject_var = subject_var,
        event_var = event_var,
        color_var = color_var,
        selected_event = input$select_event,
        colors = color_inputs(),
        symbols = adjusted_symbols,
        height = input$plot_height,
        title = sprintf("%s over time", input$selected_event),
        expr = {
          p <- dataname %>%
            filter(event_var_lang == selected_event) %>%
            spiderplotly(
              time_var = time_var,
              value_var = value_var,
              subject_var = subject_var,
              event_var = event_var,
              color_var = color_var,
              colors = colors,
              symbols = symbols,
              height = height
            ) %>%
            plotly::layout(title = title)
        }
      )
    })

    output$plot <- plotly::renderPlotly(plotly::event_register(plotly_q()$p, "plotly_selected"))
    
    plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "spiderplot"))
    
    tables_selected_q <- .plotly_selected_filter_children(
      data = plotly_q, 
      plot_dataname = plot_dataname,
      xvar = time_var, 
      yvar = value_var, 
      plotly_selected = plotly_selected, 
      children_datanames = table_datanames
    )
    
    srv_t_reactables("subtables", data = tables_selected_q, dataname = table_datanames, reactable_args = reactable_args)
  })
}

spiderplotly <- function(data, time_var, value_var, subject_var, event_var, color_var, colors, symbols, height) {
  subject_var_label <- attr(data[[subject_var]], "label")
  time_var_label <- attr(data[[time_var]], "label")
  event_var_label <- attr(data[[event_var]], "label")
  if (!length(subject_var_label)) subject_var_label <- subject_var
  if (!length(time_var_label)) time_var_label <- time_var
  if (!length(event_var_label)) event_var_label <- event_var
  
  data %>%
    arrange(!!as.name(subject_var), !!as.name(time_var)) %>%
    group_by(!!as.name(subject_var)) %>%
    mutate(
      x = dplyr::lag(!!as.name(time_var), default = 0),
      y = dplyr:::lag(!!as.name(value_var), default = 0),
      tooltip = sprintf(
        "%s: %s <br>%s: %s <br>%s: %s%% <br>", 
        subject_var_label, !!as.name(subject_var),
        time_var_label, !!as.name(time_var), 
        event_var_label, !!as.name(value_var) * 100
      )
    ) %>%
    ungroup() %>%
    plotly::plot_ly(
      source = "spiderplot", 
      height = height,
      color = as.formula(sprintf("~%s", color_var)),
      colors = colors,
      symbols = symbols
    ) %>%
    plotly::add_segments(
      x = ~x,
      y = ~y,
      xend = as.formula(sprintf("~%s", time_var)), 
      yend = as.formula(sprintf("~%s", value_var))
    ) %>%
    plotly::add_markers(
      x = as.formula(sprintf("~%s", time_var)),
      y = as.formula(sprintf("~%s", value_var)),
      symbol = as.formula(sprintf("~%s", color_var)),
      text = ~tooltip,
      hoverinfo = "text"
    ) %>%
    plotly::layout(
      xaxis = list(title = time_var_label),
      yaxis = list(title = event_var_label),
      title = title,
      dragmode = "select"
    ) %>%
    plotly::config(displaylogo = FALSE)
}
