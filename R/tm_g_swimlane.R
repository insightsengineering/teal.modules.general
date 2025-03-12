#' @export
tm_g_swimlane <- function(label = "Swimlane", 
                          dataname, 
                          time_var, 
                          subject_var, 
                          value_var, 
                          event_var, 
                          sort_var = NULL,
                          group_var = NULL,
                          value_var_color = character(0),
                          value_var_symbol,
                          plot_height = 700) {
  module(
    label = label,
    ui = ui_g_swimlane,
    server = srv_g_swimlane,
    datanames = "all",
    ui_args = list(height = plot_height),
    server_args = list(
      dataname = dataname,
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var,
      sort_var = sort_var,
      group_var = group_var,
      value_var_color = value_var_color,
      value_var_symbol = value_var_symbol
    )
  )
}

ui_g_swimlane <- function(id, height) {
  ns <- NS(id)
  bslib::page_fluid(
    bslib::layout_columns(
      selectInput(ns("sort_by"), label = "Select variable:", choices = NULL, selected = NULL, multiple = FALSE),
      sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, height)
    ),
    plotly::plotlyOutput(ns("plot"), height = "100%")
  )
}
srv_g_swimlane <- function(id, 
                           data, 
                           dataname,
                           time_var,
                           subject_var,
                           value_var,
                           event_var,
                           sort_var = time_var,
                           group_var = NULL,
                           value_var_color,
                           value_var_symbol,
                           filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    
    sort_choices <- reactiveVal()
    sort_selected <- reactiveVal()
    if (inherits(sort_var, c("choices_selected", "select_spec"))) {
      if (length(sort_var$choices) == 1) {
        sort_var <- sort_var$choices
      } else {
        updateSelectInput(inputId = "sort_by", choices = sort_var$choices, selected = sort_var$selected)
        observeEvent(input$sort_by, {
          if (!identical(input$sort_by, sort_selected())) {
            sort_selected(input$sort_by)          
          }
        })
      }
    }
    if (length(sort_var) == 1) {
      isolate(sort_choices(sort_var))
      isolate(sort_selected(sort_var))
      shinyjs::hide("sort_by")
    }
    
    
    

    
    plotly_q <- reactive({
      req(data(), sort_selected())
      adjusted_colors <- .color_palette_discrete(
        levels = unique(data()[[dataname]][[value_var]]),
        color = value_var_color
      )
      adjusted_symbols <- .shape_palette_discrete(
        levels = unique(data()[[dataname]][[value_var]]),
        symbol = value_var_symbol
      )
      subject_var_label <- c(attr(data()[[dataname]][[subject_var]], "label"), "Subject")[1]
      time_var_label <- c(attr(data()[[dataname]][[time_var]], "label"), "Study Day")[1]
      data() |>
        within(
          dataname = str2lang(dataname),
          dataname_filtered = str2lang(sprintf("%s_filtered", dataname)),
          time_var = str2lang(time_var),
          subject_var = str2lang(subject_var),
          value_var = str2lang(value_var),
          event_var = str2lang(event_var),
          sort_var = str2lang(sort_selected()),
          group_var = if (length(group_var)) group_var,
          subject_var_label = sprintf("%s:", subject_var_label),
          time_var_label = sprintf("%s:", time_var_label),
          colors = adjusted_colors,
          symbols = adjusted_symbols,
          height = input$plot_height,
          subject_axis_label = subject_var_label,
          time_axis_label = time_var_label,
          expr = {
            # todo: forcats::fct_reorder didn't work. 
            plotly_fun <- function(data) {
              data %>%
                plotly::plot_ly(
                  source = "swimlane",
                  colors = colors,
                  symbols = symbols,
                  height = height
                ) %>%
                plotly::add_markers(
                  x = ~time_var, 
                  y = ~subject_var_ordered,
                  color = ~value_var, 
                  symbol = ~value_var,
                  text = ~tooltip,
                  legendgroup = ~event_var,
                  hoverinfo = "text"
                ) %>%
                plotly::add_segments(
                  x = ~0, xend = ~study_day, 
                  y = ~subject_var_ordered, yend = ~subject_var_ordered,
                  color = ~event_var,
                  data = data |> group_by(subject_var_ordered, event_var) |> summarise(study_day = max(time_var)),
                  line = list(width = 1, color = "grey"),
                  showlegend = FALSE
                ) %>%
                plotly::layout(
                  xaxis = list(title = time_axis_label), yaxis = list(title = subject_axis_label)
                ) %>%
                plotly::layout(dragmode = "select") %>%
                plotly::config(displaylogo = FALSE)
            }
            levels <- with(dataname, tapply(sort_var, subject_var, max)) |> sort()
            p <- dataname  %>%
              mutate(subject_var_ordered = factor(subject_var, levels = names(levels)))  %>%
              group_by(subject_var, time_var)  %>%
              mutate(
                tooltip = paste(
                  unique(c(
                    paste(subject_var_label, subject_var),
                    paste(time_var_label, time_var),
                    sprintf("%s: %s", tools::toTitleCase(gsub("[^0-9A-Za-z]+", " ", event_var)), value_var)
                  )),
                  collapse = "<br>"
              )) %>%
              split(if (is.null(group_var)) "" else .[[group_var]]) %>%
              lapply(plotly_fun) %>%
              plotly::subplot(nrows = length(.), shareX = TRUE, titleX = FALSE)
          }
        )
    })
    
    output$plot <- plotly::renderPlotly({
      plotly_q()$p |>
        plotly::event_register("plotly_selected") |>
        plotly::event_register("plotly_deselect") # todo: deselect doesn't work
    })
    
    plotly_selected <- reactive({
      plotly::event_data("plotly_deselect", source = "swimlane") # todo: deselect doesn't work
      plotly::event_data("plotly_selected", source = "swimlane")
    })
    
    reactive({
      req(plotly_selected())
      within(
        plotly_q(),
        dataname = str2lang(dataname),
        time_var = str2lang(time_var),
        subject_var = subject_var,
        value_var = str2lang(value_var),
        time_vals = plotly_selected()$x,
        subject_vals = plotly_selected()$y,
        expr = {
          plotly_brushed_time <- time_vals
          plotly_brushed_subject <- subject_vals
        }
      )
    })
  })
}

