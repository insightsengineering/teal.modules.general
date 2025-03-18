#' @export
tm_g_spiderplot <- function(label = "Spiderplot",
                            plot_dataname,
                            time_var,
                            subject_var,
                            value_var,
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
      subject_var = subject_var,
      value_var = value_var,
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
  div(
    div(
      class = "row",
      column(
        width = 6,
        selectInput(ns("select_event"), "Select Y Axis", NULL)
      ),
      column(
        width = 6,
        sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, height)
      )        
    ),
    plotly::plotlyOutput(ns("plot"), height = "100%")
  
  )
}

srv_g_spiderplot <- function(id,
                             data,
                             plot_dataname,
                             time_var,
                             subject_var,
                             value_var,
                             event_var,
                             color_var,
                             point_colors,
                             point_symbols,
                             plot_height = 600,
                             table_datanames,
                             reactable_args,
                             filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    event_levels <- reactive({
      req(data())
      unique(data()[[plot_dataname]][[event_var]])
    })
    observeEvent(event_levels(), {
      updateSelectInput(inputId = "select_event",  choices = event_levels(), selected = event_levels()[1])
    })
    
    plotly_q <- reactive({
      # todo: tooltip!
      req(input$select_event)
      
      time_var_label <- c(
        attr(data()[[plot_dataname]][[time_var]], "label"),
        time_var
      )[1]
      
      subject_var_label <- c(
        attr(data()[[plot_dataname]][[subject_var]], "label"),
        subject_var
      )[1]
      
      ee <- within(
        data(),
        dataname = str2lang(plot_dataname),
        time_var = str2lang(time_var),
        subject_var = str2lang(subject_var),
        value_var = str2lang(value_var),
        event_var = str2lang(event_var),
        color_var = str2lang(color_var),
        selected_event = input$select_event,
        height = input$plot_height,
        time_var_label = time_var_label,
        event_var_label = input$select_event,
        subject_var_label = subject_var_label,
        title = paste0(input$select_event, " Over Time"),
        expr = {
          dd <- dataname %>%
            arrange(subject_var, time_var) %>%
            filter(event_var == selected_event) %>%
            mutate(
              tooltip = sprintf(
                "%s: %s <br>%s: %s%% <br>%s: %s", 
                subject_var_label, subject_var,
                time_var_label, time_var, 
                event_var_label, value_var
              )
            ) %>%
            group_by(subject_var) # %>%
          #   group_modify(~ {
          #     .first_x <- within(.x[1, ], {
          #       value_var <- 0
          #       time_var <- 0
          #     })
          #     bind_rows(.first_x, .x)
          #   })
          p <- dd |> plotly::plot_ly(source = "spiderplot", height = height) %>%
            plotly::add_trace(
              x = ~time_var, 
              y = ~value_var,
              mode = 'lines+markers',
              text = ~ tooltip,
              hoverinfo = "text"
            ) %>%
            plotly::add_markers(
              x = ~time_var, y = ~value_var, color = ~color_var, symbol = ~color_var
            ) %>%
            plotly::layout(
              xaxis = list(title = time_var_label),
              yaxis = list(title = event_var_label),
              title = title,
              showlegend = FALSE,
              dragmode = "select"
            ) %>%
            plotly::config(displaylogo = FALSE)
        }
      )
    })

    output$plot <- plotly::renderPlotly(plotly::event_register(plotly_q()$p, "plotly_selected"))

    plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "spiderplot"))

    reactive({
      req(plotly_selected())
      within(
        plotly_q(),
        dataname = str2lang(plot_dataname),
        time_var = str2lang(time_var),
        subject_var = subject_var,
        value_var = str2lang(value_var),
        time_vals = plotly_selected()$x,
        value_vals = plotly_selected()$y,
        expr = {
          plotly_brushed_time <- time_vals
          plotly_brushed_value <- value_vals
        }
      )
    })
  })
}
