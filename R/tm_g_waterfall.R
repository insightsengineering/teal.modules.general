tm_g_waterfall <- function(label = "Waterfall", time_var, subject_var, value_var, event_var, plot_height = 700) {
  time_var$dataname <- "ADRS"
  subject_var$dataname <- "ADRS"
  value_var$dataname <- "ADRS"
  event_var$dataname <- "ADRS"
  module(
    label = label,
    ui = ui_g_waterfall,
    server = srv_g_waterfall,
    datanames = "all",
    ui_args = list(height = plot_height),
    server_args = list(
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var
    )
  )
}

ui_g_waterfall <- function(id, height) {
  ns <- NS(id)
  tagList(
    fluidRow(
      class = "simple-card",
      div(
        class = "row",
        column(
          width = 4,
          selectInput(ns("select_event"), "Select Y Axis (to remove)", NULL)
        ),
        column(
          width = 4,
          sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, height)
        ),
        column(
          width = 4,
          sliderInput(ns("color_by"), "Plot Height (px)", 400, 1200, height)
        )
      ),
      h4("Waterfall"),
      plotly::plotlyOutput(ns("plot"), height = "100%")
    ),
    fluidRow(
      h4("All lesions"),
      ui_t_reactable(ns("all_lesions"))
      
    )
  )
}
srv_g_waterfall <- function(id, 
                           data, 
                           dataname,
                           time_var,
                           subject_var,
                           value_var,
                           event_var,
                           filter_panel_api, 
                           plot_height = 600) {
  moduleServer(id, function(input, output, session) {
    event_levels <- reactive({
      req(data())
      unique(data()[[event_var$dataname]][[event_var$selected]])
    })
    observeEvent(event_levels(), {
      updateSelectInput(inputId = "select_event",  choices = event_levels(), selected = event_levels()[1])
    })
    
    plotly_q <- reactive({
      data() |>
        within(
          dataname = str2lang(time_var$dataname),
          dataname_filtered = str2lang(sprintf("%s_filtered", time_var$dataname)),
          time_var = str2lang(time_var$selected),
          subject_var = str2lang(subject_var$selected),
          value_var = str2lang(value_var$selected),
          event_var = str2lang(event_var$selected),
          selected_event = input$select_event,
          height = input$plot_height,
          xaxis_label = attr(data()[[subject_var$dataname]][[subject_var$selected]], "label"),
          yaxis_label = input$select_event,
          title = paste0(input$select_event, " Over Time"),
          expr = {
            p <- dataname |>
              dplyr::filter(event_var %in% selected_event) |>
              dplyr::mutate(
                subject_var_ordered = forcats::fct_reorder(as.factor(subject_var), value_var, .fun = max, .desc = TRUE)
              ) |>
              # todo: one value for x, y: distinct or summarize(value = foo(value_var)) [foo: summarize_fun]
              plotly::plot_ly(
                source = "waterfall",
                height = height
              ) |>
                plotly::add_bars(
                  x = ~subject_var_ordered, y = ~value_var,
                  showlegend = FALSE
                ) |>
                plotly::layout(
                  xaxis = list(title = xaxis_label), yaxis = list(title = yaxis_label)
                ) |>
                plotly::layout(dragmode = "select") |>
                plotly::config(displaylogo = FALSE)
            },
            height = input$plot_height
          )
    })
    
    output$plot <- plotly::renderPlotly(plotly::event_register(plotly_q()$p, "plotly_selected"))
    
    plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "waterfall"))
    
  })
}