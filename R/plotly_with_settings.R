plotly_with_settings_ui <- function(id, height) {
  ns <- NS(id)
  plotly::plotlyOutput(ns("plot"), height = height)
} 

plotly_with_settings_srv <- function(id, plot) {
  moduleServer(id, function(input, output, session) {
    output$plot <- plotly::renderPlotly(plot())
  })
}