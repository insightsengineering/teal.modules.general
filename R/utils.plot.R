ui_plot_with_height <- function(outer_id = "plot_ui"){
  tags$div(teal.devel::white_small_well(uiOutput(outer_id)))
}

srv_plot_with_height <- function(input, output, session, plot_id = "plot", height_id = "plot_height")
    renderUI({
      plot_height <- input[[height_id]]
      validate(need(plot_height, "need valid plot height"))
      plotOutput(session$ns(plot_id), height = plot_height)
    })

ui_plot_height <- function(inputId = NULL, value = c(600, 200, 2000)){
  if(is.null(inputId)){
    stop("ui_plot_height: no ID defined.")
  }
  optionalSliderInputValMinMax(inputId, "plot height", value, ticks = FALSE)
}
