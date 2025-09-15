#' @export
tm_p_doublebargraph <- function(label = "Bar Plot",
                                plot_dataname,
                                y_var,
                                color_var,
                                count_var,
                                secondary_y_var,
                                bar_colors = NULL) {
  module(
    label = label,
    ui = ui_p_doublebargraph,
    server = srv_p_doublebargraph,
    ui_args = list(),
    server_args = list(
      plot_dataname = plot_dataname,
      y_var = y_var,
      color_var = color_var,
      count_var = count_var,
      secondary_y_var = secondary_y_var,
      bar_colors = bar_colors
    )
  )
}

ui_p_doublebargraph <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    ui_p_bargraph(ns("main_bargraph")),
    ui_p_bargraph(ns("secondary_bargraph"))
  )
}

srv_p_doublebargraph <- function(id,
                                 data,
                                 plot_dataname,
                                 y_var,
                                 color_var,
                                 count_var,
                                 secondary_y_var,
                                 bar_colors) {
  moduleServer(id, function(input, output, session) {
    plot_q <- srv_p_bargraph(
      "main_bargraph",
      data = data,
      plot_dataname = plot_dataname,
      y_var = y_var,
      color_var = color_var,
      count_var = count_var,
      bar_colors = bar_colors
    )

    brushed_q <- reactive({
      req(attr(plot_q(), "has_brushing"))
      plot_q()
    })

    srv_p_bargraph(
      "secondary_bargraph",
      data = brushed_q,
      plot_dataname = plot_dataname,
      y_var = secondary_y_var,
      color_var = color_var,
      count_var = count_var,
      bar_colors = bar_colors
    )
  })
}
