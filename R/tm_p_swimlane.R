tm_p_swimlane <- function(label = "Swimlane Plot Module", geom_specs, title) {
  module(
    label = label,
    ui = ui_p_swimlane,
    server = srv_p_swimlane,
    datanames = "all",
    server_args = list(
      geom_specs = geom_specs,
      title = title
    )
  )
}

ui_p_swimlane <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    teal.widgets::plot_with_settings_ui(ns("myplot")),
    teal::ui_brush_filter(ns("brush_filter"))
  )
}

srv_p_swimlane <- function(id,
                           data,
                           geom_specs,
                           title = "Swimlane plot",
                           filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    ggplot_call <- reactive({
      plot_call <- bquote(ggplot2::ggplot())
      points_calls <- lapply(geom_specs, function(x) {
        if (!is.null(x$mapping)) {
          x$mapping <- as.call(c(as.name("aes"), x$mapping))
        }
        basic_call <- as.call(
          c(
            list(x$geom),
            x[!names(x) %in% "geom"]
          )
        )
      })

      title_call <- substitute(ggtitle(title), list(title = title))

      rhs <- Reduce(
        x = c(plot_call, points_calls, title_call),
        f = function(x, y) call("+", x, y)
      )
      substitute(p <- rhs, list(rhs = rhs))
    })

    output_q <- reactive(eval_code(data(), ggplot_call()))

    plot_r <- reactive(output_q()$p)
    pws <- teal.widgets::plot_with_settings_srv(id = "myplot", plot_r = plot_r)

    teal::srv_brush_filter(
      "brush_filter",
      brush = pws$brush,
      dataset = reactive(teal.code::dev_suppress(output_q()$synthetic_data)),
      filter_panel_api = filter_panel_api
    )
  })
}

merge_selectors2 <- function() {
  lappl
}
