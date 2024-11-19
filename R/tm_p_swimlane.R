tm_p_swimlane <- function(label = "Swimlane Plot Module", geom_specs, title, color_manual, shape_manual, size_manual) {
  module(
    label = label,
    ui = ui_p_swimlane,
    server = srv_p_swimlane,
    datanames = "all",
    server_args = list(
      geom_specs = geom_specs, title = title,
      color_manual = color_manual, shape_manual = shape_manual, size_manual = size_manual
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
                           color_manual,
                           shape_manual,
                           size_manual,
                           filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    ggplot_call <- reactive({
      plot_call <- bquote(ggplot2::ggplot())
      points_calls <- lapply(geom_specs, function(x) {
        # todo: convert $geom, $data, and $mapping elements from character to language
        #       others can be kept as character
        basic_call <- as.call(
          c(
            list(
              x$geom,
              mapping = as.call(c(as.name("aes"), x$mapping))
            ),
            x[!names(x) %in% c("geom", "mapping")]
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
    pws <- teal.widgets::plot_with_settings_srv(id = "myplot", plot_r = plot_r, gg2plotly = FALSE)

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
