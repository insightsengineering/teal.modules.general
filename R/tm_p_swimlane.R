tm_p_swimlane <- function(label = "Swimlane Plot Module", dataname, id_var, avisit_var, shape_var, color_var) {
  module(
    label = label,
    ui = ui_p_swimlane,
    server = srv_p_swimlane,
    datanames = "synthetic_data",
    server_args = list(
      dataname = dataname,
      id_var = id_var,
      avisit_var = avisit_var,
      shape_var = shape_var,
      color_var = color_var
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

srv_p_swimlane <- function(id, data, dataname, id_var, avisit_var, shape_var, color_var, filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    output_q <- reactive({
      within(data(),
        {
          p <- ggplot(dataname, aes(x = avisit_var, y = subjid)) +
            ggtitle("Swimlane Efficacy Table") +
            geom_line(linewidth = 0.5) +
            geom_point(aes(shape = shape_var), size = 5) +
            geom_point(aes(color = color_var), size = 2) +
            scale_shape_manual(values = c("Drug A" = 1, "Drug B" = 2)) +
            scale_color_manual(values = c("CR" = "#9b59b6", "PR" = "#3498db")) +
            labs(x = "Study Day", y = "Subject ID")
        },
        dataname = as.name(dataname),
        id_var = as.name(id_var),
        avisit_var = as.name(avisit_var),
        shape_var = as.name(shape_var),
        color_var = as.name(color_var)
      )
    })

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
