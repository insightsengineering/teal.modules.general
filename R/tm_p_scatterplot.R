#' @export
tm_p_scatterplot <- function(label = "Scatter Plot",
                             plot_dataname,
                             subject_var,
                             x_var,
                             y_var,
                             color_var,
                             filter_var,
                             point_colors = character(0)) {
  module(
    label = label,
    ui = ui_p_scatterplot,
    server = srv_p_scatterplot,
    ui_args = list(),
    server_args = list(
      plot_dataname = plot_dataname,
      subject_var = subject_var,
      x_var = x_var,
      y_var = y_var,
      color_var = color_var,
      filter_var = filter_var,
      point_colors = point_colors
    )
  )
}

ui_p_scatterplot <- function(id) {
  ns <- NS(id)
  bslib::page_sidebar(
    sidebar = div(
      shinyWidgets::pickerInput(ns("event_type"), label = "Select Event Type", choices = NULL),
      colour_picker_ui(ns("colors"))
    ),
    tags$div(
      bslib::card(
        full_screen = TRUE,
        tags$div(
          trigger_tooltips_deps(),
          plotly::plotlyOutput(ns("plot"), height = "100%")
        )
      )
    )
  )
}

srv_p_scatterplot <- function(id,
                              data,
                              plot_dataname,
                              subject_var,
                              x_var,
                              y_var,
                              color_var,
                              filter_var,
                              point_colors) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      shinyWidgets::updatePickerInput(
        session, "event_type",
        choices = unique(data()[[plot_dataname]][[filter_var]])
      )
    })

    color_inputs <- colour_picker_srv(
      "colors",
      x = reactive({
        data()[[plot_dataname]][[color_var]]
      }),
      default_colors = point_colors
    )

    plotly_q <- reactive({
      req(input$event_type, color_inputs())
      within(
        data(),
        filter_var = str2lang(filter_var),
        subject_var = str2lang(subject_var),
        x_var = str2lang(x_var),
        y_var = str2lang(y_var),
        color_var = str2lang(color_var),
        colors = color_inputs(),
        expr = {
          plot_data <- scatterplot_ds |>
            dplyr::filter(filter_var == input_event_type) |>
            dplyr::select(subject_var, x_var, y_var, color_var) |>
            dplyr::mutate(color_var = factor(color_var, levels = names(colors)))
          p <- plotly::plot_ly(
            data = plot_data,
            x = ~x_var,
            y = ~y_var,
            color = ~color_var,
            colors = colors,
            mode = "markers",
            type = "scatter"
          ) |>
            plotly::layout(dragmode = "select")
          p()
        },
        input_event_type = input$event_type
      )
    })


    output$plot <- plotly::renderPlotly(plotly::event_register(
      {
        plotly_q()$p |>
          setup_trigger_tooltips(session$ns)
      },
      "plotly_selected"
    ))
  })
}
