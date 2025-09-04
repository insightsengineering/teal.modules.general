#' @export
tm_p_scatterplot <- function(label = "Scatter Plot",
                             plot_dataname,
                             subject_var,
                             x_var,
                             y_var,
                             color_var,
                             point_colors = character(0),
                             transformators = list(),
                             show_widgets = TRUE) {
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
      point_colors = point_colors,
      show_widgets = show_widgets
    ),
    transformators = transformators
  )
}

ui_p_scatterplot <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    shinyjs::useShinyjs(),
    tags$div(
      shinyWidgets::prettySwitch(
        ns("add_lines"),
        label = "Add lines",
        status = "primary",
        slim = TRUE,
        inline = TRUE
      ),
      colour_picker_ui(ns("colors")),
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
                              point_colors,
                              show_widgets) {
  moduleServer(id, function(input, output, session) {
    color_inputs <- colour_picker_srv(
      "colors",
      x = reactive({
        data()[[plot_dataname]][[color_var]]
      }),
      default_colors = point_colors
    )

    if (!show_widgets) {
      shinyjs::hide("add_lines")
      shinyjs::hide("colors")
    }

    plotly_q <- reactive({
      req(color_inputs())
      within(
        data(),
        subject_var = str2lang(subject_var),
        x_var = str2lang(x_var),
        y_var = str2lang(y_var),
        color_var = str2lang(color_var),
        colors = color_inputs(),
        add_lines = input$add_lines,
        expr = {
          plot_data <- scatterplot_ds |>
            dplyr::select(subject_var, x_var, y_var, color_var) |>
            dplyr::mutate(color_var = factor(color_var, levels = names(colors))) |>
            dplyr::mutate(customdata = dplyr::row_number())
          p <- plotly::plot_ly(
            data = plot_data,
            x = ~x_var,
            y = ~y_var,
            customdata = ~customdata,
            color = ~color_var,
            colors = colors,
            mode = "markers",
            type = "scatter",
            source = "scatterplot"
          ) |>
            plotly::layout(dragmode = "select")

          if (add_lines) {
            p <- p %>%
              plotly::add_trace(
                x = ~x_var,
                y = ~y_var,
                split = ~subject_var,
                mode = "lines",
                line = list(color = "grey"),
                showlegend = FALSE,
                inherit = FALSE
              )
          }
          p
        }
      )
    })


    output$plot <- plotly::renderPlotly(plotly::event_register(
      {
        plotly_q()$p |>
          setup_trigger_tooltips(session$ns) |>
          set_plot_data(session$ns("plot_data"))
      },
      "plotly_selected"
    ))

    plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "scatterplot"))
    reactive({
      req(plotly_selected())
      plotly_q() |>
        within(
          {
            selected_plot_data <- plot_data |>
              dplyr::filter(customdata %in% plotly_selected_customdata)
            scatterplot_ds <- scatterplot_ds |>
              filter(subject %in% selected_plot_data$subject)
          },
          plotly_selected_customdata = plotly_selected()$customdata
        )
    })
  })
}
