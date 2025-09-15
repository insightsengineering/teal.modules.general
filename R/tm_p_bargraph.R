#' @export
tm_p_bargraph <- function(label = "Bar Plot",
                          plot_dataname,
                          y_var,
                          color_var,
                          count_var,
                          bar_colors = NULL) {
  module(
    label = label,
    ui = ui_p_bargraph,
    server = srv_p_bargraph,
    ui_args = list(),
    server_args = list(
      plot_dataname = plot_dataname,
      y_var = y_var,
      color_var = color_var,
      count_var = count_var,
      bar_colors = bar_colors
    )
  )
}

ui_p_bargraph <- function(id) {
  ns <- NS(id)
  bslib::page_fluid(
    bslib::card(
      full_screen = TRUE,
      tags$div(
        # trigger_tooltips_deps(),
        plotly::plotlyOutput(ns("plot"), height = "100%")
      )
    )
  )
}

srv_p_bargraph <- function(id,
                           data,
                           plot_dataname,
                           y_var,
                           color_var,
                           count_var,
                           bar_colors) {
  moduleServer(id, function(input, output, session) {
    plotly_q <- reactive({
      data() |>
        within(
          {
            df[[color_var]] <- as.character(df[[color_var]])

            plot_data <- df %>%
              dplyr::group_by(!!as.name(y_var), !!as.name(color_var)) %>%
              dplyr::summarize(count = n_distinct(!!as.name(count_var)), .groups = "drop") %>%
              dplyr::mutate(customdata = dplyr::row_number())

            event_type_order <- plot_data %>%
              dplyr::group_by(!!as.name(y_var)) %>%
              dplyr::summarize(total = sum(count), .groups = "drop") %>%
              dplyr::arrange(total) %>%
              dplyr::pull(!!as.name(y_var))

            plot_data[[y_var]] <- factor(plot_data[[y_var]], levels = event_type_order)

            p <- plotly::plot_ly(
              data = plot_data,
              y = as.formula(paste0("~", y_var)),
              x = ~count,
              color = as.formula(paste0("~", color_var)),
              colors = bar_colors,
              type = "bar",
              orientation = "h",
              customdata = ~customdata,
              source = source
            ) %>%
              plotly::layout(
                barmode = "stack",
                xaxis = list(title = "Count"),
                yaxis = list(title = "Adverse Event Type"),
                legend = list(title = list(text = "AE Type"))
              ) %>%
              plotly::layout(dragmode = "select")
          },
          df = str2lang(plot_dataname),
          color_var = color_var,
          y_var = y_var,
          count_var = count_var,
          bar_colors = bar_colors,
          source = session$ns("bargraph")
        )
    })


    output$plot <- plotly::renderPlotly({
      plotly_q()$p %>%
        set_plot_data(session$ns("plot_data")) |>
        plotly::event_register("plotly_selected")
    })
    plotly_selected <- reactive(
      plotly::event_data("plotly_selected", source = session$ns("bargraph"))
    )

    reactive({
      if (is.null(plotly_selected())) {
        plotly_q()
      } else {
        q <- plotly_q() |>
          within(
            {
              selected_plot_data <- plot_data |>
                dplyr::filter(customdata %in% plotly_selected_customdata)
              df <- df |>
                dplyr::filter(
                  !!as.name(y_var_string) %in% selected_plot_data[[y_var_string]],
                  !!as.name(color_var_string) %in% selected_plot_data[[color_var_string]]
                )
            },
            df = str2lang(plot_dataname),
            y_var_string = y_var,
            color_var_string = color_var,
            plotly_selected_customdata = plotly_selected()$customdata
          )
        attr(q, "has_brushing") <- TRUE
        q
      }
    })
  })
}
