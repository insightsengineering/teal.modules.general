#' @export
tm_g_waterfall <- function(label = "Waterfall",
                           plot_dataname,
                           table_datanames,
                           subject_var,
                           value_var,
                           color_var = NULL,
                           bar_colors = list(),
                           value_arbitrary_hlines = c(0.2, -0.3),
                           plot_title = "Waterfall plot",
                           plot_height = 700, 
                           ...) {
  module(
    label = label,
    ui = ui_g_waterfall,
    server = srv_g_waterfall,
    datanames = union(plot_dataname, table_datanames),
    ui_args = list(height = plot_height),
    server_args = c(
      list(
        plot_dataname = plot_dataname,
        table_datanames = table_datanames,
        subject_var = subject_var,
        value_var = value_var,
        color_var = color_var,
        bar_colors = bar_colors,
        value_arbitrary_hlines = value_arbitrary_hlines,
        plot_title = plot_title
      ),
      list(...)
    )
  )
}

ui_g_waterfall <- function(id, height) {
  ns <- NS(id)
  bslib::page_fluid(
    fluidRow(
      column(6, uiOutput(ns("color_by_output"))),
      column(6, sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, height))
    ),
    plotly::plotlyOutput(ns("plot"), height = "100%"),
    uiOutput(ns("tables"))
  )
}
srv_g_waterfall <- function(id,
                            data,
                            plot_dataname,
                            table_datanames,
                            subject_var,
                            value_var,
                            color_var,
                            bar_colors,
                            filter_panel_api,
                            value_arbitrary_hlines,
                            plot_title,
                            plot_height = 600,
                            ...) {
  moduleServer(id, function(input, output, session) {
    output$color_by_output <- renderUI({
      selectInput(session$ns("color_by"), label = "Color by:", choices = color_var$choices, selected = color_var$selected)
    })
    if (length(color_var$choices) > 1) {
      shinyjs::show("color_by")
    } else {
      shinyjs::hide("color_by")
    }
    plotly_q <- reactive({
      req(data(), input$color_by)
      adjusted_colors <- .color_palette_discrete(
        levels = unique(data()[[plot_dataname]][[input$color_by]]),
        color = bar_colors[[input$color_by]]
      )

      subject_var_label <- c(
        attr(data()[[plot_dataname]][[subject_var]], "label"),
        subject_var
      )[1]

      value_var_label <- c(
        attr(data()[[plot_dataname]][[value_var]], "label"),
        value_var
      )[1]

      data() |>
        within(
          dataname = str2lang(plot_dataname),
          dataname_filtered = str2lang(sprintf("%s_filtered", plot_dataname)),
          subject_var = str2lang(subject_var),
          value_var = str2lang(value_var),
          color_var = str2lang(input$color_by),
          colors = adjusted_colors,
          value_arbitrary_hlines = value_arbitrary_hlines,
          subject_var_label = subject_var_label,
          value_var_label = value_var_label,
          title = paste0(value_var_label, " (Waterfall plot)"),
          height = input$plot_height,
          expr = {
            p <- dataname |>
              dplyr::mutate(
                subject_var_ordered = forcats::fct_reorder(as.factor(subject_var), value_var, .fun = max, .desc = TRUE)
              ) |>
              dplyr::filter(!duplicated(subject_var)) |>
              # todo: one value for x, y: distinct or summarize(value = foo(value_var)) [foo: summarize_fun]
              plotly::plot_ly(
                source = "waterfall",
                height = height
              ) |>
              plotly::add_bars(
                x = ~subject_var_ordered,
                y = ~value_var,
                color = ~color_var,
                colors = colors,
                text = ~ paste(
                  subject_var_label, ":", subject_var,
                  value_var_label, ":", value_var, "<br>"
                ),
                hoverinfo = "text"
              ) |>
              plotly::layout(
                shapes = lapply(value_arbitrary_hlines, function(y) {
                  list(
                    type = "line",
                    x0 = 0,
                    x1 = 1,
                    xref = "paper",
                    y0 = y,
                    y1 = y,
                    line = list(color = "black", dash = "dot")
                  )
                }),
                title = title,
                xaxis = list(title = subject_var_label, tickangle = -45),
                yaxis = list(title = value_var_label),
                legend = list(title = list(text = "<b>Color by:</b>")),
                barmode = "relative",
                dragmode = "select"
              ) |>
              plotly::config(displaylogo = FALSE)
          },
          height = input$plot_height
        )
    })

    output$plot <- plotly::renderPlotly(plotly::event_register(plotly_q()$p, "plotly_selected"))

    plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "waterfall"))
    plotly_selected_q <- reactive({
      req(plotly_selected())
      within(
        plotly_q(),
        subject_vals = plotly_selected()$x,
        value_vals = plotly_selected()$y,
        expr = {
          # todo: this should use the join keys instead. Probably need to filter visualization data.frame and use its column
          plotly_brushed_subjects <- subject_vals
          plotly_brushed_value <- value_vals
        }
      )
    })

    tables_selected_q <- reactive({
      req(plotly_selected_q())
      teal.code::eval_code(
        plotly_selected_q(),
        code = as.expression(
          lapply(
            table_datanames,
            function(dataname) {
              substitute(
                expr = dataname_brushed <- dplyr::filter(dataname, subject_var %in% plotly_brushed_subjects),
                env = list(
                  dataname_brushed = str2lang(sprintf("%s_brushed", dataname)),
                  dataname = str2lang(dataname),
                  subject_var = str2lang(subject_var)
                )
              )
            }
          )
        )
      )
    })

    output$tables <- renderUI(ui_t_reactables(session$ns("subtables")))
    srv_t_reactables(
      "subtables", 
      data = tables_selected_q, 
      dataname = sprintf("%s_brushed", table_datanames),
      ...
    )
  })
}
