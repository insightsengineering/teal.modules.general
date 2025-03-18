#' @export
tm_g_waterfall <- function(label = "Waterfall",
                           plot_dataname,
                           subject_var,
                           value_var,
                           color_var = NULL,
                           bar_colors = list(),
                           value_arbitrary_hlines = c(0.2, -0.3),
                           plot_title = "Waterfall plot",
                           plot_height = 700,
                           table_datanames,
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
    ui_t_reactables(ns("subtables"))
  )
}
srv_g_waterfall <- function(id,
                            data,
                            plot_dataname,
                            subject_var,
                            value_var,
                            color_var,
                            bar_colors,
                            value_arbitrary_hlines,
                            plot_title,
                            plot_height = 600,
                            table_datanames = character(0),
                            filter_panel_api,
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
      
      color_var_label <- c(
        attr(data()[[plot_dataname]][[input$color_by]], "label"),
        input$color_by
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
          color_var_label = color_var_label,
          title = paste0(value_var_label, " (Waterfall plot)"),
          height = input$plot_height,
          expr = {
            p <- dataname %>%
              dplyr::mutate(
                subject_var_ordered = forcats::fct_reorder(as.factor(subject_var), value_var, .fun = max, .desc = TRUE),
                tooltip = sprintf(
                  "%s: %s <br>%s: %s%% <br>%s: %s", 
                  subject_var_label, subject_var, 
                  value_var_label, value_var,
                  color_var_label, color_var
                )
              ) %>%
            
              dplyr::filter(!duplicated(subject_var)) %>%
              # todo: one value for x, y: distinct or summarize(value = foo(value_var)) [foo: summarize_fun]
              plotly::plot_ly(
                source = "waterfall",
                height = height
              ) %>%
              plotly::add_bars(
                x = ~subject_var_ordered,
                y = ~value_var,
                color = ~color_var,
                colors = colors,
                text = ~ tooltip,
                hoverinfo = "text"
              ) %>%
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
                barmode = "relative"
              ) %>%
              plotly::layout( dragmode = "select") %>%
              plotly::config(displaylogo = FALSE)
          },
          height = input$plot_height
        )
    })

    output$plot <- plotly::renderPlotly(plotly::event_register(plotly_q()$p, "plotly_selected"))

    plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "waterfall"))
    
    plotly_selected_q <- reactive({
      req(plotly_selected())
      primary_keys <- unname(join_keys(data())[plot_dataname, plot_dataname])
      req(primary_keys)
      within(
        plotly_q(),
        expr = {
          waterfall_selected <- dplyr::filter(dataname, xvar %in% xvals, yvar %in% yvals) %>% 
            dplyr::select(primary_keys)
        },
        dataname = str2lang(plot_dataname),
        xvar = str2lang(subject_var),
        yvar = str2lang(value_var),
        xvals = plotly_selected()$x,
        yvals = plotly_selected()$y,
        primary_keys = primary_keys
      )
    })
    
    children_names <- reactive({
      if (length(table_datanames) == 0) {
        children(plotly_selected_q(), plot_dataname)
      } else {
        table_datanames
      }
    })
    
    tables_selected_q <- eventReactive(plotly_selected_q(), {
      exprs <- as.expression(
        lapply(
          children_names(),
          function(childname) {
            join_cols <- join_keys(plotly_selected_q())[childname, plot_dataname]
            substitute(
              expr = {
                childname <- dplyr::right_join(childname, waterfall_selected, by = by)
              },
              list(
                childname = str2lang(childname),
                by = join_cols
              )
            )
          }
        )
      )
      eval_code(plotly_selected_q(), exprs)
    })
    
    srv_t_reactables("subtables", data = tables_selected_q, dataname = table_datanames, ...)
  })
}

# todo: to teal_data
children <- function(x, dataset_name = character(0)) {
  checkmate::assert_multi_class(x, c("teal_data", "join_keys"))
  checkmate::assert_character(dataset_name, max.len = 1)
  if (length(dataset_name)) {
    names(
      Filter(
        function(parent) parent == dataset_name,
        parents(x)
      )
    )
  } else {
    all_parents <- unique(unlist(parents(x)))
    names(all_parents) <- all_parents
    lapply(
      all_parents, 
      function(parent) children(x = x, dataset_name = parent)
    )
  }
}
