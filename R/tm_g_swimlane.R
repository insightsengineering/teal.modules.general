#' @export
tm_g_swimlane <- function(label = "Swimlane", 
                          plot_dataname, 
                          time_var, 
                          subject_var, 
                          value_var, 
                          event_var, 
                          sort_var = NULL,
                          group_var = NULL,
                          value_var_color = character(0),
                          value_var_symbol,
                          plot_height = 700,
                          table_datanames = character(0),
                          ...) {
  module(
    label = label,
    ui = ui_g_swimlane,
    server = srv_g_swimlane,
    datanames = c(plot_dataname, table_datanames),
    ui_args = list(height = plot_height),
    server_args = list(
      plot_dataname = plot_dataname,
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var,
      sort_var = sort_var,
      group_var = group_var,
      value_var_color = value_var_color,
      value_var_symbol = value_var_symbol,
      table_datanames = table_datanames,
      ...
    )
  )
}

ui_g_swimlane <- function(id, height) {
  
  
  ns <- NS(id)
  bslib::page_fluid(
    bslib::layout_columns(
      selectInput(ns("sort_by"), label = "Sort by:", choices = NULL, selected = NULL, multiple = FALSE),
      sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, height)
    ),
    plotly::plotlyOutput(ns("plot"), height = "100%"),
    ui_t_reactables(ns("subtables"))
  )
}
srv_g_swimlane <- function(id, 
                           data, 
                           plot_dataname,
                           time_var,
                           subject_var,
                           value_var,
                           event_var,
                           sort_var = time_var,
                           group_var = NULL,
                           value_var_color,
                           value_var_symbol,
                           table_datanames, 
                           filter_panel_api, 
                           ...) {
  moduleServer(id, function(input, output, session) {
    
    sort_choices <- reactiveVal()
    sort_selected <- reactiveVal()
    if (inherits(sort_var, c("choices_selected", "select_spec"))) {
      if (length(sort_var$choices) == 1) {
        sort_var <- sort_var$choices
      } else {
        updateSelectInput(inputId = "sort_by", choices = sort_var$choices, selected = sort_var$selected)
        observeEvent(input$sort_by, {
          if (!identical(input$sort_by, sort_selected())) {
            sort_selected(input$sort_by)          
          }
        })
      }
    }
    if (length(sort_var) == 1) {
      isolate(sort_choices(sort_var))
      isolate(sort_selected(sort_var))
      shinyjs::hide("sort_by")
    }
  
    
    plotly_q <- reactive({
      req(data(), sort_selected())
      adjusted_colors <- .color_palette_discrete(
        levels = unique(data()[[plot_dataname]][[value_var]]),
        color = value_var_color
      )
      adjusted_symbols <- .shape_palette_discrete(
        levels = unique(data()[[plot_dataname]][[value_var]]),
        symbol = value_var_symbol
      )
      subject_var_label <- c(attr(data()[[plot_dataname]][[subject_var]], "label"), "Subject")[1]
      time_var_label <- c(attr(data()[[plot_dataname]][[time_var]], "label"), "Study Day")[1]
      data() |>
        within(
          dataname = str2lang(plot_dataname),
          dataname_filtered = str2lang(sprintf("%s_filtered", plot_dataname)),
          time_var = str2lang(time_var),
          subject_var = str2lang(subject_var),
          value_var = str2lang(value_var),
          event_var = str2lang(event_var),
          sort_var = str2lang(sort_selected()),
          group_var = if (length(group_var)) group_var,
          subject_var_label = sprintf("%s:", subject_var_label),
          time_var_label = sprintf("%s:", time_var_label),
          colors = adjusted_colors,
          symbols = adjusted_symbols,
          height = input$plot_height,
          subject_axis_label = subject_var_label,
          time_axis_label = time_var_label,
          expr = {
            # todo: forcats::fct_reorder didn't work. 
            plotly_fun <- function(data) {
              data %>%
                plotly::plot_ly(
                  source = "swimlane",
                  colors = colors,
                  symbols = symbols,
                  height = height
                ) %>%
                plotly::add_markers(
                  x = ~time_var, 
                  y = ~subject_var_ordered,
                  color = ~value_var, 
                  symbol = ~value_var,
                  text = ~tooltip,
                  legendgroup = ~event_var,
                  hoverinfo = "text"
                ) %>%
                plotly::add_segments(
                  x = ~0, xend = ~study_day, 
                  y = ~subject_var_ordered, yend = ~subject_var_ordered,
                  color = ~event_var,
                  data = data |> group_by(subject_var_ordered, event_var) |> summarise(study_day = max(time_var)),
                  line = list(width = 2, color = "grey"),
                  showlegend = FALSE
                ) %>%
                plotly::layout(
                  xaxis = list(title = time_axis_label), 
                  yaxis = list(title = subject_axis_label)
                ) %>%
                plotly::layout(dragmode = "select") %>%
                plotly::config(displaylogo = FALSE)
            }
            
            levels <- dataname %>%
              group_by(subject_var, group_var) %>%
              summarize(v = max(sort_var)) %>%
              ungroup() %>%
              arrange(group_var, v) %>%
              pull(subject_var)
  
            p <- dataname  %>%
              mutate(subject_var_ordered = factor(subject_var, levels = levels))  %>%
              group_by(subject_var, time_var)  %>%
              mutate(
                tooltip = paste(
                  unique(
                    c(
                      paste(subject_var_label, subject_var),
                      paste(time_var_label, time_var),
                      sprintf("%s: %s", tools::toTitleCase(gsub("[^0-9A-Za-z]+", " ", event_var)), value_var)
                    )
                  ),
                  collapse = "<br>"
                )
              ) %>%
              plotly_fun()
          }
        )
    })
    
    output$plot <- plotly::renderPlotly({
      plotly_q()$p |>
        plotly::event_register("plotly_selected") |>
        plotly::event_register("plotly_deselect") # todo: deselect doesn't work
    })
    
    plotly_selected <- reactive({
      plotly::event_data("plotly_deselect", source = "swimlane") # todo: deselect doesn't work
      plotly::event_data("plotly_selected", source = "swimlane")
    })
    
    plotly_selected_q <- reactive({
      req(plotly_selected())
      primary_keys <- unname(join_keys(data())[plot_dataname, plot_dataname])
      req(primary_keys)
      within(
        plotly_q(),
        expr = {
          swimlane_selected <- dplyr::filter(dataname, xvar %in% xvals, yvar %in% yvals) %>% 
            dplyr::select(primary_keys)
        },
        dataname = str2lang(plot_dataname),
        xvar = str2lang(time_var),
        yvar = str2lang(subject_var),
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
                childname <- dplyr::right_join(childname, swimlane_selected, by = by)
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

