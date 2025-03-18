#' @export
tm_g_spiderplot <- function(label = "Spiderplot",
                            plot_dataname,
                            time_var,
                            subject_var,
                            value_var,
                            event_var,
                            color_var,
                            point_colors,
                            point_symbols,
                            plot_height = 600,
                            table_datanames = character(0),
                            reactable_args =  list(),
                            transformator = transformator) {
  module(
    label = label,
    ui = ui_g_spiderplot,
    server = srv_g_spiderplot,
    ui_args = list(height = plot_height),
    server_args = list(
      plot_dataname = plot_dataname,
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var,
      color_var = color_var,
      point_colors = point_colors,
      point_symbols = point_symbols,
      table_datanames = table_datanames,
      reactable_args = reactable_args
    ),
    datanames = union(plot_dataname, table_datanames)
  )
}


ui_g_spiderplot <- function(id, height) {
  ns <- NS(id)
  div(
    div(
      class = "row",
      column(
        width = 6,
        selectInput(ns("select_event"), "Select Y Axis", NULL)
      ),
      column(
        width = 6,
        sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, height)
      )        
    ),
    plotly::plotlyOutput(ns("plot"), height = "100%"),
    ui_t_reactables(ns("subtables"))
  
  )
}

srv_g_spiderplot <- function(id,
                             data,
                             plot_dataname,
                             time_var,
                             subject_var,
                             value_var,
                             event_var,
                             color_var,
                             point_colors,
                             point_symbols,
                             plot_height = 600,
                             table_datanames = character(0),
                             reactable_args = list(),
                             filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    event_levels <- reactive({
      req(data())
      unique(data()[[plot_dataname]][[event_var]])
    })
    observeEvent(event_levels(), {
      updateSelectInput(inputId = "select_event",  choices = event_levels(), selected = event_levels()[1])
    })
    
    plotly_q <- reactive({
      req(input$select_event)
      
      adjusted_colors <- .color_palette_discrete(
        levels = unique(data()[[plot_dataname]][[color_var]]),
        color = point_colors
      )
      
      adjusted_symbols <- .shape_palette_discrete(
        levels = unique(data()[[plot_dataname]][[color_var]]),
        symbol = point_symbols
      )
      
      time_var_label <- c(
        attr(data()[[plot_dataname]][[time_var]], "label"),
        time_var
      )[1]
      
      subject_var_label <- c(
        attr(data()[[plot_dataname]][[subject_var]], "label"),
        subject_var
      )[1]
      
      ee <- within(
        data(),
        dataname = str2lang(plot_dataname),
        time_var = str2lang(time_var),
        subject_var = str2lang(subject_var),
        value_var = str2lang(value_var),
        event_var = str2lang(event_var),
        color_var = str2lang(color_var),
        selected_event = input$select_event,
        colors = adjusted_colors,
        symbols = adjusted_symbols,
        height = input$plot_height,
        time_var_label = time_var_label,
        event_var_label = input$select_event,
        subject_var_label = subject_var_label,
        title = paste0(input$select_event, " Over Time"),
        expr = {
          plotly_fun <- function(data) {
            data %>%
              plotly::plot_ly(
                source = "spiderplot", 
                height = height,
                color = ~color_var,
                colors = colors,
                symbols = symbols
              ) %>%
              plotly::add_segments(
                x = ~x,
                y = ~y,
                xend = ~time_var, 
                yend = ~value_var
              ) %>%
              plotly::add_markers(
                x = ~time_var,
                y = ~value_var,
                symbol = ~color_var,
                text = ~ tooltip,
                hoverinfo = "text"
              ) %>%
              plotly::layout(
                xaxis = list(title = time_var_label),
                yaxis = list(title = event_var_label),
                title = title,
                dragmode = "select"
              ) %>%
              plotly::config(displaylogo = FALSE)
          }
          p <- dataname %>%
            filter(event_var == selected_event) %>%
            arrange(subject_var, time_var) %>%
            group_by(subject_var) %>%
            mutate(
              x = dplyr::lag(time_var, default = 0),
              y = dplyr:::lag(value_var, default = 0),
              tooltip = sprintf(
                "%s: %s <br>%s: %s <br>%s: %s%%", 
                subject_var_label, subject_var,
                time_var_label, time_var, 
                event_var_label, value_var * 100
              )
            ) %>%
            plotly_fun()
        }
      )
    })

    output$plot <- plotly::renderPlotly(plotly::event_register(plotly_q()$p, "plotly_selected"))
    
    plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "spiderplot"))
    
    plotly_selected_q <- reactive({
      req(plotly_selected())
      primary_keys <- unname(join_keys(data())[plot_dataname, plot_dataname])
      req(primary_keys)
      within(
        plotly_q(),
        expr = {
          spiderplot_selected <- dplyr::filter(dataname, xvar %in% xvals, yvar %in% yvals) %>% 
            dplyr::select(primary_keys)
        },
        dataname = str2lang(plot_dataname),
        xvar = str2lang(time_var),
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
                childname <- dplyr::right_join(childname, spiderplot_selected, by = by)
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
    
    srv_t_reactables("subtables", data = tables_selected_q, dataname = table_datanames, reactable_args = reactable_args)
  })
}
