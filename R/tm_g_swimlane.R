#' @export
tm_g_swimlane <- function(label = "Swimlane", 
                          dataname, 
                          time_var, 
                          subject_var, 
                          value_var, 
                          event_var, 
                          value_var_color = character(0),
                          value_var_symbol,
                          plot_height = 700) {
  module(
    label = label,
    ui = ui_g_swimlane,
    server = srv_g_swimlane,
    datanames = "all",
    ui_args = list(height = plot_height),
    server_args = list(
      dataname = dataname,
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var,
      value_var_color = value_var_color,
      value_var_symbol = value_var_symbol
    )
  )
}

ui_g_swimlane <- function(id, height) {
  ns <- NS(id)
  div(
    class = "simple-card",
    sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, height, width = "100%"),
    plotly::plotlyOutput(ns("plot"), height = "100%")
  )
}
srv_g_swimlane <- function(id, 
                           data, 
                           dataname,
                           time_var,
                           subject_var,
                           value_var,
                           event_var,
                           value_var_color,
                           value_var_symbol,
                           filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    plotly_q <- reactive({
      req(data())
      adjusted_colors <- .color_palette_discrete(
        levels = unique(data()[[dataname]][[value_var]]),
        color = value_var_color
      )
      subject_var_label <- c(attr(data()[[dataname]][[subject_var]], "label"), "Subject")[1]
      time_var_label <- c(attr(data()[[dataname]][[time_var]], "label"), "Study Day")[1]
      data() |>
        within(
          dataname = str2lang(dataname),
          dataname_filtered = str2lang(sprintf("%s_filtered", dataname)),
          time_var = str2lang(time_var),
          subject_var = str2lang(subject_var),
          value_var = str2lang(value_var),
          event_var = str2lang(event_var),
          subject_var_label = sprintf("%s:", subject_var_label),
          time_var_label = sprintf("%s:", time_var_label),
          colors = adjusted_colors,
          symbols = value_var_symbol,
          height = input$plot_height,
          filtered_events = c("disposition","response_assessment", "study_drug_administration"),
          subject_axis_label = subject_var_label,
          time_axis_label = time_var_label,
          expr = {
            dataname <- dataname |>
              mutate(subject_var_ordered = forcats::fct_reorder(as.factor(subject_var), time_var, .fun = max)) |>
              group_by(subject_var, time_var) |>
              mutate(
                tooltip = paste(
                  unique(c(
                    paste(subject_var_label, subject_var),
                    paste(time_var_label, time_var),
                    sprintf("%s: %s", tools::toTitleCase(gsub("[^0-9A-Za-z]+", " ", event_var)), value_var)
                  )),
                  collapse = "<br>"
              ))
          
            
            p <- dataname |> 
              dplyr::filter(
                event_var %in% filtered_events,
                !is.na(time_var)
              ) |>
              plotly::plot_ly(
              source = "swimlane",
              colors = colors,
              symbols = symbols,
              height = height
            ) |>
              plotly::add_markers(
                x = ~time_var, y = ~subject_var_ordered, color = ~value_var, symbol = ~value_var,
                text = ~tooltip,
                hoverinfo = "text"
              ) |>
              plotly::add_segments(
                x = ~0, xend = ~study_day, y = ~subject_var_ordered, yend = ~subject_var_ordered,
                data = dataname |> group_by(subject_var_ordered) |> summarise(study_day = max(time_var)),
                line = list(width = 1, color = "grey"),
                showlegend = FALSE
              ) |>
              plotly::layout(
                xaxis = list(title = time_axis_label), yaxis = list(title = subject_axis_label)
              ) |>
              plotly::layout(dragmode = "select") |>
              plotly::config(displaylogo = FALSE)
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
    
    reactive({
      req(plotly_selected())
      within(
        plotly_q(),
        dataname = str2lang(dataname),
        time_var = str2lang(time_var),
        subject_var = subject_var,
        value_var = str2lang(value_var),
        time_vals = plotly_selected()$x,
        subject_vals = plotly_selected()$y,
        expr = {
          plotly_brushed_time <- time_vals
          plotly_brushed_subject <- subject_vals
        }
      )
    })
  
  })
}

