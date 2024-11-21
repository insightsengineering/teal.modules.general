tm_p_swimlane2 <- function(label = "Swimlane Plot Module", plotly_specs, title) {
  module(
    label = label,
    ui = ui_p_swimlane2,
    server = srv_p_swimlane2,
    datanames = "all",
    server_args = list(
      plotly_specs = plotly_specs,
      title = title
    )
  )
}


ui_p_swimlane2 <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    plotly::plotlyOutput(ns("plot")),
    ui_page_data_table(ns("brush_tables"))
  )
}

srv_p_swimlane2 <- function(id,
                            data,
                            plotly_specs,
                            title = "Swimlane plot",
                            filter_panel_api) {
  moduleServer(id, function(input, output, session) {
    plotly_q <- reactive({
      plotly_call <- .make_plotly_call(specs = plotly_specs)
      code <- substitute(
        p <- plotly_call,
        list(plotly_call = plotly_call)
      )
      eval_code(data(), code = code)
    })

    output$plot <- plotly::renderPlotly(plotly::event_register(plotly_q()$p, "plotly_selected"))


    brush_filter_call <- reactive({
      d <- plotly::event_data("plotly_selected")
      req(d)
      calls <- lapply(plotly_specs, function(spec) {
        substitute(
          dataname <- dplyr::filter(dataname, var_x %in% levels_x, var_y %in% levels_y),
          list(
            dataname = spec$data,
            var_x = str2lang(all.vars(spec$x)),
            var_y = str2lang(all.vars(spec$y)),
            levels_x = d$x,
            levels_y = d$y
          )
        )
      })
      unique(calls)
    })

    brush_filtered_data <- reactive({
      if (is.null(brush_filter_call())) {
        shinyjs::hide("brush_tables")
      } else {
        shinyjs::hide("show_tables")
        eval_code(plotly_q(), as.expression(brush_filter_call()))
      }
    })

    observeEvent(brush_filtered_data(), once = TRUE, {
      srv_page_data_table("brush_tables", data = brush_filtered_data, filter_panel_api = filter_panel_api)
    })
  })
}



.make_plotly_call <- function(init_call = quote(plotly::plot_ly()), specs) {
  points_calls <- lapply(specs, function(x) {
    which_fun <- c(which(names(x) == "fun"), 1)[1]
    if (is.character(x[[which_fun]])) {
      x[[which_fun]] <- str2lang(x[[which_fun]])
    }
    basic_call <- as.call(
      c(
        list(x[[which_fun]]),
        x[-which_fun]
      )
    )
  })

  rhs <- Reduce(
    x = c(init_call, points_calls),
    f = function(x, y) call("%>%", x, y)
  )
}
