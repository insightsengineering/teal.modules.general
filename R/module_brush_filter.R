ui_brush_filter <- function(id) {
  ns <- NS(id)
  div(
    tags$h1(id = ns("title"), tags$strong("Selected points:"), class = "text-center font-150p"),
    teal.widgets::get_dt_rows(ns("data_table"), ns("data_table_rows")),
    div(
      actionButton(ns("apply_brush_filter"), "Apply filter"),
      actionButton(ns("remove_brush_filter"), "Remove applied filter")
    ),
    DT::dataTableOutput(ns("data_table"), width = "100%")
  )
}

srv_brush_filter <- function(id, brush, data, filter_panel_api, selectors, table_dec) {
  moduleServer(id, function(input, output, session) {
    selector_list <- isolate(selectors())

    observeEvent(brush(), ignoreNULL = FALSE, {
      if (is.null(brush())) {
        shinyjs::hide("title")
        shinyjs::hide("apply_brush_filter")
        shinyjs::hide("data_table")
      } else {
        shinyjs::show("title")
        shinyjs::show("apply_brush_filter")
        shinyjs::show("data_table")
      }
    })

    states_list <- reactive({
      as.list(get_filter_state(filter_panel_api))
    })

    observeEvent(states_list(), {
      brushed_states <- Filter(
        function(state) state$id == "brush_filter",
        states_list()
      )
      if (length(brushed_states)) {
        shinyjs::show("remove_brush_filter")
      } else {
        shinyjs::hide("remove_brush_filter")
      }
    })

    observeEvent(input$remove_brush_filter, {
      remove_filter_state(
        filter_panel_api,
        teal_slices(
          teal_slice(
            dataname = "ADSL",
            varname = "USUBJID",
            id = "brush_filter"
          )
        )
      )
    })

    observeEvent(input$apply_brush_filter, {
      plot_brush <- brush()
      merged_data <- isolate(teal.code::dev_suppress(data()[["ANL"]]))
      filter_call <- str2lang(sprintf(
        "merged_data <- dplyr::filter(merged_data, %1$s >= %2$s & %1$s <= %3$s & %4$s >= %5$s & %4$s <= %6$s)",
        plot_brush$mapping$x, plot_brush$xmin, plot_brush$xmax,
        plot_brush$mapping$y, plot_brush$ymin, plot_brush$ymax
      ))
      eval(filter_call)

      # todo: when added another time then it is duplicated
      slice <- teal_slices(teal_slice(
        dataname = "ADSL",
        varname = "USUBJID",
        selected = unique(merged_data$USUBJID),
        id = "brush_filter"
      ))
      set_filter_state(filter_panel_api, slice)
    })

    output$data_table <- DT::renderDataTable({
      plot_brush <- brush()
      if (is.null(plot_brush)) {
        return(NULL)
      }

      isolate({
        foo1(brush, selector_list)
      })

      dataset <- isolate(teal.code::dev_suppress(data()[["ANL"]]))
      brushed_df <- teal.widgets::clean_brushedPoints(dataset, plot_brush)
      numeric_cols <- names(brushed_df)[
        vapply(brushed_df, function(x) is.numeric(x) && !is.integer(x), FUN.VALUE = logical(1))
      ]

      if (length(numeric_cols) > 0) {
        DT::formatRound(
          DT::datatable(brushed_df,
            rownames = FALSE,
            options = list(scrollX = TRUE, pageLength = input$data_table_rows)
          ),
          numeric_cols,
          table_dec
        )
      } else {
        DT::datatable(brushed_df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = input$data_table_rows))
      }
    })
  })
}

#' get axis dataname, varname and ranges
foo1 <- function(brush, selector_list) {
  lapply(names(brush()$mapping), function(selector) {
    list(
      dataname = selector_list[[selector]]()$dataname,
      varname = brush()$mapping[[selector]],
      values = unlist(brush()[paste0(selector, c("min", "max"))])
    )
  })
}
