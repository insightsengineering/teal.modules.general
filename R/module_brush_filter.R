ui_brush_filter <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("brush_filter")),
    DT::dataTableOutput(ns("data_table"), width = "100%")
  )
}

srv_brush_filter <- function(id, brush, data, filter_panel_api, selectors, table_dec) {
  moduleServer(id, function(input, output, session) {
    selector_list <- isolate(selectors())

    output$brush_filter <- renderUI({
      states <- get_filter_state(filter_panel_api)
      brushed_states <- Filter(
        function(state) state$id == "brush_filter",
        states
      )
      if (!is.null(brush())) {
        actionButton(session$ns("apply_brush_filter"), "Apply filter")
      } else if (length(brushed_states)) {
        actionButton(session$ns("remove_brush_filter"), "Remove applied filter")
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

      slice <- teal_slices(teal_slice(
        dataname = "ADSL",
        varname = "USUBJID",
        selected = merged_data$USUBJID,
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
