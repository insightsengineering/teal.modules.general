#' @export
tm_t_reactables <- function(label = "Table", datanames = "all", columns = list(), layout = "grid", transformators = list(), decorators = list(), ...) {
  module(
    label = label,
    ui = ui_t_reactable,
    srv = srv_t_reactable,
    ui_args = list(decorators = decorators),
    srv_args = c(
      list(datanames = datanames, columns = columns, layout = layout, decorators = decorators), 
      rlang::list2(...)
    ),
    datanames = subtables,
    transformers = transformers
  )
}

ui_t_reactables <- function(id) {
  ns <- NS(id)
  uiOutput(ns("subtables"), container = fluidRow)
}

srv_t_reactables <- function(id, data, filter_panel_api, datanames, columns, decorators, layout = "grid", ...) {
  moduleServer(id, function(input, output, session) {
    all_datanames_r <- reactive({
      req(data())
      names(Filter(is.data.frame, as.list(data())))
    })
    
    datanames_r <- reactive({
      req(all_datanames_r())
      df_datanames <- all_datanames_r()
      if (identical(datanames, "all")) {
        df_datanames
      } else {
        intersect(datanames, df_datanames)
      }
    }) |> bindEvent(all_datanames_r())
    
    columns_r <- reactive({
      req(datanames_r())
      sapply(datanames_r(), function(dataname) {
        if (length(columns[[dataname]])) {
          columns()[[dataname]]
        } else {
          colnames(isolate(data())[[dataname]])
        }
      })
    }) |> bindEvent(datanames_r())
    
    datalabels_r <- reactive({
      req(datanames_r())
      sapply(datanames_r(), function(dataname) {
        datalabel <- attr(isolate(data())[[dataname]], "label")
        if (length(datalabel)) datalabel else dataname
      })
    }) |> bindEvent(datanames_r())
  
    # todo: re-render only if datanames changes
    output$subtables <- renderUI({
      if (length(datanames_r()) == 0) return(NULL)
      
      if (layout == "grid") {
        tagList(
          lapply(
            datanames_r(),
            function(dataname) {
              column(
                width = if (length(datanames_r()) == 1) 12 else 6,
                div(
                  class = "simple-card",
                  h4(datalabels_r()[dataname]),
                  ui_t_reactable(session$ns(dataname))                  
                )
              )
            }
          )
        )
      } else if (layout == "tabs") {
        isolate({
          do.call(
            tabsetPanel,
            c(
              list(id = session$ns("reactables")),
              lapply(
                datanames_r(),
                function(dataname) {
                  tabPanel(
                    title = datalabels_r()[dataname],
                    ui_t_reactable(session$ns(dataname))
                  )
                }
              )
            )
          )
        })
      }

    }) |> bindCache(datanames_r())
    
    called_datanames <- reactiveVal()
    observeEvent(datanames_r(), {
      lapply(
        setdiff(datanames_r(), called_datanames()), # call module only once per dataname
        function(dataname) srv_t_reactable(dataname, data = data, dataname = dataname, filter_panel_api = filter_panel_api, ...)
      )
      called_datanames(union(called_datanames(), datanames_r()))
    })
    
    
    # lapply(
    #   seq_along(subtables),
    #   function(i) {
    #     table_q <- reactive({
    #       within(
    #         plotly_selected_q(),
    #         dataname = str2lang(dataname),
    #         subtable_name = subtable_names[i],
    #         time_var = str2lang(time_var),
    #         subject_var = str2lang(subject_var),
    #         col_defs = subtables[[i]],
    #         expr = {
    #           subtable_name <- dataname |>
    #             dplyr::filter(
    #               time_var %in% plotly_brushed_time, 
    #               subject_var %in% plotly_brushed_subject
    #             ) |>
    #             dplyr::select(dplyr::all_of(col_defs))
    #         }
    #       )
    #     })
    #     srv_t_reactable(subtable_names[i], data = table_q, dataname = subtable_names[i], selection = NULL)
    #   }
    # )
  })
}

ui_t_reactable <- function(id) {
  ns <- NS(id)
  div(
    class = "simple-card",
    reactable::reactableOutput(ns("table"))
  )
}

srv_t_reactable <- function(id, data, filter_panel_api, dataname, decorators, ...) {
  moduleServer(id, function(input, output, session) {
    dataname_reactable <- sprintf("%s_reactable", dataname)
    
    reactable_call <- reactive({
      default_args <- list(
        columns = .make_reactable_columns_call(data()[[dataname]]),
        onClick = "select",
        defaultPageSize = 15,
        wrap = FALSE,
        rowClass = JS("
          function(rowInfo) {
              if (rowInfo.selected) {
                return 'selected-row';
              }
          }
        ")
      )
      args <- modifyList(default_args, rlang::list2(...))
      substitute(
        lhs <- rhs,
        list(
          lhs = dataname_reactable,
          rhs = .make_reactable_call(dataname = dataname, args = args)          
        )
      )
      
    })
    table_q <- reactive({
      req(data())
      eval_code(data(), reactable_call())
    })
    output$table <- reactable::renderReactable(table_q()[[dataname_reactable]])
    table_selected_q <- reactive({
      selected_row <- reactable::getReactableState("table", "selected")
      if (!is.null(selected_row)) {
        within(
          table_q(),
          selected_row = selected_row,
          dataname_selected = str2lang(sprintf("%s_selected", dataname)),
          dataname = str2lang(dataname),
          expr = {
            dataname_selected <- dataname[selected_row, ] 
          }
        )
      } else {
        table_q()
      }
    })
    table_selected_q
  })
}

.make_reactable_call <- function(dataname, args) {
  args <- c(
    list(data = str2lang(dataname)),
    args
  )
  do.call(call, c(list(name = "reactable"), args), quote = TRUE)

}

#' Makes `reactable::colDef` call containing:
#' name = <column label attribute>
#' cell = <url formatter>
#' Arguments of [reactable::colDef()] are specified only if necessary
#' @param dataset (`data.frame`)
#' @return named list of `colDef` calls
#' @keywords internal
.make_reactable_columns_call <- function(dataset) {
  checkmate::assert_data_frame(dataset)
  args <- lapply(
    seq_along(dataset), 
    function(i) {
      label <- attr(dataset[[i]], "label")
      is_labelled <- length(label) == 1 && !is.na(label) && !identical(label, "")
      is_url <- is.character(dataset[[i]]) && any(
        grepl(
          "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)",
          x = head(dataset[[i]]),
          perl = TRUE
        )
      )

      args <- c(
        if (is_labelled) list(name = label),
        if (is_url) list(cell = quote(function(value) {
            if (!is.na(value) && !is.null(value) && value != "") {
              htmltools::tags$a(href = value, target = "_blank", "Link")
            } else {
              "N/A"
            }
          })
        ) 
      )

      if (length(args)) {
        do.call(call, c(list(name = "colDef"), args), quote = TRUE)
      }
    }
  )
  names(args) <- names(dataset)
  args <- Filter(length, args)
  if (length(args)) {
    do.call(call, c(list("list"), args), quote = TRUE)    
  }
}

.name_to_id <- function(name) {
  gsub("[[:space:][:punct:]]+", "_", x = tolower(name))
}
