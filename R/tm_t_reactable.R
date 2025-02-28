#' @param ... () additional [reactable()] arguments
#' @export
tm_t_reactables <- function(label = "Table", datanames, transformators = list(), decorators = list(), ...) {
  module(
    label = label,
    ui = ui_t_reactable,
    srv = srv_t_reactable,
    ui_args = list(decorators = decorators),
    srv_args = c(list(datanames = datanames, decorators = decorators), rlang::list2(...)),
    datanames = datanames,
    transformers = transformers
  )
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


