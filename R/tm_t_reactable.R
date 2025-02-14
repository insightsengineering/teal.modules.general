#' @param ... () additional [reactable()] arguments
#' @export
tm_t_reactables <- function(label = "Table", datanames, transformators = list(), decorators = list(), ...) {
  module(
    label = label,
    ui = ui_t_reactable,
    srv = srv_t_reactable,
    ui_args = list(decorators = decorators),
    srv_args = c(list(datanames = datanames, decorators = decorators), rlang::list(...))
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
  moduleServer(id, function(input, output, session)) {
    output$table <- reactable::renderReactable({
      req(data())
      dataset <- data()[[dataname]]
      args <- modifyList(
        list(
          dataset,
          columns = teal.data::col_labels(dataset)
          selection = "single",
          onClick = "select",
          defaultPageSize = 15,
          wrap = FALSE,
          rowClass = JS("
            function(rowInfo) {
            console.log(rowInfo);
              if (rowInfo.selected) {
                return 'selected-row';
              }
            }
          ")
        ),
        list(...)
      )
      do.call(reactable::reactable, args = args)
    })
  })
}
