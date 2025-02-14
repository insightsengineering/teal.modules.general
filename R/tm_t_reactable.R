#' @param ... () additional [reactable()] arguments
#' @export
tm_t_reactables <- function(label = "Table", datanames, transformators = list(), decorators = list(), ...) {
  module(
    label = label,
    ui = ui_t_reactable,
    srv = srv_t_reactable,
    ui_args = list(decorators = decorators),
    srv_args = c(list(datanames = datanames, decorators = decorators), rlang::list(...)),
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
    table_q <- reactive({
      req(data())
      within(
        data(),
        dataname_reactable = str2lang(dataname_reactable),
        dataname = str2lang(dataname),
        {
          dataname_reactable <- reactable::reactable(
            dataname,
            #columns = teal.data::col_labels(dataset), # todo: replace with labels
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
          )
          dataname_reactable
          
        }
      )
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
