#' Data Table Viewer Teal Module
#'
#' A data table viewer shows the data using a paginated table.
#'
#' @param label (\code{character})
#' @param variables_selected (\code{list}) a named list that says which variables should be
#'   initially shown for particular dataset. Names in list should correspond with names provided in list `data()`.
#'   If not specified for any dataset - first six variables from dataset will be shown.
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- radsl(cached = TRUE)",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_data_table(
#'       variables_selected = list(ADSL  = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"))
#'     )
#'   )
#' )
#' \dontrun{
#'   shinyApp(app$ui, app$server)
#' }
#'
#' # two-datasets example
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radaette(cached = TRUE)
#'
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADTTE", ADTTE),
#'     code = "ADSL <- radsl(cached = TRUE); ADTTE <- radaette(cached = TRUE)",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_data_table(
#'       variables_selected = list(ADSL  = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"),
#'                                 ADTTE = c("STUDYID", "USUBJID", "SUBJID", "SITEID",
#'                                           "PARAM", "PARAMCD", "ARM", "ARMCD", "AVAL", "CNSR")))
#'   )
#' )
#' \dontrun{
#'   shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: different subsets of long dataset
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADLB <- radlb(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = "ADSL <- radsl(cached = TRUE); ADLB <- radlb(cached = TRUE)",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_data_table(
#'       variables_selected = list(ADSL  = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"),
#'                                 ADLB = c("STUDYID", "USUBJID", "SUBJID", "SITEID",
#'                                          "PARAM", "PARAMCD", "AVISIT", "AVISITN", "AVAL", "CHG")))
#'   )
#' )
#'
#' \dontrun{
#'   shinyApp(app$ui, app$server)
#' }
#'
tm_data_table <- function(label = "Data table",
                          variables_selected = list()) {
  stopifnot(
    is.character.single(label),
    is.list(variables_selected),
    `if`(length(variables_selected) > 0,
         !is.null(names(variables_selected)), TRUE),
    `if`(length(variables_selected) > 0,
         all(vapply(names(variables_selected), is.character, FUN.VALUE = logical(1))), TRUE),
    `if`(length(variables_selected) > 0,
         all(vapply(names(variables_selected), nchar, FUN.VALUE = integer(1)) > 0), TRUE),
    `if`(length(variables_selected) > 0,
         all(vapply(variables_selected, is.character, FUN.VALUE = logical(1))), TRUE),
    `if`(length(variables_selected) > 0,
         all(vapply(variables_selected, length, FUN.VALUE = integer(1)) > 0), TRUE)
  )

  module(
    label,
    server = srv_page_data_table,
    ui = ui_page_data_table,
    filters = "all",
    ui_args = list(selected = variables_selected)
  )
}


# ui page module
#' @importFrom utils head
ui_page_data_table <- function(id,
                               datasets,
                               selected) {
  ns <- NS(id)

  datanames <- datasets$datanames()
  tagList(
    fluidRow(
      column(
        width = 6,
        radioButtons(
          ns("if_filtered"),
          NULL,
          choices = c("unfiltered data" = FALSE, "filtered data" = TRUE),
          selected = TRUE,
          inline = TRUE
        )
      ),
      column(
        width = 6,
        checkboxInput(
          ns("if_distinct"),
          "Show only distinct rows:",
          value = FALSE
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        do.call(
          tabsetPanel,
          lapply(
            datanames,
            function(x) {
              choices <- names(datasets$get_data(x, filtered = FALSE, reactive = FALSE))
              selected <- if (!is.null(selected[[x]])) {
                selected[[x]]
              } else {
                head(choices)
              }
              tabPanel(
                title = x,
                column(
                  width = 12,
                  div(style = "height:10px;"),
                  ui_data_table(
                    id = ns(x),
                    choices = choices,
                    selected = selected
                  )
                )
              )
            }
          )
        )
      )
    ),
    div(style = "height:30px;")
  )
}


# server page module
srv_page_data_table <- function(input,
                                output,
                                session,
                                datasets) {

  if_filtered <- reactive(as.logical(input$if_filtered))
  if_distinct <- reactive(as.logical(input$if_distinct))

  lapply(
    datasets$datanames(),
    function(x) {
      callModule(
        module = srv_data_table,
        id = x,
        datasets = datasets,
        dataname = x,
        if_filtered = if_filtered,
        if_distinct = if_distinct
      )
    }
  )
}

ui_data_table <- function(id,
                          choices,
                          selected) {
  ns <- NS(id)

  tagList(
    fluidRow(
      selectInput(
        ns("variables"),
        "Select variables:",
        choices = choices,
        selected = selected,
        multiple = TRUE,
        width = "100%"
      )
    ),
    fluidRow(
      DT::dataTableOutput(ns("data_table"), width = "100%")
    )
  )
}


#' @importFrom dplyr count_
srv_data_table <- function(input,
                           output,
                           session,
                           datasets,
                           dataname,
                           if_filtered,
                           if_distinct) {

  output$data_table <- DT::renderDataTable({
    variables <- input$variables

    validate(need(variables, "need valid variable names"))

    .log("data table update", dataname)

    df <- datasets$get_data(
      dataname,
      filtered = if_filtered(),
      reactive = TRUE
    )

    validate(need(df, paste("data", dataname, "is empty")))

    validate(need(all(variables %in% names(df)), "not all selected variables exist"))

    dataframe_selected <- if (if_distinct()) {
      count_(df, variables)
    } else {
      df[variables]
    }

    DT::datatable(
      dataframe_selected,
      options = list(
        searching = FALSE,
        pageLength = 30,
        lengthMenu = c(5, 15, 30, 100),
        scrollX = TRUE
      )
    )
  })
}
