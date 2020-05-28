#' Data Table Viewer Teal Module
#'
#' A data table viewer shows the data using a paginated table.
#'
#' @param label (\code{character})
#' @param variables_selected (\code{list}) a named list that says which variables should be
#'   initially shown for particular dataset. Names in list should correspond with names provided in list `data()`.
#'   If not specified for any dataset - first six variables from dataset will be shown.
#' @param datasets_selected (\code{character}) a character vector that says which datasets should be
#'   shown and in what order. Names in a vector have to correspond with datasets names.
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
#'       variables_selected = list(ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"))
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
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
#'       variables_selected = list(
#'         ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"),
#'         ADTTE = c(
#'           "STUDYID", "USUBJID", "SUBJID", "SITEID",
#'           "PARAM", "PARAMCD", "ARM", "ARMCD", "AVAL", "CNSR"
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
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
#'       variables_selected = list(
#'         ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"),
#'         ADLB = c(
#'           "STUDYID", "USUBJID", "SUBJID", "SITEID",
#'           "PARAM", "PARAMCD", "AVISIT", "AVISITN", "AVAL", "CHG"
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#' # datasets: subsetting or changing order of datasets inside tm_data_table
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADLB <- radlb(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     cdisc_dataset("ADTTE", ADTTE),
#'     code = "ADSL <- radsl(cached = TRUE); ADLB <- radlb(cached = TRUE); ADTTE <- radtte(cached = TRUE);",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_data_table(
#'       variables_selected = list(
#'         ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"),
#'         ADLB = c(
#'           "STUDYID", "USUBJID", "SUBJID", "SITEID",
#'           "PARAM", "PARAMCD", "AVISIT", "AVISITN", "AVAL", "CHG"
#'         )
#'       ),
#'       datasets_selected = c("ADLB", "ADSL")
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_data_table <- function(label = "Data table",
                          variables_selected = list(),
                          datasets_selected = character(0)) {
  stopifnot(
    is_character_single(label),
    is.list(variables_selected),
    is_character_empty(datasets_selected) || is_character_vector(datasets_selected),

    `if`(
      length(variables_selected) > 0,
      !is.null(names(variables_selected)), TRUE
    ),
    `if`(
      length(variables_selected) > 0,
      all(vapply(names(variables_selected), is.character, FUN.VALUE = logical(1))), TRUE
    ),
    `if`(
      length(variables_selected) > 0,
      all(vapply(names(variables_selected), nchar, FUN.VALUE = integer(1)) > 0), TRUE
    ),
    `if`(
      length(variables_selected) > 0,
      all(vapply(variables_selected, is.character, FUN.VALUE = logical(1))), TRUE
    ),
    `if`(
      length(variables_selected) > 0,
      all(vapply(variables_selected, length, FUN.VALUE = integer(1)) > 0), TRUE
    ),

    `if`(
      length(datasets_selected) > 0,
      all(vapply(datasets_selected, nchar, FUN.VALUE = integer(1)) > 0), TRUE
    ),
    `if`(
      length(datasets_selected) > 0,
      all(vapply(datasets_selected, is.character, FUN.VALUE = logical(1))), TRUE
    )
  )

  module(
    label,
    server = srv_page_data_table,
    ui = ui_page_data_table,
    filters = if_character_empty(datasets_selected, "all"),
    server_args = list(datasets_selected = datasets_selected),
    ui_args = list(
      selected = variables_selected,
      datasets_selected = datasets_selected
    )
  )
}


# ui page module
#' @importFrom utils head
ui_page_data_table <- function(id,
                               datasets,
                               selected,
                               datasets_selected) {
  ns <- NS(id)

  datanames <- get_datanames_selected(datasets, datasets_selected)

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
              dataset <- datasets$get_data(x, filtered = FALSE, reactive = FALSE)
              choices <- names(dataset)
              labels <- vapply(
                dataset,
                function(x) ifelse(is.null(attr(x, "label")), "", attr(x, "label")),
                character(1)
              )
              names(choices) <- ifelse(
                is.na(labels) | labels == "",
                choices,
                paste(choices, labels, sep = ": ")
              )
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
                                datasets,
                                datasets_selected) {
  if_filtered <- reactive(as.logical(input$if_filtered))
  if_distinct <- reactive(as.logical(input$if_distinct))

  datanames <- get_datanames_selected(datasets, datasets_selected)

  lapply(
    datanames,
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

  if (!is.null(selected)) {
    all_choices <- choices
    choices <- c(selected, setdiff(choices, selected))
    names(choices) <- names(all_choices)[match(choices, all_choices)]
  }

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

#' a tool for ui and server for getting datanames taking into account the datasets_selected vector
#'
#' @param datasets teal datasets object
#' @param datasets_selected (\code{character}) a character vector that says which datasets should be
#'   shown and in what order. Names in a vector have to correspond with datasets names.
#' @return (\code{character}) a character vector
get_datanames_selected <- function(datasets, datasets_selected) {
  datanames <- datasets$datanames()

  if (!is_character_empty(datasets_selected)) {
    stopifnot(all(datasets_selected %in% datanames))
    datanames <- datasets_selected
  }

  return(datanames)
}
