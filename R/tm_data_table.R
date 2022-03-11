#' Data Table Viewer Teal Module
#'
#' A data table viewer shows the data using a paginated table.
#' @md
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param variables_selected (`list`) A named list of character vectors of the variables (i.e. columns)
#'   which should be initially shown for each dataset. Names of list elements should correspond to the names
#'   of the datasets available in the app. If no entry is specified for a dataset, the first six variables from that
#'   dataset will initially be shown.
#' @param datasets_selected (`character`) A vector of datasets which should be
#'   shown and in what order. Names in the vector have to correspond with datasets names.
#'   If vector of length zero (default) then all datasets are shown.
#' @param dt_args (named `list`) Additional arguments to be passed to `DT::datatable`
#'   (must not include `data` or `options`).
#' @param dt_options (named `list`) The `options` argument to `DT::datatable`. By default
#'   `list(searching = FALSE, pageLength = 30, lengthMenu = c(5, 15, 30, 100), scrollX = TRUE)`
#' @details
#'   The `DT` package has an option `DT.TOJSON_ARGS` to show `Inf` and `NA` in data tables. If this is something
#'   you require then set `options(DT.TOJSON_ARGS =  list(na = "string"))` before running the module.
#'   Note though that sorting of numeric columns with `NA`/`Inf` will be lexicographic not numerical.
#' @export
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_data_table(
#'       variables_selected = list(ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX")),
#'       dt_args = list(caption = "ADSL Table Caption")
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # two-datasets example
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     cdisc_dataset("ADTTE", ADTTE, code = "ADTTE <- synthetic_cdisc_data(\"latest\")$adtte"),
#'     check = TRUE
#'   ),
#'   modules = modules(
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
#' # datasets: subsetting or changing order of datasets inside tm_data_table
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADLB <- synthetic_cdisc_data("latest")$adlb
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     cdisc_dataset("ADLB", ADLB, code = "ADLB <- synthetic_cdisc_data(\"latest\")$adlb"),
#'     cdisc_dataset("ADTTE", ADTTE, code = "ADTTE <- synthetic_cdisc_data(\"latest\")$adtte"),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_data_table(
#'       variables_selected = list(
#'         ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"),
#'         ADLB = c(
#'           "STUDYID", "USUBJID", "SUBJID", "SITEID",
#'           "PARAM", "PARAMCD", "AVISIT", "AVISITN", "AVAL", "CHG"
#'         )
#'       ),
#'       datasets_selected = c("ADTTE", "ADLB", "ADSL")
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # advanced usage of DT options and extensions
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADTTE <- synthetic_cdisc_data("latest")$adtte
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     cdisc_dataset("ADTTE", ADTTE, code = "ADTTE <- synthetic_cdisc_data(\"latest\")$adtte"),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_data_table(
#'       dt_args = list(extensions = c("Buttons", "ColReorder", "FixedHeader")),
#'       dt_options = list(
#'         searching = FALSE,
#'         pageLength = 30,
#'         lengthMenu = c(5, 15, 25, 50, 100),
#'         scrollX = TRUE,
#'         dom = "lBrtip",
#'         buttons = c("copy", "csv", "excel", "pdf", "print"),
#'         colReorder = TRUE,
#'         fixedHeader = TRUE
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_data_table <- function(label = "Data Table",
                          variables_selected = list(),
                          datasets_selected = character(0),
                          dt_args = list(),
                          dt_options = list(
                            searching = FALSE,
                            pageLength = 30,
                            lengthMenu = c(5, 15, 30, 100),
                            scrollX = TRUE
                          ),
                          pre_output = NULL,
                          post_output = NULL) {
  logger::log_info("Initializing tm_data_table")
  checkmate::assert_string(label)
  checkmate::assert_list(variables_selected, min.len = 0, types = "character", names = "named")
  if (length(variables_selected) > 0) {
    lapply(seq_along(variables_selected), function(i) {
      checkmate::assert_character(variables_selected[[i]], min.chars = 1, min.len = 1)
      if (!is.null(names(variables_selected[[i]]))) {
        checkmate::assert_names(names(variables_selected[[i]]))
      }
    })
  }
  checkmate::assert_character(datasets_selected, min.len = 0, min.chars = 1)
  checkmate::check_list(dt_options, names = "named")
  checkmate::assert(
    checkmate::check_list(dt_args, len = 0),
    checkmate::check_subset(names(dt_args), choices = names(formals(DT::datatable)))
  )

  module(
    label,
    server = srv_page_data_table,
    ui = ui_page_data_table,
    filters = if (length(datasets_selected) == 0) "all" else datasets_selected,
    server_args = list(datasets_selected = datasets_selected, dt_args = dt_args, dt_options = dt_options),
    ui_args = list(
      selected = variables_selected,
      datasets_selected = datasets_selected,
      pre_output = pre_output,
      post_output = post_output
    )
  )
}


# ui page module
ui_page_data_table <- function(id,
                               datasets,
                               selected,
                               datasets_selected,
                               pre_output = NULL,
                               post_output = NULL) {
  ns <- NS(id)

  datanames <- get_datanames_selected(datasets, datasets_selected)
  teal.devel::standard_layout(
    output = tagList(
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
                dataset <- datasets$get_data(x, filtered = FALSE)
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
                  utils::head(choices)
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
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}


# server page module
srv_page_data_table <- function(id,
                                datasets,
                                datasets_selected,
                                dt_args,
                                dt_options) {
  moduleServer(id, function(input, output, session) {
    if_filtered <- reactive(as.logical(input$if_filtered))
    if_distinct <- reactive(as.logical(input$if_distinct))

    datanames <- get_datanames_selected(datasets, datasets_selected)

    lapply(
      datanames,
      function(x) {
        srv_data_table(
          id = x,
          datasets = datasets,
          dataname = x,
          if_filtered = if_filtered,
          if_distinct = if_distinct,
          dt_args = dt_args,
          dt_options = dt_options
        )
      }
    )
  })
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
    teal.devel::get_dt_rows(ns("data_table"), ns("dt_rows")),
    fluidRow(
      teal::optionalSelectInput(
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

srv_data_table <- function(id,
                           datasets,
                           dataname,
                           if_filtered,
                           if_distinct,
                           dt_args,
                           dt_options) {
  moduleServer(id, function(input, output, session) {
    output$data_table <- DT::renderDataTable(server = FALSE, {
      variables <- input$variables

      validate(need(variables, "need valid variable names"))

      df <- datasets$get_data(
        dataname,
        filtered = if_filtered()
      )

      validate(need(df, paste("data", dataname, "is empty")))

      validate(need(all(variables %in% names(df)), "not all selected variables exist"))

      dataframe_selected <- if (if_distinct()) {
        dplyr::count(df, dplyr::across(tidyselect::all_of(variables)))
      } else {
        df[variables]
      }

      dt_args$options <- dt_options
      if (!is.null(input$dt_rows)) {
        dt_args$options$pageLength <- input$dt_rows # nolint
      }
      dt_args$data <- dataframe_selected

      do.call(DT::datatable, dt_args)
    })
  })
}

#' a tool for ui and server for getting datanames taking into account the datasets_selected vector
#'
#' @param datasets teal datasets object
#' @param datasets_selected (\code{character}) a character vector that says which datasets should be
#'   shown and in what order. Names in a vector have to correspond with datasets names.
#' @return (\code{character}) a character vector
#' @keywords internal
get_datanames_selected <- function(datasets, datasets_selected) {
  datanames <- datasets$datanames()

  if (!identical(datasets_selected, character(0))) {
    stopifnot(all(datasets_selected %in% datanames))
    datanames <- datasets_selected
  }

  return(datanames)
}
