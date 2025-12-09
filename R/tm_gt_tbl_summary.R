#' `teal` module: Summary table
#'
#' Generates a table summary from a dataset using gtsummary.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param ... Other argumments passed (eventually) to gtsummary::tbl_summary()
#' @param table (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Object with all available choices with pre-selected option for being summarized.
#' @inherit shared_params return
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `table` (`gtsummary` - output of `crane::tbl_roche_summary()`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_t_crosstable(
#'    ..., # arguments for module
#'    decorators = list(
#'      table = teal_transform_module(...) # applied to the `table` output
#'    )
#' )
#' ```
#' For additional details and examples of decorators, refer to the vignette
#' `vignette("decorate-module-output", package = "teal.modules.general")`.
#'
#' To learn more please refer to the vignette
#' `vignette("transform-module-output", package = "teal")` or the [`teal::teal_transform_module()`] documentation.
#'
#' @inheritSection teal::example_module Reporting
#' @export
#' @examples
#' data <- within(teal_data(), {
#'   ADSL <- teal.data::rADSL
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_gt_tbl_summary(
#'       table= data_extract_spec(dataname = "ADSL"),
#'       by = data_extract_spec(dataname = "ADSL", "SEX"),
#'       include = data_extract_spec(dataname = "ADSL",
#'                             select = select_spec(
#'                                choices = c("SITEID", "COUNTRY", "ACTARM"),
#'                                selected = "SITEID",
#'                                multiple = TRUE,
#'                                fixed = FALSE
#'                                )
#'                            )
#'     )
#'   )
#' )
#'if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_gt_tbl_summary <- function(
    label = "Table summary",
    # table,
    # passed to tbl_summary()
    by,
    col_label = NULL,
    statistics = list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~
                      "{n} ({p}%)"),
    digits = NULL,
    type = NULL,
    value = NULL,
    missing = c("ifany", "no", "always"),
    missing_text = "<Missing>",
    missing_stat = "{N_miss}",
    sort = all_categorical(FALSE) ~ "alphanumeric",
    percent = c("column", "row", "cell"),
    include = NULL,

    transformators = list(),
    decorators = list()
) {
  message("Initializing tm_gt_tbl_summary")
  checkmate::assert_string(label)
  # if (inherits(by, "data_extract_spec")) table <- list(by)
  # checkmate::assert_list(by, types = "data_extract_spec")
  # assert_single_selection(by)
  assert_decorators(decorators, "table")

  # Make UI args
  args <- as.list(environment())

  data_extract_list <- list(table = table)

  module <- module(
    label = label,
    server = srv_gt_tbl_summary,
    ui = ui_gt_tbl_summary,
    ui_args = args,
    server_args = c(data_extract_list,
                    as.list(
                      by = by,
                      col_label = col_label,
                      statistics = statistics,
                      digits = digits,
                      type = type,
                      value = value,
                      missing = missing,
                      nonmissing_text = nonmissing_text,
                      nonmissing_stat = nonmissing_stat,
                      sort = sort,
                      percent = percent,
                      include = include),
                    decorators = decorators),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
  attr(module, "teal_bookmarkable") <- TRUE
  module


}


ui_gt_tbl_summary <- function(id, ...) {
  args <- list(by = by,
               # col_label = col_label,
               # statistics = statistics,
               # digits = digits,
               # type = type,
               # value = value,
               missing = missing,
               # nonmissing_text = nonmissing_text,
               # nonmissing_stat = nonmissing_stat,
               # sort = sort,
               percent = percent,
               include = include)
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      textOutput(ns("title")),
      teal.widgets::table_with_settings_ui(ns("table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(list(by, include)),
      teal.transform::data_extract_ui(ns("by"), label = "Variable(s) to stratify with", by),
      teal.transform::data_extract_ui(ns("include"), label = "Variable(s) to include", include),
      radioButtons(
        ns("missing"),
        label = "Display NA counts",
        choices = c("If any" = "ifany", "No" = "no", "Always" = "always"),
        selected = "ifany"
      ),
      radioButtons(
        ns("percent"),
        label = "Percentage based on",
        choices = c("Column" = "column", "Column" = "row", "Cell" = "cell"),
        selected = "column"
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(args$decorators, "table"))
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

srv_gt_tbl_summary <- function(id,
                               data,
                               by,
                               col_label,
                               statistics,
                               digits,
                               type,
                               value,
                               missing,
                               nonmissing_text,
                               nonmissing_stat,
                               sort,
                               percent,
                               include,
                               decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")


      # table,
    # by,
      # col_label,
      # statistics,
      # digits,
      # type,
      # value,
    # missing,
      # nonmissing_text,
      # nonmissing_stat,
      # sort,
    # percent,
    # include


    qenv <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      teal.code::eval_code(obj, "library(crane);library(dplyr)")
    })

    qenv_table <- reactive({
      q <- qenv()
      witin(q, {
        gt <- crane::tbl_roche_summary(ADSL)
      },
      table = as.name(table))

    })


    # by
    # label columns
    # statistics
    # digits
    # type
    # value
    # nonmissing
    # nonmissing_text
    # nonmissing_stat
    # sort
    # percent
    # include

    # crane::tbl_roche_summary
    qenv_table
  })
}
