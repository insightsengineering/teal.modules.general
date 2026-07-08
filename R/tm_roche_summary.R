#' `teal` module: Roche GT Summary table
#'
#' Summary table from a given dataset, using `crane::tbl_roche_summary`.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @inheritParams tm_gtsummary
# nolint start
#' @inheritDotParams crane::tbl_roche_summary statistic digits type sort nonmissing nonmissing_text nonmissing_stat value
# nolint ends
#' @inherit shared_params return
#' @inheritSection gtsummary::tbl_summary statistic argument
#' @inheritSection gtsummary::tbl_summary digits argument
#' @inheritSection gtsummary::tbl_summary type and value arguments
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `table` (`gtsummary` - output of [`crane::tbl_roche_summary()`])
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_gtsummary(
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
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#' data <- within(teal.data::teal_data(), {
#'   ADSL <- teal.data::rADSL
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_roche_summary(
#'       by = teal.transform::data_extract_spec(
#'         dataname = "ADSL",
#'         select = teal.transform::select_spec(
#'           choices = c("SEX", "COUNTRY", "SITEID", "ACTARM"),
#'           selected = "SEX",
#'           multiple = FALSE
#'         )
#'       ),
#'       include = teal.transform::data_extract_spec(
#'         dataname = "ADSL",
#'         select = teal.transform::select_spec(
#'           choices = c("SITEID", "COUNTRY", "ACTARM"),
#'           selected = "SITEID",
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_roche_summary <- function(
  label = "Summary table",
  by,
  include,
  .fun = crane::tbl_roche_summary,
  ...,
  col_label = NULL,
  pre_output = NULL,
  post_output = NULL,
  transformators = list(),
  decorators = list()
) {
  message("Initializing tm_roche_summary")

  if (inherits(by, "data_extract_spec")) by <- list(by)
  if (inherits(include, "data_extract_spec")) include <- list(include)
  checkmate::assert_list(by, types = "data_extract_spec")
  assert_single_selection(by)
  checkmate::assert_list(include, types = "data_extract_spec")

  .fun_quo <- rlang::enquo(.fun) # Capture the function as a quosure for later processing

  attr(by, "label") <- "By variable"
  attr(include, "label") <- "Include variable(s)"

  tm_gt_template(
    label = label,
    by = by,
    include = include,
    .fun = .fun_quo,
    .ui = ui_roche_summary,
    .srv = srv_roche_summary,
    ...,
    col_label = col_label,
    pre_output = pre_output,
    post_output = post_output,
    transformators = transformators,
    decorators = decorators
  )
}

ui_roche_summary <- function(id, ...) {
  ui_gt_template(id = id, partial_ui = ui_roche_summary_partial(id, ...), ...)
}

srv_roche_summary <- function(id, data, ...) {
  srv_gt_template(id = id, data = data, ..., partial_srv = srv_roche_summary_partial)
}

ui_roche_summary_partial <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)

  tagList(
    radioButtons(
      ns(NS("custom", "nonmissing")),
      label = "Display missing counts:",
      choices = c("No" = "no", "If any" = "ifany", "Always" = "always"),
      selected = args$nonmissing
    ),
    radioButtons(
      ns(NS("custom", "percent")),
      label = "Percentage based on:",
      choices = c("Column" = "column", "Row" = "row", "Cell" = "cell"),
      selected = args$percent
    )
  )
}

srv_roche_summary_partial <- function(id,
                                      data,
                                      by,
                                      include,
                                      .fun_quo,
                                      ...,
                                      decorators,
                                      summary_args_r) {
  moduleServer(id, function(input, output, session) {
    summary_args_processed <- reactive({
      tbl_summary_args <- req(summary_args_r()) # Additional arguments from UI
      tbl_summary_args$nonmissing <- input$nonmissing
      tbl_summary_args$percent <- input$percent
      tbl_summary_args
    })

    tbl_summary_call <- reactive({
      as.call(c(list(rlang::get_expr(.fun_quo)), req(summary_args_processed())))
    })

    validated_q <- reactive({
      q <- req(data())
      summary_args <- req(summary_args_processed())
      validate(
        need(
          length(summary_args$include) != 0L && all(!summary_args$include %in% summary_args$by),
          "Variables to stratify with and variables to include should be different"
        ),
      )
      q
    })

    qenv <- reactive({
      obj <- req(validated_q())
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      teal.code::eval_code(obj, "library(gtsummary)")
    })

    reactive({
      within(req(qenv()),
        expr = table <- table_call,
        table_call = req(tbl_summary_call())
      )
    })
  })
}
