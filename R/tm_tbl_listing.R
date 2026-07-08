#' `teal` module: GT Summary table
#'
#' Summary table from a given dataset, using `gtsummary`.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param by (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' An object with all available choices and with a pre-selected option on how to split rows.
#'
#' `data_extract_spec` multiple selection: not allowed
#' @param include  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' An object with all available choices and with a pre-selected option that picks columns to include as rows.
#'
#' `data_extract_spec` multiple selection: allowed
#' @param col_label Used to override default labels in summary table, e.g. `list(age = "Age, years")`.
#' The default for each variable is the column label attribute, `attr(., 'label')`.
#' If no label has been set, the column name is used.
# nolint start
#' @inheritDotParams gtsummary::tbl_summary statistic digits type value missing missing_text missing_stat sort
# nolint ends
#' @inherit shared_params return
#' @inheritSection gtsummary::tbl_summary statistic argument
#' @inheritSection gtsummary::tbl_summary digits argument
#' @inheritSection gtsummary::tbl_summary type and value arguments
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `table` (`gtsummary` - output of [`gtsummary::tbl_summary()`])
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
#'     tm_tbl_listing()
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_tbl_listing <- function(
  label = "Listing table",
  dataname = NULL,
  .fun = crane::tbl_listing,
  ...,
  col_label = NULL,
  pre_output = NULL,
  post_output = NULL,
  transformators = list(),
  decorators = list()) {

  message("Initializing tm_gtsummary")

  .fun_quo <- rlang::enquo(.fun) # Capture the function as a quosure for later processing

  tm_gt_template(
    label = label,
    .fun = .fun_quo,
    .ui = ui_gt_template,
    .srv = srv_tbl_listing,
    .dataname = dataname,
    ...,
    col_label = col_label,
    pre_output = pre_output,
    post_output = post_output,
    transformators = transformators,
    decorators = decorators
  )
}

srv_tbl_listing <- function(id, data, ...) {
  srv_gt_template(id = id, data = data, ..., partial_srv = srv_gt_template_partial)
}
