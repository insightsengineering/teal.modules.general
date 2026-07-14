#' `teal` module: GT Summary table
#'
#' Summary table from a given dataset, using `gtsummary`.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @inheritDotParams crane::tbl_listing split_by_rows split_by_columns add_blank_rows
#' @inherit shared_params return
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `listing` (`gt_summary` - output of [`crane::tbl_listing()`])
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_tbl_listing(
#'    ..., # arguments for module
#'    decorators = list(
#'      listing = teal_transform_module(...) # applied to the `listing` output
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
  ...,
  col_label = NULL,
  pre_output = NULL,
  post_output = NULL,
  transformators = list(),
  decorators = list()
) {
  message("Initializing tm_tbl_listing")

  tm_gt_template(
    label = label,
    .fun = crane::tbl_listing,
    .ui = ui_gt_template,
    .srv = function(id, data, ...) {
      srv_gt_template(id = id, data = data, ..., partial_srv = srv_gt_template_partial)
    },
    .dataname = dataname,
    ...,
    col_label = col_label,
    pre_output = pre_output,
    post_output = post_output,
    transformators = transformators,
    .decorator_name = "listing",
    decorators = decorators
  )
}
