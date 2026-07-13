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
#' - `listing` (`gtsummary` - output of [`gtsummary::tbl_summary()`])
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_gtsummary(
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
#'     tm_gtsummary(
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
tm_tbl_summary <- function(
  label = "Summary table",
  by = NULL,
  include = gtsummary::everything(),
  dataname = NULL,
  .fun = gtsummary::tbl_summary,
  ...,
  col_label = NULL,
  pre_output = NULL,
  post_output = NULL,
  transformators = list(),
  decorators = list()
) {
  message("Initializing tm_gtsummary")

  include_expr <- substitute(include)
  .fun_quo <- rlang::enquo(.fun) # Capture the function as a quosure for later processing

  dots <- c(
    rlang::dots_list(..., .named = TRUE),
    list(by = by),
    list(include = tryCatch(include, error = function(e) include_expr))
  )
  include <- dots$include # Tidyselect expression that needs to be defused for assertion bellow
  stopifnot(
    "dataname string must be provided if teal.picks::picks is not used for other arguments." =
      (length(dots) > 0L && any(vapply(dots, inherits, FUN.VALUE = logical(1L), "picks"))) ||
        checkmate::test_string(dataname)
  )

  if (inherits(by, "picks")) {
    checkmate::assert(
      .var.name = "by",
      if (teal.picks::is_pick_multiple(by$variables)) {
        "Argument `by` must be a single selection (`multiple = FALSE`)."
      } else {
        TRUE
      }
    )
      attr(by, "label") <- "By variable"
  }

  if (inherits(include, "picks")) {
    attr(include, "label") <- "Include variable(s)"
  }

  tm_gt_template(
    label = label,
    by = by,
    include = include,
    .fun = .fun_quo,
    .ui = function(id, ...) {
      ui_gt_template(id = id, partial_ui = ui_tbl_summary_partial, ...)
    },
    .srv = function(id, data, ...) {
      srv_gt_template(id = id, data = data, ..., partial_srv = srv_tbl_summary_partial)
    },
    ...,
    .dataname = dataname,
    col_label = col_label,
    pre_output = pre_output,
    post_output = post_output,
    transformators = transformators,
    decorators = decorators
  )
}

ui_tbl_summary_partial <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)

  tagList(
    radioButtons(
      ns("missing"),
      label = "Display missing counts:",
      choices = c("No" = "no", "If any" = "ifany", "Always" = "always"),
      selected = args$missing
    ),
    radioButtons(
      ns("percent"),
      label = "Percentage based on:",
      choices = c("Column" = "column", "Row" = "row", "Cell" = "cell"),
      selected = args$percent
    )
  )
}

srv_tbl_summary_partial <- function(id,
                                    data,
                                    .fun_quo,
                                    ...,
                                    summary_args_r) {
  moduleServer(id, function(input, output, session) {
    summary_args_processed <- reactive({
      tbl_summary_args <- req(summary_args_r()) # Arguments forwarded from the main server function (template)
      tbl_summary_args$missing <- input$missing # Additional argument from custom UI
      tbl_summary_args$percent <- input$percent # Additional argument from custom UI

      # Defaults to include all variables if none selected
      if (length(tbl_summary_args$include) == 0L) {
        tbl_summary_args$include <- rlang::expr(gtsummary::everything())
      }
      tbl_summary_args
    })

    validated_q <- reactive({ # Custom validation for gtsummary
      q <- req(data())
      summary_args <- req(summary_args_processed())
      validate(
        need(
          is.null(summary_args$include) && is.null(summary_args$by) ||
            rlang::is_quosure(summary_args$include) ||
            rlang::is_expression(summary_args$include) ||
            (length(summary_args$include) != 0L && all(!summary_args$include %in% summary_args$by)),
          "Variables to stratify with and variables to include should be different"
        ),
      )
      q
    })

    srv_gt_template_partial(
      id = id,
      data = validated_q,
      .fun_quo = .fun_quo,
      ...,
      summary_args_r = summary_args_processed
    )
  })
}
