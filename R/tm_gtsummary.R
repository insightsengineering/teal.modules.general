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
tm_gtsummary <- function(
  label = "Summary table",
  by,
  include,
  ...,
  col_label = NULL,
  pre_output = NULL,
  post_output = NULL,
  transformators = list(),
  decorators = list()
) {
  message("Initializing tm_gtsummary")

  # Normalize the parameters
  if (inherits(by, "data_extract_spec")) by <- list(by)
  if (inherits(include, "data_extract_spec")) include <- list(include)

  checkmate::assert_string(label)
  checkmate::assert_list(col_label, null.ok = TRUE, types = "character")
  checkmate::assert_list(by, types = "data_extract_spec")
  assert_single_selection(by)
  checkmate::assert_list(include, types = "data_extract_spec")
  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  assert_decorators(decorators, "table")
  datanames <- teal.transform::get_extract_datanames(list(by = by, include = include))
  checkmate::assert_character(datanames, len = 1L, any.missing = FALSE, all.missing = FALSE)

  # Make UI args
  ui_args <- as.list(environment())
  ui_args <- c(ui_args, list(...))
  srv_args <- list(...)
  srv_args$by <- by
  srv_args$include <- include
  srv_args$decorators <- decorators
  srv_args$label <- col_label
  module <- module(
    label = label,
    server = srv_gtsummary,
    ui = ui_gtsummary,
    ui_args = ui_args,
    server_args = srv_args,
    transformators = transformators,
    datanames = datanames
  )
  attr(module, "teal_bookmarkable") <- TRUE
  module
}


ui_gtsummary <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)

  teal.widgets::standard_layout(
    output = teal.widgets::table_with_settings_ui(ns("table")),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(args[c("by", "include")]),
      teal.transform::data_extract_ui(ns("by"),
        label = "Variable(s) to stratify with",
        data_extract_spec = args$by
      ),
      teal.transform::data_extract_ui(ns("include"),
        label = "Variable(s) to include",
        data_extract_spec = args$include
      ),
      radioButtons(
        ns("nonmissing"),
        label = "Display missing counts:",
        choices = c("No" = "no", "If any" = "ifany", "Always" = "always"),
        selected = args$nonmissing
      ),
      radioButtons(
        ns("percent"),
        label = "Percentage based on:",
        choices = c("Column" = "column", "Row" = "row", "Cell" = "cell"),
        selected = args$percent
      ),
      # Allow multiple decorators for a single object (table)
      div(
        id = ns("decorator_container"),
        lapply(seq_along(args$decorators), function(i) {
          name_decorator <- names(args$decorators)[i]
          div(
            id = ns(paste0("decorate_", name_decorator)),
            ui_transform_teal_data(
              ns(paste0("decorate_", name_decorator)),
              transformators = args$decorators[[i]]
            )
          )
        })
      )
    ),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

srv_gtsummary <- function(id,
                          data,
                          by,
                          include,
                          ...,
                          decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    de <- list(by = by, include = include)
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = de,
      datasets = data
    )

    qenv <- reactive({
      obj <- req(data())
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      teal.code::eval_code(obj, "library(crane)")
    })

    summary_args <- reactive({
      sl <- req(selector_list())
      dataset <- sl$by()$dataname

      validate(
        need(
          !is.null(dataset),
          "Specify variables to stratify or to include on the summary table."
        ),
        need(
          do.call(teal.transform::is_single_dataset, de),
          "Input from multiple tables: this module doesn't accept that."
        )
      )

      # by: input + all variables (default on gtsummary)
      by_variable <- req(sl$by()$select)
      include_variables <- sl$include()$select
      if (length(include_variables) != 0L) {
        validate(
          need(
            by_variable != include_variables,
            "Variables to stratify with and variables to include should be different"
          ),
        )
      }

      tbl_summary_args <- list(...)
      tbl_summary_args$by <- by_variable
      if (length(include_variables)) {
        tbl_summary_args$include <- include_variables
      }
      tbl_summary_args <- c(tbl_summary_args,
        nonmissing = input$nonmissing,
        percent = input$percent
      )
      as.call(
        c(
          list(
            quote(crane::tbl_roche_summary),
            data = as.name(dataset)
          ),
          tbl_summary_args
        )
      )
    })

    output_q <- reactive({
      q <- req(qenv())
      table_call <- req(summary_args())
      qq <- within(q,
        expr = {
          table <- table_crane
        },
        table_crane = table_call
      )
      qq
    })

    decorations <- lapply(names(decorators), function(decorator_name) {
      function(data) {
        srv_transform_teal_data(
          paste0("decorate_", decorator_name),
          data = data,
          transformators = decorators[[decorator_name]]
        )
      }
    })
    output_data_decorated <- Reduce(function(f, ...) f(...), decorations, init = output_q, right = TRUE)
    print_output_decorated <- reactive({
      q <- req(output_data_decorated())
      within(q, {
        table
      })
    })

    table_r <- reactive({
      req(output_data_decorated())[["table"]]
    })

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )
    print_output_decorated
  })
}
