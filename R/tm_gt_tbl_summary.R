#' `teal` module: Summary table
#'
#' Generates a table summary from a dataset using gtsummary.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param by (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Object with all available choices with pre-selected option for how to split the rows
#'
#' `data_extract_spec` must not allow multiple selection.
#' @param include  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Object with all available choices with pre-selected option for which columns to include as rows.
#'
#' `data_extract_spec` can allow multiple selection in this case.
#' @param col_label Used to override default labels in summary table, e.g. `list(age = "Age, years")`.
#' The default for each variable is the column label attribute, `attr(., 'label')`.
#' If no label has been set, the column name is used.
#' @inheritParams gtsummary::tbl_summary
#' @inherit shared_params return
#' @param missing_text String indicating text shown on missing row.
#' @param missing_stat statistic to show on missing row. Default is `"{N_miss}"`.
#' Possible values are `N_miss`, `N_obs`, `N_nonmiss`, `p_miss`, `p_nonmiss`.
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
#' tm_gt_tbl_summary(
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
#' @importFrom methods is
#' @examples
#' data <- within(teal_data(), {
#'   ADSL <- teal.data::rADSL
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_gt_tbl_summary(
#'       by = data_extract_spec(dataname = "ADSL",
#'                              select = select_spec(
#'                                choices = c("SEX", "COUNTRY", "SITEID", "ACTARM"),
#'                                selected = "SEX",
#'                                multiple = FALSE)
#'       ),
#'       include = data_extract_spec(dataname = "ADSL",
#'                                   select = select_spec(
#'                                     choices = c("SITEID", "COUNTRY", "ACTARM"),
#'                                     selected = "SITEID",
#'                                     multiple = TRUE,
#'                                     fixed = FALSE
#'                                   )
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_gt_tbl_summary <- function(
  label = "Table summary",
  by = NULL,
  col_label = NULL,
  statistic = list(
    gtsummary::all_continuous() ~ c("{mean} ({sd})", "{median}", "{min} - {max}"),
    gtsummary::all_categorical() ~ "{n} ({p}%)"
  ),
  digits = NULL,
  type = NULL,
  value = NULL,
  missing_text = "<Missing>",
  missing_stat = "{N_nonmiss}",
  sort = gtsummary::all_categorical(FALSE) ~ "alphanumeric",
  include = tidyselect::everything(),
  transformators = list(),
  decorators = list()
) {
  message("Initializing tm_gt_tbl_summary")
  checkmate::assert_string(label)
  if (inherits(by, "data_extract_spec")) {
    checkmate::assert_list(list(by), types = "data_extract_spec", null.ok = TRUE, any.missing = FALSE, all.missing = FALSE)
    assert_single_selection(list(by))
  }
  if (inherits(include, "data_extract_spec")) {
    checkmate::assert_list(list(include), types = "data_extract_spec", any.missing = FALSE, all.missing = FALSE)
  }
  assert_decorators(decorators, "table")

  # Make UI args
  args <- as.list(environment())

  module <- module(
    label = label,
    server = srv_gt_tbl_summary,
    ui = ui_gt_tbl_summary,
    ui_args = args,
    server_args = list(
      by = by,
      col_label = col_label,
      statistic = statistic,
      digits = digits,
      type = type,
      value = value,
      # missing = missing,
      missing_text = missing_text,
      missing_stat = missing_stat,
      sort = sort,
      # percent = percent,
      include = include,
      decorators = decorators
    ),
    transformators = transformators
  )
  attr(module, "teal_bookmarkable") <- TRUE
  module
}


ui_gt_tbl_summary <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  teal.widgets::standard_layout(
    output = gt::gt_output(ns("table")),
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
        ns("missing"),
        label = "Display missing counts:",
        choices = c("No" = "no", "If any" = "ifany", "Always" = "always"),
        selected = "no"
      ),
      radioButtons(
        ns("percent"),
        label = "Percentage based on:",
        choices = c("Column" = "column", "Row" = "row", "Cell" = "cell"),
        selected = "column"
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(args$decorators, "table"))
    ),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

srv_gt_tbl_summary <- function(id,
                               data,
                               by,
                               col_label,
                               statistic,
                               digits,
                               type,
                               value,
                               missing_text,
                               missing_stat,
                               sort,
                               include,
                               decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  checkmate::assert_character(missing_text, len = 1L)
  checkmate::assert_character(missing_stat, len = 1L)

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")


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
      req(qenv())

      # table
      if (!is.null(by) || !is.null(include)) {
        validate(need(is_single_dataset(list(by = by, include = include)), "Variables should come from the same dataset."))
      }

      dataset <- if (!is.null(by)) {
        by$dataname
      } else {
        include$dataname
      }

      validate(
        need(!is.null(dataset), "Specify variables to stratify or to include on the summary table."),
        need(teal.transform::is_single_dataset(by, include), "Input from multiple tables: this module doesn't accept that.")
      )

      nam_input <- names(input)

      # by: input + corner cases
      if (!is.null(by)) {
        by_variable <- input[[nam_input[startsWith(nam_input, "by") & endsWith(nam_input, "select")]]]
      }

      # label columns
      if (!is.null(col_label)) {
        checkmate::check_character(col_label)
      }

      # statistic
      if (!is.null(statistic)) {
        validate(need(all(vapply(statistic, is, class2 = "formula", logical(1L))), "All elements of statistic should be formulas"))
      }

      # digits
      if (!is.null(digits)) {
        integer <- is.integer(digits) && length(digits) >= 1L
        functions <- is.function(digits) || all(vapply(digits, is.function, logical(1L)))
        validate(need(any(integer || functions), "digits should be integer(s) or a function (or list of)."))
      }
      # type
      if (!is.null(type)) {
        possible_types <- c("continuous", "continuous2", "categorical", "dichotomous")
        validate(need(
          length(type) == 1L && type %in% possible_types,
          paste0("One of: c(", toString(dQuote(possible_types)), ").")
        ))
      }

      # value
      if (!is.null(type)) {
        possible_types <- c("continuous", "continuous2", "categorical", "dichotomous")
        validate(need(
          length(type) == 1L && type %in% possible_types,
          paste0("One of: c(", toString(dQuote(possible_types)), ").")
        ))
      }

      # nonmissing: input

      # nonmissing_text
      if (!identical(missing_text, "<Missing>")) {
        validate(need(is.character(missing_text), "Must be a character."))
      }

      # nonmissing_stat
      if (!identical(missing_stat, "{N_miss}")) {
        validate(need(is.character(missing_stat), "Must be a character to be parsed by glue."))
      }

      # sort
      if (!is.null(sort)) {
        validate(need(all(vapply(sort, is, class2 = "formula", logical(1L))), "All elements of sort should be formulas"))
      }
      # percent: input
      # include: input + corner cases
      include_variables <- input[[nam_input[startsWith(nam_input, "include") & endsWith(nam_input, "select")]]]
      if (is.null(include_variables)) {
        include_variables <- formals(tbl_summary)$include
      }

      call("tbl_roche_summary",
        data = as.name(dataset),
        by = by_variable,
        label = col_label,
        statistic = statistic,
        digits = digits,
        type = type,
        value = value,
        nonmissing = input$missing,
        nonmissing_text = missing_text,
        nonmissing_stat = missing_stat,
        sort = sort,
        percent = input$percent,
        include = include_variables
      )
    })

    output_q <- reactive({
      q <- req(qenv())
      table_call <- req(summary_args())
      within(q,
        expr = {
          table <- table_crane
        },
        table_crane = table_call
      )
    })

    decorated_output_q <- srv_decorate_teal_data(
      id = "decorator",
      data = output_q,
      decorators = select_decorators(decorators, "table"),
      expr = quote(table)
    )

    table_r <- reactive({
      req(decorated_output_q())
      table <- decorated_output_q()[["table"]]
      gtsummary::as_gt(table)
    })
    output$table <- gt::render_gt({
      gtsummary::as_gt(output_q()[["table"]])
    })

    decorated_output_q
  })
}
