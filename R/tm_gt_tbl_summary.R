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
#'       table = data_extract_spec(dataname = "ADSL"),
#'       by = data_extract_spec(dataname = "ADSL", "SEX"),
#'       include = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
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
tm_gt_tbl_summary <- function(
  label = "Table summary",
  # table,
  # passed to tbl_summary()
  by = NULL,
  col_label = NULL,
  statistics = list(
    all_continuous() ~ "{median} ({p25}, {p75})",
    all_categorical() ~ "{n} ({p}%)"
  ),
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

  module <- module(
    label = label,
    server = srv_gt_tbl_summary,
    ui = ui_gt_tbl_summary,
    ui_args = args,
    server_args = list(
      by = by,
      col_label = col_label,
      statistics = statistics,
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
  # args <- list(by = by,
  #              # col_label = col_label,
  #              # statistics = statistics,
  #              # digits = digits,
  #              # type = type,
  #              # value = value,
  #              missing = missing,
  #              # nonmissing_text = nonmissing_text,
  #              # nonmissing_stat = nonmissing_stat,
  #              # sort = sort,
  #              percent = percent,
  #              include = include)
  ns <- NS(id)
  args <- list(...)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      textOutput(ns("title")),
      # teal.widgets::table_with_settings_ui(ns("table"))
      gt::gt_output(ns("table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(args[c("by", "include")]),
      teal.transform::data_extract_ui(ns("by"),
        label = "Variable(s) to stratify with", ,
        data_extract_spec = args$by
      ),
      teal.transform::data_extract_ui(ns("include"),
        label = "Variable(s) to include",
        data_extract_spec = args$include
      ),
      radioButtons(
        ns("missing"),
        label = "Display NA counts",
        choices = c("If any" = "ifany", "No" = "no", "Always" = "always"),
        selected = "ifany"
      ),
      radioButtons(
        ns("percent"),
        label = "Percentage based on",
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
                               statistics,
                               digits,
                               type,
                               value,
                               # missing,
                               missing_text,
                               missing_stat,
                               sort,
                               # percent,
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
      obj <- req(data())
      if (!is.null(input$by) || !is.null(input$include)) {
        validate(need(is_single_dataset(list(by = input$by, include = input$include)), "Variables should come from the same dataset"))
        browser()
      }
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )
      teal.code::eval_code(obj, "library(crane);library(dplyr)")
    })

    summary_args <- reactive({
      # browser()
      # by_input <- names(input)[endsWith(names(input), "-select")]
      # selector_list <- teal.transform::data_extract_multiple_srv(
      #   data_extract = list(by = input$`by-dataset_ADSL_singleextract-select`, include = input$`include-dataset_ADSL_singleextract-select`),
      #   datasets = data)

      dataset <- if (!is.null(by)) {
        by$dataname
      } else {
        include$dataname
      }

      # validate(need(!is.null(dataset), "Specify variables to stratify or to include on the summary table."),
      #          need(teal.transform::is_single_dataset(by, include), "Input from multiple tables: this module doesn't accept that.")
      # )
      #
      # nam_input <- names(input)
      # # by
      # if (!is.null(by)) {
      #   # browser()
      #   isolate({by_variable <- input[[nam_input[startsWith(nam_input, "by") & endsWith(nam_input, "select")]]]})
      # }
      # # label columns
      # if (!is.null(col_label)) {
      #   labels <- col_label
      # }
      #
      # # statistics
      # if (!is.null(statistics)) {
      #   validate(need(all(vapply(statistics, is, class2 = "formula", logical(1L))), "All elements of statistics should be formulas"))
      #   stats <- statistics
      # }
      #
      # # digits
      # if (!is.null(digits)) {
      #   integer <- is.integer(digits) && length(digits) >= 1L
      #   functions <- is.function(digits) || all(vapply(digits, is.function, logical(1L)))
      #   validate(need(any(integer || functions), "digits should be integer(s) or a function (or list of)"))
      # }
      # # type
      # if (!is.null(type)) {
      #   possible_types <- c("continuous", "continuous2", "categorical", "dichotomous")
      #   validate(need(length(type) == 1L && type %in% possible_types,
      #                 paste0("One of: c(", toString(dQuote(possible_types)), ").")
      #   ))
      # }
      #
      # # value
      # if (!is.null(type)) {
      #   possible_types <- c("continuous", "continuous2", "categorical", "dichotomous")
      #   validate(need(length(type) == 1L && type %in% possible_types,
      #                 paste0("One of: c(", toString(dQuote(possible_types)), ").")
      #   ))
      # }
      #
      # # nonmissing
      # if (req(input$missing) != "ifany") {
      #   nonmissing <- input$missing
      # }
      #
      # # nonmissing_text
      # if (!identical(missing_text, "<Missing>")) {
      #   valiate(need(is.character(missing_text), "Must be a character."))
      #   nonmissing_text <- input$missing_text
      # }
      # # nonmissing_stat
      # if (!identical(missing_stat, "{N_miss}")) {
      #   valiate(need(is.character(missing_stat), "Must be a character to be parsed by glue."))
      #   nonmissing_stat <- missing_stat
      # }
      # # sort
      # if (!is.null(sort)) {
      #   validate(need(all(vapply(statistics, is, class2 = "formula", logical(1L))), "All elements of sort should be formulas"))
      # }
      # # percent
      # if (req(input$percent) != "column") {
      #   percent <- input$percent
      # }
      #
      # # include
      # if (!is.null(include)) {
      #   isolate({include_variables <- input[[nam_input[startsWith(nam_input, "include") & endsWith(nam_input, "select")]]]})
      # }

      table_crane <- call("tbl_roche_summary",
        data = as.name(dataset)
      )
    })

    output_q <- reactive({
      q <- req(qenv(), summary_args())
      browser()
      within(q,
        {
          table <- table_crane
        },
        table_crane = summary_args()
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

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    table_r
  })
}
