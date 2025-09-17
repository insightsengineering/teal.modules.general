#' `teal` module: Cross-table
#'
#' Generates a simple cross-table of two variables from a dataset with custom
#' options for showing percentages and sub-totals.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param x (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Object with all available choices with pre-selected option for variable X - row values.
#' In case of `data_extract_spec` use `select_spec(..., ordered = TRUE)` if table elements should be
#' rendered according to selection order.
#' @param y (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Object with all available choices with pre-selected option for variable Y - column values.
#'
#' `data_extract_spec` must not allow multiple selection in this case.
#' @param show_percentage (`logical(1)`)
#' Indicates whether to show percentages (relevant only when `x` is a `factor`).
#' Defaults to `TRUE`.
#' @param show_total (`logical(1)`)
#' Indicates whether to show total column.
#' Defaults to `TRUE`.
#' @param remove_zero_columns (`logical(1)`)
#' Indicates whether to remove columns that contain only zeros from the output table.
#' Defaults to `FALSE`.
#'
#' @note For more examples, please see the vignette "Using cross table" via
#' `vignette("using-cross-table", package = "teal.modules.general")`.
#'
#' @inherit shared_params return
#'
#' @section Table Settings:
#' The module provides several table settings that can be adjusted:
#' \itemize{
#'   \item \code{Show column percentage}: Shows column percentages when enabled
#'   \item \code{Show total column}: Shows a total column when enabled
#'   \item \code{Remove zero-only columns}: Removes columns that contain only zeros from the output table
#' }
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `table` (`ElementaryTable` - output of `rtables::build_table`)
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
#'
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#' # general data example
#' data <- teal_data()
#' data <- within(data, {
#'   mtcars <- mtcars
#'   for (v in c("cyl", "vs", "am", "gear")) {
#'     mtcars[[v]] <- as.factor(mtcars[[v]])
#'   }
#'   mtcars[["primary_key"]] <- seq_len(nrow(mtcars))
#' })
#' join_keys(data) <- join_keys(join_key("mtcars", "mtcars", "primary_key"))
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_crosstable(
#'       label = "Cross Table",
#'       x = data_extract_spec(
#'         dataname = "mtcars",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["mtcars"]], c("cyl", "vs", "am", "gear")),
#'           selected = c("cyl", "gear"),
#'           multiple = TRUE,
#'           ordered = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "mtcars",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["mtcars"]], c("cyl", "vs", "am", "gear")),
#'           selected = "vs",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#' # CDISC data example
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- teal.data::rADSL
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_t_crosstable(
#'       label = "Cross Table",
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["ADSL"]], subset = function(data) {
#'             idx <- !vapply(data, inherits, logical(1), c("Date", "POSIXct", "POSIXlt"))
#'             return(names(data)[idx])
#'           }),
#'           selected = "COUNTRY",
#'           multiple = TRUE,
#'           ordered = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["ADSL"]], subset = function(data) {
#'             idx <- vapply(data, is.factor, logical(1))
#'             return(names(data)[idx])
#'           }),
#'           selected = "SEX",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
#'
tm_t_crosstable <- function(label = "Cross Table",
                            x,
                            y,
                            show_percentage = TRUE,
                            show_total = TRUE,
                            remove_zero_columns = FALSE,
                            pre_output = NULL,
                            post_output = NULL,
                            basic_table_args = teal.widgets::basic_table_args(),
                            transformators = list(),
                            decorators = list()) {
  message("Initializing tm_t_crosstable")

  # Normalize the parameters
  if (inherits(x, "data_extract_spec")) x <- list(x)
  if (inherits(y, "data_extract_spec")) y <- list(y)

  # Start of assertions
  checkmate::assert_string(label)
  checkmate::assert_list(x, types = "data_extract_spec")

  checkmate::assert_list(y, types = "data_extract_spec")
  assert_single_selection(y)

  checkmate::assert_flag(show_percentage)
  checkmate::assert_flag(show_total)
  checkmate::assert_flag(remove_zero_columns)
  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_class(basic_table_args, classes = "basic_table_args")

  assert_decorators(decorators, "table")
  # End of assertions

  # Make UI args
  ui_args <- as.list(environment())

  server_args <- list(
    label = label,
    x = x,
    y = y,
    remove_zero_columns = remove_zero_columns,
    basic_table_args = basic_table_args,
    decorators = decorators
  )

  ans <- module(
    label = label,
    server = srv_t_crosstable,
    ui = ui_t_crosstable,
    ui_args = ui_args,
    server_args = server_args,
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(list(x = x, y = y))
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the cross-table module
ui_t_crosstable <- function(id, x, y, show_percentage, show_total, remove_zero_columns, pre_output, post_output, ...) {
  args <- list(...)
  ns <- NS(id)
  is_single_dataset <- teal.transform::is_single_dataset(x, y)

  join_default_options <- c(
    "Full Join" = "dplyr::full_join",
    "Inner Join" = "dplyr::inner_join",
    "Left Join" = "dplyr::left_join",
    "Right Join" = "dplyr::right_join"
  )

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      textOutput(ns("title")),
      teal.widgets::table_with_settings_ui(ns("table"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(list(x, y)),
      teal.transform::data_extract_ui(ns("x"), label = "Row values", x, is_single_dataset = is_single_dataset),
      teal.transform::data_extract_ui(ns("y"), label = "Column values", y, is_single_dataset = is_single_dataset),
      teal.widgets::optionalSelectInput(
        ns("join_fun"),
        label = "Row to Column type of join",
        choices = join_default_options,
        selected = join_default_options[1],
        multiple = FALSE
      ),
      tags$hr(),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Table settings",
          checkboxInput(ns("show_percentage"), "Show column percentage", value = show_percentage),
          checkboxInput(ns("show_total"), "Show total column", value = show_total),
          checkboxInput(ns("remove_zero_columns"), "Remove zero-only columns", value = remove_zero_columns)
        )
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(args$decorators, "table"))
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

# Server function for the cross-table module
srv_t_crosstable <- function(id, data, label, x, y, remove_zero_columns, basic_table_args, decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(x = x, y = y),
      datasets = data,
      select_validation_rule = list(
        x = shinyvalidate::sv_required("Please define column for row variable."),
        y = shinyvalidate::sv_required("Please define column for column variable.")
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("join_fun", function(value) {
        if (!identical(selector_list()$x()$dataname, selector_list()$y()$dataname)) {
          if (!shinyvalidate::input_provided(value)) {
            "Please select a joining function."
          }
        }
      })
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    observeEvent(
      eventExpr = {
        req(!is.null(selector_list()$x()) && !is.null(selector_list()$y()))
        list(selector_list()$x(), selector_list()$y())
      },
      handlerExpr = {
        if (identical(selector_list()$x()$dataname, selector_list()$y()$dataname)) {
          shinyjs::hide("join_fun")
        } else {
          shinyjs::show("join_fun")
        }
      }
    )

    merge_function <- reactive({
      if (is.null(input$join_fun)) {
        "dplyr::full_join"
      } else {
        input$join_fun
      }
    })

    anl_merged_input <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list,
      merge_function = merge_function
    )
    qenv <- reactive({
      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Cross Table"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      teal.code::eval_code(obj, 'library("rtables");library("tern");library("dplyr")') # nolint quotes
    })
    anl_merged_q <- reactive({
      req(anl_merged_input())
      qenv() %>%
        teal.code::eval_code(as.expression(anl_merged_input()$expr))
    })

    merged <- list(
      anl_input_r = anl_merged_input,
      anl_q_r = anl_merged_q
    )

    output_q <- reactive({
      teal::validate_inputs(iv_r())
      ANL <- merged$anl_q_r()[["ANL"]]

      # As this is a summary
      x_name <- as.vector(merged$anl_input_r()$columns_source$x)
      y_name <- as.vector(merged$anl_input_r()$columns_source$y)

      teal::validate_has_data(ANL, 3)
      teal::validate_has_data(ANL[, c(x_name, y_name)], 3, complete = TRUE, allow_inf = FALSE)

      is_allowed_class <- function(x) is.numeric(x) || is.factor(x) || is.character(x) || is.logical(x)
      validate(need(
        all(vapply(ANL[x_name], is_allowed_class, logical(1))),
        "Selected row variable has an unsupported data type."
      ))
      validate(need(
        is_allowed_class(ANL[[y_name]]),
        "Selected column variable has an unsupported data type."
      ))

      show_percentage <- input$show_percentage
      show_total <- input$show_total
      remove_zero_columns <- input$remove_zero_columns

      plot_title <- paste(
        "Cross-Table of",
        paste0(varname_w_label(x_name, ANL), collapse = ", "),
        "(rows)", "vs.",
        varname_w_label(y_name, ANL),
        "(columns)"
      )

      labels_vec <- vapply(
        x_name,
        varname_w_label,
        character(1),
        ANL
      )

      obj <- merged$anl_q_r()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "# Table")
      obj <- teal.code::eval_code(
        obj,
        substitute(
          expr = {
            title <- plot_title
          },
          env = list(plot_title = plot_title)
        )
      ) %>%
        teal.code::eval_code(
          substitute(
            expr = {
              table <- basic_tables %>%
                split_call %>% # styler: off
                rtables::add_colcounts() %>%
                tern::analyze_vars(
                  vars = x_name,
                  var_labels = labels_vec,
                  na.rm = FALSE,
                  denom = "N_col",
                  .stats = c("mean_sd", "median", "range", count_value)
                )
            },
            env = list(
              basic_tables = teal.widgets::parse_basic_table_args(
                basic_table_args = teal.widgets::resolve_basic_table_args(basic_table_args)
              ),
              split_call = if (show_total) {
                substitute(
                  expr = rtables::split_cols_by(
                    y_name,
                    split_fun = rtables::add_overall_level(label = "Total", first = FALSE)
                  ),
                  env = list(y_name = y_name)
                )
              } else {
                substitute(rtables::split_cols_by(y_name), env = list(y_name = y_name))
              },
              x_name = x_name,
              labels_vec = labels_vec,
              count_value = ifelse(show_percentage, "count_fraction", "count")
            )
          )
        ) %>%
        teal.code::eval_code(
          expression(ANL <- tern::df_explicit_na(ANL))
        )

      if (remove_zero_columns) {
        obj <- obj %>%
          teal.code::eval_code(
            substitute(
              expr = {
                ANL[[y_name]] <- droplevels(ANL[[y_name]])
                table <- rtables::build_table(lyt = table, df = ANL[order(ANL[[y_name]]), ])
              },
              env = list(y_name = y_name)
            )
          )
      } else {
        obj <- obj %>%
          teal.code::eval_code(
            substitute(
              expr = {
                table <- rtables::build_table(lyt = table, df = ANL[order(ANL[[y_name]]), ])
              },
              env = list(y_name = y_name)
            )
          )
      }
      obj
    })

    decorated_output_q <- srv_decorate_teal_data(
      id = "decorator",
      data = output_q,
      decorators = select_decorators(decorators, "table"),
      expr = quote(table)
    )

    output$title <- renderText(req(decorated_output_q())[["title"]])

    table_r <- reactive({
      req(iv_r()$is_valid())
      req(decorated_output_q())[["table"]]
    })

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    decorated_output_q
  })
}
