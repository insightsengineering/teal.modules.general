#' Create a simple cross-table
#' @md
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#' @param x (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'  Object with all available choices with pre-selected option for variable X - row values.
#' @param y (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'  Object with all available choices with pre-selected option for variable Y - column values
#'  \code{data_extract_spec} must not allow multiple selection in this case.
#'
#' @param show_percentage optional, (`logical`) Whether to show percentages
#'   (relevant only when `x` is a `factor`). Defaults to `TRUE`.
#' @param show_total optional, (`logical`) Whether to show total column. Defaults to `TRUE`.
#'
#' @note For more examples, please see the vignette "Using cross table" via
#'   `vignette("using-cross-table", package = "teal.modules.general")`.
#'
#' @export
#'
#' @examples
#' # Percentage cross table of variables from ADSL dataset
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_crosstable(
#'       label = "Cross Table",
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL, subset = function(data) {
#'             idx <- !vapply(data, inherits, logical(1), c("Date", "POSIXct", "POSIXlt"))
#'             return(names(data)[idx])
#'           }),
#'           selected = "COUNTRY",
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL, subset = function(data) {
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
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_t_crosstable <- function(label = "Cross Table",
                            x,
                            y,
                            show_percentage = TRUE,
                            show_total = TRUE,
                            pre_output = NULL,
                            post_output = NULL,
                            basic_table_args = teal.devel::basic_table_args()) {
  logger::log_info("Initializing tm_t_crosstable")
  utils.nest::stop_if_not(
    utils.nest::is_character_single(label),
    utils.nest::is_class_list("data_extract_spec")(x) || inherits(x, "data_extract_spec"),
    utils.nest::is_class_list("data_extract_spec")(y) || inherits(y, "data_extract_spec"),
    utils.nest::is_logical_single(show_percentage),
    utils.nest::is_logical_single(show_total),
    list(
      (inherits(y, "data_extract_spec") && !isTRUE(y$select$multiple)) ||
        (utils.nest::is_class_list("data_extract_spec")(y) &&
          all(vapply(y, function(yy) !isTRUE(yy$select$multiple), logical(1)))
        ),
      "y variable should not allow multiple selection"
    )
  )

  utils.nest::stop_if_not(inherits(basic_table_args, "basic_table_args"))

  ui_args <- as.list(environment())

  server_args <- list(
    label = label,
    x = x,
    y = y,
    basic_table_args = basic_table_args
  )

  module(
    label = label,
    server = srv_t_crosstable,
    ui = ui_t_crosstable,
    ui_args = ui_args,
    server_args = server_args,
    filters = teal.devel::get_extract_datanames(list(x = x, y = y))
  )
}

ui_t_crosstable <- function(id, datasets, x, y, show_percentage, show_total, pre_output, post_output, ...) {
  ns <- NS(id)
  is_single_dataset <- teal.devel::is_single_dataset(x, y)

  join_default_options <- c(
    "Full Join" = "dplyr::full_join",
    "Inner Join" = "dplyr::inner_join",
    "Left Join" = "dplyr::left_join",
    "Right Join" = "dplyr::right_join"
  )

  teal.devel::standard_layout(
    output = teal.devel::white_small_well(
      textOutput(ns("title")),
      teal.devel::table_with_settings_ui(ns("table"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(list(x, y)),
      teal.devel::data_extract_ui(ns("x"), label = "Row values", x, is_single_dataset = is_single_dataset),
      teal.devel::data_extract_ui(ns("y"), label = "Column values", y, is_single_dataset = is_single_dataset),
      optionalSelectInput(
        ns("join_fun"),
        label = "Row to Column type of join",
        choices = join_default_options,
        selected = join_default_options[1],
        multiple = FALSE
      ),
      tags$hr(),
      teal.devel::panel_group(
        teal.devel::panel_item(
          title = "Table settings",
          checkboxInput(ns("show_percentage"), "Show percentage", value = show_percentage),
          checkboxInput(ns("show_total"), "Show total column", value = show_total)
        )
      )
    ),
    forms = teal.devel::get_rcode_ui(ns("rcode")),
    pre_output = pre_output,
    post_output = post_output
  )
}

srv_t_crosstable <- function(input, output, session, datasets, label, x, y, basic_table_args) {
  teal.devel::init_chunks()

  selector_list <- teal.devel::data_extract_multiple_srv(data_extract = list(x = x, y = y), datasets = datasets)

  observeEvent(list(selector_list()$x(), selector_list()$y()), {
    if (identical(selector_list()$x()$dataname, selector_list()$y()$dataname)) {
      shinyjs::hide("join_fun")
    } else {
      shinyjs::show("join_fun")
    }
  })

  merge_function <- reactive({
    if (is.null(input$merge_fun)) {
      "dplyr::full_join"
    } else {
      input$merge_fun
    }
  })

  merged_data_r <- teal.devel::data_merge_srv(
    datasets = datasets,
    selector_list = selector_list,
    merge_function = merge_function
  )

  x_ordered <- reactive({
    selector_list()$x()$select_ordered
  })

  create_table <- reactive({
    teal.devel::chunks_reset()
    teal.devel::chunks_push_data_merge(merged_data_r())

    ANL <- teal.devel::chunks_get_var("ANL") # nolint

    # As this is a summary
    teal.devel::validate_has_data(ANL, 3)

    x_name <- x_ordered()
    y_name <- as.vector(merged_data_r()$columns_source$y)

    validate(need(!utils.nest::is_character_empty(x_name), "Please define column for row variable that is not empty."))
    validate(
      need(
        !utils.nest::is_character_empty(y_name),
        "Please define column for column variable that is not empty."
      )
    )

    teal.devel::validate_has_data(ANL[, c(x_name, y_name)], 3, complete = TRUE, allow_inf = FALSE)

    is_allowed_class <- function(x) is.numeric(x) || is.factor(x) || is.character(x) || is.logical(x)
    validate(need(
      all(vapply(ANL[x_name], is_allowed_class, logical(1))),
      "Selected row variable has an unsupported data type."
    ))
    validate(need(
      is_allowed_class(ANL[[y_name]]),
      "Selected column variable has an unsupported data type."
    ))

    show_percentage <- input$show_percentage # nolint
    show_total <- input$show_total # nolint

    plot_title <- paste(
      "Cross-Table of",
      paste0(varname_w_label(x_name, ANL), collapse = ", "),
      "(rows)", "vs.",
      varname_w_label(y_name, ANL),
      "(columns)"
    )

    teal.devel::chunks_push(substitute(
      expr = {
        title <- plot_title
        print(title)
      },
      env = list(plot_title = plot_title)
    ))

    labels_vec <- vapply( # nolint
      x_ordered(),
      varname_w_label,
      character(1),
      ANL
    )

    teal.devel::chunks_push(substitute(
      expr = {
        lyt <- basic_tables %>%
          split_call() %>%
          rtables::add_colcounts() %>%
          tern::summarize_vars(
            vars = x_name,
            var_labels = labels_vec,
            na.rm = FALSE,
            denom = "N_col",
            .stats = c("n", "mean_sd", "median", "range", count_value)
          )
      },
      env = list(
        basic_tables = teal.devel::parse_basic_table_args(
          basic_table_args = teal.devel::resolve_basic_table_args(basic_table_args)
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
    ))

    teal.devel::chunks_push(substitute(
      expr = {
        ANL <- tern::df_explicit_na(ANL) # nolint
        tbl <- rtables::build_table(lyt = lyt, df = ANL[order(ANL[[y_name]]), ])
        tbl
      },
      env = list(y_name = y_name)
    ))

    teal.devel::chunks_safe_eval()
  })

  output$title <- renderText({
    create_table()
    teal.devel::chunks_get_var("title")
  })

  table <- reactive({
    create_table()
    teal.devel::chunks_get_var("tbl")
  })

  callModule(
    teal.devel::table_with_settings_srv,
    id = "table",
    table_r = table
  )

  show_r_code_title <- reactive(
    paste(
      "Cross-Table of",
      paste0(merged_data_r()$columns_source$x, collapse = ", "),
      "vs.",
      merged_data_r()$columns_source$y
    )
  )

  callModule(
    teal.devel::get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = teal.devel::get_extract_datanames(list(x, y)),
    modal_title = show_r_code_title(),
    code_header = show_r_code_title()
  )
}
