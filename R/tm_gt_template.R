tm_gt_template <- function(
  label = "Summary table",
  .fun = gtsummary::tbl_summary,
  .ui = ui_gt_template,
  .srv = srv_gt_template,
  .dataname = NULL,
  ...,
  col_label = NULL,
  pre_output = NULL,
  post_output = NULL,
  .decorator_name = "table",
  transformators = list(),
  decorators = list()
) {
  dots <- rlang::dots_list(..., .named = TRUE)

  # Normalize the parameters and extract teal.picks::picks, teal.picks::values and
  #  named static arguments from the dots
  picks_index <- vapply(dots, inherits, FUN.VALUE = logical(1L), "picks")
  values_index <- vapply(dots, checkmate::test_class, FUN.VALUE = logical(1L), classes = c("pick", "values"))

  opts_picks <- dots[picks_index]
  opts_values <- dots[values_index]
  opts_static <- dots[!(picks_index | values_index)]
  if (!is.null(col_label)) {
    opts_static$label <- col_label # label conflicts with teal modules argument, so we need to relabel it
  }

  vapply(names(dots)[picks_index], function(x) {
    checkmate::assert(
      .var.name = x,
      if (checkmate::test_class(dots[[x]]$variables, c("pick", "variables"))) {
        TRUE
      } else {
        "picks must contain `variables()`"
      }
    )
  }, FUN.VALUE = logical(1L))

  checkmate::assert_string(label)
  checkmate::assert_list(col_label, null.ok = TRUE, types = "character")
  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_string(.decorator_name)
  teal::assert_decorators(decorators, .decorator_name)
  datanames <- if (length(opts_picks) == 0L) {
    .dataname
  } else {
    .picks_datanames(opts_picks)
  }
  checkmate::assert_character(datanames, any.missing = FALSE, min.len = 1L)

  .fun_quo <- rlang::enquo(.fun) # Capture the function as a quosure for later evaluation
  if (rlang::is_quosure(.fun)) {
    .fun_quo <- .fun
  }
  checkmate::assert(
    .var.name = ".fun",
    combine = "or",
    if (rlang::is_function(rlang::get_expr(.fun_quo))) TRUE else "Must be a function reference.",
    checkmate::check_function(rlang::eval_tidy(.fun_quo))
  )

  args <- list(
    opts_picks = opts_picks,
    opts_values = opts_values,
    opts_static = opts_static,
    decorators = decorators,
    .decorator_name = .decorator_name,
    .dataname = .dataname,
    .fun_quo = .fun_quo,
    pre_output = pre_output,
    post_output = post_output
  )
  # Make UI args
  ui_args <- args[names(args) %in% names(formals(ui_gt_template))]
  srv_args <- args[names(args) %in% names(formals(srv_gt_template))]
  module <- module(
    label = label,
    server = .srv,
    ui = .ui,
    ui_args = ui_args,
    server_args = srv_args,
    transformators = transformators,
    datanames = datanames
  )
  attr(module, "teal_bookmarkable") <- TRUE
  module
}

ui_gt_template <- function(id,
                           opts_picks,
                           opts_values,
                           pre_output,
                           post_output,
                           decorators,
                           .decorator_name = "table",
                           partial_ui = NULL) {
  ns <- NS(id)
  checkmate::assert_function(partial_ui, null.ok = TRUE)
  partial_ui_rendered <- if (!is.null(partial_ui)) {
    partial_ui(id = ns("custom"), opts_picks = opts_picks, opts_values = opts_values)
  }
  checkmate::assert_multi_class(partial_ui_rendered, c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)

  picks_ui <- lapply(names(opts_picks), function(x_name) {
    x <- opts_picks[[x_name]]
    label <- attr(x, "label", exact = TRUE)
    if (!checkmate::test_string(label)) label <- x_name
    tags$div(
      tags$label(label, class = "text-primary"),
      teal.picks::picks_ui(ns(x_name), x)
    )
  })
  values_ui <- lapply(names(opts_values), function(x_name) {
    x <- opts_values[[x_name]]
    label <- attr(x, "label", exact = TRUE)
    if (!checkmate::test_string(label)) label <- x_name
    teal.widgets::optionalSelectInput(
      inputId = ns(x_name),
      label = label,
      choices = x$choices,
      selected = x$selected,
      multiple = FALSE,
      fixed = x$fixed %||% FALSE
    )
  })

  encodings <- if (length(picks_ui) > 0L || length(values_ui) > 0L || !is.null(partial_ui_rendered) || length(decorators) > 0L) {
    tags$div(
      tags$label("Encodings", class = "text-primary"),
      tagList(!!!picks_ui),
      tagList(!!!values_ui),
      partial_ui_rendered,
      # Allow multiple decorators for a single object (table)
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, .decorator_name)),
    )
  }

  teal.widgets::standard_layout(
    output = teal.widgets::table_with_settings_ui(ns("table")),
    encoding = encodings,
    pre_output = pre_output,
    post_output = post_output
  )
}

srv_gt_template <- function(id,
                            data,
                            opts_picks,
                            opts_values,
                            opts_static,
                            .fun_quo,
                            .dataname,
                            ...,
                            partial_srv = srv_gt_template_partial,
                            .decorator_name,
                            decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    summary_args <- if (length(opts_picks) > 0L) {
      selectors <- teal.picks::picks_srv(picks = opts_picks, data = data)
      merged <- teal.picks::merge_srv("merge", data = data, selectors = selectors, output_name = "ANL")

      reactive({
        datanames <- vapply(names(selectors), function(x) selectors[[x]]()$datasets$selected, character(1L))
        validate(
          need(length(datanames) > 0L, "No table selected in the module. Please check inputs")
        )
        rlang::list2(
          data = as.name("ANL"),
          !!!rlang::set_names(
            sapply(names(selectors), function(x_name) merged$variables()[[x_name]], simplify = FALSE),
            names(selectors)
          ),
          !!!rlang::set_names(
            sapply(names(opts_values), function(x_name) input[[x_name]], simplify = FALSE),
            names(opts_values)
          ),
          !!!opts_static
        )
      })
    } else {
      merged <- list(data = data)
      reactive(
        rlang::list2(
          data = as.name(.dataname),
          !!!rlang::set_names(
            sapply(names(opts_values), function(x_name) input[[x_name]], simplify = FALSE),
            names(opts_values)
          ),
          !!!opts_static
        )
      )
    }

    output_q <- partial_srv(
      "custom",
      merged$data,
      summary_args_r = summary_args,
      .fun_quo = .fun_quo,
      .decorator_name = .decorator_name
    )

    validated_q <- reactive({
      q <- output_q()
      validate_qenv(q)
      q
    })

    print_output_decorated <- teal::srv_transform_teal_data(
      id = "decorator",
      data = validated_q,
      transformators = select_decorators(decorators, .decorator_name),
      expr = substitute(decorator_name, list(decorator_name = as.name(.decorator_name)))
    )

    table_r <- reactive({
      req(print_output_decorated())[[.decorator_name]]
    })

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )
    print_output_decorated
  })
}

srv_gt_template_partial <- function(id,
                                    data,
                                    .fun_quo,
                                    ...,
                                    decorators,
                                    .decorator_name = "table",
                                    summary_args_r) {
  moduleServer(id, function(input, output, session) {
    summary_args_processed <- summary_args_r

    tbl_summary_call <- reactive({
      as.call(c(list(rlang::get_expr(.fun_quo)), req(summary_args_processed())))
    })

    library_name <- rlang::call_ns(as.call(list(rlang::get_expr(.fun_quo))))

    qenv <- reactive({
      obj <- teal.code::eval_code(req(data()), "library(dplyr)")
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Module's output(s)")
      if (is.null(library_name)) {
        obj
      } else {
        teal.code::eval_code(obj, sprintf("library(%s)", library_name))
      }
    })

    reactive({
      within(req(qenv()),
        expr = decorator_name <- table_call,
        table_call = req(tbl_summary_call()),
        decorator_name = as.name(.decorator_name)
      )
    })
  })
}
