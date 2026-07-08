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
  transformators = list(),
  decorators = list()
) {
  dots <- rlang::dots_list(..., .named = TRUE)

  # Normalize the parameters and extract data_extract_spec, choices_selected and
  #  named static arguments from the dots
  des_index <- vapply(dots, inherits, FUN.VALUE = logical(1L), "data_extract_spec")
  des_list_index <- vapply(
    dots,
    function(x) is.list(x) && all(vapply(x, inherits, logical(1), "data_extract_spec")),
    logical(1L)
  )
  cs_index <- vapply(dots, inherits, FUN.VALUE = logical(1L), "choices_selected")
  dots[des_index] <- lapply(dots[des_index], function(x) list(x))

  opts_des <- dots[des_index | des_list_index]
  opts_cs <- dots[cs_index]
  opts_static <- dots[!(des_index | des_list_index | cs_index)]
  if (!is.null(col_label)) {
    opts_static$label <- col_label # label conflicts with teal modules argument, so we need to relabel it
  }

  checkmate::assert_string(label)
  checkmate::assert_list(col_label, null.ok = TRUE, types = "character")
  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  teal::assert_decorators(decorators, "table")
  datanames <- if (length(opts_des) == 0L) {
    .dataname
  } else {
    c(teal.transform::get_extract_datanames(opts_des), .dataname)
  }
  checkmate::assert_character(datanames, len = 1L, any.missing = FALSE, all.missing = FALSE)
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
    opts_des = opts_des,
    opts_cs = opts_cs,
    opts_static = opts_static,
    decorators = decorators,
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

ui_gt_template <- function(id, opts_des, opts_cs, pre_output, post_output, decorators, partial_ui = NULL) {
  ns <- NS(id)
  checkmate::assert_multi_class(partial_ui, c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)

  des_ui <- lapply(names(opts_des), function(x_name) {
    x <- opts_des[[x_name]]
    teal.transform::data_extract_ui(
      ns(x_name),
      label = attr(x, "label", exact = TRUE) %||% attr(x[[1]], "label", exact = TRUE) %||% x_name,
      data_extract_spec = x
    )
  })
  cs_ui <- lapply(names(opts_cs), function(x_name) {
    x <- opts_cs[[x_name]]
    teal.widgets::optionalSelectInput(
      ns(x_name),
      attr(x, "label", exact = TRUE) %||% x_name,
      x$choices,
      x$selected,
      multiple = FALSE,
      fixed = x$fixed
    )
  })

  encodings <- if (length(des_ui) > 0L || length(cs_ui) > 0L || !is.null(partial_ui) || length(decorators > 0L)) {
    tags$div(
      tags$label("Encodings", class = "text-primary"),
      if (length(opts_des) > 0L) teal.transform::datanames_input(opts_des),
      tagList(!!!des_ui),
      tagList(!!!cs_ui),
      partial_ui,
      # Allow multiple decorators for a single object (table)
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "table")),
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
                            opts_des,
                            opts_cs,
                            opts_static,
                            .fun_quo,
                            .dataname,
                            ...,
                            partial_srv = srv_gt_template_partial,
                            decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    summary_args <- if (length(opts_des) > 0L) {
      selector_list <- teal.transform::data_extract_multiple_srv(
        data_extract = opts_des,
        datasets = data
      )

      reactive({
        sl <- req(selector_list())
        dataset <- unique(vapply(names(sl), function(x) sl[[x]]()$dataname, character(1L)))
        validate(
          need(
            length(dataset) > 0L,
            "Specify variables to use in tables."
          ),
          need(
            do.call(teal.transform::is_single_dataset, opts_des),
            "Input from multiple tables: this module doesn't accept that."
          )
        )
        rlang::list2(
          data = as.name(dataset[[1]]),
          !!!rlang::set_names(
            sapply(names(sl), function(x_name) sl[[x_name]]()$select, simplify = FALSE),
            names(sl)
          ),
          !!!rlang::set_names(
            sapply(names(opts_cs), function(x_name) input[[x_name]], simplify = FALSE),
            names(opts_cs)
          ),
          !!!opts_static
        )
      })
    } else {
      reactive(
        rlang::list2(
          data = as.name(.dataname),
          !!!rlang::set_names(
            sapply(names(opts_cs), function(x_name) input[[x_name]], simplify = FALSE),
            names(opts_cs)
          ),
          !!!opts_static
        )
      )
    }

    output_q <- partial_srv("custom", data, summary_args_r = summary_args, .fun_quo = .fun_quo)

    validated_q <- reactive({
      q <- output_q()
      validate_qenv(q)
      q
    })

    print_output_decorated <- teal::srv_transform_teal_data(
      id = "decorator",
      data = validated_q,
      transformators = select_decorators(decorators, "table"),
      expr = quote(table)
    )

    table_r <- reactive({
      req(print_output_decorated())[["table"]]
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
                                    summary_args_r) {
  moduleServer(id, function(input, output, session) {
    summary_args_processed <- summary_args_r

    tbl_summary_call <- reactive({
      as.call(c(list(rlang::get_expr(.fun_quo)), req(summary_args_processed())))
    })

    library_name <- rlang::call_ns(as.call(list(rlang::get_expr(.fun_quo))))

    qenv <- reactive({
      obj <- req(data())
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Module's output(s)")
      if (is.null(library_name)) {
        obj
      } else {
        teal.code::eval_code(obj, sprintf("library(%s)", library_name))
      }
    })

    reactive({
      within(req(qenv()),
        expr = table <- table_call,
        table_call = req(tbl_summary_call())
      )
    })
  })
}
