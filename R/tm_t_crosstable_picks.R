#' @export
tm_t_crosstable.picks <- function(label = "Cross Table",
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

  # Start of assertions
  checkmate::assert_string(label)
  checkmate::assert_class(x, "picks")

  checkmate::assert_class(y, "picks")
  if (isTRUE(attr(y$variables, "multiple"))) {
    warning("`y` accepts only a single variable selection. Forcing `variables(multiple) to FALSE`")
    attr(y$variables, "multiple") <- FALSE
  }

  checkmate::assert_flag(show_percentage)
  checkmate::assert_flag(show_total)
  checkmate::assert_flag(remove_zero_columns)
  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_class(basic_table_args, classes = "basic_table_args")

  assert_decorators(decorators, "table")
  # End of assertions

  args <- as.list(environment())
  ans <- module(
    label = label,
    server = srv_t_crosstable.picks,
    ui = ui_t_crosstable.picks,
    ui_args = args[names(args) %in% names(formals(ui_t_crosstable.picks))],
    server_args = args[names(args) %in% names(formals(srv_t_crosstable.picks))],
    transformators = transformators,
    datanames = {
      datanames <- datanames(list(x, y))
      if (length(datanames)) datanames else "all"
    }
  )

  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the cross-table module
ui_t_crosstable.picks <- function(id, x, y, show_percentage, show_total, remove_zero_columns, pre_output, post_output, decorators) {
  ns <- NS(id)

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
      teal::teal_nav_item(
        label = tags$strong("Row values"),
        teal.transform::module_input_ui(id = ns("x"), spec = x)
      ),
      teal::teal_nav_item(
        label = tags$strong("Column values"),
        teal.transform::module_input_ui(id = ns("y"), spec = y)
      ),
      shinyWidgets::pickerInput(
        ns("join_fun"),
        label = "Row to Column type of join",
        choices = join_default_options,
        selected = join_default_options[1]
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
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "table"))
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

# Server function for the cross-table module
srv_t_crosstable.picks <- function(id, data, label, x, y, remove_zero_columns, basic_table_args, decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    selectors <- teal.transform::module_input_srv(spec = list(x = x, y = y), data = data)

    validated_q <- reactive({
      validate_input(
        inputId = "x-variables-selected",
        condition = length(selectors$x()$variables$selected) > 0,
        message = "Please define column(s) for row variables."
      )
      validate_input(
        inputId = "y-variables-selected",
        condition = length(selectors$y()$variables$selected) == 1,
        message = "Please define column for column variable."
      )

      obj <- data()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Cross Table"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      teal.code::eval_code(obj, 'library("rtables");library("tern");library("dplyr")') # nolint quotes
    })

    observeEvent(
      eventExpr = {
        selectors$x()
        selectors$y()
      },
      handlerExpr = {
        if (identical(selectors$x()$datasets$selected, selectors$x()$datasets$selected)) {
          shinyjs::hide("join_fun")
        } else {
          shinyjs::show("join_fun")
        }
      }
    )

    merged <- teal.transform::merge_srv(
      "merge",
      data = validated_q,
      selectors = selectors,
      output_name = "anl",
      join_fun = input$join_fun # todo: make reactive
    )

    output_q <- reactive({
      anl <- merged$data()[["anl"]]

      # As this is a summary
      x_name <- merged$merge_vars()$x
      y_name <- merged$merge_vars()$y

      teal::validate_has_data(anl, 3)
      teal::validate_has_data(anl[, c(x_name, y_name)], 3, complete = TRUE, allow_inf = FALSE)

      is_allowed_class <- function(x) is.numeric(x) || is.factor(x) || is.character(x) || is.logical(x)
      validate(need(
        all(vapply(anl[x_name], is_allowed_class, logical(1))),
        "Selected row variable has an unsupported data type."
      ))
      validate(need(
        is_allowed_class(anl[[y_name]]),
        "Selected column variable has an unsupported data type."
      ))

      show_percentage <- input$show_percentage
      show_total <- input$show_total
      remove_zero_columns <- input$remove_zero_columns

      plot_title <- paste(
        "Cross-Table of",
        paste0(varname_w_label(x_name, anl), collapse = ", "),
        "(rows)", "vs.",
        varname_w_label(y_name, anl),
        "(columns)"
      )

      labels_vec <- vapply(x_name, varname_w_label, character(1), anl)

      obj <- merged$data()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "# Table")
      obj <- within(
        obj,
        expr = title <- plot_title,
        plot_title = plot_title
      ) %>%
        within(
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
        ) %>%
        within(anl <- tern::df_explicit_na(anl))

      obj <- if (remove_zero_columns) {
        within(
          obj,
          {
            anl[[y_name]] <- droplevels(anl[[y_name]])
            table <- rtables::build_table(lyt = table, df = anl[order(anl[[y_name]]), ])
          },
          y_name = y_name
        )
      } else {
        within(
          obj,
          table <- rtables::build_table(lyt = table, df = anl[order(anl[[y_name]]), ]),
          y_name = y_name
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
      obj <- req(decorated_output_q())
      tail(teal.code::get_outputs(obj), 1)[[1]]
    })

    teal.widgets::table_with_settings_srv(
      id = "table",
      table_r = table_r
    )

    # Render R code.
    source_code_r <- reactive(teal.code::get_code(req(decorated_output_q())))

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = "Show R Code for Cross-Table"
    )
    decorated_output_q
  })
}
