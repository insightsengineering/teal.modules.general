#' `teal` module: Missing data analysis
#'
#' This module analyzes missing data in `data.frame`s to help users explore missing observations and
#' gain insights into the completeness of their data.
#' It is useful for clinical data analysis within the context of `CDISC` standards and
#' adaptable for general data analysis purposes.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param parent_dataname (`character(1)`) Specifies the parent dataset name. Default is `ADSL` for `CDISC` data.
#' If provided and exists, enables additional analysis "by subject". For non-`CDISC` data, this parameter can be
#' ignored.
# nolint start: line_length.
#' @param ggtheme (`character`) optional, specifies the default `ggplot2` theme for plots. Defaults to `classic`.
#' @param ggplot2_args `r roxygen_ggplot2_args_param("Summary Obs", "Summary Patients", "Combinations Main", "Combinations Hist", "By Subject")`
# nolint end: line_length.
#'
#' @inherit shared_params return
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `summary_plot` (`grob` created with [ggplot2::ggplotGrob()])
#' - `combination_plot` (`grob` created with [ggplot2::ggplotGrob()])
#' - `by_subject_plot` (`ggplot`)
#' - `table` (`ElementaryTable` created with [rtables::df_to_tt()])
#'   - The decorated table is only shown in the reporter as it is presented as an interactive DataTable in the module.
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_missing_data(
#'    ..., # arguments for module
#'    decorators = list(
#'      summary_plot = teal_transform_module(...), # applied only to `summary_plot` output
#'      combination_plot = teal_transform_module(...), # applied only to `combination_plot` output
#'      by_subject_plot = teal_transform_module(...), # applied only to `by_subject_plot` output
#'      table = teal_transform_module(...) # applied only to `table` output
#'    )
#' )
#' ```
#'
#' For additional details and examples of decorators, refer to the vignette
#' `vignette("decorate-module-output", package = "teal.modules.general")`.
#'
#' To learn more please refer to the vignette
#' `vignette("transform-module-output", package = "teal")` or the [`teal::teal_transform_module()`] documentation.
#'
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#' # general example data
#' data <- teal_data()
#' data <- within(data, {
#'   require(nestcolor)
#'
#'   add_nas <- function(x) {
#'     x[sample(seq_along(x), floor(length(x) * runif(1, .05, .17)))] <- NA
#'     x
#'   }
#'
#'   iris <- iris
#'   mtcars <- mtcars
#'
#'   iris[] <- lapply(iris, add_nas)
#'   mtcars[] <- lapply(mtcars, add_nas)
#'   mtcars[["cyl"]] <- as.factor(mtcars[["cyl"]])
#'   mtcars[["gear"]] <- as.factor(mtcars[["gear"]])
#' })
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_missing_data(parent_dataname = "mtcars")
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
#' # CDISC example data
#' data <- teal_data()
#' data <- within(data, {
#'   require(nestcolor)
#'   ADSL <- teal.data::rADSL
#'   ADRS <- rADRS
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_missing_data()
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
#'
tm_missing_data <- function(label = "Missing data",
                            plot_height = c(600, 400, 5000),
                            plot_width = NULL,
                            datanames = "all",
                            parent_dataname = "ADSL",
                            ggtheme = c("classic", "gray", "bw", "linedraw", "light", "dark", "minimal", "void"),
                            ggplot2_args = list(
                              "Combinations Hist" = teal.widgets::ggplot2_args(labs = list(caption = NULL)),
                              "Combinations Main" = teal.widgets::ggplot2_args(labs = list(title = NULL))
                            ),
                            pre_output = NULL,
                            post_output = NULL,
                            transformators = list(),
                            decorators = list()) {
  message("Initializing tm_missing_data")

  # Normalize the parameters
  if (inherits(ggplot2_args, "ggplot2_args")) ggplot2_args <- list(default = ggplot2_args)

  # Start of assertions
  checkmate::assert_string(label)

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  checkmate::assert_character(datanames, min.len = 0, min.chars = 1, null.ok = TRUE)
  checkmate::assert_character(parent_dataname, min.len = 0, max.len = 1)
  ggtheme <- match.arg(ggtheme)

  plot_choices <- c("Summary Obs", "Summary Patients", "Combinations Main", "Combinations Hist", "By Subject")
  checkmate::assert_list(ggplot2_args, types = "ggplot2_args")
  checkmate::assert_subset(names(ggplot2_args), c("default", plot_choices))

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)

  available_decorators <- c("summary_plot", "combination_plot", "by_subject_plot", "table")
  assert_decorators(decorators, names = available_decorators)
  # End of assertions

  datanames_module <- if (identical(datanames, "all") || is.null(datanames)) {
    datanames
  } else {
    union(datanames, parent_dataname)
  }

  ans <- module(
    label,
    server = srv_page_missing_data,
    datanames = datanames_module,
    server_args = list(
      datanames = if (is.null(datanames)) "all" else datanames,
      parent_dataname = parent_dataname,
      plot_height = plot_height,
      plot_width = plot_width,
      ggplot2_args = ggplot2_args,
      ggtheme = ggtheme,
      decorators = decorators
    ),
    ui = ui_page_missing_data,
    transformators = transformators,
    ui_args = list(pre_output = pre_output, post_output = post_output)
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the missing data module (all datasets)
ui_page_missing_data <- function(id, pre_output = NULL, post_output = NULL) {
  ns <- NS(id)
  tagList(
    include_css_files("custom"),
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        uiOutput(ns("dataset_tabs"))
      ),
      encoding = tags$div(
        uiOutput(ns("dataset_encodings"))
      ),
      uiOutput(ns("dataset_reporter")),
      pre_output = pre_output,
      post_output = post_output
    )
  )
}

# Server function for the missing data module (all datasets)
srv_page_missing_data <- function(id, data, reporter, filter_panel_api, datanames, parent_dataname,
                                  plot_height, plot_width, ggplot2_args, ggtheme, decorators) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    datanames <- Filter(function(name) {
      is.data.frame(isolate(data())[[name]])
    }, if (identical(datanames, "all")) names(isolate(data())) else datanames)

    if_subject_plot <- length(parent_dataname) > 0 && parent_dataname %in% datanames

    ns <- session$ns

    output$dataset_tabs <- renderUI({
      do.call(
        tabsetPanel,
        c(
          id = ns("dataname_tab"),
          lapply(
            datanames,
            function(x) {
              tabPanel(
                title = x,
                ui_missing_data(id = ns(x), by_subject_plot = if_subject_plot)
              )
            }
          )
        )
      )
    })

    output$dataset_encodings <- renderUI({
      tagList(
        lapply(
          datanames,
          function(x) {
            conditionalPanel(
              is_tab_active_js(ns("dataname_tab"), x),
              encoding_missing_data(
                id = ns(x),
                summary_per_patient = if_subject_plot,
                ggtheme = ggtheme,
                datanames = datanames,
                decorators = decorators
              )
            )
          }
        )
      )
    })

    output$dataset_reporter <- renderUI({
      lapply(datanames, function(x) {
        dataname_ns <- NS(ns(x))

        conditionalPanel(
          is_tab_active_js(ns("dataname_tab"), x),
          tagList(
            teal.widgets::verbatim_popup_ui(dataname_ns("rcode"), "Show R code")
          )
        )
      })
    })

    lapply(
      datanames,
      function(x) {
        srv_missing_data(
          id = x,
          data = data,
          reporter = if (with_reporter) reporter,
          filter_panel_api = if (with_filter) filter_panel_api,
          dataname = x,
          parent_dataname = parent_dataname,
          plot_height = plot_height,
          plot_width = plot_width,
          ggplot2_args = ggplot2_args,
          decorators = decorators
        )
      }
    )
  })
}

# UI function for the missing data module (single dataset)
ui_missing_data <- function(id, by_subject_plot = FALSE) {
  ns <- NS(id)

  tab_list <- list(
    tabPanel(
      "Summary",
      teal.widgets::plot_with_settings_ui(id = ns("summary_plot")),
      helpText(
        tags$p(paste(
          'The "Summary" graph shows the number of missing values per variable (both absolute and percentage),',
          "sorted by magnitude."
        )),
        tags$p(
          'The "summary per patients" graph is showing how many subjects have at least one missing observation',
          "for each variable. It will be most useful for panel datasets."
        )
      )
    ),
    tabPanel(
      "Combinations",
      teal.widgets::plot_with_settings_ui(id = ns("combination_plot")),
      helpText(
        tags$p(paste(
          'The "Combinations" graph is used to explore the relationship between the missing data within',
          "different columns of the dataset.",
          "It shows the different patterns of missingness in the rows of the data.",
          'For example, suppose that 70 rows of the data have exactly columns "A" and "B" missing.',
          "In this case there would be a bar of height 70 in the top graph and",
          'the column below this in the second graph would have rows "A" and "B" cells shaded red.'
        )),
        tags$p(paste(
          "Due to the large number of missing data patterns possible, only those with a large set of observations",
          'are shown in the graph and the "Combination cut-off" slider can be used to adjust the number shown.'
        ))
      )
    ),
    tabPanel(
      "By Variable Levels",
      teal.widgets::get_dt_rows(ns("levels_table"), ns("levels_table_rows")),
      DT::dataTableOutput(ns("levels_table"))
    )
  )
  if (isTRUE(by_subject_plot)) {
    tab_list <- append(
      tab_list,
      list(tabPanel(
        "Grouped by Subject",
        teal.widgets::plot_with_settings_ui(id = ns("by_subject_plot")),
        helpText(
          tags$p(paste(
            "This graph shows the missingness with respect to subjects rather than individual rows of the",
            "dataset. Each row represents one dataset variable and each column a single subject. Only subjects",
            "with at least one record in this dataset are shown. For a given subject, if they have any missing",
            "values of a specific variable then the appropriate cell in the graph is marked as missing."
          ))
        )
      ))
    )
  }

  do.call(
    tabsetPanel,
    c(
      id = ns("summary_type"),
      tab_list
    )
  )
}

# UI encoding for the missing data module (all datasets)
encoding_missing_data <- function(id, summary_per_patient = FALSE, ggtheme, datanames, decorators) {
  ns <- NS(id)

  tagList(
    ### Reporter
    teal.reporter::add_card_button_ui(ns("add_reporter"), label = "Add Report Card"),
    tags$br(), tags$br(),
    ###
    tags$label("Encodings", class = "text-primary"),
    helpText(
      paste0("Dataset", `if`(length(datanames) > 1, "s", ""), ":"),
      tags$code(paste(datanames, collapse = ", "))
    ),
    uiOutput(ns("variables")),
    actionButton(
      ns("filter_na"),
      tags$span("Select only vars with missings", class = "whitespace-normal"),
      width = "100%",
      class = "mb-4"
    ),
    conditionalPanel(
      is_tab_active_js(ns("summary_type"), "Summary"),
      bslib::input_switch(
        id = ns("any_na"),
        label = tags$div(
          HTML("Add <b>anyna</b> variable"),
          bslib::tooltip(
            icon("circle-info"),
            tags$span(
              "Describes the number of observations with at least one missing value in any variable."
            )
          )
        ),
        value = FALSE
      ),
      if (summary_per_patient) {
        bslib::input_switch(
          id = ns("if_patients_plot"),
          label = tags$div(
            "Add summary per patients",
            bslib::tooltip(
              icon("circle-info"),
              tags$span(
                paste(
                  "Displays the number of missing values per observation,",
                  "where the x-axis is sorted by observation appearance in the table."
                )
              )
            )
          ),
          value = FALSE
        )
      },
      ui_decorate_teal_data(ns("dec_summary_plot"), decorators = select_decorators(decorators, "summary_plot"))
    ),
    conditionalPanel(
      is_tab_active_js(ns("summary_type"), "Combinations"),
      uiOutput(ns("cutoff")),
      ui_decorate_teal_data(ns("dec_combination_plot"), decorators = select_decorators(decorators, "combination_plot"))
    ),
    conditionalPanel(
      is_tab_active_js(ns("summary_type"), "Grouped by Subject"),
      ui_decorate_teal_data(ns("dec_by_subject_plot"), decorators = select_decorators(decorators, "by_subject_plot"))
    ),
    conditionalPanel(
      is_tab_active_js(ns("summary_type"), "By Variable Levels"),
      uiOutput(ns("group_by_var_ui")),
      uiOutput(ns("group_by_vals_ui")),
      radioButtons(
        ns("count_type"),
        label = "Display missing as",
        choices = c("counts", "proportions"),
        selected = "counts",
        inline = TRUE
      ),
      ui_decorate_teal_data(ns("dec_summary_table"), decorators = select_decorators(decorators, "table"))
    ),
    bslib::accordion(
      bslib::accordion_panel(
        title = "Plot settings",
        selectInput(
          inputId = ns("ggtheme"),
          label = "Theme (by ggplot):",
          choices = ggplot_themes,
          selected = ggtheme,
          multiple = FALSE
        )
      )
    )
  )
}

# Server function for the missing data (single dataset)
srv_missing_data <- function(id,
                             data,
                             reporter,
                             filter_panel_api,
                             dataname,
                             parent_dataname,
                             plot_height,
                             plot_width,
                             ggplot2_args,
                             decorators) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    prev_group_by_var <- reactiveVal("")
    data_r <- reactive(data()[[dataname]])
    data_keys <- reactive(unlist(teal.data::join_keys(data())[[dataname]]))

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule(
        "variables_select",
        shinyvalidate::sv_required("At least one reference variable needs to be selected.")
      )
      iv$add_rule(
        "variables_select",
        ~ if (length(setdiff((.), data_keys())) < 1) "Please also select non-key columns."
      )
      iv_summary_table <- shinyvalidate::InputValidator$new()
      iv_summary_table$condition(~ isTRUE(input$summary_type == "By Variable Levels"))
      iv_summary_table$add_rule("count_type", shinyvalidate::sv_required("Please select type of counts"))
      iv_summary_table$add_rule(
        "group_by_vals",
        shinyvalidate::sv_required("Please select both group-by variable and values")
      )
      iv_summary_table$add_rule(
        "group_by_var",
        ~ if (length(.) > 0 && length(input$variables_select) == 1 && (.) == input$variables_select) {
          "If only one reference variable is selected it must not be the grouping variable."
        }
      )
      iv_summary_table$add_rule(
        "variables_select",
        ~ if (length(input$group_by_var) > 0 && length(.) == 1 && (.) == input$group_by_var) {
          "If only one reference variable is selected it must not be the grouping variable."
        }
      )
      iv$add_validator(iv_summary_table)
      iv$enable()
      iv
    })

    data_parent_keys <- reactive({
      if (length(parent_dataname) > 0 && parent_dataname %in% names(data())) {
        keys <- teal.data::join_keys(data())[[dataname]]
        if (parent_dataname %in% names(keys)) {
          keys[[parent_dataname]]
        } else {
          keys[[dataname]]
        }
      } else {
        NULL
      }
    })

    common_code_q <- reactive({
      teal::validate_inputs(iv_r())

      group_var <- input$group_by_var
      anl <- data_r()
      qenv <- teal.code::eval_code(data(), {
        'library("dplyr");library("ggplot2");library("tidyr");library("gridExtra")' # nolint quotes
      })

      qenv <- if (!is.null(selected_vars()) && length(selected_vars()) != ncol(anl)) {
        teal.code::eval_code(
          qenv,
          substitute(
            expr = ANL <- anl_name[, selected_vars, drop = FALSE],
            env = list(anl_name = as.name(dataname), selected_vars = selected_vars())
          )
        )
      } else {
        teal.code::eval_code(
          qenv,
          substitute(expr = ANL <- anl_name, env = list(anl_name = as.name(dataname)))
        )
      }

      if (input$summary_type == "By Variable Levels" && !is.null(group_var) && !(group_var %in% selected_vars())) {
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = ANL[[group_var]] <- anl_name[[group_var]],
            env = list(group_var = group_var, anl_name = as.name(dataname))
          )
        )
      }

      new_col_name <- "**anyna**"

      qenv <- teal.code::eval_code(
        qenv,
        substitute(
          expr =
            create_cols_labels <- function(cols, just_label = FALSE) {
              column_labels <- column_labels_value
              column_labels[is.na(column_labels) | length(column_labels) == 0] <- ""
              if (just_label) {
                labels <- column_labels[cols]
              } else {
                labels <- ifelse(cols == new_col_name | cols == "", cols, paste0(column_labels[cols], " [", cols, "]"))
              }
              labels
            },
          env = list(
            new_col_name = new_col_name,
            column_labels_value = c(teal.data::col_labels(data_r())[selected_vars()],
              new_col_name = new_col_name
            )
          )
        )
      )
      qenv
    })

    selected_vars <- reactive({
      req(input$variables_select)
      keys <- data_keys()
      vars <- unique(c(keys, input$variables_select))
      vars
    })

    vars_summary <- reactive({
      na_count <- data_r() %>%
        sapply(function(x) mean(is.na(x)), USE.NAMES = TRUE) %>%
        sort(decreasing = TRUE)

      tibble::tibble(
        key = names(na_count),
        value = unname(na_count),
        label = cut(na_count, breaks = seq(from = 0, to = 1, by = 0.1), include.lowest = TRUE)
      )
    })

    # Keep encoding panel up-to-date
    output$variables <- renderUI({
      choices <- split(x = vars_summary()$key, f = vars_summary()$label, drop = TRUE) %>% rev()
      selected <- choices <- unname(unlist(choices))

      teal.widgets::optionalSelectInput(
        ns("variables_select"),
        label = "Select variables",
        label_help = HTML(paste0("Dataset: ", tags$code(dataname))),
        choices = teal.transform::variable_choices(data_r(), choices),
        selected = selected,
        multiple = TRUE
      )
    })

    observeEvent(input$filter_na, {
      choices <- vars_summary() %>%
        dplyr::select(!!as.name("key")) %>%
        getElement(name = 1)

      selected <- vars_summary() %>%
        dplyr::filter(!!as.name("value") > 0) %>%
        dplyr::select(!!as.name("key")) %>%
        getElement(name = 1)

      teal.widgets::updateOptionalSelectInput(
        session = session,
        inputId = "variables_select",
        choices = teal.transform::variable_choices(data_r()),
        selected = restoreInput(ns("variables_select"), selected)
      )
    })

    output$group_by_var_ui <- renderUI({
      all_choices <- teal.transform::variable_choices(data_r())
      cat_choices <- all_choices[!sapply(data_r(), function(x) is.numeric(x) || inherits(x, "POSIXct"))]
      validate(
        need(cat_choices, "Dataset does not have any non-numeric or non-datetime variables to use to group data with")
      )
      teal.widgets::optionalSelectInput(
        ns("group_by_var"),
        label = "Group by variable",
        choices = cat_choices,
        selected = `if`(
          is.null(isolate(input$group_by_var)),
          cat_choices[1],
          isolate(input$group_by_var)
        ),
        multiple = FALSE,
        label_help = paste0("Dataset: ", dataname)
      )
    })

    output$group_by_vals_ui <- renderUI({
      req(input$group_by_var)

      choices <- teal.transform::value_choices(data_r(), input$group_by_var, input$group_by_var)
      prev_choices <- isolate(input$group_by_vals)

      # determine selected value based on filtered data
      # display those previously selected values that are still available
      selected <- if (!is.null(prev_choices) && any(prev_choices %in% choices)) {
        prev_choices[match(choices[choices %in% prev_choices], prev_choices)]
      } else if (
        !is.null(prev_choices) &&
          !any(prev_choices %in% choices) &&
          isolate(prev_group_by_var()) == input$group_by_var
      ) {
        # if not any previously selected value is available and the grouping variable is the same,
        # then display NULL
        NULL
      } else {
        # if new grouping variable (i.e. not any previously selected value is available),
        # then display all choices
        choices
      }

      prev_group_by_var(input$group_by_var) # set current group_by_var
      validate(need(length(choices) < 100, "Please select group-by variable with fewer than 100 unique values"))
      teal.widgets::optionalSelectInput(
        ns("group_by_vals"),
        label = "Filter levels",
        choices = choices,
        selected = selected,
        multiple = TRUE,
        label_help = paste0("Dataset: ", dataname)
      )
    })

    combination_cutoff_q <- reactive({
      req(common_code_q())
      teal.code::eval_code(
        common_code_q(),
        quote(
          combination_cutoff <- ANL %>%
            dplyr::mutate_all(is.na) %>%
            dplyr::group_by_all() %>%
            dplyr::tally() %>%
            dplyr::ungroup()
        )
      )
    })

    output$cutoff <- renderUI({
      x <- combination_cutoff_q()[["combination_cutoff"]]$n

      # select 10-th from the top
      n <- length(x)
      idx <- max(1, n - 10)
      prev_value <- isolate(input$combination_cutoff)
      value <- if (is.null(prev_value) || prev_value > max(x) || prev_value < min(x)) {
        sort(x, partial = idx)[idx]
      } else {
        prev_value
      }

      teal.widgets::optionalSliderInputValMinMax(
        ns("combination_cutoff"),
        "Combination cut-off",
        c(value, range(x))
      )
    })

    # Prepare qenvs for output objects

    summary_plot_q <- reactive({
      req(input$summary_type == "Summary") # needed to trigger show r code update on tab change
      teal::validate_has_data(data_r(), 1)

      qenv <- common_code_q()
      if (input$any_na) {
        new_col_name <- "**anyna**"
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = ANL[[new_col_name]] <- ifelse(rowSums(is.na(ANL)) > 0, NA, FALSE),
            env = list(new_col_name = new_col_name)
          )
        )
      }

      qenv <- teal.code::eval_code(
        qenv,
        substitute(
          expr = analysis_vars <- setdiff(colnames(ANL), data_keys),
          env = list(data_keys = data_keys())
        )
      ) %>%
        teal.code::eval_code(
          substitute(
            expr = summary_plot_obs <- data_frame_call[, analysis_vars] %>%
              dplyr::summarise_all(list(function(x) sum(is.na(x)))) %>%
              tidyr::pivot_longer(dplyr::everything(), names_to = "col", values_to = "n_na") %>%
              dplyr::mutate(n_not_na = nrow(ANL) - n_na) %>%
              tidyr::pivot_longer(-col, names_to = "isna", values_to = "n") %>%
              dplyr::mutate(isna = isna == "n_na", n_pct = n / nrow(ANL) * 100),
            env = list(data_frame_call = if (!inherits(data_r(), "tbl_df")) {
              quote(tibble::as_tibble(ANL))
            } else {
              quote(ANL)
            })
          )
        ) %>%
        # x axis ordering according to number of missing values and alphabet
        teal.code::eval_code(
          quote(
            expr = x_levels <- dplyr::filter(summary_plot_obs, isna) %>%
              dplyr::arrange(n_pct, dplyr::desc(col)) %>%
              dplyr::pull(col) %>%
              create_cols_labels()
          )
        )

      # always set "**anyna**" level as the last one
      if (isolate(input$any_na)) {
        qenv <- teal.code::eval_code(
          qenv,
          quote(x_levels <- c(setdiff(x_levels, "**anyna**"), "**anyna**"))
        )
      }

      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        labs = list(x = "Variable", y = "Missing observations"),
        theme = list(legend.position = "bottom", axis.text.x = quote(ggplot2::element_text(angle = 45, hjust = 1)))
      )

      all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Summary Obs"]],
        user_default = ggplot2_args$default,
        module_plot = dev_ggplot2_args
      )

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args,
        ggtheme = input$ggtheme
      )

      qenv <- teal.code::eval_code(
        qenv,
        substitute(
          summary_plot_top <- summary_plot_obs %>%
            ggplot2::ggplot() +
            ggplot2::aes(
              x = factor(create_cols_labels(col), levels = x_levels),
              y = n_pct,
              fill = isna
            ) +
            ggplot2::geom_bar(position = "fill", stat = "identity") +
            ggplot2::scale_fill_manual(
              name = "",
              values = c("grey90", c(getOption("ggplot2.discrete.colour")[2], "#ff2951ff")[1]),
              labels = c("Present", "Missing")
            ) +
            ggplot2::scale_y_continuous(
              labels = scales::percent_format(),
              breaks = seq(0, 1, by = 0.1),
              expand = c(0, 0)
            ) +
            ggplot2::geom_text(
              ggplot2::aes(label = ifelse(isna == TRUE, sprintf("%d [%.02f%%]", n, n_pct), ""), y = 1),
              hjust = 1,
              color = "black"
            ) +
            labs +
            ggthemes +
            themes +
            ggplot2::coord_flip(),
          env = list(
            labs = parsed_ggplot2_args$labs,
            themes = parsed_ggplot2_args$theme,
            ggthemes = parsed_ggplot2_args$ggtheme
          )
        )
      )

      if (isTRUE(input$if_patients_plot)) {
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = parent_keys <- keys,
            env = list(keys = data_parent_keys())
          )
        ) %>%
          teal.code::eval_code(quote(ndistinct_subjects <- dplyr::n_distinct(ANL[, parent_keys]))) %>%
          teal.code::eval_code(
            quote(
              summary_plot_patients <- ANL[, c(parent_keys, analysis_vars)] %>%
                dplyr::group_by_at(parent_keys) %>%
                dplyr::summarise_all(anyNA) %>%
                tidyr::pivot_longer(cols = !dplyr::all_of(parent_keys), names_to = "col", values_to = "anyna") %>%
                dplyr::group_by_at(c("col")) %>%
                dplyr::summarise(count_na = sum(anyna)) %>%
                dplyr::mutate(count_not_na = ndistinct_subjects - count_na) %>%
                tidyr::pivot_longer(-c(col), names_to = "isna", values_to = "n") %>%
                dplyr::mutate(isna = isna == "count_na", n_pct = n / ndistinct_subjects * 100) %>%
                dplyr::arrange_at(c("isna", "n"), .funs = dplyr::desc)
            )
          )

        dev_ggplot2_args <- teal.widgets::ggplot2_args(
          labs = list(x = "", y = "Missing patients"),
          theme = list(
            legend.position = "bottom",
            axis.text.x = quote(ggplot2::element_text(angle = 45, hjust = 1)),
            axis.text.y = quote(ggplot2::element_blank())
          )
        )

        all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Summary Patients"]],
          user_default = ggplot2_args$default,
          module_plot = dev_ggplot2_args
        )

        parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
          all_ggplot2_args,
          ggtheme = input$ggtheme
        )

        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            summary_plot_bottom <- summary_plot_patients %>%
              ggplot2::ggplot() +
              ggplot2::aes_(
                x = ~ factor(create_cols_labels(col), levels = x_levels),
                y = ~n_pct,
                fill = ~isna
              ) +
              ggplot2::geom_bar(alpha = 1, stat = "identity", position = "fill") +
              ggplot2::scale_y_continuous(
                labels = scales::percent_format(),
                breaks = seq(0, 1, by = 0.1),
                expand = c(0, 0)
              ) +
              ggplot2::scale_fill_manual(
                name = "",
                values = c("grey90", c(getOption("ggplot2.discrete.colour")[2], "#ff2951ff")[1]),
                labels = c("Present", "Missing")
              ) +
              ggplot2::geom_text(
                ggplot2::aes(label = ifelse(isna == TRUE, sprintf("%d [%.02f%%]", n, n_pct), ""), y = 1),
                hjust = 1,
                color = "black"
              ) +
              labs +
              ggthemes +
              themes +
              ggplot2::coord_flip(),
            env = list(
              labs = parsed_ggplot2_args$labs,
              themes = parsed_ggplot2_args$theme,
              ggthemes = parsed_ggplot2_args$ggtheme
            )
          )
        )
      }

      if (isTRUE(input$if_patients_plot)) {
        within(qenv, {
          g1 <- ggplot2::ggplotGrob(summary_plot_top)
          g2 <- ggplot2::ggplotGrob(summary_plot_bottom)
          summary_plot <- gridExtra::gtable_cbind(g1, g2, size = "first")
          summary_plot$heights <- grid::unit.pmax(g1$heights, g2$heights)
        })
      } else {
        within(qenv, {
          g1 <- ggplot2::ggplotGrob(summary_plot_top)
          summary_plot <- g1
        })
      }
    })

    combination_plot_q <- reactive({
      req(input$summary_type == "Combinations", input$combination_cutoff, combination_cutoff_q())
      teal::validate_has_data(data_r(), 1)

      qenv <- teal.code::eval_code(
        combination_cutoff_q(),
        substitute(
          expr = data_combination_plot_cutoff <- combination_cutoff %>%
            dplyr::filter(n >= combination_cutoff_value) %>%
            dplyr::mutate(id = rank(-n, ties.method = "first")) %>%
            tidyr::pivot_longer(-c(n, id), names_to = "key", values_to = "value") %>%
            dplyr::arrange(n),
          env = list(combination_cutoff_value = input$combination_cutoff)
        )
      )

      # find keys in dataset not selected in the UI and remove them from dataset
      keys_not_selected <- setdiff(data_keys(), input$variables_select)
      if (length(keys_not_selected) > 0) {
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = data_combination_plot_cutoff <- data_combination_plot_cutoff %>%
              dplyr::filter(!key %in% keys_not_selected),
            env = list(keys_not_selected = keys_not_selected)
          )
        )
      }

      qenv <- teal.code::eval_code(
        qenv,
        quote(
          labels <- data_combination_plot_cutoff %>%
            dplyr::filter(key == key[[1]]) %>%
            getElement(name = 1)
        )
      )

      dev_ggplot2_args1 <- teal.widgets::ggplot2_args(
        labs = list(x = "", y = ""),
        theme = list(
          legend.position = "bottom",
          axis.text.x = quote(ggplot2::element_blank())
        )
      )

      all_ggplot2_args1 <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Combinations Hist"]],
        user_default = ggplot2_args$default,
        module_plot = dev_ggplot2_args1
      )

      parsed_ggplot2_args1 <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args1,
        ggtheme = "void"
      )

      dev_ggplot2_args2 <- teal.widgets::ggplot2_args(
        labs = list(x = "", y = ""),
        theme = list(
          legend.position = "bottom",
          axis.text.x = quote(ggplot2::element_blank()),
          axis.ticks = quote(ggplot2::element_blank()),
          panel.grid.major = quote(ggplot2::element_blank())
        )
      )

      all_ggplot2_args2 <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Combinations Main"]],
        user_default = ggplot2_args$default,
        module_plot = dev_ggplot2_args2
      )

      parsed_ggplot2_args2 <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args2,
        ggtheme = input$ggtheme
      )

      qenv <- teal.code::eval_code(
        qenv,
        substitute(
          expr = {
            combination_plot_top <- data_combination_plot_cutoff %>%
              dplyr::select(id, n) %>%
              dplyr::distinct() %>%
              ggplot2::ggplot(ggplot2::aes(x = id, y = n)) +
              ggplot2::geom_bar(stat = "identity", fill = c(getOption("ggplot2.discrete.colour")[2], "#ff2951ff")[1]) +
              ggplot2::geom_text(
                ggplot2::aes(label = n),
                position = ggplot2::position_dodge(width = 0.9),
                vjust = -0.25
              ) +
              ggplot2::ylim(c(0, max(data_combination_plot_cutoff$n) * 1.5)) +
              labs1 +
              ggthemes1 +
              themes1

            graph_number_rows <- length(unique(data_combination_plot_cutoff$id))
            graph_number_cols <- nrow(data_combination_plot_cutoff) / graph_number_rows

            combination_plot_bottom <- data_combination_plot_cutoff %>% ggplot2::ggplot() +
              ggplot2::aes(x = create_cols_labels(key), y = id - 0.5, fill = value) +
              ggplot2::geom_tile(alpha = 0.85, height = 0.95) +
              ggplot2::scale_fill_manual(
                name = "",
                values = c("grey90", c(getOption("ggplot2.discrete.colour")[2], "#ff2951ff")[1]),
                labels = c("Present", "Missing")
              ) +
              ggplot2::geom_hline(yintercept = seq_len(1 + graph_number_rows) - 1) +
              ggplot2::geom_vline(xintercept = seq_len(1 + graph_number_cols) - 0.5, linetype = "dotted") +
              ggplot2::coord_flip() +
              labs2 +
              ggthemes2 +
              themes2
          },
          env = list(
            labs1 = parsed_ggplot2_args1$labs,
            themes1 = parsed_ggplot2_args1$theme,
            ggthemes1 = parsed_ggplot2_args1$ggtheme,
            labs2 = parsed_ggplot2_args2$labs,
            themes2 = parsed_ggplot2_args2$theme,
            ggthemes2 = parsed_ggplot2_args2$ggtheme
          )
        )
      )

      within(qenv, {
        g1 <- ggplot2::ggplotGrob(combination_plot_top)
        g2 <- ggplot2::ggplotGrob(combination_plot_bottom)

        combination_plot <- gridExtra::gtable_rbind(g1, g2, size = "last")
        combination_plot$heights[7] <- grid::unit(0.2, "null") # rescale to get the bar chart smaller
      })
    })

    summary_table_q <- reactive({
      req(
        input$summary_type == "By Variable Levels", # needed to trigger show r code update on tab change
        common_code_q()
      )
      teal::validate_has_data(data_r(), 1)

      # extract the ANL dataset for use in further validation
      anl <- common_code_q()[["ANL"]]

      group_var <- input$group_by_var
      validate(
        need(
          is.null(group_var) ||
            length(unique(anl[[group_var]])) < 100,
          "Please select group-by variable with fewer than 100 unique values"
        )
      )

      group_vals <- input$group_by_vals
      variables_select <- input$variables_select
      vars <- unique(variables_select, group_var)
      count_type <- input$count_type

      if (!is.null(selected_vars()) && length(selected_vars()) != ncol(anl)) {
        variables <- selected_vars()
      } else {
        variables <- colnames(anl)
      }

      summ_fn <- if (input$count_type == "counts") {
        function(x) sum(is.na(x))
      } else {
        function(x) round(sum(is.na(x)) / length(x), 4)
      }

      qenv <- if (!is.null(group_var)) {
        common_code_libraries_q <- teal.code::eval_code(
          common_code_q(),
          'library("forcats");library("glue");' # nolint quotes
        )
        teal.code::eval_code(
          common_code_libraries_q,
          substitute(
            expr = {
              summary_data <- ANL %>%
                dplyr::mutate(group_var_name := forcats::fct_na_value_to_level(as.factor(group_var_name), "NA")) %>%
                dplyr::group_by_at(group_var) %>%
                dplyr::filter(group_var_name %in% group_vals)

              count_data <- dplyr::summarise(summary_data, n = dplyr::n())

              summary_data <- dplyr::summarise_all(summary_data, summ_fn) %>%
                dplyr::mutate(group_var_name := paste0(group_var, ":", group_var_name, "(N=", count_data$n, ")")) %>%
                tidyr::pivot_longer(!dplyr::all_of(group_var), names_to = "Variable", values_to = "out") %>%
                tidyr::pivot_wider(names_from = group_var, values_from = "out") %>%
                dplyr::mutate(`Variable label` = create_cols_labels(Variable, just_label = TRUE), .after = Variable)
            },
            env = list(
              group_var = group_var, group_var_name = as.name(group_var), group_vals = group_vals, summ_fn = summ_fn
            )
          )
        )
      } else {
        teal.code::eval_code(
          common_code_q(),
          substitute(
            expr = summary_data <- ANL %>%
              dplyr::summarise_all(summ_fn) %>%
              tidyr::pivot_longer(dplyr::everything(),
                names_to = "Variable",
                values_to = paste0("Missing (N=", nrow(ANL), ")")
              ) %>%
              dplyr::mutate(`Variable label` = create_cols_labels(Variable), .after = Variable),
            env = list(summ_fn = summ_fn)
          )
        )
      }

      within(qenv, table <- rtables::df_to_tt(summary_data))
    })

    by_subject_plot_q <- reactive({
      # needed to trigger show r code update on tab change
      req(input$summary_type == "Grouped by Subject", common_code_q())

      teal::validate_has_data(data_r(), 1)

      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        labs = list(x = "", y = ""),
        theme = list(legend.position = "bottom", axis.text.x = quote(ggplot2::element_blank()))
      )

      all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["By Subject"]],
        user_default = ggplot2_args$default,
        module_plot = dev_ggplot2_args
      )

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args,
        ggtheme = input$ggtheme
      )

      # Unlikely that `rlang` is not available, new hashing may be expensive
      hashing_function <- if (requireNamespace("rlang", quietly = TRUE)) {
        quote(rlang::hash)
      } else {
        function(x) paste(as.integer(x), collapse = "")
      }

      teal.code::eval_code(
        common_code_q(),
        substitute(
          expr = parent_keys <- keys,
          env = list(keys = data_parent_keys())
        )
      ) %>%
        teal.code::eval_code(
          substitute(
            expr = analysis_vars <- setdiff(colnames(ANL), data_keys),
            env = list(data_keys = data_keys())
          )
        ) %>%
        teal.code::eval_code(
          substitute(
            expr = {
              summary_plot_patients <- ANL[, c(parent_keys, analysis_vars)] %>%
                dplyr::group_by_at(parent_keys) %>%
                dplyr::mutate(id = dplyr::cur_group_id()) %>%
                dplyr::ungroup() %>%
                dplyr::group_by_at(c(parent_keys, "id")) %>%
                dplyr::summarise_all(anyNA) %>%
                dplyr::ungroup()

              # order subjects by decreasing number of missing and then by
              # missingness pattern (defined using sha1)
              order_subjects <- summary_plot_patients %>%
                dplyr::select(-"id", -dplyr::all_of(parent_keys)) %>%
                dplyr::transmute(
                  id = dplyr::row_number(),
                  number_NA = apply(., 1, sum),
                  sha = apply(., 1, hashing_function)
                ) %>%
                dplyr::arrange(dplyr::desc(number_NA), sha) %>%
                getElement(name = "id")

              # order columns by decreasing percent of missing values
              ordered_columns <- summary_plot_patients %>%
                dplyr::select(-"id", -dplyr::all_of(parent_keys)) %>%
                dplyr::summarise(
                  column = create_cols_labels(colnames(.)),
                  na_count = apply(., MARGIN = 2, FUN = sum),
                  na_percent = na_count / nrow(.) * 100
                ) %>%
                dplyr::arrange(na_percent, dplyr::desc(column))

              summary_plot_patients <- summary_plot_patients %>%
                tidyr::gather("col", "isna", -"id", -dplyr::all_of(parent_keys)) %>%
                dplyr::mutate(col = create_cols_labels(col))
            },
            env = list(hashing_function = hashing_function)
          )
        ) %>%
        teal.code::eval_code(
          substitute(
            expr = {
              by_subject_plot <- ggplot2::ggplot(summary_plot_patients, ggplot2::aes(
                x = factor(id, levels = order_subjects),
                y = factor(col, levels = ordered_columns[["column"]]),
                fill = isna
              )) +
                ggplot2::geom_raster() +
                ggplot2::annotate(
                  "text",
                  x = length(order_subjects),
                  y = seq_len(nrow(ordered_columns)),
                  hjust = 1,
                  label = sprintf("%d [%.02f%%]", ordered_columns[["na_count"]], ordered_columns[["na_percent"]])
                ) +
                ggplot2::scale_fill_manual(
                  name = "",
                  values = c("grey90", c(getOption("ggplot2.discrete.colour")[2], "#ff2951ff")[1]),
                  labels = c("Present", "Missing (at least one)")
                ) +
                labs +
                ggthemes +
                themes
            },
            env = list(
              labs = parsed_ggplot2_args$labs,
              themes = parsed_ggplot2_args$theme,
              ggthemes = parsed_ggplot2_args$ggtheme
            )
          )
        )
    })

    # Decorated outputs

    # Summary_plot_q
    decorated_summary_plot_q <- srv_decorate_teal_data(
      id = "dec_summary_plot",
      data = summary_plot_q,
      decorators = select_decorators(decorators, "summary_plot"),
      expr = {
        grid::grid.newpage()
        grid::grid.draw(summary_plot)
      }
    )

    decorated_combination_plot_q <- srv_decorate_teal_data(
      id = "dec_combination_plot",
      data = combination_plot_q,
      decorators = select_decorators(decorators, "combination_plot"),
      expr = {
        grid::grid.newpage()
        grid::grid.draw(combination_plot)
      }
    )

    decorated_summary_table_q <- srv_decorate_teal_data(
      id = "dec_summary_table",
      data = summary_table_q,
      decorators = select_decorators(decorators, "table"),
      expr = table
    )

    decorated_by_subject_plot_q <- srv_decorate_teal_data(
      id = "dec_by_subject_plot",
      data = by_subject_plot_q,
      decorators = select_decorators(decorators, "by_subject_plot"),
      expr = print(by_subject_plot)
    )

    # Plots & tables reactives

    summary_plot_r <- reactive({
      req(decorated_summary_plot_q())[["summary_plot"]]
    })

    combination_plot_r <- reactive({
      req(decorated_combination_plot_q())[["combination_plot"]]
    })

    summary_table_r <- reactive({
      req(decorated_summary_table_q())

      if (length(input$variables_select) == 0) {
        # so that zeroRecords message gets printed
        # using tibble as it supports weird column names, such as " "
        DT::datatable(
          tibble::tibble(` ` = logical(0)),
          options = list(language = list(zeroRecords = "No variable selected."), pageLength = input$levels_table_rows)
        )
      } else {
        decorated_summary_table_q()[["summary_data"]]
      }
    })

    by_subject_plot_r <- reactive({
      req(decorated_by_subject_plot_q()[["by_subject_plot"]])
    })

    # Generate output
    pws1 <- teal.widgets::plot_with_settings_srv(
      id = "summary_plot",
      plot_r = summary_plot_r,
      height = plot_height,
      width = plot_width
    )

    pws2 <- teal.widgets::plot_with_settings_srv(
      id = "combination_plot",
      plot_r = combination_plot_r,
      height = plot_height,
      width = plot_width
    )

    output$levels_table <- DT::renderDataTable(summary_table_r())

    pws3 <- teal.widgets::plot_with_settings_srv(
      id = "by_subject_plot",
      plot_r = by_subject_plot_r,
      height = plot_height,
      width = plot_width
    )

    decorated_final_q <- reactive({
      sum_type <- req(input$summary_type)
      if (sum_type == "Summary") {
        decorated_summary_plot_q()
      } else if (sum_type == "Combinations") {
        decorated_combination_plot_q()
      } else if (sum_type == "By Variable Levels") {
        decorated_summary_table_q()
      } else if (sum_type == "Grouped by Subject") {
        decorated_by_subject_plot_q()
      }
    })

    # Render R code.
    source_code_r <- reactive(teal.code::get_code(req(decorated_final_q())))

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = "Show R Code for Missing Data"
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::TealReportCard$new()
        sum_type <- input$summary_type
        title <- if (sum_type == "By Variable Levels") paste0(sum_type, " Table") else paste0(sum_type, " Plot")
        title_dataname <- paste(title, dataname, sep = " - ")
        label <- if (label == "") {
          paste("Missing Data", sum_type, dataname, sep = " - ")
        } else {
          label
        }
        card$set_name(label)
        card$append_text(title_dataname, "header2")
        if (with_filter) card$append_fs(filter_panel_api$get_filter_state())
        if (sum_type == "Summary") {
          card$append_text("Plot", "header3")
          card$append_plot(summary_plot_r(), dim = pws1$dim())
        } else if (sum_type == "Combinations") {
          card$append_text("Plot", "header3")
          card$append_plot(combination_plot_r(), dim = pws2$dim())
        } else if (sum_type == "By Variable Levels") {
          card$append_text("Table", "header3")
          if (nrow(decorated_summary_table_q()[["summary_data"]]) == 0L) {
            card$append_text("No data available for table.")
          } else {
            card$append_table(decorated_summary_table_q()[["table"]])
          }
        } else if (sum_type == "Grouped by Subject") {
          card$append_text("Plot", "header3")
          card$append_plot(by_subject_plot_r(), dim = pws3$dim())
        }
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(source_code_r())
        card
      }
      teal.reporter::add_card_button_srv("add_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
