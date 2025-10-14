#' `teal` module: Distribution analysis
#'
#' Module is designed to explore the distribution of a single variable within a given dataset.
#' It offers several tools, such as histograms, Q-Q plots, and various statistical tests to
#' visually and statistically analyze the variable's distribution.
#'
#' @inheritParams teal::module
#' @inheritParams teal.widgets::standard_layout
#' @inheritParams shared_params
#'
#' @param dist_var (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Variable(s) for which the distribution will be analyzed.
#' @param strata_var (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Categorical variable used to split the distribution analysis.
#' @param group_var (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Variable used for faceting plot into multiple panels.
#' @param freq (`logical`) optional, whether to display frequency (`TRUE`) or density (`FALSE`).
#' Defaults to density (`FALSE`).
#' @param bins (`integer(1)` or `integer(3)`) optional,  specifies the number of bins for the histogram.
#' - When the length of `bins` is one: The histogram bins will have a fixed size based on the `bins` provided.
#' - When the length of `bins` is three: The histogram bins are dynamically adjusted based on vector of `value`, `min`,
#' and `max`.
#' Defaults to `c(30L, 1L, 100L)`.
#'
#' @param ggplot2_args `r roxygen_ggplot2_args_param("Histogram", "QQplot")`
#'
#' @inherit shared_params return
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators::
#' - `histogram_plot` (`ggplot`)
#' - `qq_plot` (`ggplot`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_g_distribution(
#'    ..., # arguments for module
#'    decorators = list(
#'      histogram_plot = teal_transform_module(...), # applied only to `histogram_plot` output
#'      qq_plot = teal_transform_module(...) # applied only to `qq_plot` output
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
#' @inheritSection teal::example_module Reporting
#'
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
# nolint start: line_length_linter.
#' @examples
# nolint end: line_length_linter.
#' # general data example
#' data <- teal_data()
#' data <- within(data, {
#'   iris <- iris
#' })
#'
#' app <- init(
#'   data = data,
#'   modules = list(
#'     tm_g_distribution(
#'       dist_var = data_extract_spec(
#'         dataname = "iris",
#'         select = select_spec(variable_choices("iris"), "Petal.Length")
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
# nolint start: line_length_linter.
#' @examples
# nolint end: line_length_linter.
#' # CDISC data example
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- teal.data::rADSL
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' vars1 <- choices_selected(
#'   variable_choices(data[["ADSL"]], c("ARM", "COUNTRY", "SEX")),
#'   selected = NULL
#' )
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_distribution(
#'       dist_var = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       strata_var = data_extract_spec(
#'         dataname = "ADSL",
#'         filter = filter_spec(
#'           vars = vars1,
#'           multiple = TRUE
#'         )
#'       ),
#'       group_var = data_extract_spec(
#'         dataname = "ADSL",
#'         filter = filter_spec(
#'           vars = vars1,
#'           multiple = TRUE
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
tm_g_distribution <- function(label = "Distribution Module",
                              dist_var,
                              strata_var = NULL,
                              group_var = NULL,
                              freq = FALSE,
                              ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
                              ggplot2_args = teal.widgets::ggplot2_args(),
                              bins = c(30L, 1L, 100L),
                              plot_height = c(600, 200, 2000),
                              plot_width = NULL,
                              pre_output = NULL,
                              post_output = NULL,
                              transformators = list(),
                              decorators = list()) {
  message("Initializing tm_g_distribution")

  # Normalize the parameters
  if (inherits(dist_var, "data_extract_spec")) dist_var <- list(dist_var)
  if (inherits(strata_var, "data_extract_spec")) strata_var <- list(strata_var)
  if (inherits(group_var, "data_extract_spec")) group_var <- list(group_var)
  if (inherits(ggplot2_args, "ggplot2_args")) ggplot2_args <- list(default = ggplot2_args)

  # Start of assertions
  checkmate::assert_string(label)

  checkmate::assert_list(dist_var, "data_extract_spec")
  checkmate::assert_false(dist_var[[1L]]$select$multiple)

  checkmate::assert_list(strata_var, types = "data_extract_spec", null.ok = TRUE)
  checkmate::assert_list(group_var, types = "data_extract_spec", null.ok = TRUE)
  checkmate::assert_flag(freq)
  ggtheme <- match.arg(ggtheme)

  plot_choices <- c("Histogram", "QQplot")
  checkmate::assert_list(ggplot2_args, types = "ggplot2_args")
  checkmate::assert_subset(names(ggplot2_args), c("default", plot_choices))

  if (length(bins) == 1) {
    checkmate::assert_numeric(bins, any.missing = FALSE, lower = 1)
  } else {
    checkmate::assert_numeric(bins, len = 3, any.missing = FALSE, lower = 1)
    checkmate::assert_numeric(bins[1], lower = bins[2], upper = bins[3], .var.name = "bins")
  }

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)

  assert_decorators(decorators, names = c("histogram_plot", "qq_plot"))

  # End of assertions

  # Make UI args
  args <- as.list(environment())

  data_extract_list <- list(
    dist_var = dist_var,
    strata_var = strata_var,
    group_var = group_var
  )

  ans <- module(
    label = label,
    server = srv_distribution,
    server_args = c(
      data_extract_list,
      list(
        plot_height = plot_height,
        plot_width = plot_width,
        ggplot2_args = ggplot2_args,
        decorators = decorators
      )
    ),
    ui = ui_distribution,
    ui_args = args,
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the distribution module
ui_distribution <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  is_single_dataset_value <- teal.transform::is_single_dataset(args$dist_var, args$strata_var, args$group_var)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("Histogram", teal.widgets::plot_with_settings_ui(id = ns("hist_plot"))),
        tabPanel("QQplot", teal.widgets::plot_with_settings_ui(id = ns("qq_plot")))
      ),
      tags$h3("Statistics Table"),
      DT::dataTableOutput(ns("summary_table")),
      tags$h3("Tests"),
      conditionalPanel(
        sprintf("input['%s'].length === 0", ns("dist_tests")),
        div(
          id = ns("please_select_a_test"),
          "Please select a test"
        )
      ),
      conditionalPanel(
        sprintf("input['%s'].length > 0", ns("dist_tests")),
        DT::dataTableOutput(ns("t_stats"))
      )
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(args[c("dist_var", "strata_var")]),
      teal.transform::data_extract_ui(
        id = ns("dist_i"),
        label = "Variable",
        data_extract_spec = args$dist_var,
        is_single_dataset = is_single_dataset_value
      ),
      if (!is.null(args$group_var)) {
        tagList(
          teal.transform::data_extract_ui(
            id = ns("group_i"),
            label = "Group by",
            data_extract_spec = args$group_var,
            is_single_dataset = is_single_dataset_value
          ),
          uiOutput(ns("scales_types_ui"))
        )
      },
      if (!is.null(args$strata_var)) {
        teal.transform::data_extract_ui(
          id = ns("strata_i"),
          label = "Stratify by",
          data_extract_spec = args$strata_var,
          is_single_dataset = is_single_dataset_value
        )
      },
      bslib::accordion(
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == 'Histogram'"),
          bslib::accordion_panel(
            "Histogram",
            teal.widgets::optionalSliderInputValMinMax(ns("bins"), "Bins", args$bins, ticks = FALSE, step = 1),
            shinyWidgets::prettyRadioButtons(
              ns("main_type"),
              label = "Plot Type:",
              choices = c("Density", "Frequency"),
              selected = if (!args$freq) "Density" else "Frequency",
              bigger = FALSE,
              inline = TRUE
            ),
            checkboxInput(ns("add_dens"), label = "Overlay Density", value = TRUE),
            ui_decorate_teal_data(
              ns("d_density"),
              decorators = select_decorators(args$decorators, "histogram_plot")
            )
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == 'QQplot'"),
          bslib::accordion_panel(
            "QQ Plot",
            checkboxInput(ns("qq_line"), label = "Add diagonal line(s)", TRUE),
            ui_decorate_teal_data(
              ns("d_qq"),
              decorators = select_decorators(args$decorators, "qq_plot")
            ),
            collapsed = FALSE
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("main_type"), "'] == 'Density'"),
          bslib::accordion_panel(
            "Theoretical Distribution",
            teal.widgets::optionalSelectInput(
              ns("t_dist"),
              tags$div(
                tagList(
                  "Distribution:",
                  bslib::tooltip(
                    icon("circle-info"),
                    tags$span(
                      "Default parameters are optimized with MASS::fitdistr function."
                    )
                  )
                )
              ),
              choices = c("normal", "lognormal", "gamma", "unif"),
              selected = NULL,
              multiple = FALSE
            ),
            numericInput(ns("dist_param1"), label = "param1", value = NULL),
            numericInput(ns("dist_param2"), label = "param2", value = NULL),
            tags$span(actionButton(ns("params_reset"), "Default params")),
            collapsed = FALSE
          )
        ),
        bslib::accordion_panel(
          title = "Tests",
          teal.widgets::optionalSelectInput(
            ns("dist_tests"),
            "Tests:",
            choices = c(
              "Shapiro-Wilk",
              if (!is.null(args$strata_var)) "t-test (two-samples, not paired)",
              if (!is.null(args$strata_var)) "one-way ANOVA",
              if (!is.null(args$strata_var)) "Fligner-Killeen",
              if (!is.null(args$strata_var)) "F-test",
              "Kolmogorov-Smirnov (one-sample)",
              "Anderson-Darling (one-sample)",
              "Cramer-von Mises (one-sample)",
              if (!is.null(args$strata_var)) "Kolmogorov-Smirnov (two-samples)"
            ),
            selected = NULL
          )
        ),
        bslib::accordion_panel(
          title = "Statistics Table",
          sliderInput(ns("roundn"), "Round to n digits", min = 0, max = 10, value = 2)
        ),
        bslib::accordion_panel(
          title = "Plot settings",
          selectInput(
            inputId = ns("ggtheme"),
            label = "Theme (by ggplot):",
            choices = ggplot_themes,
            selected = args$ggtheme,
            multiple = FALSE
          )
        )
      )
    ),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

# Server function for the distribution module
srv_distribution <- function(id,
                             data,
                             dist_var,
                             strata_var,
                             group_var,
                             plot_height,
                             plot_width,
                             ggplot2_args,
                             decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    setBookmarkExclude("params_reset")

    ns <- session$ns

    rule_req <- function(value) {
      if (isTRUE(input$dist_tests %in% c(
        "Fligner-Killeen",
        "t-test (two-samples, not paired)",
        "F-test",
        "Kolmogorov-Smirnov (two-samples)",
        "one-way ANOVA"
      ))) {
        if (!shinyvalidate::input_provided(value)) {
          "Please select stratify variable."
        }
      }
    }
    rule_dupl <- function(...) {
      if (identical(input$dist_tests, "Fligner-Killeen")) {
        strata <- selector_list()$strata_i()$select
        group <- selector_list()$group_i()$select
        if (isTRUE(strata == group)) {
          "Please select different variables for strata and group."
        }
      }
    }

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(
        dist_i = dist_var,
        strata_i = strata_var,
        group_i = group_var
      ),
      data,
      select_validation_rule = list(
        dist_i = shinyvalidate::sv_required("Please select a variable")
      ),
      filter_validation_rule = list(
        strata_i = shinyvalidate::compose_rules(
          rule_req,
          rule_dupl
        ),
        group_i = rule_dupl
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list, validator_names = "dist_i")
    })

    iv_r_dist <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(
        iv, selector_list,
        validator_names = c("strata_i", "group_i")
      )
    })
    rule_dist_1 <- function(value) {
      if (!is.null(input$t_dist)) {
        switch(input$t_dist,
          "normal" = if (!shinyvalidate::input_provided(value)) "mean is required",
          "lognormal" = if (!shinyvalidate::input_provided(value)) "meanlog is required",
          "gamma" = {
            if (!shinyvalidate::input_provided(value)) "shape is required" else if (value <= 0) "shape must be positive"
          },
          "unif" = NULL
        )
      }
    }
    rule_dist_2 <- function(value) {
      if (!is.null(input$t_dist)) {
        switch(input$t_dist,
          "normal" = {
            if (!shinyvalidate::input_provided(value)) {
              "sd is required"
            } else if (value < 0) {
              "sd must be non-negative"
            }
          },
          "lognormal" = {
            if (!shinyvalidate::input_provided(value)) {
              "sdlog is required"
            } else if (value < 0) {
              "sdlog must be non-negative"
            }
          },
          "gamma" = {
            if (!shinyvalidate::input_provided(value)) {
              "rate is required"
            } else if (value <= 0) {
              "rate must be positive"
            }
          },
          "unif" = NULL
        )
      }
    }

    rule_dist <- function(value) {
      if (isTRUE(input$tabs == "QQplot") ||
        isTRUE(input$dist_tests %in% c(
          "Kolmogorov-Smirnov (one-sample)",
          "Anderson-Darling (one-sample)",
          "Cramer-von Mises (one-sample)"
        ))) {
        if (!shinyvalidate::input_provided(value)) {
          "Please select the theoretical distribution."
        }
      }
    }

    iv_dist <- shinyvalidate::InputValidator$new()
    iv_dist$add_rule("t_dist", rule_dist)
    iv_dist$add_rule("dist_param1", rule_dist_1)
    iv_dist$add_rule("dist_param2", rule_dist_2)
    iv_dist$enable()

    anl_merged_input <- teal.transform::merge_expression_srv(
      selector_list = selector_list,
      datasets = data
    )

    qenv <- reactive(
      teal.code::eval_code(data(), 'library("ggplot2");library("dplyr")') # nolint quotes
    )

    anl_merged_q <- reactive({
      req(anl_merged_input())
      qenv() %>%
        teal.code::eval_code(as.expression(anl_merged_input()$expr))
    })

    merged <- list(
      anl_input_r = anl_merged_input,
      anl_q_r = anl_merged_q
    )

    output$scales_types_ui <- renderUI({
      if ("group_i" %in% names(selector_list()) && length(selector_list()$group_i()$filters[[1]]$selected) > 0) {
        shinyWidgets::prettyRadioButtons(
          ns("scales_type"),
          label = "Scales:",
          choices = c("Fixed", "Free"),
          selected = "Fixed",
          bigger = FALSE,
          inline = TRUE
        )
      }
    })

    observeEvent(
      eventExpr = list(
        input$t_dist,
        input$params_reset,
        selector_list()$dist_i()$select
      ),
      handlerExpr = {
        params <-
          if (length(input$t_dist) != 0) {
            get_dist_params <- function(x, dist) {
              if (dist == "unif") {
                return(stats::setNames(range(x, na.rm = TRUE), c("min", "max")))
              }
              tryCatch(
                MASS::fitdistr(x, densfun = dist)$estimate,
                error = function(e) c(param1 = NA_real_, param2 = NA_real_)
              )
            }

            ANL <- merged$anl_q_r()[["ANL"]]
            round(get_dist_params(as.numeric(stats::na.omit(ANL[[merge_vars()$dist_var]])), input$t_dist), 2)
          } else {
            c("param1" = NA_real_, "param2" = NA_real_)
          }

        params_vals <- unname(params)
        map_distr_nams <- list(
          normal = c("mean", "sd"),
          lognormal = c("meanlog", "sdlog"),
          gamma = c("shape", "rate"),
          unif = c("min", "max")
        )

        if (!is.null(input$t_dist) && input$t_dist %in% names(map_distr_nams)) {
          params_names <- map_distr_nams[[input$t_dist]]
        } else {
          params_names <- names(params)
        }

        updateNumericInput(
          inputId = "dist_param1",
          label = params_names[1],
          value = restoreInput(ns("dist_param1"), params_vals[1])
        )
        updateNumericInput(
          inputId = "dist_param2",
          label = params_names[2],
          value = restoreInput(ns("dist_param1"), params_vals[2])
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(input$params_reset, {
      updateActionButton(inputId = "params_reset", label = "Reset params")
    })

    merge_vars <- reactive({
      teal::validate_inputs(iv_r())

      dist_var <- as.vector(merged$anl_input_r()$columns_source$dist_i)
      s_var <- as.vector(merged$anl_input_r()$columns_source$strata_i)
      g_var <- as.vector(merged$anl_input_r()$columns_source$group_i)

      dist_var_name <- if (length(dist_var)) as.name(dist_var) else NULL
      s_var_name <- if (length(s_var)) as.name(s_var) else NULL
      g_var_name <- if (length(g_var)) as.name(g_var) else NULL

      list(
        dist_var = dist_var,
        s_var = s_var,
        g_var = g_var,
        dist_var_name = dist_var_name,
        s_var_name = s_var_name,
        g_var_name = g_var_name
      )
    })

    # common qenv
    common_q <- reactive({
      # Create a private stack for this function only.

      obj <- merged$anl_q_r()
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Distribution Plot"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's output(s)")
        )

      ANL <- obj[["ANL"]]
      dist_var <- merge_vars()$dist_var
      s_var <- merge_vars()$s_var
      g_var <- merge_vars()$g_var

      dist_var_name <- merge_vars()$dist_var_name
      s_var_name <- merge_vars()$s_var_name
      g_var_name <- merge_vars()$g_var_name

      roundn <- input$roundn
      dist_param1 <- input$dist_param1
      dist_param2 <- input$dist_param2
      # isolated as dist_param1/dist_param2 already triggered the reactivity
      t_dist <- isolate(input$t_dist)

      qenv <- obj

      if (length(g_var) > 0) {
        validate(
          need(
            inherits(ANL[[g_var]], c("integer", "factor", "character")),
            "Group by variable must be `factor`, `character`, or `integer`"
          )
        )
        qenv <- teal.code::eval_code(qenv, 'library("forcats")') # nolint quotes
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = ANL[[g_var]] <- forcats::fct_na_value_to_level(as.factor(ANL[[g_var]]), "NA"),
            env = list(g_var = g_var)
          )
        )
      }

      if (length(s_var) > 0) {
        validate(
          need(
            inherits(ANL[[s_var]], c("integer", "factor", "character")),
            "Stratify by variable must be `factor`, `character`, or `integer`"
          )
        )

        qenv <- teal.code::eval_code(qenv, 'library("forcats")') # nolint quotes
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = ANL[[s_var]] <- forcats::fct_na_value_to_level(as.factor(ANL[[s_var]]), "NA"),
            env = list(s_var = s_var)
          )
        )
      }

      validate(need(is.numeric(ANL[[dist_var]]), "Please select a numeric variable."))
      teal::validate_has_data(ANL, 1, complete = TRUE)

      if (length(t_dist) != 0) {
        map_distr_nams <- list(
          normal = c("mean", "sd"),
          lognormal = c("meanlog", "sdlog"),
          gamma = c("shape", "rate"),
          unif = c("min", "max")
        )
        params_names_raw <- map_distr_nams[[t_dist]]

        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = {
              params <- as.list(c(dist_param1, dist_param2))
              names(params) <- params_names_raw
            },
            env = list(
              dist_param1 = dist_param1,
              dist_param2 = dist_param2,
              params_names_raw = params_names_raw
            )
          )
        )
      }

      if (length(s_var) == 0 && length(g_var) == 0) {
        teal.code::eval_code(
          qenv,
          substitute(
            expr = {
              summary_table_data <- ANL %>%
                dplyr::summarise(
                  min = round(min(dist_var_name, na.rm = TRUE), roundn),
                  median = round(stats::median(dist_var_name, na.rm = TRUE), roundn),
                  mean = round(mean(dist_var_name, na.rm = TRUE), roundn),
                  max = round(max(dist_var_name, na.rm = TRUE), roundn),
                  sd = round(stats::sd(dist_var_name, na.rm = TRUE), roundn),
                  count = dplyr::n()
                )
            },
            env = list(
              dist_var_name = as.name(dist_var),
              roundn = roundn
            )
          )
        )
      } else {
        teal.code::eval_code(
          qenv,
          substitute(
            expr = {
              strata_vars <- strata_vars_raw
              summary_table_data <- ANL %>%
                dplyr::group_by_at(dplyr::vars(dplyr::any_of(strata_vars))) %>%
                dplyr::summarise(
                  min = round(min(dist_var_name, na.rm = TRUE), roundn),
                  median = round(stats::median(dist_var_name, na.rm = TRUE), roundn),
                  mean = round(mean(dist_var_name, na.rm = TRUE), roundn),
                  max = round(max(dist_var_name, na.rm = TRUE), roundn),
                  sd = round(stats::sd(dist_var_name, na.rm = TRUE), roundn),
                  count = dplyr::n()
                )
            },
            env = list(
              dist_var_name = dist_var_name,
              strata_vars_raw = c(g_var, s_var),
              roundn = roundn
            )
          )
        )
      }
    })

    # distplot qenv ----
    dist_q <- eventReactive(
      eventExpr = {
        common_q()
        input$scales_type
        input$main_type
        input$bins
        input$add_dens
        is.null(input$ggtheme)
      },
      valueExpr = {
        dist_var <- merge_vars()$dist_var
        s_var <- merge_vars()$s_var
        g_var <- merge_vars()$g_var
        dist_var_name <- merge_vars()$dist_var_name
        s_var_name <- merge_vars()$s_var_name
        g_var_name <- merge_vars()$g_var_name
        t_dist <- input$t_dist
        dist_param1 <- input$dist_param1
        dist_param2 <- input$dist_param2

        scales_type <- input$scales_type

        ndensity <- 512
        main_type_var <- input$main_type
        bins_var <- input$bins
        add_dens_var <- input$add_dens
        ggtheme <- input$ggtheme

        teal::validate_inputs(iv_dist)

        qenv <- common_q()

        m_type <- if (main_type_var == "Density") "density" else "count"

        plot_call <- if (length(s_var) == 0 && length(g_var) == 0) {
          substitute(
            expr = ggplot2::ggplot(ANL, ggplot2::aes(dist_var_name)) +
              ggplot2::geom_histogram(
                position = "identity", ggplot2::aes(y = ggplot2::after_stat(m_type)), bins = bins_var, alpha = 0.3
              ),
            env = list(
              m_type = as.name(m_type), bins_var = bins_var, dist_var_name = as.name(dist_var)
            )
          )
        } else if (length(s_var) != 0 && length(g_var) == 0) {
          substitute(
            expr = ggplot2::ggplot(ANL, ggplot2::aes(dist_var_name, col = s_var_name)) +
              ggplot2::geom_histogram(
                position = "identity", ggplot2::aes(y = ggplot2::after_stat(m_type), fill = s_var),
                bins = bins_var, alpha = 0.3
              ),
            env = list(
              m_type = as.name(m_type),
              bins_var = bins_var,
              dist_var_name = dist_var_name,
              s_var = as.name(s_var),
              s_var_name = s_var_name
            )
          )
        } else if (length(s_var) == 0 && length(g_var) != 0) {
          req(scales_type)
          substitute(
            expr = ggplot2::ggplot(ANL[ANL[[g_var]] != "NA", ], ggplot2::aes(dist_var_name)) +
              ggplot2::geom_histogram(
                position = "identity", ggplot2::aes(y = ggplot2::after_stat(m_type)), bins = bins_var, alpha = 0.3
              ) +
              ggplot2::facet_wrap(~g_var_name, ncol = 1, scales = scales_raw),
            env = list(
              m_type = as.name(m_type),
              bins_var = bins_var,
              dist_var_name = dist_var_name,
              g_var = g_var,
              g_var_name = g_var_name,
              scales_raw = tolower(scales_type)
            )
          )
        } else {
          req(scales_type)
          substitute(
            expr = ggplot2::ggplot(ANL[ANL[[g_var]] != "NA", ], ggplot2::aes(dist_var_name, col = s_var_name)) +
              ggplot2::geom_histogram(
                position = "identity",
                ggplot2::aes(y = ggplot2::after_stat(m_type), fill = s_var), bins = bins_var, alpha = 0.3
              ) +
              ggplot2::facet_wrap(~g_var_name, ncol = 1, scales = scales_raw),
            env = list(
              m_type = as.name(m_type),
              bins_var = bins_var,
              dist_var_name = dist_var_name,
              g_var = g_var,
              s_var = as.name(s_var),
              g_var_name = g_var_name,
              s_var_name = s_var_name,
              scales_raw = tolower(scales_type)
            )
          )
        }

        if (add_dens_var) {
          plot_call <- substitute(
            expr = plot_call +
              ggplot2::stat_density(
                ggplot2::aes(y = ggplot2::after_stat(const * m_type2)),
                geom = "line",
                position = "identity",
                alpha = 0.5,
                size = 2,
                n = ndensity
              ),
            env = list(
              plot_call = plot_call,
              const = if (main_type_var == "Density") {
                1
              } else {
                diff(range(qenv[["ANL"]][[dist_var]], na.rm = TRUE)) / bins_var
              },
              m_type2 = if (main_type_var == "Density") as.name("density") else as.name("count"),
              ndensity = ndensity
            )
          )
        }

        if (length(t_dist) != 0 && main_type_var == "Density" && length(g_var) == 0 && length(s_var) == 0) {
          qenv <- teal.code::eval_code(qenv, 'library("ggpp")') # nolint quotes
          qenv <- teal.code::eval_code(
            qenv,
            substitute(
              df_params <- as.data.frame(append(params, list(name = t_dist))),
              env = list(t_dist = t_dist)
            )
          )
          datas <- quote(data.frame(x = 0.7, y = 1, tb = I(list(df_params = df_params))))
          label <- quote(tb)

          plot_call <- substitute(
            expr = plot_call + ggpp::geom_table_npc(
              data = data,
              ggplot2::aes(npcx = x, npcy = y, label = label),
              hjust = 0, vjust = 1, size = 4
            ),
            env = list(plot_call = plot_call, data = datas, label = label)
          )
        }

        if (
          length(s_var) == 0 &&
            length(g_var) == 0 &&
            main_type_var == "Density" &&
            length(t_dist) != 0 &&
            main_type_var == "Density"
        ) {
          map_dist <- stats::setNames(
            c("dnorm", "dlnorm", "dgamma", "dunif"),
            c("normal", "lognormal", "gamma", "unif")
          )
          plot_call <- substitute(
            expr = plot_call + stat_function(
              data = data.frame(x = range(ANL[[dist_var]]), color = mapped_dist),
              ggplot2::aes(x, color = color),
              fun = mapped_dist_name,
              n = ndensity,
              size = 2,
              args = params
            ) +
              ggplot2::scale_color_manual(values = stats::setNames("blue", mapped_dist), aesthetics = "color"),
            env = list(
              plot_call = plot_call,
              dist_var = dist_var,
              ndensity = ndensity,
              mapped_dist = unname(map_dist[t_dist]),
              mapped_dist_name = as.name(unname(map_dist[t_dist]))
            )
          )
        }

        all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Histogram"]],
          user_default = ggplot2_args$default
        )

        parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
          all_ggplot2_args,
          ggtheme = ggtheme
        )

        teal.reporter::teal_card(qenv) <- c(teal.reporter::teal_card(qenv), "### Histogram Plot")
        teal.code::eval_code(
          qenv,
          substitute(
            expr = histogram_plot <- plot_call,
            env = list(plot_call = Reduce(function(x, y) call("+", x, y), c(plot_call, parsed_ggplot2_args)))
          )
        )
      }
    )

    # qqplot qenv ----
    qq_q <- eventReactive(
      eventExpr = {
        common_q()
        input$scales_type
        input$qq_line
        is.null(input$ggtheme)
        input$tabs
      },
      valueExpr = {
        dist_var <- merge_vars()$dist_var
        s_var <- merge_vars()$s_var
        g_var <- merge_vars()$g_var
        dist_var_name <- merge_vars()$dist_var_name
        s_var_name <- merge_vars()$s_var_name
        g_var_name <- merge_vars()$g_var_name
        dist_param1 <- input$dist_param1
        dist_param2 <- input$dist_param2

        scales_type <- input$scales_type
        ggtheme <- input$ggtheme

        teal::validate_inputs(iv_r_dist(), iv_dist)
        t_dist <- req(input$t_dist) # Not validated when tab is not selected
        qenv <- common_q()

        plot_call <- if (length(s_var) == 0 && length(g_var) == 0) {
          substitute(
            expr = ggplot2::ggplot(ANL, ggplot2::aes_string(sample = dist_var)),
            env = list(dist_var = dist_var)
          )
        } else if (length(s_var) != 0 && length(g_var) == 0) {
          substitute(
            expr = ggplot2::ggplot(ANL, ggplot2::aes_string(sample = dist_var, color = s_var)),
            env = list(dist_var = dist_var, s_var = s_var)
          )
        } else if (length(s_var) == 0 && length(g_var) != 0) {
          substitute(
            expr = ggplot2::ggplot(ANL[ANL[[g_var]] != "NA", ], ggplot2::aes_string(sample = dist_var)) +
              ggplot2::facet_wrap(~g_var_name, ncol = 1, scales = scales_raw),
            env = list(
              dist_var = dist_var,
              g_var = g_var,
              g_var_name = g_var_name,
              scales_raw = tolower(scales_type)
            )
          )
        } else {
          substitute(
            expr = ggplot2::ggplot(ANL[ANL[[g_var]] != "NA", ], ggplot2::aes_string(sample = dist_var, color = s_var)) +
              ggplot2::facet_wrap(~g_var_name, ncol = 1, scales = scales_raw),
            env = list(
              dist_var = dist_var,
              g_var = g_var,
              s_var = s_var,
              g_var_name = g_var_name,
              scales_raw = tolower(scales_type)
            )
          )
        }

        map_dist <- stats::setNames(
          c("qnorm", "qlnorm", "qgamma", "qunif"),
          c("normal", "lognormal", "gamma", "unif")
        )

        plot_call <- substitute(
          expr = plot_call +
            ggplot2::stat_qq(distribution = mapped_dist, dparams = params),
          env = list(plot_call = plot_call, mapped_dist = as.name(unname(map_dist[t_dist])))
        )

        if (length(t_dist) != 0 && length(g_var) == 0 && length(s_var) == 0) {
          qenv <- teal.code::eval_code(qenv, 'library("ggpp")') # nolint quotes
          qenv <- teal.code::eval_code(
            qenv,
            substitute(
              df_params <- as.data.frame(append(params, list(name = t_dist))),
              env = list(t_dist = t_dist)
            )
          )
          datas <- quote(data.frame(x = 0.7, y = 1, tb = I(list(df_params = df_params))))
          label <- quote(tb)

          plot_call <- substitute(
            expr = plot_call +
              ggpp::geom_table_npc(
                data = data,
                ggplot2::aes(npcx = x, npcy = y, label = label),
                hjust = 0,
                vjust = 1,
                size = 4
              ),
            env = list(
              plot_call = plot_call,
              data = datas,
              label = label
            )
          )
        }

        if (isTRUE(input$qq_line)) {
          plot_call <- substitute(
            expr = plot_call +
              ggplot2::stat_qq_line(distribution = mapped_dist, dparams = params),
            env = list(plot_call = plot_call, mapped_dist = as.name(unname(map_dist[t_dist])))
          )
        }

        all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["QQplot"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(labs = list(x = "theoretical", y = "sample"))
        )

        parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
          all_ggplot2_args,
          ggtheme = ggtheme
        )

        teal.reporter::teal_card(qenv) <- c(teal.reporter::teal_card(qenv), "### QQ Plot")
        teal.code::eval_code(
          qenv,
          substitute(
            expr = qq_plot <- plot_call,
            env = list(plot_call = Reduce(function(x, y) call("+", x, y), c(plot_call, parsed_ggplot2_args)))
          )
        )
      }
    )

    # test qenv ----
    test_q <- eventReactive(
      ignoreNULL = FALSE,
      eventExpr = {
        common_q()
        input$dist_param1
        input$dist_param2
        input$dist_tests
      },
      valueExpr = {
        # Create a private stack for this function only.
        ANL <- common_q()[["ANL"]]

        dist_var <- merge_vars()$dist_var
        s_var <- merge_vars()$s_var
        g_var <- merge_vars()$g_var

        dist_var_name <- merge_vars()$dist_var_name
        s_var_name <- merge_vars()$s_var_name
        g_var_name <- merge_vars()$g_var_name

        dist_param1 <- input$dist_param1
        dist_param2 <- input$dist_param2
        dist_tests <- input$dist_tests
        t_dist <- input$t_dist

        req(dist_tests)

        teal::validate_inputs(iv_dist)

        if (length(s_var) > 0 || length(g_var) > 0) {
          counts <- ANL %>%
            dplyr::group_by_at(dplyr::vars(dplyr::any_of(c(s_var, g_var)))) %>%
            dplyr::summarise(n = dplyr::n())

          validate(need(all(counts$n > 5), "Please select strata*group with at least 5 observation each."))
        }


        if (dist_tests %in% c(
          "t-test (two-samples, not paired)",
          "F-test",
          "Kolmogorov-Smirnov (two-samples)"
        )) {
          if (length(g_var) == 0 && length(s_var) > 0) {
            validate(need(
              length(unique(ANL[[s_var]])) == 2,
              "Please select stratify variable with 2 levels."
            ))
          }
          if (length(g_var) > 0 && length(s_var) > 0) {
            validate(need(
              all(stats::na.omit(as.vector(
                tapply(ANL[[s_var]], list(ANL[[g_var]]), function(x) length(unique(x))) == 2
              ))),
              "Please select stratify variable with 2 levels, per each group."
            ))
          }
        }

        map_dist <- stats::setNames(
          c("pnorm", "plnorm", "pgamma", "punif"),
          c("normal", "lognormal", "gamma", "unif")
        )
        sks_args <- list(
          test = quote(stats::ks.test),
          args = bquote(append(list(.[[.(dist_var)]], .(map_dist[t_dist])), params)),
          groups = c(g_var, s_var)
        )
        ssw_args <- list(
          test = quote(stats::shapiro.test),
          args = bquote(list(.[[.(dist_var)]])),
          groups = c(g_var, s_var)
        )
        mfil_args <- list(
          test = quote(stats::fligner.test),
          args = bquote(list(.[[.(dist_var)]], .[[.(s_var)]])),
          groups = c(g_var)
        )
        sad_args <- list(
          test = quote(goftest::ad.test),
          args = bquote(append(list(.[[.(dist_var)]], .(map_dist[t_dist])), params)),
          groups = c(g_var, s_var)
        )
        scvm_args <- list(
          test = quote(goftest::cvm.test),
          args = bquote(append(list(.[[.(dist_var)]], .(map_dist[t_dist])), params)),
          groups = c(g_var, s_var)
        )
        manov_args <- list(
          test = quote(stats::aov),
          args = bquote(list(stats::formula(.(dist_var_name) ~ .(s_var_name)), .)),
          groups = c(g_var)
        )
        mt_args <- list(
          test = quote(stats::t.test),
          args = bquote(unname(split(.[[.(dist_var)]], .[[.(s_var)]], drop = TRUE))),
          groups = c(g_var)
        )
        mv_args <- list(
          test = quote(stats::var.test),
          args = bquote(unname(split(.[[.(dist_var)]], .[[.(s_var)]], drop = TRUE))),
          groups = c(g_var)
        )
        mks_args <- list(
          test = quote(stats::ks.test),
          args = bquote(unname(split(.[[.(dist_var)]], .[[.(s_var)]], drop = TRUE))),
          groups = c(g_var)
        )

        tests_base <- switch(dist_tests,
          "Kolmogorov-Smirnov (one-sample)" = sks_args,
          "Shapiro-Wilk" = ssw_args,
          "Fligner-Killeen" = mfil_args,
          "one-way ANOVA" = manov_args,
          "t-test (two-samples, not paired)" = mt_args,
          "F-test" = mv_args,
          "Kolmogorov-Smirnov (two-samples)" = mks_args,
          "Anderson-Darling (one-sample)" = sad_args,
          "Cramer-von Mises (one-sample)" = scvm_args
        )

        env <- list(
          t_test = t_dist,
          dist_var = dist_var,
          g_var = g_var,
          s_var = s_var,
          args = tests_base$args,
          groups = tests_base$groups,
          test = tests_base$test,
          dist_var_name = dist_var_name,
          g_var_name = g_var_name,
          s_var_name = s_var_name
        )

        qenv <- common_q()

        if (length(s_var) == 0 && length(g_var) == 0) {
          qenv <- teal.code::eval_code(qenv, 'library("generics")') # nolint quotes
          qenv <- teal.code::eval_code(
            qenv,
            substitute(
              expr = {
                test_table_data <- ANL %>%
                  dplyr::select(dist_var) %>%
                  with(., generics::glance(do.call(test, args))) %>%
                  dplyr::mutate_if(is.numeric, round, 3)
              },
              env = env
            )
          )
        } else {
          qenv <- teal.code::eval_code(qenv, 'library("tidyr")') # nolint quotes
          qenv <- teal.code::eval_code(
            qenv,
            substitute(
              expr = {
                test_table_data <- ANL %>%
                  dplyr::select(dist_var, s_var, g_var) %>%
                  dplyr::group_by_at(dplyr::vars(dplyr::any_of(groups))) %>%
                  dplyr::do(tests = generics::glance(do.call(test, args))) %>%
                  tidyr::unnest(tests) %>%
                  dplyr::mutate_if(is.numeric, round, 3)
              },
              env = env
            )
          )
        }
      }
    )

    # outputs ----
    output_dist_q <- reactive(c(common_q(), req(dist_q())))
    output_qq_q <- reactive(c(common_q(), req(qq_q())))

    output_summary_q <- reactive({
      # Summary table listing has to be created separately to allow for qenv join
      q_common <- common_q()
      teal.reporter::teal_card(q_common) <- c(
        teal.reporter::teal_card(q_common),
        "### Statistics table"
      )
      if (iv_r()$is_valid()) {
        within(q_common, {
          summary_table <- rtables::df_to_tt(summary_table_data)
        })
      } else {
        within(
          q_common,
          summary_table <- rtables::rtable(header = rtables::rheader(colnames(summary_table_data)))
        )
      }
    })

    output_test_q <- reactive({
      # wrapped in if since could lead into validate error - we do want to continue
      test_q_out <- try(test_q(), silent = TRUE)
      q_common <- common_q()
      teal.reporter::teal_card(q_common) <- c(
        teal.reporter::teal_card(q_common),
        "### Distribution Tests table"
      )
      if (inherits(test_q_out, c("try-error", "error"))) {
        within(
          q_common,
          test_table <- rtables::rtable(header = rtables::rheader("No data available in table"), rtables::rrow())
        )
      } else {
        within(c(q_common, test_q_out), {
          test_table <- rtables::df_to_tt(test_table_data)
        })
      }
    })

    decorated_output_dist_q <- srv_decorate_teal_data(
      "d_density",
      data = output_dist_q,
      decorators = select_decorators(decorators, "histogram_plot"),
      expr = quote(histogram_plot)
    )

    decorated_output_qq_q <- srv_decorate_teal_data(
      "d_qq",
      data = output_qq_q,
      decorators = select_decorators(decorators, "qq_plot"),
      expr = quote(qq_plot)
    )

    decorated_output_summary_q <- srv_decorate_teal_data(
      "d_summary",
      data = output_summary_q,
      decorators = select_decorators(decorators, "summary_table"),
      expr = quote(summary_table)
    )

    decorated_output_test_q <- srv_decorate_teal_data(
      "d_test",
      data = output_test_q,
      decorators = select_decorators(decorators, "test_table"),
      expr = quote(test_table)
    )

    dist_r <- reactive(req(decorated_output_dist_q())[["histogram_plot"]])
    qq_r <- reactive(req(decorated_output_qq_q())[["qq_plot"]])

    summary_r <- reactive({
      q <- req(output_summary_q())

      DT::datatable(
        q[["summary_table_data"]],
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(width = "200px", targets = "_all"))
        ),
        rownames = FALSE
      )
    })

    output$summary_table <- DT::renderDataTable(summary_r())

    tests_r <- reactive({
      q <- req(decorated_output_test_q())
      DT::datatable(q[["test_table_data"]])
    })

    pws1 <- teal.widgets::plot_with_settings_srv(
      id = "hist_plot",
      plot_r = dist_r,
      height = plot_height,
      width = plot_width,
      brushing = FALSE
    )

    pws2 <- teal.widgets::plot_with_settings_srv(
      id = "qq_plot",
      plot_r = qq_r,
      height = plot_height,
      width = plot_width,
      brushing = FALSE
    )

    decorated_output_dist_dims_q <- set_chunk_dims(pws1, decorated_output_dist_q)

    decorated_output_qq_dims_q <- set_chunk_dims(pws2, decorated_output_qq_q)

    decorated_output_q <- reactive({
      tab <- req(input$tabs) # tab is NULL upon app launch, hence will crash without this statement
      test_q_out <- decorated_output_test_q()

      out_q <- switch(tab,
        Histogram = decorated_output_dist_dims_q(),
        QQplot = decorated_output_qq_dims_q()
      )
      withCallingHandlers(
        c(out_q, output_summary_q(), test_q_out),
        warning = function(w) {
          if (grepl("Restoring original content and adding only", conditionMessage(w))) {
            invokeRestart("muffleWarning")
          }
        }
      )
    })

    output$t_stats <- DT::renderDataTable(tests_r())

    decorated_output_q
  })
}
