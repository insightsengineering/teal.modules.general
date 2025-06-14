#' `teal` module: Scatterplot and regression analysis
#'
#' Module for visualizing regression analysis, including scatterplots and
#' various regression diagnostics plots.
#' It allows users to explore the relationship between a set of regressors and a response variable,
#' visualize residuals, and identify outliers.
#'
#' @note For more examples, please see the vignette "Using regression plots" via
#' `vignette("using-regression-plots", package = "teal.modules.general")`.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param regressor (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Regressor variables from an incoming dataset with filtering and selecting.
#' @param response (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Response variables from an incoming dataset with filtering and selecting.
#' @param default_outlier_label (`character`) optional, default column selected to label outliers.
#' @param default_plot_type (`numeric`) optional, defaults to "Response vs Regressor".
#' 1. Response vs Regressor
#' 2. Residuals vs Fitted
#' 3. Normal Q-Q
#' 4. Scale-Location
#' 5. Cook's distance
#' 6. Residuals vs Leverage
#' 7. Cook's dist vs Leverage
#' @param label_segment_threshold (`numeric(1)` or `numeric(3)`)
#' Minimum distance between label and point on the plot that triggers the creation of
#' a line segment between the two.
#' This may happen when the label cannot be placed next to the point as it overlaps another
#' label or point.
#' The value is used as the `min.segment.length` parameter to the [ggrepel::geom_text_repel()] function.
#'
#' It can take the following forms:
#' - `numeric(1)`: Fixed value used for the minimum distance and the slider is not presented in the UI.
#' - `numeric(3)`: A slider is presented in the UI (under "Plot settings") to adjust the minimum distance dynamically.
#'
#'     It takes the form of `c(value, min, max)` and it is passed to the `value_min_max`
#'     argument in `teal.widgets::optionalSliderInputValMinMax`.
#'
# nolint start: line_length.
#' @param ggplot2_args `r roxygen_ggplot2_args_param("Response vs Regressor", "Residuals vs Fitted", "Scale-Location", "Cook's distance", "Residuals vs Leverage", "Cook's dist vs Leverage")`
# nolint end: line_length.
#'
#' @inherit shared_params return
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `plot` (`ggplot`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_a_regression(
#'    ..., # arguments for module
#'    decorators = list(
#'      plot = teal_transform_module(...) # applied to the `plot` output
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
#'
#' # general data example
#' data <- teal_data()
#' data <- within(data, {
#'   require(nestcolor)
#'   CO2 <- CO2
#' })
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_a_regression(
#'       label = "Regression",
#'       response = data_extract_spec(
#'         dataname = "CO2",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = "uptake",
#'           selected = "uptake",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       ),
#'       regressor = data_extract_spec(
#'         dataname = "CO2",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = variable_choices(data[["CO2"]], c("conc", "Treatment")),
#'           selected = "conc",
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
#'
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#' # CDISC data example
#' data <- teal_data()
#' data <- within(data, {
#'   require(nestcolor)
#'   ADSL <- teal.data::rADSL
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_a_regression(
#'       label = "Regression",
#'       response = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = "BMRKR1",
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       ),
#'       regressor = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = variable_choices(data[["ADSL"]], c("AGE", "SEX", "RACE")),
#'           selected = "AGE",
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
#'
#' @export
#'
tm_a_regression <- function(label = "Regression Analysis",
                            regressor,
                            response,
                            plot_height = c(600, 200, 2000),
                            plot_width = NULL,
                            alpha = c(1, 0, 1),
                            size = c(2, 1, 8),
                            ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
                            ggplot2_args = teal.widgets::ggplot2_args(),
                            pre_output = NULL,
                            post_output = NULL,
                            default_plot_type = 1,
                            default_outlier_label = "USUBJID",
                            label_segment_threshold = c(0.5, 0, 10),
                            transformators = list(),
                            decorators = list()) {
  message("Initializing tm_a_regression")

  # Normalize the parameters
  if (inherits(regressor, "data_extract_spec")) regressor <- list(regressor)
  if (inherits(response, "data_extract_spec")) response <- list(response)
  if (inherits(ggplot2_args, "ggplot2_args")) ggplot2_args <- list(default = ggplot2_args)

  # Start of assertions
  checkmate::assert_string(label)
  checkmate::assert_list(regressor, types = "data_extract_spec")

  checkmate::assert_list(response, types = "data_extract_spec")
  assert_single_selection(response)

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")

  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2],
    upper = plot_width[3],
    null.ok = TRUE,
    .var.name = "plot_width"
  )

  if (length(alpha) == 1) {
    checkmate::assert_numeric(alpha, any.missing = FALSE, finite = TRUE)
  } else {
    checkmate::assert_numeric(alpha, len = 3, any.missing = FALSE, finite = TRUE)
    checkmate::assert_numeric(alpha[1], lower = alpha[2], upper = alpha[3], .var.name = "alpha")
  }

  if (length(size) == 1) {
    checkmate::assert_numeric(size, any.missing = FALSE, finite = TRUE)
  } else {
    checkmate::assert_numeric(size, len = 3, any.missing = FALSE, finite = TRUE)
    checkmate::assert_numeric(size[1], lower = size[2], upper = size[3], .var.name = "size")
  }

  ggtheme <- match.arg(ggtheme)

  plot_choices <- c(
    "Response vs Regressor", "Residuals vs Fitted", "Normal Q-Q", "Scale-Location",
    "Cook's distance", "Residuals vs Leverage", "Cook's dist vs Leverage"
  )
  checkmate::assert_list(ggplot2_args, types = "ggplot2_args")
  checkmate::assert_subset(names(ggplot2_args), c("default", plot_choices))

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_choice(default_plot_type, seq.int(1L, length(plot_choices)))
  checkmate::assert_string(default_outlier_label)
  checkmate::assert_list(decorators, "teal_transform_module")

  if (length(label_segment_threshold) == 1) {
    checkmate::assert_numeric(label_segment_threshold, any.missing = FALSE, finite = TRUE)
  } else {
    checkmate::assert_numeric(label_segment_threshold, len = 3, any.missing = FALSE, finite = TRUE)
    checkmate::assert_numeric(
      label_segment_threshold[1],
      lower = label_segment_threshold[2],
      upper = label_segment_threshold[3],
      .var.name = "label_segment_threshold"
    )
  }
  assert_decorators(decorators, "plot")
  # End of assertions

  # Make UI args
  args <- as.list(environment())
  args[["plot_choices"]] <- plot_choices
  data_extract_list <- list(
    regressor = regressor,
    response = response
  )

  ans <- module(
    label = label,
    server = srv_a_regression,
    ui = ui_a_regression,
    ui_args = args,
    server_args = c(
      data_extract_list,
      list(
        plot_height = plot_height,
        plot_width = plot_width,
        default_outlier_label = default_outlier_label,
        ggplot2_args = ggplot2_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
  attr(ans, "teal_bookmarkable") <- FALSE
  ans
}

# UI function for the regression module
ui_a_regression <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(args$regressor, args$response)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(tags$div(
      teal.widgets::plot_with_settings_ui(id = ns("myplot")),
      tags$div(verbatimTextOutput(ns("text")))
    )),
    encoding = tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal.transform::datanames_input(args[c("response", "regressor")]),
      teal.transform::data_extract_ui(
        id = ns("response"),
        label = "Response variable",
        data_extract_spec = args$response,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("regressor"),
        label = "Regressor variables",
        data_extract_spec = args$regressor,
        is_single_dataset = is_single_dataset_value
      ),
      radioButtons(
        ns("plot_type"),
        label = "Plot type:",
        choices = args$plot_choices,
        selected = args$plot_choices[args$default_plot_type]
      ),
      checkboxInput(ns("show_outlier"), label = "Display outlier labels", value = TRUE),
      conditionalPanel(
        condition = "input['show_outlier']",
        ns = ns,
        teal.widgets::optionalSliderInput(
          ns("outlier"),
          tags$div(
            tagList(
              "Outlier definition:",
              bslib::tooltip(
                icon("fas fa-circle-info"),
                paste(
                  "Use the slider to choose the cut-off value to define outliers.",
                  "Points with a Cook's distance greater than",
                  "the value on the slider times the mean of the Cook's distance of the dataset will have labels."
                )
              )
            )
          ),
          min = 1, max = 10, value = 9, ticks = FALSE, step = .1
        ),
        teal.widgets::optionalSelectInput(
          ns("label_var"),
          multiple = FALSE,
          label = "Outlier label"
        )
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(args$decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(ns("alpha"), "Opacity:", args$alpha, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(ns("size"), "Points size:", args$size, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(
            inputId = ns("label_min_segment"),
            label = tags$div(
              tagList(
                "Label min. segment:",
                bslib::tooltip(
                  icon("circle-info"),
                  tags$span(
                    paste(
                      "Use the slider to choose the cut-off value to define minimum distance between label and point",
                      "that generates a line segment.",
                      "It's only valid when 'Display outlier labels' is checked."
                    )
                  )
                )
              )
            ),
            value_min_max = args$label_segment_threshold,
            # Extra parameters to sliderInput
            ticks = FALSE,
            step = .1,
            round = FALSE
          ),
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
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    ),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

# Server function for the regression module
srv_a_regression <- function(id,
                             data,
                             reporter,
                             filter_panel_api,
                             response,
                             regressor,
                             plot_height,
                             plot_width,
                             ggplot2_args,
                             default_outlier_label,
                             decorators) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    ns <- session$ns

    rule_rvr1 <- function(value) {
      if (isTRUE(input$plot_type == "Response vs Regressor")) {
        if (length(value) > 1L) {
          "This plot can only have one regressor."
        }
      }
    }
    rule_rvr2 <- function(other) {
      function(value) {
        if (isTRUE(input$plot_type == "Response vs Regressor")) {
          otherval <- selector_list()[[other]]()$select
          if (isTRUE(value == otherval)) {
            "Response and Regressor must be different."
          }
        }
      }
    }

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(response = response, regressor = regressor),
      datasets = data,
      select_validation_rule = list(
        regressor = shinyvalidate::compose_rules(
          shinyvalidate::sv_required("At least one regressor should be selected."),
          rule_rvr1,
          rule_rvr2("response")
        ),
        response = shinyvalidate::compose_rules(
          shinyvalidate::sv_required("At least one response should be selected."),
          rule_rvr2("regressor")
        )
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    iv_out <- shinyvalidate::InputValidator$new()
    iv_out$condition(~ isTRUE(input$show_outlier))
    iv_out$add_rule("label_var", shinyvalidate::sv_required("Please provide an `Outlier label` variable"))
    iv_out$enable()

    anl_merged_input <- teal.transform::merge_expression_srv(
      selector_list = selector_list,
      datasets = data
    )

    regression_var <- reactive({
      teal::validate_inputs(iv_r())

      list(
        response = as.vector(anl_merged_input()$columns_source$response),
        regressor = as.vector(anl_merged_input()$columns_source$regressor)
      )
    })

    qenv <- reactive(
      teal.code::eval_code(data(), 'library("ggplot2");library("dplyr")') # nolint quotes
    )

    anl_merged_q <- reactive({
      req(anl_merged_input())
      qenv() %>%
        teal.code::eval_code(as.expression(anl_merged_input()$expr))
    })

    # sets qenv object and populates it with data merge call and fit expression
    fit_r <- reactive({
      ANL <- anl_merged_q()[["ANL"]]
      teal::validate_has_data(ANL, 10)

      validate(need(is.numeric(ANL[regression_var()$response][[1]]), "Response variable should be numeric."))

      teal::validate_has_data(
        ANL[, c(regression_var()$response, regression_var()$regressor)], 10,
        complete = TRUE, allow_inf = FALSE
      )

      form <- stats::as.formula(
        paste(
          regression_var()$response,
          paste(
            regression_var()$regressor,
            collapse = " + "
          ),
          sep = " ~ "
        )
      )

      if (input$show_outlier) {
        opts <- teal.transform::variable_choices(ANL)
        selected <- if (!is.null(isolate(input$label_var)) && isolate(input$label_var) %in% as.character(opts)) {
          isolate(input$label_var)
        } else {
          if (length(opts[as.character(opts) == default_outlier_label]) == 0) {
            opts[[1]]
          } else {
            opts[as.character(opts) == default_outlier_label]
          }
        }
        teal.widgets::updateOptionalSelectInput(
          session = session,
          inputId = "label_var",
          choices = opts,
          selected = restoreInput(ns("label_var"), selected)
        )

        data <- ggplot2::fortify(stats::lm(form, data = ANL))
        cooksd <- data$.cooksd[!is.nan(data$.cooksd)]
        max_outlier <- max(ceiling(max(cooksd) / mean(cooksd)), 2)
        cur_outlier <- isolate(input$outlier)
        updateSliderInput(
          session = session,
          inputId = "outlier",
          min = 1,
          max = max_outlier,
          value = restoreInput(ns("outlier"), if (cur_outlier < max_outlier) cur_outlier else max_outlier * .9)
        )
      }

      anl_merged_q() %>%
        teal.code::eval_code(substitute(fit <- stats::lm(form, data = ANL), env = list(form = form))) %>%
        teal.code::eval_code(quote({
          for (regressor in names(fit$contrasts)) {
            alts <- paste0(levels(ANL[[regressor]]), collapse = "|")
            names(fit$coefficients) <- gsub(
              paste0("^(", regressor, ")(", alts, ")$"), paste0("\\1", ": ", "\\2"), names(fit$coefficients)
            )
          }
        })) %>%
        teal.code::eval_code(quote(summary(fit)))
    })

    label_col <- reactive({
      teal::validate_inputs(iv_out)

      substitute(
        expr = dplyr::if_else(
          data$.cooksd > outliers * mean(data$.cooksd, na.rm = TRUE),
          as.character(stats::na.omit(ANL)[[label_var]]),
          ""
        ) %>%
          dplyr::if_else(is.na(.), "cooksd == NaN", .),
        env = list(outliers = input$outlier, label_var = input$label_var)
      )
    })

    label_min_segment <- reactive({
      input$label_min_segment
    })

    outlier_label <- reactive({
      substitute(
        expr = ggrepel::geom_text_repel(
          label = label_col,
          color = "red",
          hjust = 0,
          vjust = 1,
          max.overlaps = Inf,
          min.segment.length = label_min_segment,
          segment.alpha = 0.5,
          seed = 123
        ),
        env = list(label_col = label_col(), label_min_segment = label_min_segment())
      )
    })

    output_plot_base <- reactive({
      base_fit <- fit_r()
      teal.code::eval_code(
        base_fit,
        quote({
          class(fit$residuals) <- NULL

          data <- ggplot2::fortify(fit)

          smooth <- function(x, y) {
            as.data.frame(stats::lowess(x, y, f = 2 / 3, iter = 3))
          }

          smoothy_aes <- ggplot2::aes_string(x = "x", y = "y")

          reg_form <- deparse(fit$call[[2]])
        })
      )
    })

    output_plot_0 <- reactive({
      fit <- fit_r()[["fit"]]
      ANL <- anl_merged_q()[["ANL"]]

      stopifnot(ncol(fit$model) == 2)

      if (!is.factor(ANL[[regression_var()$regressor]])) {
        shinyjs::show("size")
        shinyjs::show("alpha")
        plot <- substitute(
          expr = ggplot2::ggplot(fit$model[, 2:1], ggplot2::aes_string(regressor, response)) +
            ggplot2::geom_point(size = size, alpha = alpha) +
            ggplot2::stat_smooth(method = "lm", formula = y ~ x, se = FALSE),
          env = list(
            regressor = regression_var()$regressor,
            response = regression_var()$response,
            size = input$size,
            alpha = input$alpha
          )
        )
        if (input$show_outlier) {
          plot <- substitute(
            expr = plot + outlier_label,
            env = list(plot = plot, outlier_label = outlier_label())
          )
        }
      } else {
        shinyjs::hide("size")
        shinyjs::hide("alpha")
        plot <- substitute(
          expr = ggplot2::ggplot(fit$model[, 2:1], ggplot2::aes_string(regressor, response)) +
            ggplot2::geom_boxplot(),
          env = list(regressor = regression_var()$regressor, response = regression_var()$response)
        )
        if (input$show_outlier) {
          plot <- substitute(expr = plot + outlier_label, env = list(plot = plot, outlier_label = outlier_label()))
        }
      }

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Response vs Regressor"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(
            labs = list(
              title = "Response vs Regressor",
              x = varname_w_label(regression_var()$regressor, ANL),
              y = varname_w_label(regression_var()$response, ANL)
            ),
            theme = list()
          )
        ),
        ggtheme = input$ggtheme
      )

      teal.code::eval_code(
        fit_r(),
        substitute(
          expr = {
            class(fit$residuals) <- NULL
            data <- ggplot2::fortify(fit)
            plot <- graph
          },
          env = list(
            graph = Reduce(function(x, y) call("+", x, y), c(plot, parsed_ggplot2_args))
          )
        )
      )
    })

    output_plot_1 <- reactive({
      plot_base <- output_plot_base()
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot <- substitute(
        expr = ggplot2::ggplot(data = data, ggplot2::aes(.fitted, .resid)) +
          ggplot2::geom_point(size = size, alpha = alpha) +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
          ggplot2::geom_line(data = smoothy, mapping = smoothy_aes),
        env = list(size = input$size, alpha = input$alpha)
      )
      if (input$show_outlier) {
        plot <- substitute(expr = plot + outlier_label, env = list(plot = plot, outlier_label = outlier_label()))
      }

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Residuals vs Fitted"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(
            labs = list(
              x = quote(paste0("Fitted values\nlm(", reg_form, ")")),
              y = "Residuals",
              title = "Residuals vs Fitted"
            )
          )
        ),
        ggtheme = input$ggtheme
      )

      teal.code::eval_code(
        plot_base,
        substitute(
          expr = {
            smoothy <- smooth(data$.fitted, data$.resid)
            plot <- graph
          },
          env = list(
            graph = Reduce(function(x, y) call("+", x, y), c(plot, parsed_ggplot2_args))
          )
        )
      )
    })

    output_plot_2 <- reactive({
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot_base <- output_plot_base()
      plot <- substitute(
        expr = ggplot2::ggplot(data = data, ggplot2::aes(sample = .stdresid)) +
          ggplot2::stat_qq(size = size, alpha = alpha) +
          ggplot2::geom_abline(linetype = "dashed"),
        env = list(size = input$size, alpha = input$alpha)
      )
      if (input$show_outlier) {
        plot <- substitute(
          expr = plot +
            ggplot2::stat_qq(
              geom = ggrepel::GeomTextRepel,
              label = label_col %>%
                data.frame(label = .) %>%
                dplyr::filter(label != "cooksd == NaN") %>%
                unlist(),
              color = "red",
              hjust = 0,
              vjust = 0,
              max.overlaps = Inf,
              min.segment.length = label_min_segment,
              segment.alpha = .5,
              seed = 123
            ),
          env = list(plot = plot, label_col = label_col(), label_min_segment = label_min_segment())
        )
      }

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Normal Q-Q"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(
            labs = list(
              x = quote(paste0("Theoretical Quantiles\nlm(", reg_form, ")")),
              y = "Standardized residuals",
              title = "Normal Q-Q"
            )
          )
        ),
        ggtheme = input$ggtheme
      )

      teal.code::eval_code(
        plot_base,
        substitute(
          expr = {
            plot <- graph
          },
          env = list(
            graph = Reduce(function(x, y) call("+", x, y), c(plot, parsed_ggplot2_args))
          )
        )
      )
    })

    output_plot_3 <- reactive({
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot_base <- output_plot_base()
      plot <- substitute(
        expr = ggplot2::ggplot(data = data, ggplot2::aes(.fitted, sqrt(abs(.stdresid)))) +
          ggplot2::geom_point(size = size, alpha = alpha) +
          ggplot2::geom_line(data = smoothy, mapping = smoothy_aes),
        env = list(size = input$size, alpha = input$alpha)
      )
      if (input$show_outlier) {
        plot <- substitute(expr = plot + outlier_label, env = list(plot = plot, outlier_label = outlier_label()))
      }

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Scale-Location"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(
            labs = list(
              x = quote(paste0("Fitted values\nlm(", reg_form, ")")),
              y = quote(expression(sqrt(abs(`Standardized residuals`)))),
              title = "Scale-Location"
            )
          )
        ),
        ggtheme = input$ggtheme
      )

      teal.code::eval_code(
        plot_base,
        substitute(
          expr = {
            smoothy <- smooth(data$.fitted, sqrt(abs(data$.stdresid)))
            plot <- graph
          },
          env = list(
            graph = Reduce(function(x, y) call("+", x, y), c(plot, parsed_ggplot2_args))
          )
        )
      )
    })

    output_plot_4 <- reactive({
      shinyjs::hide("size")
      shinyjs::show("alpha")
      plot_base <- output_plot_base()
      plot <- substitute(
        expr = ggplot2::ggplot(data = data, ggplot2::aes(seq_along(.cooksd), .cooksd)) +
          ggplot2::geom_col(alpha = alpha),
        env = list(alpha = input$alpha)
      )
      if (input$show_outlier) {
        plot <- substitute(
          expr = plot +
            ggplot2::geom_hline(
              yintercept = c(
                outlier * mean(data$.cooksd, na.rm = TRUE),
                mean(data$.cooksd, na.rm = TRUE)
              ),
              color = "red",
              linetype = "dashed"
            ) +
            ggplot2::geom_text(
              ggplot2::aes(
                x = 0,
                y = mean(data$.cooksd, na.rm = TRUE),
                label = paste("mu", "=", round(mean(data$.cooksd, na.rm = TRUE), 4)),
                vjust = -1,
                hjust = 0,
                color = "red",
                angle = 90
              ),
              parse = TRUE,
              show.legend = FALSE
            ) +
            outlier_label,
          env = list(plot = plot, outlier = input$outlier, outlier_label = outlier_label())
        )
      }

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Cook's distance"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(
            labs = list(
              x = quote(paste0("Obs. number\nlm(", reg_form, ")")),
              y = "Cook's distance",
              title = "Cook's distance"
            )
          )
        ),
        ggtheme = input$ggtheme
      )

      teal.code::eval_code(
        plot_base,
        substitute(
          expr = {
            plot <- graph
          },
          env = list(
            graph = Reduce(function(x, y) call("+", x, y), c(plot, parsed_ggplot2_args))
          )
        )
      )
    })

    output_plot_5 <- reactive({
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot_base <- output_plot_base()
      plot <- substitute(
        expr = ggplot2::ggplot(data = data, ggplot2::aes(.hat, .stdresid)) +
          ggplot2::geom_vline(
            size = 1,
            colour = "black",
            linetype = "dashed",
            xintercept = 0
          ) +
          ggplot2::geom_hline(
            size = 1,
            colour = "black",
            linetype = "dashed",
            yintercept = 0
          ) +
          ggplot2::geom_point(size = size, alpha = alpha) +
          ggplot2::geom_line(data = smoothy, mapping = smoothy_aes),
        env = list(size = input$size, alpha = input$alpha)
      )
      if (input$show_outlier) {
        plot <- substitute(expr = plot + outlier_label, env = list(plot = plot, outlier_label = outlier_label()))
      }

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Residuals vs Leverage"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(
            labs = list(
              x = quote(paste0("Standardized residuals\nlm(", reg_form, ")")),
              y = "Leverage",
              title = "Residuals vs Leverage"
            )
          )
        ),
        ggtheme = input$ggtheme
      )

      teal.code::eval_code(
        plot_base,
        substitute(
          expr = {
            smoothy <- smooth(data$.hat, data$.stdresid)
            plot <- graph
          },
          env = list(
            graph = Reduce(function(x, y) call("+", x, y), c(plot, parsed_ggplot2_args))
          )
        )
      )
    })

    output_plot_6 <- reactive({
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot_base <- output_plot_base()
      plot <- substitute(
        expr = ggplot2::ggplot(data = data, ggplot2::aes(.hat, .cooksd)) +
          ggplot2::geom_vline(xintercept = 0, colour = NA) +
          ggplot2::geom_abline(
            slope = seq(0, 3, by = 0.5),
            colour = "black",
            linetype = "dashed",
            size = 1
          ) +
          ggplot2::geom_line(data = smoothy, mapping = smoothy_aes) +
          ggplot2::geom_point(size = size, alpha = alpha),
        env = list(size = input$size, alpha = input$alpha)
      )
      if (input$show_outlier) {
        plot <- substitute(expr = plot + outlier_label, env = list(plot = plot, outlier_label = outlier_label()))
      }

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Cook's dist vs Leverage"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(
            labs = list(
              x = quote(paste0("Leverage\nlm(", reg_form, ")")),
              y = "Cooks's distance",
              title = "Cook's dist vs Leverage"
            )
          )
        ),
        ggtheme = input$ggtheme
      )

      teal.code::eval_code(
        plot_base,
        substitute(
          expr = {
            smoothy <- smooth(data$.hat, data$.cooksd)
            plot <- graph
          },
          env = list(
            graph = Reduce(function(x, y) call("+", x, y), c(plot, parsed_ggplot2_args))
          )
        )
      )
    })

    output_q <- reactive({
      teal::validate_inputs(iv_r())
      switch(input$plot_type,
        "Response vs Regressor" = output_plot_0(),
        "Residuals vs Fitted" = output_plot_1(),
        "Normal Q-Q" = output_plot_2(),
        "Scale-Location" = output_plot_3(),
        "Cook's distance" = output_plot_4(),
        "Residuals vs Leverage" = output_plot_5(),
        "Cook's dist vs Leverage" = output_plot_6()
      )
    })

    decorated_output_q <- srv_decorate_teal_data(
      "decorator",
      data = output_q,
      decorators = select_decorators(decorators, "plot"),
      expr = plot
    )

    fitted <- reactive({
      req(output_q())
      decorated_output_q()[["fit"]]
    })
    plot_r <- reactive({
      req(output_q())
      decorated_output_q()[["plot"]]
    })

    # Insert the plot into a plot_with_settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    output$text <- renderText({
      req(iv_r()$is_valid())
      req(iv_out$is_valid())
      paste(utils::capture.output(summary(fitted()))[-1], collapse = "\n")
    })

    # Render R code.
    source_code_r <- reactive(teal.code::get_code(req(decorated_output_q())))

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = "R code for the regression plot",
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Linear Regression Plot",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api
        )
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(source_code_r())
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
