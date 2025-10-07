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
#' @param dist_var (`picks` or `list` of multiple `picks`)
#' Variable(s) for which the distribution will be analyzed.
#' @param strata_var (`picks` or `list` of multiple `picks`)
#' Categorical variable used to split the distribution analysis.
#' @param group_var (`picks` or `list` of multiple `picks`)
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
#'       dist_var = picks(
#'         datasets("iris"),
#'         variables(tidyselect::where(is.numeric)),
#'         values(selected = "Petal.Length")
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
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_distribution(
#'       dist_var = picks(
#'         datasets("ADSL"),
#'         variables(c("BMRKR1", "AGE")),
#'         values(multiple = FALSE)
#'       ),
#'       strata_var = picks(
#'         datasets("ADSL"),
#'         variables(c("ARM", "COUNTRY", "SEX"), selected = NULL)
#'       ),
#'       group_var = picks(
#'         datasets("ADSL"),
#'         variables(c("ARM", "COUNTRY", "SEX"), selected = NULL)
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
  UseMethod("tm_g_distribution", dist_var)
}

#' @export
tm_g_distribution.picks <- function(label = "Distribution Module",
                                    dist_var = picks(
                                      datasets(),
                                      variables(where(is.numeric)),
                                      values(selected = tidyselect::everything())
                                    ),
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

  # Start of assertions
  checkmate::assert_string(label)

  checkmate::assert_class(dist_var, "picks")
  if (isTRUE(attr(dist_var$variables, "multiple"))) {
    warning("dist_var accepts only a single variable selection. Forcing `variables(multiple) to FALSE`")
    attr(dist_var$variables, "multiple") <- FALSE
  }
  checkmate::assert_class(strata_var, "picks", null.ok = TRUE)
  checkmate::assert_class(group_var, "picks", null.ok = TRUE)

  checkmate::assert_flag(freq)
  ggtheme <- match.arg(ggtheme)

  plot_choices <- c("Histogram", "QQplot")

  if (inherits(ggplot2_args, "ggplot2_args")) ggplot2_args <- list(default = ggplot2_args)
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

  ans <- module(
    label = label,
    server = srv_g_distribution.picks,
    ui = ui_g_distribution.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_distribution.picks))],
    server_args = args[names(args) %in% names(formals(srv_g_distribution.picks))], ,
    transformators = transformators,
    datanames = {
      datanames <- datanames(list(dist_var, strata_var, group_var))
      if (length(datanames)) datanames else "all"
    }
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}


# UI function for the distribution module
ui_g_distribution.picks <- function(id,
                                    strata_var,
                                    dist_var,
                                    group_var,
                                    freq,
                                    bins,
                                    ggtheme,
                                    pre_output,
                                    post_output,
                                    decorators) {
  ns <- NS(id)

  hist_elem <- .ui_hist(
    ns("histogram_plot"),
    bins = bins,
    freq = freq,
    decorators = select_decorators(decorators, "histogram_plot")
  )
  qq_elem <- .ui_qq(ns("qq_plot"), decorators = select_decorators(decorators, "qq_plot"))
  summary_table_elem <- .ui_summary_table(ns("summary_table"), select_decorators(decorators, "Statistics Table"))
  test_table_elem <- .ui_test_table(ns("test_table"),
    is_stratified = !is.null(strata_var),
    decorators = select_decorators(decorators, "Test Table")
  )

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("Histogram", hist_elem$output),
        tabPanel("QQplot", qq_elem$output)
      ),
      bslib::card(summary_table_elem$output),
      bslib::card(test_table_elem$output)
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      teal::teal_nav_item(
        label = tags$strong("Variable"),
        teal.transform::module_input_ui(id = ns("dist_var"), spec = dist_var)
      ),
      if (!is.null(group_var)) {
        tagList(
          teal::teal_nav_item(
            label = tags$strong("Group by:"),
            teal.transform::module_input_ui(id = ns("group_var"), spec = group_var)
          ),
          uiOutput(ns("scales_types_ui"))
        )
      },
      if (!is.null(strata_var)) {
        tagList(
          teal::teal_nav_item(
            label = tags$strong("Stratify by:"),
            teal.transform::module_input_ui(id = ns("strata_var"), spec = strata_var)
          )
        )
      },
      bslib::accordion(
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == 'Histogram'"),
          bslib::accordion_panel(title = "Histogram", hist_elem$encodings, collapsed = FALSE)
        ),
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == 'QQplot'"),
          bslib::accordion_panel(title = "QQ Plot", qq_elem$encodings, collapsed = FALSE)
        ),
        bslib::accordion_panel( # todo: hide ONLY when frequency is selected for histogram
          "Theoretical Distribution",
          teal.widgets::optionalSelectInput(
            ns("t_dist"),
            tags$div(
              tagList(
                "Distribution:",
                bslib::tooltip(
                  icon("circle-info"),
                  tags$span("Default parameters are optimized with MASS::fitdistr function.")
                )
              )
            ),
            choices = c("normal", "lognormal", "gamma", "unif"),
            selected = NULL,
            multiple = FALSE
          ),
          conditionalPanel(
            condition = paste0("input['", ns("t_dist"), "'] != null && input['", ns("t_dist"), "'] != ''"),
            numericInput(ns("dist_param1"), label = "param1", value = NULL),
            numericInput(ns("dist_param2"), label = "param2", value = NULL),
            tags$span(actionButton(ns("params_reset"), "Default params"))
          ),
          collapsed = FALSE
        ),
        bslib::accordion_panel(title = "Tests", test_table_elem$encodings),
        bslib::accordion_panel(title = "Statistics Table", summary_table_elem$encodings),
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
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

# Server function for the distribution module
srv_g_distribution.picks <- function(id,
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


    selectors <- teal.transform::module_input_srv(
      spec = list(dist_var = dist_var, strata_var = strata_var, group_var = group_var),
      data = data
    )

    qenv <- reactive({
      validate_input(
        inputId = "dist_var-variables-selected",
        condition = length(selectors$dist_var()$variables$selected) == 1,
        message = "Distribution variable must be selected."
      )

      obj <- req(data())
      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card("# Distribution Plot"),
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's code")
      )
      teal.code::eval_code(obj, 'library("ggplot2");library("dplyr")')
    })

    merged <- teal.transform::merge_srv("merge", data = qenv, selectors = selectors, output_name = "anl")

    validate_merged <- reactive({
      obj <- merged$data()
      anl <- obj[["anl"]]

      validate_input(
        inputId = "dist_var-variables-selected",
        condition = is.numeric(anl[[merged$merge_vars()$dist_var]]),
        message = "Distribution variable must be numeric."
      )

      if (length(merged$merge_vars()$group_var) > 0) {
        validate_input(
          "group_var-variables-selected",
          condition = inherits(anl[[merged$merge_vars()$group_var]], c("integer", "factor", "character")),
          message = "Group by variable must be `factor`, `character`, or `integer`"
        )
        obj <- within(obj, library("forcats"))
        obj <- within(
          obj,
          expr = anl[[group_var]] <- forcats::fct_na_value_to_level(as.factor(anl[[group_var]]), "NA"),
          group_var = merged$merge_vars()$group_var
        )
      }

      if (length(merged$merge_vars()$strata_var) > 0) {
        validate_input(
          "strata_var-variables-selected",
          condition = inherits(anl[[merged$merge_vars()$strata_var]], c("integer", "factor", "character")),
          message = "Stratify by variable must be `factor`, `character`, or `integer`"
        )

        obj <- within(obj, library("forcats"))
        obj <- within(
          obj,
          expr = anl[[strata_var]] <- forcats::fct_na_value_to_level(as.factor(anl[[strata_var]]), "NA"),
          strata_var = merged$merge_vars()$strata_var
        )
      }

      teal::validate_has_data(anl, 1, complete = TRUE)

      obj
    })

    output$scales_types_ui <- renderUI({
      validate_merged()
      if (length(merged$merge_vars()$group_var) > 0) {
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
      eventExpr = {
        input$t_dist
        input$params_reset
        merged$merge_vars()$dist_var
      },
      handlerExpr = {
        params <- if (length(input$t_dist)) {
          validate_merged()
          req(merged$data())
          anl <- merged$data()[["anl"]]
          round(
            .calc_dist_params(
              x = as.numeric(stats::na.omit(anl[[merged$merge_vars()$dist_var]])),
              dist = input$t_dist
            ),
            2
          )
        } else {
          c("param1" = NA_real_, "param2" = NA_real_)
        }

        updateNumericInput(
          inputId = "dist_param1",
          label = names(params)[1],
          value = restoreInput(ns("dist_param1"), params[[1]])
        )
        updateNumericInput(
          inputId = "dist_param2",
          label = names(params)[2],
          value = restoreInput(ns("dist_param1"), params[[2]])
        )
      },
      ignoreInit = TRUE
    )

    observeEvent(input$params_reset, {
      updateActionButton(inputId = "params_reset", label = "Reset params")
    })

    validate_dist <- reactive({
      # Validate dist_param1
      if (!is.null(input$t_dist) && input$t_dist == "normal") {
        validate_input(
          "dist_param1",
          condition = !is.null(input$dist_param1) && !is.na(input$dist_param1),
          message = "mean is required"
        )
        validate_input(
          "dist_param2",
          condition = !is.null(input$dist_param2) && !is.na(input$dist_param2),
          message = "sd is required"
        )
        validate_input(
          "dist_param2",
          condition = is.null(input$dist_param2) || is.na(input$dist_param2) || input$dist_param2 >= 0,
          message = "sd must be non-negative"
        )
      }
      if (!is.null(input$t_dist) && input$t_dist == "lognormal") {
        validate_input(
          "dist_param1",
          condition = !is.null(input$dist_param1) && !is.na(input$dist_param1),
          message = "meanlog is required"
        )
        validate_input(
          "dist_param2",
          condition = !is.null(input$dist_param2) && !is.na(input$dist_param2),
          message = "sdlog is required"
        )
        validate_input(
          "dist_param2",
          condition = is.null(input$dist_param2) || is.na(input$dist_param2) || input$dist_param2 >= 0,
          message = "sdlog must be non-negative"
        )
      }
      if (!is.null(input$t_dist) && input$t_dist == "gamma") {
        validate_input(
          "dist_param1",
          condition = !is.null(input$dist_param1) && !is.na(input$dist_param1),
          message = "shape is required"
        )
        validate_input(
          "dist_param1",
          condition = is.null(input$dist_param1) || is.na(input$dist_param1) || input$dist_param1 > 0,
          message = "shape must be positive"
        )
        validate_input(
          "dist_param2",
          condition = !is.null(input$dist_param2) && !is.na(input$dist_param2),
          message = "rate is required"
        )
        validate_input(
          "dist_param2",
          condition = is.null(input$dist_param2) || is.na(input$dist_param2) || input$dist_param2 > 0,
          message = "rate must be positive"
        )
      }
    })

    # outputs ----
    hist_output <- .srv_hist(
      "histogram_plot",
      data = reactive({
        validate_merged()
        validate_dist()
        merged$data()
      }),
      merge_vars = merged$merge_vars,
      t_dist = reactive(input$t_dist),
      dist_param1 = reactive(input$dist_param1),
      dist_param2 = reactive(input$dist_param2),
      scales_type = reactive(input$scales_type),
      ggtheme = reactive(input$ggtheme),
      plot_height = plot_height,
      plot_width = plot_width,
      ggplot2_args = teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Histogram"]],
        user_default = ggplot2_args$default,
        module_plot = teal.widgets::ggplot2_args(labs = list(x = "theoretical", y = "sample"))
      ),
      decorators = select_decorators(decorators, "histogram_plot")
    )

    qq_output <- .srv_qq(
      "qq_plot",
      data = reactive({
        validate_merged()
        validate_input(
          "t_dist",
          condition = !is.null(input$t_dist),
          message = "QQ Plot requires Theoretical Distribution to be selected"
        )
        validate_dist()
        merged$data()
      }),
      merge_vars = merged$merge_vars,
      t_dist = reactive(input$t_dist),
      dist_param1 = reactive(input$dist_param1),
      dist_param2 = reactive(input$dist_param2),
      scales_type = reactive(input$scales_type),
      ggtheme = reactive(input$ggtheme),
      plot_height = plot_height,
      plot_width = plot_width,
      ggplot2_args = teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["QQplot"]],
        user_default = ggplot2_args$default,
        module_plot = teal.widgets::ggplot2_args(labs = list(x = "theoretical", y = "sample"))
      ),
      decorators = select_decorators(decorators, "qq_plot")
    )

    summary_table_output <- .srv_summary_table(
      "summary_table",
      data = reactive({
        validate_merged()
        merged$data()
      }),
      merge_vars = merged$merge_vars,
      decorators = select_decorators(decorators, "Statistics Table")
    )

    test_q <- reactive({
      validate_merged()
      obj <- merged$data()
      anl <- obj[["anl"]]
      s_var <- merged$merge_vars()$strata_var
      g_var <- merged$merge_vars()$group_var
      dist_test <- input$`test_table-dist_test`

      if (identical(dist_test, "Fligner-Killeen")) {
        validate_input(
          "strata_var-variables-selected",
          condition = !isTRUE(s_var == g_var),
          message = "Please select different variables for strata and group."
        )
      }

      if (!is.null(dist_test) && dist_test %in% c(
        "Fligner-Killeen",
        "t-test (two-samples, not paired)",
        "F-test",
        "Kolmogorov-Smirnov (two-samples)",
        "one-way ANOVA"
      )) {
        if (length(g_var) == 0 && length(s_var) > 0) {
          validate_input(
            "strata_var-variables-selected",
            condition = length(unique(anl[[s_var]])) == 2,
            message = "Please select stratify variable with 2 levels."
          )
        } else if (length(g_var) > 0 && length(s_var) > 0) {
          validate_input(
            "strata_var-variables-selected",
            condition = all(stats::na.omit(as.vector(
              tapply(anl[[s_var]], list(anl[[g_var]]), function(x) length(unique(x))) == 2
            ))),
            message = "Please select stratify variable with 2 levels, per each group."
          )
        }
      }
      validate_dist()
      obj
    })
    test_output <- .srv_test_table(
      "test_table",
      data = test_q,
      merge_vars = merged$merge_vars,
      t_dist = reactive(input$t_dist),
      decorators = select_decorators(decorators, "Test Table")
    )

    # decorated_output_q <- reactive({
    #   req(input$tabs, hist_output(), qq_output(), summary_table_output(), output_test_q())
    #   test_q_out <- output_test_q()

    #   # return everything except switch
    #   out_q <- switch(input$tabs,
    #     Histogram = hist_output(),
    #     QQplot = qq_output()
    #   )
    #   out_q
    # })

    # Render R code.
    # source_code_r <- reactive(teal.code::get_code(req(decorated_output_q())))

    # teal.widgets::verbatim_popup_srv(
    #   id = "rcode",
    #   verbatim_content = source_code_r,
    #   title = "R Code for distribution"
    # )
    NULL
  })
}


.ui_hist <- function(id, bins, freq, decorators) {
  ns <- NS(id)
  tagList(
    encodings = tagList(
      teal.widgets::optionalSliderInputValMinMax(ns("bins"), "Bins", bins, ticks = FALSE, step = 1),
      shinyWidgets::prettyRadioButtons(
        ns("statistic"),
        label = "Plot Type:",
        choices = c("Density", "Frequency"),
        selected = if (!freq) "Density" else "Frequency",
        bigger = FALSE,
        inline = TRUE
      ),
      checkboxInput(ns("add_density"), label = "Overlay Density", value = TRUE),
      ui_decorate_teal_data(ns("decorators"), decorators = decorators)
    ),
    output = teal.widgets::plot_with_settings_ui(id = ns("plot"))
  )
}

.srv_hist <- function(id,
                      data,
                      merge_vars,
                      ggtheme,
                      scales_type,
                      t_dist,
                      dist_param1,
                      dist_param2,
                      plot_height,
                      plot_width,
                      ggplot2_args,
                      decorators) {
  moduleServer(id, function(input, output, session) {
    output_q <- eventReactive(
      list(
        data(),
        input$bins,
        input$statistic,
        input$add_density,
        dist_param1(), # don't observe t_dist as dist_param1 is changed by t_dist
        dist_param2(), # don't observe t_dist as dist_param2 is changed by t_dist
        scales_type()
      ),
      {
        obj <- req(data())
        bins <- req(input$bins)
        statistic <- if (req(input$statistic) == "Density") "density" else "count"
        logger::log_debug(".srv_hist@1 Recalculating Histogram")
        add_density <- input$add_density
        d_var <- merge_vars()$dist_var
        s_var <- merge_vars()$strata_var
        g_var <- merge_vars()$group_var
        ndensity <- 512

        teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Histogram Plot")

        plot_call <- substitute(
          expr = ggplot2::ggplot(anl, mapping = ggplot2::aes(d_var_name)) +
            ggplot2::geom_histogram(
              ggplot2::aes(y = ggplot2::after_stat(stat)),
              position = "identity", bins = bins, alpha = 0.3
            ),
          env = list(stat = as.name(statistic), bins = bins, d_var_name = as.name(d_var))
        )

        if (length(s_var)) {
          plot_call[[2]]$mapping$col <- as.name(s_var)
          plot_call[[2]]$mapping$fill <- as.name(s_var)
        }

        if (length(g_var)) {
          req(scales_type())
          plot_call <- call(
            "+",
            plot_call,
            substitute(
              ggplot2::facet_wrap(~g_var_name, ncol = 1, scales = scales),
              list(g_var_name = as.name(g_var), scales = tolower(scales_type()))
            )
          )
        }

        if (add_density) {
          plot_call <- substitute(
            expr = plot_call +
              ggplot2::stat_density(
                ggplot2::aes(y = ggplot2::after_stat(const * stat)),
                geom = "line",
                position = "identity",
                alpha = 0.5,
                size = 2,
                n = ndensity
              ),
            env = list(
              plot_call = plot_call,
              const = if (statistic == "density") {
                1
              } else {
                diff(range(obj[["anl"]][[d_var]], na.rm = TRUE)) / bins
              },
              stat = as.name(statistic),
              ndensity = ndensity
            )
          )
        }

        if (length(s_var) == 0 && length(g_var) == 0 && statistic == "density" && length(t_dist()) != 0) {
          req(dist_param1(), dist_param2())
          obj <- teal.code::eval_code(obj, 'library("ggpp")') # nolint quotes
          param_list <- .dist_param_list(t_dist(), dist_param1(), dist_param2())
          map_dist <- c(normal = "dnorm", lognormal = "dlnorm", gamma = "dgamma", unif = "dunif")

          plot_call <- substitute(
            expr = plot_call +
              ggpp::geom_table_npc(
                data = data.frame(x = .7, y = 1, tb = I(list(nested_df))),
                ggplot2::aes(npcx = x, npcy = y, label = tb),
                hjust = 0, vjust = 1, size = 4
              ) +
              stat_function(
                data = data.frame(x = range(anl[[d_var]]), color = density_dist),
                ggplot2::aes(x, color = color),
                fun = density_dist_name,
                n = ndensity,
                size = 2,
                args = param_list
              ) +
              ggplot2::scale_color_manual(values = stats::setNames("blue", density_dist), aesthetics = "color"),
            env = list(
              plot_call = plot_call,
              d_var = d_var,
              density_dist = unname(map_dist[t_dist()]),
              density_dist_name = as.name(unname(map_dist[t_dist()])),
              ndensity = ndensity,
              nested_df = as.call(
                c(
                  as.name("data.frame"),
                  param_list,
                  list(distribution = t_dist())
                )
              ),
              param_list = param_list
            )
          )
        }

        parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(ggplot2_args, ggtheme = ggtheme())

        teal.code::eval_code(
          obj,
          substitute(
            expr = histogram_plot <- plot_call,
            env = list(plot_call = Reduce(function(x, y) call("+", x, y), c(plot_call, parsed_ggplot2_args)))
          )
        )
      }
    )

    decorated_output_q <- srv_decorate_teal_data(
      "decorators",
      data = output_q,
      decorators = decorators,
      expr = quote(histogram_plot)
    )

    output_r <- reactive(req(decorated_output_q())[["histogram_plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = output_r,
      height = plot_height,
      width = plot_width,
      brushing = FALSE
    )

    set_chunk_dims(pws, decorated_output_q)
  })
}

.ui_qq <- function(id, decorators) {
  ns <- NS(id)
  tagList(
    encodings = tagList(
      checkboxInput(ns("qq_line"), label = "Add diagonal line(s)", TRUE),
      ui_decorate_teal_data(ns("decorators"), decorators = decorators)
    ),
    output = teal.widgets::plot_with_settings_ui(id = ns("plot"))
  )
}

.srv_qq <- function(id,
                    data,
                    merge_vars,
                    t_dist,
                    dist_param1,
                    dist_param2,
                    scales_type,
                    ggtheme,
                    plot_height,
                    plot_width,
                    ggplot2_args,
                    decorators) {
  moduleServer(id, function(input, output, session) {
    output_q <- eventReactive(
      {
        data()
        t_dist()
        dist_param1()
        dist_param2()
        input$qq_line
        ggtheme()
      },
      {
        req(data(), merge_vars(), ggtheme(), t_dist())
        logger::log_debug(".srv_qq@1 Recalculating QQ Plot...")
        obj <- data()
        d_var <- merge_vars()$dist_var
        s_var <- merge_vars()$strata_var
        g_var <- merge_vars()$group_var

        teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## QQ Plot")

        plot_call <- substitute(
          expr = ggplot2::ggplot(dataname, mapping = ggplot2::aes(sample = d_var_name)),
          env = list(
            dataname = if (length(g_var)) {
              bquote(anl[anl[[.(g_var)]] != "NA", ])
            } else {
              quote(anl)
            },
            d_var_name = as.name(d_var)
          )
        )
        if (length(s_var)) plot_call$mapping$color <- as.name(s_var)
        if (length(g_var)) {
          plot_call <- substitute(
            plot_call + ggplot2::facet_wrap(~g_var_name, ncol = 1, scales = scales_raw),
            list(
              plot_call = plot_call,
              g_var_name = as.name(g_var),
              scales_raw = tolower(scales_type())
            )
          )
        }

        map_quantile_fun <- c(normal = "qnorm", lognormal = "qlnorm", gamma = "qgamma", unif = "qunif")

        plot_call <- substitute(
          expr = plot_call + ggplot2::stat_qq(distribution = quantile_fun, dparams = dparams),
          env = list(
            plot_call = plot_call,
            quantile_fun = as.name(unname(map_quantile_fun[t_dist()])),
            dparams = list(dist_param1(), dist_param2())
          )
        )

        if (isTRUE(input$qq_line)) {
          plot_call <- substitute(
            expr = plot_call + ggplot2::stat_qq_line(distribution = quantile_fun, dparams = dparams),
            env = list(
              plot_call = plot_call,
              quantile_fun = as.name(unname(map_quantile_fun[t_dist()])),
              dparams = list(dist_param1(), dist_param2())
            )
          )
        }

        if (length(s_var) == 0 && length(g_var) == 0) {
          req(dist_param1(), dist_param2())
          obj <- teal.code::eval_code(obj, 'library("ggpp")') # nolint quotes
          plot_call <- substitute(
            expr = plot_call +
              ggpp::geom_table_npc(
                data = data.frame(x = .7, y = 1, tb = I(list(nested_df))),
                ggplot2::aes(npcx = x, npcy = y, label = tb),
                hjust = 0, vjust = 1, size = 4
              ),
            env = list(
              plot_call = plot_call,
              nested_df = as.call(
                c(
                  as.name("data.frame"),
                  .dist_param_list(t_dist(), dist_param1(), dist_param2()),
                  list(distribution = t_dist())
                )
              )
            )
          )
        }

        parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(ggplot2_args, ggtheme = ggtheme())
        teal.code::eval_code(
          obj,
          substitute(
            expr = qq_plot <- plot_call,
            env = list(plot_call = Reduce(function(x, y) call("+", x, y), c(plot_call, parsed_ggplot2_args)))
          )
        )
      }
    )

    decorated_output_q <- srv_decorate_teal_data(
      "decorators",
      decorators = decorators,
      data = output_q,
      expr = quote(qq_plot)
    )

    output_r <- reactive(req(decorated_output_q())[["qq_plot"]])


    pws <- teal.widgets::plot_with_settings_srv(
      id = "plot",
      plot_r = output_r,
      height = plot_height,
      width = plot_width,
      brushing = FALSE
    )

    # set_chunk_dims(pws, decorated_output_q)
  })
}

.ui_summary_table <- function(id, decorators) {
  ns <- NS(id)
  tagList(
    encodings = tagList(
      sliderInput(ns("roundn"), "Round to n digits", min = 0, max = 10, value = 2),
      ui_decorate_teal_data(ns("decorators"), decorators = decorators)
    ),
    output = tags$div(
      tags$h3("Statistics Table"),
      DT::dataTableOutput(ns("summary_table"))
    )
  )
}

.srv_summary_table <- function(id, data, merge_vars, decorators) {
  moduleServer(id, function(input, output, session) {
    output_q <- reactive({
      obj <- req(data())
      roundn <- input$roundn
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Statistics table")

      obj <- if (length(merge_vars()$strata_var) == 0 && length(merge_vars()$group_var) == 0) {
        within(
          obj,
          expr = {
            summary_table_data <- anl %>%
              dplyr::summarise(
                min = round(min(d_var_name, na.rm = TRUE), roundn),
                median = round(stats::median(d_var_name, na.rm = TRUE), roundn),
                mean = round(mean(d_var_name, na.rm = TRUE), roundn),
                max = round(max(d_var_name, na.rm = TRUE), roundn),
                sd = round(stats::sd(d_var_name, na.rm = TRUE), roundn),
                count = dplyr::n()
              )
          },
          d_var_name = as.name(merge_vars()$dist_var),
          roundn = roundn
        )
      } else {
        within(
          obj,
          expr = {
            summary_table_data <- anl %>%
              dplyr::group_by_at(dplyr::vars(dplyr::any_of(strata_vars))) %>%
              dplyr::summarise(
                min = round(min(d_var_name, na.rm = TRUE), roundn),
                median = round(stats::median(d_var_name, na.rm = TRUE), roundn),
                mean = round(mean(d_var_name, na.rm = TRUE), roundn),
                max = round(max(d_var_name, na.rm = TRUE), roundn),
                sd = round(stats::sd(d_var_name, na.rm = TRUE), roundn),
                count = dplyr::n()
              )
          },
          d_var_name = as.name(merge_vars()$dist_var),
          strata_vars = c(merge_vars()$group_var, merge_vars()$strata_var),
          roundn = roundn
        )
      }

      within(obj, summary_table <- rtables::df_to_tt(summary_table_data))
      # if (iv_r()$is_valid()) {

      # } else {
      #   within(
      #     q_common,
      #     summary_table <- rtables::rtable(header = rtables::rheader(colnames(summary_table_data)))
      #   )
      # }
    })

    decorated_output_q <- srv_decorate_teal_data(
      "decorators",
      data = output_q,
      decorators = decorators,
      expr = quote(summary_table)
    )

    output_r <- reactive({
      obj <- req(decorated_output_q())

      # todo: why summary_table_data is returned while summary_table is printed in a code?
      DT::datatable(
        obj[["summary_table_data"]],
        options = list(
          autoWidth = TRUE,
          columnDefs = list(list(width = "200px", targets = "_all"))
        ),
        rownames = FALSE
      )
    })

    output$summary_table <- DT::renderDataTable(output_r())

    decorated_output_q
  })
}

.ui_test_table <- function(id, is_stratified, decorators) {
  ns <- NS(id)
  tagList(
    encodings = tagList(
      shinyWidgets::pickerInput(
        ns("dist_test"),
        "Tests:",
        choices = c(
          "Shapiro-Wilk",
          if (is_stratified) "Kolmogorov-Smirnov (two-samples)",
          if (is_stratified) "one-way ANOVA",
          if (is_stratified) "Fligner-Killeen",
          if (is_stratified) "F-test",
          "Kolmogorov-Smirnov (one-sample)",
          "Anderson-Darling (one-sample)",
          "Cramer-von Mises (one-sample)",
          if (is_stratified) "t-test (two-samples, not paired)"
        ),
        selected = NULL,
        options = list(
          `allow-clear` = TRUE,
          "none-selected-text" = "- Nothing selected -"
        )
      ),
      ui_decorate_teal_data(ns("decorators"), decorators = decorators)
    ),
    output = tagList(
      tags$h3("Tests"),
      DT::dataTableOutput(ns("table"))
    )
  )
}

.srv_test_table <- function(id, data, merge_vars, t_dist, decorators) {
  moduleServer(id, function(input, output, session) {
    output_q <- eventReactive(
      ignoreNULL = FALSE,
      eventExpr = {
        data()
        input$dist_test
      },
      valueExpr = {
        obj <- data()
        anl <- obj[["anl"]]
        d_var <- merge_vars()$dist_var
        s_var <- merge_vars()$strata_var
        g_var <- merge_vars()$group_var
        d_var_name <- as.name(d_var)
        s_var_name <- if (!is.null(s_var)) as.name(s_var)
        g_var_name <- if (!is.null(g_var)) as.name(g_var)

        dist_test <- input$dist_test
        validate(need(length(dist_test) > 0, "Please select a test"))

        if (length(s_var) > 0 || length(g_var) > 0) {
          counts <- anl %>%
            dplyr::group_by_at(dplyr::vars(dplyr::any_of(c(s_var, g_var)))) %>%
            dplyr::summarise(n = dplyr::n())
          validate(need(all(counts$n > 5), "Please select strata*group with at least 5 observation each."))
        }

        map_dist <- c(normal = "dnorm", lognormal = "dlnorm", gamma = "dgamma", unif = "dunif")
        sks_args <- list(
          test = quote(stats::ks.test),
          args = bquote(append(list(.[[.(d_var)]], .(map_dist[t_dist()])), params)),
          groups = c(g_var, s_var)
        )
        ssw_args <- list(
          test = quote(stats::shapiro.test),
          args = bquote(list(.[[.(d_var)]])),
          groups = c(g_var, s_var)
        )
        mfil_args <- list(
          test = quote(stats::fligner.test),
          args = bquote(list(.[[.(d_var)]], .[[.(s_var)]])),
          groups = c(g_var)
        )
        sad_args <- list(
          test = quote(goftest::ad.test),
          args = bquote(append(list(.[[.(d_var)]], .(map_dist[t_dist()])), params)),
          groups = c(g_var, s_var)
        )
        scvm_args <- list(
          test = quote(goftest::cvm.test),
          args = bquote(append(list(.[[.(d_var)]], .(map_dist[t_dist()])), params)),
          groups = c(g_var, s_var)
        )
        manov_args <- list(
          test = quote(stats::aov),
          args = bquote(list(stats::formula(.(d_var_name) ~ .(s_var_name)), .)),
          groups = c(g_var)
        )
        mt_args <- list(
          test = quote(stats::t.test),
          args = bquote(unname(split(.[[.(d_var)]], .[[.(s_var)]], drop = TRUE))),
          groups = c(g_var)
        )
        mv_args <- list(
          test = quote(stats::var.test),
          args = bquote(unname(split(.[[.(d_var)]], .[[.(s_var)]], drop = TRUE))),
          groups = c(g_var)
        )
        mks_args <- list(
          test = quote(stats::ks.test),
          args = bquote(unname(split(.[[.(d_var)]], .[[.(s_var)]], drop = TRUE))),
          groups = c(g_var)
        )

        tests_base <- switch(dist_test,
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
          t_test = t_dist(),
          d_var = d_var,
          g_var = g_var,
          s_var = s_var,
          args = tests_base$args,
          groups = tests_base$groups,
          test = tests_base$test,
          d_var_name = d_var_name,
          g_var_name = g_var_name,
          s_var_name = s_var_name
        )


        teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Distribution Tests table")

        obj <- if (length(s_var) == 0 && length(g_var) == 0) {
          obj <- teal.code::eval_code(obj, 'library("generics")') # nolint quotes
          teal.code::eval_code(
            obj,
            substitute(
              expr = {
                test_table_data <- anl %>%
                  dplyr::select(d_var) %>%
                  with(., generics::glance(do.call(test, args))) %>%
                  dplyr::mutate_if(is.numeric, round, 3)
              },
              env = env
            )
          )
        } else {
          # todo: why there is a `library` call when `tidyr::unnest` is prefixed, same for `generics`
          obj <- teal.code::eval_code(obj, 'library("tidyr")') # nolint quotes
          teal.code::eval_code(
            obj,
            substitute(
              expr = {
                test_table_data <- anl %>%
                  dplyr::select(d_var, s_var, g_var) %>%
                  dplyr::group_by_at(dplyr::vars(dplyr::any_of(groups))) %>%
                  dplyr::do(tests = generics::glance(do.call(test, args))) %>%
                  tidyr::unnest(tests) %>%
                  dplyr::mutate_if(is.numeric, round, 3)
              },
              env = env
            )
          )
        }

        within(obj, {
          test_table <- rtables::df_to_tt(test_table_data)
        })
      }
    )

    decorated_output_q <- srv_decorate_teal_data(
      "decorators",
      data = output_q,
      decorators = decorators,
      expr = quote(test_table)
    )

    output_r <- reactive({
      obj <- req(decorated_output_q())
      DT::datatable(obj[["test_table_data"]])
    })

    output$table <- DT::renderDataTable(output_r())

    decorated_output_q
  })
}

.calc_dist_params <- function(x, dist) {
  if (dist == "unif") {
    return(stats::setNames(range(x, na.rm = TRUE), c("min", "max")))
  }
  tryCatch(
    MASS::fitdistr(x, densfun = dist)$estimate,
    error = function(e) c(param1 = NA_real_, param2 = NA_real_)
  )
}

.dist_param_list <- function(dist, param1, param2) {
  dist_param_names <- list(
    normal = c("mean", "sd"),
    lognormal = c("meanlog", "sdlog"),
    gamma = c("shape", "rate"),
    unif = c("min", "max")
  )

  params <- list(param1, param2)
  names(params) <- dist_param_names[[dist]]
  params
}
