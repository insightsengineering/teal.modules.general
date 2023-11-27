#' Create a scatterplot matrix
#'
#' The available datasets to choose from for each dataset selector is the same and
#' determined by the argument `variables`.
#' @md
#'
#' @inheritParams teal::module
#' @inheritParams tm_g_scatterplot
#' @inheritParams shared_params
#'
#' @param variables (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'  Plotting variables from an incoming dataset with filtering and selecting. In case of
#'  `data_extract_spec` use `select_spec(..., ordered = TRUE)` if plot elements should be
#'  rendered according to selection order.
#'
#' @note For more examples, please see the vignette "Using scatterplot matrix" via
#'   \code{vignette("using-scatterplot-matrix", package = "teal.modules.general")}.
#' @export
#'
#' @examples
#' # Scatterplot matrix of variables from ADSL dataset
#'
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- teal.modules.general::rADSL
#'   ADRS <- teal.modules.general::rADRS
#' })
#' datanames <- c("ADSL", "ADRS")
#' datanames(data) <- datanames
#' join_keys(data) <- default_cdisc_join_keys[datanames]
#'
#' app <- teal::init(
#'   data = data,
#'   modules = teal::modules(
#'     teal.modules.general::tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix",
#'       variables = list(
#'         teal.transform::data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = variable_choices(data[["ADSL"]]),
#'             selected = c("AGE", "RACE", "SEX"),
#'             multiple = TRUE,
#'             ordered = TRUE,
#'             fixed = FALSE
#'           )
#'         ),
#'         teal.transform::data_extract_spec(
#'           dataname = "ADRS",
#'           filter = teal.transform::filter_spec(
#'             label = "Select endpoints:",
#'             vars = c("PARAMCD", "AVISIT"),
#'             choices = value_choices(data[["ADRS"]], c("PARAMCD", "AVISIT"), c("PARAM", "AVISIT")),
#'             selected = "INVET - END OF INDUCTION",
#'             multiple = TRUE
#'           ),
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = variable_choices(data[["ADRS"]]),
#'             selected = c("AGE", "AVAL", "ADY"),
#'             multiple = TRUE,
#'             ordered = TRUE,
#'             fixed = FALSE
#'           )
#'         )
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
tm_g_scatterplotmatrix <- function(label = "Scatterplot Matrix",
                                   variables,
                                   plot_height = c(600, 200, 2000),
                                   plot_width = NULL,
                                   pre_output = NULL,
                                   post_output = NULL) {
  logger::log_info("Initializing tm_g_scatterplotmatrix")
  if (!requireNamespace("lattice", quietly = TRUE)) {
    stop("Cannot load lattice - please install the package or restart your session.")
  }
  if (inherits(variables, "data_extract_spec")) variables <- list(variables)

  checkmate::assert_string(label)
  checkmate::assert_list(variables, types = "data_extract_spec")
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  args <- as.list(environment())
  module(
    label = label,
    server = srv_g_scatterplotmatrix,
    ui = ui_g_scatterplotmatrix,
    ui_args = args,
    server_args = list(variables = variables, plot_height = plot_height, plot_width = plot_width),
    datanames = teal.transform::get_extract_datanames(variables)
  )
}

ui_g_scatterplotmatrix <- function(id, ...) {
  args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(args$variables)
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      textOutput(ns("message")),
      br(),
      teal.widgets::plot_with_settings_ui(id = ns("myplot"))
    ),
    encoding = div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(args$variables),
      teal.transform::data_extract_ui(
        id = ns("variables"),
        label = "Variables",
        data_extract_spec = args$variables,
        is_single_dataset = is_single_dataset_value
      ),
      hr(),
      teal.widgets::panel_group(
        teal.widgets::panel_item(
          title = "Plot settings",
          sliderInput(
            ns("alpha"), "Opacity:",
            min = 0, max = 1,
            step = .05, value = .5, ticks = FALSE
          ),
          sliderInput(
            ns("cex"), "Points size:",
            min = 0.2, max = 3,
            step = .05, value = .65, ticks = FALSE
          ),
          checkboxInput(ns("cor"), "Add Correlation", value = FALSE),
          radioButtons(
            ns("cor_method"), "Select Correlation Method",
            choiceNames = c("Pearson", "Kendall", "Spearman"),
            choiceValues = c("pearson", "kendall", "spearman"),
            inline = TRUE
          ),
          checkboxInput(ns("cor_na_omit"), "Omit Missing Values", value = TRUE)
        )
      )
    ),
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("warning"), "Show Warnings"),
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    ),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

srv_g_scatterplotmatrix <- function(id, data, reporter, filter_panel_api, variables, plot_height, plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(variables = variables),
      datasets = data,
      select_validation_rule = list(
        variables = ~ if (length(.) <= 1) "Please select at least 2 columns."
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_merged_input <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list
    )

    anl_merged_q <- reactive({
      req(anl_merged_input())
      data() %>%
        teal.code::eval_code(as.expression(anl_merged_input()$expr))
    })

    merged <- list(
      anl_input_r = anl_merged_input,
      anl_q_r = anl_merged_q
    )

    # plot
    output_q <- reactive({
      teal::validate_inputs(iv_r())

      qenv <- merged$anl_q_r()
      ANL <- qenv[["ANL"]] # nolint

      cols_names <- merged$anl_input_r()$columns_source$variables
      alpha <- input$alpha # nolint
      cex <- input$cex # nolint
      add_cor <- input$cor # nolint
      cor_method <- input$cor_method # nolint
      cor_na_omit <- input$cor_na_omit # nolint

      cor_na_action <- if (isTruthy(cor_na_omit)) {
        "na.omit"
      } else {
        "na.fail"
      }

      teal::validate_has_data(ANL, 10)
      teal::validate_has_data(ANL[, cols_names, drop = FALSE], 10, complete = TRUE, allow_inf = FALSE)

      # get labels and proper variable names
      varnames <- varname_w_label(cols_names, ANL, wrap_width = 20) # nolint

      # check character columns. If any, then those are converted to factors
      check_char <- vapply(ANL[, cols_names], is.character, logical(1))
      if (any(check_char)) {
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = ANL <- ANL[, cols_names] %>% # nolint
              dplyr::mutate_if(is.character, as.factor) %>%
              droplevels(),
            env = list(cols_names = cols_names)
          )
        )
      } else {
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = ANL <- ANL[, cols_names] %>% # nolint
              droplevels(),
            env = list(cols_names = cols_names)
          )
        )
      }


      # create plot
      if (add_cor) {
        shinyjs::show("cor_method")
        shinyjs::show("cor_use")
        shinyjs::show("cor_na_omit")

        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = {
              g <- lattice::splom(
                ANL,
                varnames = varnames_value,
                panel = function(x, y, ...) {
                  lattice::panel.splom(x = x, y = y, ...)
                  cpl <- lattice::current.panel.limits()
                  lattice::panel.text(
                    mean(cpl$xlim),
                    mean(cpl$ylim),
                    get_scatterplotmatrix_stats(
                      x,
                      y,
                      .f = stats::cor.test,
                      .f_args = list(method = cor_method, na.action = cor_na_action)
                    ),
                    alpha = 0.6,
                    fontsize = 18,
                    fontface = "bold"
                  )
                },
                pch = 16,
                alpha = alpha_value,
                cex = cex_value
              )
              print(g)
            },
            env = list(
              varnames_value = varnames,
              cor_method = cor_method,
              cor_na_action = cor_na_action,
              alpha_value = alpha,
              cex_value = cex
            )
          )
        )
      } else {
        shinyjs::hide("cor_method")
        shinyjs::hide("cor_use")
        shinyjs::hide("cor_na_omit")
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = {
              g <- lattice::splom(ANL, varnames = varnames_value, pch = 16, alpha = alpha_value, cex = cex_value)
              g
            },
            env = list(varnames_value = varnames, alpha_value = alpha, cex_value = cex)
          )
        )
      }
      qenv
    })

    plot_r <- reactive(output_q()[["g"]])

    # Insert the plot into a plot_with_settings module
    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    # show a message if conversion to factors took place
    output$message <- renderText({
      shiny::req(iv_r()$is_valid())
      req(selector_list()$variables())
      ANL <- merged$anl_q_r()[["ANL"]] # nolint
      cols_names <- unique(unname(do.call(c, merged$anl_input_r()$columns_source)))
      check_char <- vapply(ANL[, cols_names], is.character, logical(1))
      if (any(check_char)) {
        is_single <- sum(check_char) == 1
        paste(
          "Character",
          ifelse(is_single, "variable", "variables"),
          paste0("(", paste(cols_names[check_char], collapse = ", "), ")"),
          ifelse(is_single, "was", "were"),
          "converted to",
          ifelse(is_single, "factor.", "factors.")
        )
      } else {
        ""
      }
    })

    teal.widgets::verbatim_popup_srv(
      id = "warning",
      verbatim_content = reactive(teal.code::get_warnings(output_q())),
      title = "Warning",
      disabled = reactive(is.null(teal.code::get_warnings(output_q())))
    )

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = reactive(teal.code::get_code(output_q())),
      title = "Show R Code for Scatterplotmatrix"
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Scatter Plot Matrix",
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
        card$append_src(teal.code::get_code(output_q()))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}

#' Get stats for x-y pairs in scatterplot matrix
#' @description uses stats::cor.test per default for all numerical input variables and converts results
#'  to character vector. Could be extended if different stats for different variable
#'  types are needed. Meant to be called from \code{lattice::panel.text}.
#' @param x \code{numeric}
#' @param y \code{numeric}
#' @param .f \code{function}, function that accepts x and y as formula input \code{~ x + y}.
#' Default \code{stats::cor.test}
#' @param .f_args \code{list} of arguments to be passed to \code{.f}
#' @param round_stat \code{integer}
#' @param round_pval \code{integer}
#' @details presently we need to use a formula input for \code{stats::cor.test} because
#' \code{na.fail} only gets evaluated when a formula is passed (see below).
#' \preformatted{
#' x = c(1,3,5,7,NA)
#' y = c(3,6,7,8,1)
#' stats::cor.test(x, y, na.action = "na.fail")
#' stats::cor.test(~ x + y,  na.action = "na.fail")
#' }
#' @return \code{character} with stats. For \code{stats::cor.test} correlation coefficient and p-value.
#' @export
#' @examples
#' set.seed(1)
#' x <- runif(25, 0, 1)
#' y <- runif(25, 0, 1)
#' x[c(3, 10, 18)] <- NA
#'
#' get_scatterplotmatrix_stats(x, y, .f = stats::cor.test, .f_args = list(method = "pearson"))
#' get_scatterplotmatrix_stats(x, y, .f = stats::cor.test, .f_args = list(
#'   method = "pearson",
#'   na.action = na.fail
#' ))
get_scatterplotmatrix_stats <- function(x, y,
                                        .f = stats::cor.test,
                                        .f_args = list(),
                                        round_stat = 2,
                                        round_pval = 4) {
  if (is.numeric(x) && is.numeric(y)) {
    stat <- tryCatch(do.call(.f, c(list(~ x + y), .f_args)), error = function(e) NA)

    if (anyNA(stat)) {
      return("NA")
    } else if (all(c("estimate", "p.value") %in% names(stat))) {
      return(paste(
        c(
          paste0(names(stat$estimate), ":", round(stat$estimate, round_stat)),
          paste0("P:", round(stat$p.value, round_pval))
        ),
        collapse = "\n"
      ))
    } else {
      stop("function not supported")
    }
  } else {
    if ("method" %in% names(.f_args)) {
      if (.f_args$method == "pearson") {
        return("cor:-")
      }
      if (.f_args$method == "kendall") {
        return("tau:-")
      }
      if (.f_args$method == "spearman") {
        return("rho:-")
      }
    }
    return("-")
  }
}
