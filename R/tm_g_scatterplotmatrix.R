#' `teal` module: Scatterplot matrix
#'
#' Generates a scatterplot matrix from selected `variables` from datasets.
#' Each plot within the matrix represents the relationship between two variables,
#' providing the overview of correlations and distributions across selected data.
#'
#' @note For more examples, please see the vignette "Using scatterplot matrix" via
#' `vignette("using-scatterplot-matrix", package = "teal.modules.general")`.
#'
#' @inheritParams teal::module
#' @inheritParams tm_g_scatterplot
#' @inheritParams shared_params
#'
#' @param variables (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Specifies plotting variables from an incoming dataset with filtering and selecting. In case of
#' `data_extract_spec` use `select_spec(..., ordered = TRUE)` if plot elements should be
#' rendered according to selection order.
#'
#' @inherit shared_params return
#'
#' @examples
#' # general data example
#' data <- teal_data()
#' data <- within(data, {
#'   countries <- data.frame(
#'     id = c("DE", "FR", "IT", "ES", "PT", "GR", "NL", "BE", "LU", "AT"),
#'     government = factor(
#'       c(2, 2, 2, 1, 2, 2, 1, 1, 1, 2),
#'       labels = c("Monarchy", "Republic")
#'     ),
#'     language_family = factor(
#'       c(1, 3, 3, 3, 3, 2, 1, 1, 3, 1),
#'       labels = c("Germanic", "Hellenic", "Romance")
#'     ),
#'     population = c(83, 67, 60, 47, 10, 11, 17, 11, 0.6, 9),
#'     area = c(357, 551, 301, 505, 92, 132, 41, 30, 2.6, 83),
#'     gdp = c(3.4, 2.7, 2.1, 1.4, 0.3, 0.2, 0.7, 0.5, 0.1, 0.4),
#'     debt = c(2.1, 2.3, 2.4, 2.6, 2.3, 2.4, 2.3, 2.4, 2.3, 2.4)
#'   )
#'   sales <- data.frame(
#'     id = 1:50,
#'     country_id = sample(
#'       c("DE", "FR", "IT", "ES", "PT", "GR", "NL", "BE", "LU", "AT"),
#'       size = 50,
#'       replace = TRUE
#'     ),
#'     year = sort(sample(2010:2020, 50, replace = TRUE)),
#'     venue = sample(c("small", "medium", "large", "online"), 50, replace = TRUE),
#'     cancelled = sample(c(TRUE, FALSE), 50, replace = TRUE),
#'     quantity = rnorm(50, 100, 20),
#'     costs = rnorm(50, 80, 20),
#'     profit = rnorm(50, 20, 10)
#'   )
#' })
#' datanames(data) <- c("countries", "sales")
#' join_keys(data) <- join_keys(
#'   join_key("countries", "countries", "id"),
#'   join_key("sales", "sales", "id"),
#'   join_key("countries", "sales", c("id" = "country_id"))
#' )
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix",
#'       variables = list(
#'         data_extract_spec(
#'           dataname = "countries",
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = variable_choices(data[["countries"]]),
#'             selected = c("area", "gdp", "debt"),
#'             multiple = TRUE,
#'             ordered = TRUE,
#'             fixed = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "sales",
#'           filter = filter_spec(
#'             label = "Select variable:",
#'             vars = "country_id",
#'             choices = value_choices(data[["sales"]], "country_id"),
#'             selected = c("DE", "FR", "IT", "ES", "PT", "GR", "NL", "BE", "LU", "AT"),
#'             multiple = TRUE
#'           ),
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = variable_choices(data[["sales"]], c("quantity", "costs", "profit")),
#'             selected = c("quantity", "costs", "profit"),
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
#'
#' # CDISC data example
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- rADSL
#'   ADRS <- rADRS
#' })
#' datanames(data) <- c("ADSL", "ADRS")
#' join_keys(data) <- default_cdisc_join_keys[datanames(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix",
#'       variables = list(
#'         data_extract_spec(
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
#'         data_extract_spec(
#'           dataname = "ADRS",
#'           filter = filter_spec(
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
#'
#' @export
#'
tm_g_scatterplotmatrix <- function(label = "Scatterplot Matrix",
                                   variables,
                                   plot_height = c(600, 200, 2000),
                                   plot_width = NULL,
                                   pre_output = NULL,
                                   post_output = NULL) {
  message("Initializing tm_g_scatterplotmatrix")

  # Requires Suggested packages
  if (!requireNamespace("lattice", quietly = TRUE)) {
    stop("Cannot load lattice - please install the package or restart your session.")
  }

  # Normalize the parameters
  if (inherits(variables, "data_extract_spec")) variables <- list(variables)

  # Start of assertions
  checkmate::assert_string(label)
  checkmate::assert_list(variables, types = "data_extract_spec")

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  # End of assertions

  # Make UI args
  args <- as.list(environment())

  ans <- module(
    label = label,
    server = srv_g_scatterplotmatrix,
    ui = ui_g_scatterplotmatrix,
    ui_args = args,
    server_args = list(variables = variables, plot_height = plot_height, plot_width = plot_width),
    datanames = teal.transform::get_extract_datanames(variables)
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the scatterplot matrix module
ui_g_scatterplotmatrix <- function(id, ...) {
  args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(args$variables)
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      textOutput(ns("message")),
      tags$br(),
      teal.widgets::plot_with_settings_ui(id = ns("myplot"))
    ),
    encoding = tags$div(
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
      tags$hr(),
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
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    ),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

# Server function for the scatterplot matrix module
srv_g_scatterplotmatrix <- function(id, data, reporter, filter_panel_api, variables, plot_height, plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

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
      ANL <- qenv[["ANL"]]

      cols_names <- merged$anl_input_r()$columns_source$variables
      alpha <- input$alpha
      cex <- input$cex
      add_cor <- input$cor
      cor_method <- input$cor_method
      cor_na_omit <- input$cor_na_omit

      cor_na_action <- if (isTruthy(cor_na_omit)) {
        "na.omit"
      } else {
        "na.fail"
      }

      teal::validate_has_data(ANL, 10)
      teal::validate_has_data(ANL[, cols_names, drop = FALSE], 10, complete = TRUE, allow_inf = FALSE)

      # get labels and proper variable names
      varnames <- varname_w_label(cols_names, ANL, wrap_width = 20)

      # check character columns. If any, then those are converted to factors
      check_char <- vapply(ANL[, cols_names], is.character, logical(1))
      if (any(check_char)) {
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = ANL <- ANL[, cols_names] %>%
              dplyr::mutate_if(is.character, as.factor) %>%
              droplevels(),
            env = list(cols_names = cols_names)
          )
        )
      } else {
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = ANL <- ANL[, cols_names] %>%
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
      req(iv_r()$is_valid())
      req(selector_list()$variables())
      ANL <- merged$anl_q_r()[["ANL"]]
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
#'
#' Uses [stats::cor.test()] per default for all numerical input variables and converts results
#' to character vector.
#' Could be extended if different stats for different variable types are needed.
#' Meant to be called from [lattice::panel.text()].
#'
#' Presently we need to use a formula input for `stats::cor.test` because
#' `na.fail` only gets evaluated when a formula is passed (see below).
#' ```
#' x = c(1,3,5,7,NA)
#' y = c(3,6,7,8,1)
#' stats::cor.test(x, y, na.action = "na.fail")
#' stats::cor.test(~ x + y,  na.action = "na.fail")
#' ```
#'
#' @param x,y (`numeric`) vectors of data values. `x` and `y` must have the same length.
#' @param .f (`function`) function that accepts x and y as formula input `~ x + y`.
#' Default `stats::cor.test`.
#' @param .f_args (`list`) of arguments to be passed to `.f`.
#' @param round_stat (`integer(1)`) optional, number of decimal places to use when rounding the estimate.
#' @param round_pval (`integer(1)`) optional, number of decimal places to use when rounding the p-value.
#'
#' @return Character with stats. For [stats::cor.test()] correlation coefficient and p-value.
#'
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
#'
#' @export
#'
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
