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
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADRS <- synthetic_cdisc_data("latest")$adrs
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     cdisc_dataset("ADRS", ADRS, code = "ADRS <- synthetic_cdisc_data(\"latest\")$adrs"),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix",
#'       variables = list(
#'         data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = variable_choices(ADSL),
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
#'             choices = value_choices(ADRS, c("PARAMCD", "AVISIT"), c("PARAM", "AVISIT")),
#'             selected = "INVET - END OF INDUCTION",
#'             multiple = TRUE
#'           ),
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = variable_choices(ADRS),
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
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_scatterplotmatrix <- function(label = "Scatterplot Matrix",
                                   variables,
                                   plot_height = c(600, 200, 2000),
                                   plot_width = NULL,
                                   pre_output = NULL,
                                   post_output = NULL) {
  logger::log_info("Initializing tm_g_scatterplotmatrix")
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
    filters = teal.transform::get_extract_datanames(variables)
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
      shiny::tags$div(
        teal.reporter::add_card_button_ui(ns("addReportCard")),
        teal.reporter::download_report_button_ui(ns("downloadButton")),
        teal.reporter::reset_report_button_ui(ns("resetButton"))
      ),
      shiny::tags$br(),
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
    forms = teal::get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

srv_g_scatterplotmatrix <- function(id, datasets, reporter, variables, plot_height, plot_width) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  moduleServer(id, function(input, output, session) {
    teal.code::init_chunks()

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(variables = variables),
      datasets = datasets
    )

    merged_data <- teal.transform::data_merge_srv(
      datasets = datasets,
      selector_list = selector_list
    )

    # plot
    plot_r <- reactive({
      validate({
        need(!is.null(selector_list()$variables()), "Please select variables")
      })
      teal.code::chunks_reset()
      teal.code::chunks_push_data_merge(merged_data())

      ANL <- teal.code::chunks_get_var("ANL") # nolint
      teal::validate_has_data(ANL, 10)

      alpha <- input$alpha # nolint
      cex <- input$cex # nolint
      add_cor <- input$cor # nolint
      cor_method <- input$cor_method # nolint
      cor_na_omit <- input$cor_na_omit # nolint

      cor_na_action <- if (cor_na_omit) {
        "na.omit"
      } else {
        "na.fail"
      }

      cols_names <- merged_data()$columns_source$variable

      validate(need(length(cols_names) > 1, "Need at least 2 columns."))
      teal::validate_has_data(ANL[, cols_names], 10, complete = TRUE, allow_inf = FALSE)

      # get labels and proper variable names
      varnames <- varname_w_label(cols_names, ANL, wrap_width = 20) # nolint

      # check character columns. If any, then those are converted to factors
      check_char <- vapply(ANL[, cols_names], is.character, logical(1))
      if (any(check_char)) {
        teal.code::chunks_push(
          id = "factorize_ANL_call",
          expression = substitute(
            expr = ANL <- ANL[, cols_names] %>% # nolint
              dplyr::mutate_if(is.character, as.factor) %>%
              droplevels(),
            env = list(cols_names = cols_names)
          )
        )
      } else {
        teal.code::chunks_push(
          id = "ANL_drop_levels_call",
          expression = substitute(
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

        teal.code::chunks_push(
          id = "plot_call",
          expression = substitute(
            expr = {
              plot <- lattice::splom(
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
              print(plot)
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
        teal.code::chunks_push(
          id = "plot_call",
          expression = substitute(
            expr = lattice::splom(ANL, varnames = varnames_value, pch = 16, alpha = alpha_value, cex = cex_value),
            env = list(varnames_value = varnames, alpha_value = alpha, cex_value = cex)
          )
        )
      }
      teal.code::chunks_safe_eval()
    })

    # Insert the plot into a plot_with_settings module
    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    # show a message if conversion to factors took place
    output$message <- renderText({
      req(selector_list()$variables())
      ANL <- merged_data()$data() # nolint
      cols_names <- unique(unname(do.call(c, merged_data()$columns_source)))
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

    show_r_code_title <- reactive(
      paste0(
        "Scatterplotmatrix of ",
        paste(unlist(merged_data()$columns_source), collapse = ", ")
      )
    )

    teal::get_rcode_srv(
      id = "rcode",
      datasets = datasets,
      datanames = teal.transform::get_extract_datanames(list(variables)),
      modal_title = show_r_code_title(),
      code_header = show_r_code_title()
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        card$set_name("Scatter Plot Matrix")
        card$append_text("Scatter Plot Matrix", "header2")
        card$append_text("Filter State", "header3")
        card$append_fs(datasets$get_filter_state())
        card$append_text("Main Element", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_text("Show R Code", "header3")
        card$append_src(paste(get_rcode(
          chunks = teal.code::get_chunks_object(parent_idx = 1L),
          datasets = datasets,
          title = "",
          description = ""
        ), collapse = "\n"))
        card
      }

      teal.reporter::add_card_button_srv("addReportCard", reporter = reporter, card_fun = card_fun)
      teal.reporter::download_report_button_srv("downloadButton", reporter = reporter)
      teal.reporter::reset_report_button_srv("resetButton", reporter)
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
      return(paste(c(
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
