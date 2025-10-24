#' @export
tm_g_scatterplotmatrix.picks <- function(label = "Scatterplot Matrix",
                                         variables = list(
                                           picks(
                                             datasets(),
                                             variables(selected = seq(1, 5), multiple = TRUE)
                                           )
                                         ),
                                         plot_height = c(600, 200, 2000),
                                         plot_width = NULL,
                                         pre_output = NULL,
                                         post_output = NULL,
                                         transformators = list(),
                                         decorators = list()) {
  message("Initializing tm_g_scatterplotmatrix")

  if (is.null(names(variables))) {
    names(variables) <- sprintf("pick_%s", seq_along(variables))
  }

  # Start of assertions
  checkmate::assert_string(label)
  checkmate::assert_list(variables, types = "picks", names = "named")

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)

  assert_decorators(decorators, "plot")
  # End of assertions

  args <- as.list(environment())
  ans <- module(
    label = label,
    ui = ui_g_scatterplotmatrix.picks,
    server = srv_g_scatterplotmatrix.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_scatterplotmatrix.picks))],
    server_args = args[names(args) %in% names(formals(srv_g_scatterplotmatrix.picks))],
    transformators = transformators,
    datanames = {
      datanames <- datanames(variables)
      if (length(datanames)) datanames else "all"
    }
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the scatterplot matrix module
ui_g_scatterplotmatrix.picks <- function(id,
                                         variables,
                                         pre_output,
                                         post_output,
                                         decorators) {
  checkmate::assert_list(variables, "picks", names = "named")
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      textOutput(ns("message")),
      tags$br(),
      teal.widgets::plot_with_settings_ui(id = ns("myplot"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      tagList(
        lapply(names(variables), function(id) {
          teal::teal_nav_item(
            teal.transform::picks_ui(id = ns(id), picks = variables[[id]])
          )
        })
      ),
      tags$hr(),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
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
    pre_output = pre_output,
    post_output = post_output
  )
}

# Server function for the scatterplot matrix module
srv_g_scatterplotmatrix.picks <- function(id,
                                          data,
                                          variables,
                                          plot_height,
                                          plot_width,
                                          decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    selectors <- teal.transform::picks_srv(
      picks = variables,
      data = data
    )

    validated_q <- reactive({
      obj <- req(data())

      input_ids <- sprintf("%s-variables-selected", names(variables))
      selected_variables <- unname(unlist(lapply(selectors, function(selector) selector()$variables$selected)))
      validate_input(
        inputId = input_ids, # validate all inputs where variable can be selected
        condition = length(selected_variables) > 1,
        message = "Please select at least 2 columns"
      )

      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card("# Scatter Plot Matrix"),
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's code")
      )
      teal.code::eval_code(obj, 'library("ggplot2");library("dplyr");')
    })

    merged <- teal.transform::merge_srv("merge", data = validated_q, selectors = selectors, output_name = "anl")
    variables <- reactive(unname(unlist(merged$variables())))

    # plot
    output_q <- reactive({
      qenv <- req(merged$data())
      anl <- qenv[["anl"]]
      cols_names <- variables()
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

      teal::validate_has_data(anl, 10)
      teal::validate_has_data(anl[, cols_names, drop = FALSE], 10, complete = TRUE, allow_inf = FALSE)

      # get labels and proper variable names
      varnames <- varname_w_label(cols_names, anl, wrap_width = 20)

      # check character columns. If any, then those are converted to factors
      check_char <- vapply(anl[, cols_names], is.character, logical(1))
      if (any(check_char)) {
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = anl <- anl[, cols_names] %>%
              dplyr::mutate_if(is.character, as.factor) %>%
              droplevels(),
            env = list(cols_names = cols_names)
          )
        )
      } else {
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = anl <- anl[, cols_names] %>%
              droplevels(),
            env = list(cols_names = cols_names)
          )
        )
      }


      # create plot
      teal.reporter::teal_card(qenv) <- c(teal.reporter::teal_card(qenv), "## Plot")

      if (add_cor) {
        shinyjs::show("cor_method")
        shinyjs::show("cor_use")
        shinyjs::show("cor_na_omit")

        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = {
              plot <- lattice::splom(
                anl,
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
              plot <- lattice::splom(
                anl,
                varnames = varnames_value,
                pch = 16,
                alpha = alpha_value,
                cex = cex_value
              )
            },
            env = list(varnames_value = varnames, alpha_value = alpha, cex_value = cex)
          )
        )
      }
      qenv
    })

    decorated_output_q <- srv_decorate_teal_data(
      id = "decorator",
      data = output_q,
      decorators = select_decorators(decorators, "plot"),
      expr = quote(plot)
    )

    plot_r <- reactive(req(decorated_output_q())[["plot"]])

    # Insert the plot into a plot_with_settings module
    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    # show a message if conversion to factors took place
    output$message <- renderText({
      cols_names <- req(variables())
      anl <- merged$data()[["anl"]]
      check_char <- vapply(anl[, cols_names], is.character, logical(1))
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

    set_chunk_dims(pws, decorated_output_q)
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
