#' `teal` module: Scatterplot matrix
#'
#' Generates a scatterplot matrix from selected `variables` from datasets.
#' Each plot within the matrix represents the relationship between two variables,
#' providing the overview of correlations and distributions across selected data.
#'
#' @note For more examples, please see the vignette "Using scatterplot matrix" via
#' `vignette("using-scatterplot-matrix", package = "teal.modules.general")`.
#'
#' @note When *Add Correlation* is enabled, a simple **Omit NAs** checkbox
#'   controls NA handling (checked = `"pairwise.complete.obs"`, matching the
#'   historical default).  Unchecking it reveals a dropdown with all five
#'   `stats::cor()` `use` options for advanced control.
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
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `plot` (`ggplot` - a `patchwork` assembled from individual `ggplot` panels)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_g_scatterplotmatrix(
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
#' @inheritSection teal::example_module Reporting
#'
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
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
#'             selected = c("DE", "FR", "IT", "PT", "GR", "NL", "BE", "LU", "AT"),
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
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#' # CDISC data example
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- teal.data::rADSL
#'   ADRS <- teal.data::rADRS
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
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
                                   post_output = NULL,
                                   transformators = list(),
                                   decorators = list()) {
  message("Initializing tm_g_scatterplotmatrix")

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

  teal::assert_decorators(decorators, "plot")
  # End of assertions

  # Make UI args
  args <- as.list(environment())

  ans <- module(
    label = label,
    server = srv_g_scatterplotmatrix,
    ui = ui_g_scatterplotmatrix,
    ui_args = args,
    server_args = list(
      variables = variables,
      plot_height = plot_height,
      plot_width = plot_width,
      decorators = decorators
    ),
    transformators = transformators,
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
      tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(args$variables),
      teal.transform::data_extract_ui(
        id = ns("variables"),
        label = "Variables",
        data_extract_spec = args$variables,
        is_single_dataset = is_single_dataset_value
      ),
      tags$hr(),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(args$decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Plot settings",
          sliderInput(
            ns("alpha"), "Opacity:",
            min = 0, max = 1,
            step = .05, value = .5, ticks = FALSE
          ),
          checkboxInput(ns("cor"), "Add Correlation", value = FALSE),
          shinyjs::hidden(
            radioButtons(
              ns("cor_method"), "Select Correlation Method",
              choiceNames = c("Pearson", "Kendall", "Spearman"),
              choiceValues = c("pearson", "kendall", "spearman"),
              inline = TRUE
            )
          ),
          shinyjs::hidden(
            checkboxInput(
              ns("cor_na_omit"),
              label = tags$span(
                "Omit NAs",
                bslib::popover(
                  icon("circle-info"),
                  title = "NA handling",
                  tags$p(tags$b("Checked:"), "use pairwise complete observations (each pair correlated over rows where both values are present)."),
                  tags$p(tags$b("Unchecked:"), "reveals a dropdown with all five", tags$code("stats::cor()"), "use= options."),
                  options = list(trigger = "hover focus")
                )
              ),
              value = TRUE
            )
          ),
          shinyjs::hidden(
            selectInput(
              ns("cor_use"),
              label = tags$span(
                "NA handling:",
                bslib::popover(
                  icon("circle-info"),
                  title = "NA handling options",
                  tags$dl(
                    tags$dt("Everything"),
                    tags$dd("Return NA for a pair if either variable contains any missing value."),
                    tags$dt("All observations"),
                    tags$dd("Assume no NAs are present; throws an error if any are found."),
                    tags$dt("Complete observations"),
                    tags$dd("Listwise deletion – only rows with no NAs across all selected variables."),
                    tags$dt("NA or complete"),
                    tags$dd("Like complete observations but returns NA instead of an error when no complete cases exist."),
                    tags$dt("Pairwise complete"),
                    tags$dd("Use all rows where both variables in a pair are non-missing (maximises available data).")
                  ),
                  options = list(trigger = "hover focus")
                )
              ),
              choices = c(
                "Everything" = "everything",
                "All observations" = "all.obs",
                "Complete observations" = "complete.obs",
                "NA or complete" = "na.or.complete",
                "Pairwise complete" = "pairwise.complete.obs"
              ),
              selected = "everything"
            )
          )
        )
      )
    ),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

# Server function for the scatterplot matrix module
srv_g_scatterplotmatrix <- function(id,
                                    data,
                                    variables,
                                    plot_height,
                                    plot_width,
                                    decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(variables = variables),
      datasets = data,
      select_validation_rule = list(
        variables = shinyvalidate::compose_rules(
          ~ if (length(.) <= 1) "Please select at least 2 columns.",
          ~ if (length(.) > 5) "Please select at most 5 columns."
        )
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_merged_input <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list
    ) |> debounce(500)

    anl_merged_q <- reactive({
      req(anl_merged_input())
      obj <- data()
      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's output(s)")
      )
      qenv <- teal.code::eval_code(obj, "library(dplyr)")
      teal.code::eval_code(qenv, as.expression(anl_merged_input()$expr))
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
      alpha_val <- input$alpha
      add_cor <- input$cor
      cor_method <- input$cor_method
      cor_na_omit <- input$cor_na_omit
      cor_use <- if (isTRUE(cor_na_omit)) "pairwise.complete.obs" else input$cor_use

      teal::validate_has_data(ANL, 10)
      teal::validate_has_data(ANL[, cols_names, drop = FALSE], 10, complete = TRUE, allow_inf = FALSE)

      # get labels and proper variable names
      varnames <- varname_w_label(cols_names, ANL, wrap_width = 20)

      # check character columns. If any, then those are converted to factors
      check_char <- vapply(ANL[, cols_names], is.character, logical(1))
      if (any(check_char)) {
        qenv <- within(
          qenv,
          ANL <- ANL[, cols_names] %>%
            dplyr::mutate_if(is.character, as.factor) %>%
            droplevels(),
          cols_names = cols_names
        )
      } else {
        qenv <- within(
          qenv,
          ANL <- ANL[, cols_names] %>%
            droplevels(),
          cols_names = cols_names
        )
      }

      # create plot
      teal.reporter::teal_card(qenv) <- c(teal.reporter::teal_card(qenv), "### Plot")

      if (add_cor) {
        shinyjs::show("cor_method")
        shinyjs::show("cor_na_omit")
        if (isTRUE(cor_na_omit)) {
          shinyjs::hide("cor_use")
        } else {
          shinyjs::show("cor_use")
        }
      } else {
        shinyjs::hide("cor_method")
        shinyjs::hide("cor_na_omit")
        shinyjs::hide("cor_use")
      }

      qenv <- within(
        qenv,
        {
          add_cor    <- add_cor_value
          cor_method <- cor_method_value
          cor_use    <- cor_use_value
          alpha      <- alpha_value
          varnames   <- varnames_value

          col_names <- names(ANL)
          n_vars    <- length(col_names)
          base_size <- max(6L, 14L - n_vars)

          num_idx <- which(vapply(ANL, is.numeric, logical(1L)))
          cor_mat <- if (add_cor && length(num_idx) >= 2L) {
            tryCatch(
              stats::cor(ANL[num_idx], method = cor_method, use = cor_use),
              error = function(e) NULL
            )
          }

          make_panel <- function(i, j) {
            xi <- ANL[[col_names[i]]]
            xj <- ANL[[col_names[j]]]
            if (i == j) {
              p <- ggplot2::ggplot(data.frame(x = xi), ggplot2::aes(x = x)) +
                ggplot2::labs(x = NULL, y = NULL, title = varnames[i])
              if (is.numeric(xi)) {
                p <- p + ggplot2::geom_density(fill = "steelblue", alpha = alpha)
              } else {
                p <- p + ggplot2::geom_bar(fill = "steelblue", alpha = alpha)
              }
            } else if (i < j && add_cor) {
              cv  <- if (!is.null(cor_mat) && is.numeric(xi) && is.numeric(xj)) cor_mat[col_names[i], col_names[j]] else NA_real_
              col <- if (is.na(cv)) "grey50" else if (cv > 0) "firebrick" else "steelblue"
              ggplot2::ggplot() +
                ggplot2::annotate("text", x = 0.5, y = 0.5, fontface = "bold", color = col,
                                  label = if (!is.na(cv)) sprintf("%.2f", cv) else if (is.numeric(xi) && is.numeric(xj)) "NA" else "-",
                                  size  = if (!is.na(cv)) max(3, abs(cv) * 8 + 3) else if (is.numeric(xi) && is.numeric(xj)) 3 else 4) +
                ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1) + ggplot2::theme_void()
            } else {
              p     <- ggplot2::ggplot(data.frame(x = xj, y = xi)) + ggplot2::labs(x = NULL, y = NULL)
              n_num <- is.numeric(xi) + is.numeric(xj)
              if (n_num == 2) p <- p + ggplot2::aes(x = x, y = y) + ggplot2::geom_point(color = "steelblue", alpha = alpha)
              if (n_num == 1) p <- p + ggplot2::aes(x = x, y = y) + ggplot2::geom_boxplot(fill = "steelblue", alpha = alpha)
              if (n_num == 0) p <- p + ggplot2::aes(x = x, fill = y) + ggplot2::geom_bar(position = "dodge", alpha = alpha) + ggplot2::labs(fill = NULL)
              p
            }
          }

          plot_list <- unlist(
            lapply(seq_len(n_vars), function(i) lapply(seq_len(n_vars), function(j) make_panel(i, j))),
            recursive = FALSE
          )
          plot <- patchwork::wrap_plots(plot_list, ncol = n_vars, nrow = n_vars) &
            ggplot2::theme_minimal(base_size = base_size) &
            ggplot2::theme(
              plot.title  = ggplot2::element_text(hjust = 0.5, face = "bold"),
              axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
              legend.position = "none"
            )
        },
        add_cor_value    = add_cor,
        cor_method_value = cor_method,
        cor_use_value    = cor_use,
        alpha_value      = alpha_val,
        varnames_value   = varnames
      )
      qenv
    })

    decorated_output_q <- teal::srv_transform_teal_data(
      id = "decorator",
      data = output_q,
      transformators = select_decorators(decorators, "plot"),
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

    set_chunk_dims(pws, decorated_output_q)
  })
}
