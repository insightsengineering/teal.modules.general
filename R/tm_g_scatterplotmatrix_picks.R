#' @export
tm_g_scatterplotmatrix.picks <- function(label = "Scatterplot Matrix",
                                         variables = list(
                                           teal.picks::picks(
                                             teal.picks::datasets(),
                                             teal.picks::variables(selected = seq(1L, 5L), multiple = TRUE)
                                           )
                                         ),
                                         min_n_variables = 2L,
                                         max_n_variables = 5L,
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
  checkmate::assert_count(min_n_variables, positive = TRUE)
  checkmate::assert_count(max_n_variables, positive = TRUE)
  checkmate::assert_true(min_n_variables <= max_n_variables)

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
    datanames = .picks_datanames(variables)
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the scatterplot matrix module
ui_g_scatterplotmatrix.picks <- function(id,
                                         variables,
                                         min_n_variables,
                                         max_n_variables,
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
      tags$br(),
      tags$strong("Variable selection"),
      tagList(
        lapply(names(variables), function(id) {
          tags$div(
            teal.picks::picks_ui(id = ns(id), picks = variables[[id]])
          )
        })
      ),
      helpText(
        sprintf(
          "Supports between %d and %d columns.",
          min_n_variables, max_n_variables
        )
      ),
      tags$hr(),
      teal::ui_transform_teal_data(ns("decorator"), transformators = select_decorators(decorators, "plot")),
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
#' @importFrom lattice splom
#' @importFrom lattice panel.splom
#' @importFrom lattice current.panel.limits
#' @importFrom lattice panel.text
srv_g_scatterplotmatrix.picks <- function(id,
                                          data,
                                          variables,
                                          min_n_variables,
                                          max_n_variables,
                                          plot_height,
                                          plot_width,
                                          decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  checkmate::assert_list(variables, "picks", names = "named")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    selectors <- teal.picks::picks_srv(
      picks = variables,
      data = data
    )

    validated_q <- reactive({
      obj <- req(data())
      input_ids <- sprintf("%s-variables-selected", names(variables))
      selected_variables <- unname(unlist(lapply(selectors, function(selector) selector()$variables$selected)))
      teal::validate_input(
        inputId = input_ids, # validate all inputs where variable can be selected
        condition = length(selected_variables) >= min_n_variables,
        message = sprintf("Please select at least %d columns", min_n_variables)
      )

      teal::validate_input(
        inputId = input_ids, # validate all inputs where variable can be selected
        condition = length(selected_variables) <= max_n_variables,
        message = sprintf("Please select no more than %d columns", max_n_variables)
      )

      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card("# Scatter Plot Matrix"),
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's code")
      )
      teal.code::eval_code(obj, "library(ggplot2);library(dplyr);")
    })

    merged <- teal.picks::merge_srv("merge", data = validated_q, selectors = selectors, output_name = "ANL")

        # plot
    output_q <- reactive({
      qenv <- merged$data()
      ANL <- qenv[["ANL"]]

      cols_names <- unname(unlist(merged$variables()))
      alpha_val <- input$alpha
      cex <- input$cex
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
          add_cor <- add_cor_value
          cor_method <- cor_method_value
          cor_use <- cor_use_value
          alpha <- alpha_value
          cex <- cex_value
          varnames <- varnames_value

          col_names <- names(ANL)
          n_vars <- length(col_names)
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
              cv <- if (!is.null(cor_mat) && is.numeric(xi) && is.numeric(xj)) cor_mat[col_names[i], col_names[j]] else NA_real_ # nolint line_length_linter.
              col <- if (is.na(cv)) "grey50" else if (cv > 0) "firebrick" else "steelblue"
              return(
                ggplot2::ggplot() +
                  ggplot2::annotate("text",
                    x = 0.5, y = 0.5, fontface = "bold", color = col,
                    label = if (!is.na(cv)) sprintf("%.2f", cv) else if (is.numeric(xi) && is.numeric(xj)) "NA" else "-", # nolint line_length_linter.
                    size = if (!is.na(cv)) max(3, abs(cv) * 8 + 3) else if (is.numeric(xi) && is.numeric(xj)) 3 else 4 # nolint line_length_linter.
                  ) +
                  ggplot2::xlim(0, 1) +
                  ggplot2::ylim(0, 1) +
                  ggplot2::theme_void()
              )
            } else {
              p <- ggplot2::ggplot(data.frame(x = xj, y = xi)) +
                ggplot2::labs(x = NULL, y = NULL)
              n_num <- is.numeric(xi) + is.numeric(xj)
              # nolint start: line_length_linter.
              if (n_num == 2) p <- p + ggplot2::aes(x = x, y = y) + ggplot2::geom_point(color = "steelblue", alpha = alpha, size = cex)
              if (n_num == 1) p <- p + ggplot2::aes(x = x, y = y) + ggplot2::geom_boxplot(fill = "steelblue", alpha = alpha, outlier.size = cex, linesize = 1)
              if (n_num == 0) p <- p + ggplot2::aes(x = x, fill = y) + ggplot2::geom_bar(position = "dodge", alpha = alpha) + ggplot2::labs(fill = NULL)
              # nolint end: line_length_linter.
            }
            p <- p + ggplot2::theme_minimal(base_size = base_size)
            if (i == n_vars) {
              p <- p + ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
              if (!is.numeric(xj)) {
                p <- p + ggplot2::scale_x_discrete(
                  labels = function(x) ifelse(nchar(x) > 10, paste0(substr(x, 1, 9), "\u2026"), x)
                )
              }
            } else {
              p <- p + ggplot2::theme(axis.text.x = ggplot2::element_blank(), axis.ticks.x = ggplot2::element_blank())
            }
            p
          }

          plot_list <- unlist(
            lapply(seq_len(n_vars), function(i) lapply(seq_len(n_vars), function(j) make_panel(i, j))),
            recursive = FALSE
          )
          plot <- patchwork::wrap_plots(plot_list, ncol = n_vars, nrow = n_vars) &
            ggplot2::theme(
              plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
              legend.position = "none"
            )
        },
        add_cor_value = add_cor,
        cor_method_value = cor_method,
        cor_use_value = cor_use,
        alpha_value = alpha_val,
        cex_value = cex,
        varnames_value = varnames
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
      req(validated_q())
      req(merged$variables())
      ANL <- merged$data()[["ANL"]]
      cols_names <- unique(unname(do.call(c, list(unname(unlist(merged$variables()))))))
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
