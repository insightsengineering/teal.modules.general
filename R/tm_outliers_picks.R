#' @export
tm_outliers.picks <- function(label = "Outliers Module",
                              outlier_var = teal.picks::picks(
                                teal.picks::datasets(),
                                teal.picks::variables(is.numeric, 1L)
                              ),
                              categorical_var = teal.picks::picks(
                                teal.picks::datasets(),
                                teal.picks::variables(
                                  choices = teal.picks::is_categorical(min.len = 1, max.len = 10),
                                  selected = 1L
                                )
                              ),
                              ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
                              ggplot2_args = teal.widgets::ggplot2_args(),
                              plot_height = c(600, 200, 2000),
                              plot_width = NULL,
                              pre_output = NULL,
                              post_output = NULL,
                              transformators = list(),
                              decorators = list()) {
  message("Initializing tm_outliers")

  # Normalize the parameters
  if (inherits(ggplot2_args, "ggplot2_args")) ggplot2_args <- list(default = ggplot2_args)

  # Start of assertions
  checkmate::assert_string(label)

  checkmate::assert_class(outlier_var, "picks")
  if (isTRUE(attr(outlier_var$variables, "multiple"))) {
    warning("`outlier_var` accepts only a single variable selection. Forcing `teal.picks::variables(multiple)` to FALSE.")
    attr(outlier_var$variables, "multiple") <- FALSE
  }
  checkmate::assert_class(categorical_var, "picks", null.ok = TRUE)

  ggtheme <- match.arg(ggtheme)

  plot_choices <- c("Boxplot", "Density Plot", "Cumulative Distribution Plot")
  checkmate::assert_list(ggplot2_args, types = "ggplot2_args")
  checkmate::assert_subset(names(ggplot2_args), c("default", plot_choices))

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)

  teal::assert_decorators(decorators, names = c("box_plot", "density_plot", "cumulative_plot"))
  # End of assertions

  # Make UI args
  args <- as.list(environment())

  ans <- module(
    label = label,
    server = srv_outliers.picks,
    ui = ui_outliers.picks,
    ui_args = args[names(args) %in% names(formals(ui_outliers.picks))],
    server_args = args[names(args) %in% names(formals(srv_outliers.picks))],
    transformators = transformators,
    datanames = .picks_datanames(list(outlier_var, categorical_var))
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the outliers module
ui_outliers.picks <- function(id,
                              outlier_var,
                              categorical_var = NULL,
                              ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
                              pre_output = NULL,
                              post_output = NULL,
                              decorators = list()) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      uiOutput(ns("total_outliers")),
      tags$div(
        style = "overflow: auto;",
        DT::dataTableOutput(ns("summary_table"))
      ),
      uiOutput(ns("total_missing")),
      tags$br(), tags$hr(),
      tabsetPanel(
        id = ns("tabs"),
        tabPanel(
          "Boxplot",
          teal.widgets::plot_with_settings_ui(id = ns("box_plot"))
        ),
        tabPanel(
          "Density Plot",
          teal.widgets::plot_with_settings_ui(id = ns("density_plot"))
        ),
        tabPanel(
          "Cumulative Distribution Plot",
          teal.widgets::plot_with_settings_ui(id = ns("cum_density_plot"))
        )
      ),
      tags$br(), tags$hr(),
      uiOutput(ns("table_ui_wrap")),
      DT::dataTableOutput(ns("table_ui"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      tags$div(
        tags$strong("Variable"),
        teal.picks::picks_ui(id = ns("outlier_var"), picks = outlier_var)
      ),
      if (!is.null(categorical_var)) {
        tags$div(
          tags$strong("Categorical factor"),
          teal.picks::picks_ui(id = ns("categorical_var"), picks = categorical_var)
        )
      },
      conditionalPanel(
        condition = paste0("input['", ns("tabs"), "'] == 'Boxplot'"),
        teal.widgets::optionalSelectInput(
          inputId = ns("boxplot_alts"),
          label = "Plot type",
          choices = c("Box plot", "Violin plot"),
          selected = "Box plot",
          multiple = FALSE
        )
      ),
      shinyjs::hidden(checkboxInput(ns("split_outliers"), "Define outliers based on group splitting", value = FALSE)),
      shinyjs::hidden(checkboxInput(ns("order_by_outlier"), "Re-order categories by outliers [by %]", value = FALSE)),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Method parameters",
          teal.widgets::optionalSelectInput(
            inputId = ns("method"),
            label = "Method",
            choices = c("IQR", "Z-score", "Percentile"),
            selected = "IQR",
            multiple = FALSE
          ),
          conditionalPanel(
            condition =
              paste0("input['", ns("method"), "'] == 'IQR'"),
            sliderInput(
              ns("iqr_slider"),
              "Outlier range:",
              min = 1,
              max = 5,
              value = 3,
              step = 0.5
            )
          ),
          conditionalPanel(
            condition =
              paste0("input['", ns("method"), "'] == 'Z-score'"),
            sliderInput(
              ns("zscore_slider"),
              "Outlier range:",
              min = 1,
              max = 5,
              value = 3,
              step = 0.5
            )
          ),
          conditionalPanel(
            condition =
              paste0("input['", ns("method"), "'] == 'Percentile'"),
            sliderInput(
              ns("percentile_slider"),
              "Outlier range:",
              min = 0.001,
              max = 0.5,
              value = 0.01,
              step = 0.001
            )
          ),
          uiOutput(ns("ui_outlier_help"))
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("tabs"), "'] == 'Boxplot'"),
        teal::ui_transform_teal_data(
          ns("d_box_plot"),
          transformators = select_decorators(decorators, "box_plot")
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("tabs"), "'] == 'Density Plot'"),
        teal::ui_transform_teal_data(
          ns("d_density_plot"),
          transformators = select_decorators(decorators, "density_plot")
        )
      ),
      conditionalPanel(
        condition = paste0("input['", ns("tabs"), "'] == 'Cumulative Distribution Plot'"),
        teal::ui_transform_teal_data(
          ns("d_cumulative_plot"),
          transformators = select_decorators(decorators, "cumulative_plot")
        )
      ),
      bslib::accordion(
        open = TRUE,
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
    pre_output = pre_output,
    post_output = post_output
  )
}

# Server function for the outliers module
srv_outliers.picks <- function(id,
                               data,
                               outlier_var,
                               categorical_var,
                               plot_height,
                               plot_width,
                               ggplot2_args,
                               decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    ns <- session$ns

    # Used to create outlier table and the dropdown with additional columns.
    # A dummy `.row_id` join key is added when the data has no join keys so that
    # the outlier table can be extended with additional columns from the original dataset.
    data_obj <- reactive({
      obj <- data()
      dataname_first <- names(obj)[[1]]
      if (length(teal.data::join_keys(obj)) == 0) {
        if (!".row_id" %in% names(obj[[dataname_first]])) {
          obj[[dataname_first]]$.row_id <- seq_len(nrow(obj[[dataname_first]]))
        }
        teal.data::join_keys(obj) <-
          teal.data::join_keys(teal.data::join_key(dataname_first, dataname_first, ".row_id"))
      }
      obj
    })

    selectors <- teal.picks::picks_srv(
      picks = Filter(
        Negate(is.null),
        list(outlier_var = outlier_var, categorical_var = categorical_var)
      ),
      data = data_obj
    )

    # dataset holding the outlier variable, used to fetch additional columns
    outlier_dataname <- reactive(selectors$outlier_var()$datasets$selected)

    validated_q <- reactive({
      teal::validate_input(
        inputId = "outlier_var-variables-selected",
        condition = length(selectors$outlier_var()$variables$selected) == 1,
        message = "Please select a variable"
      )
      if (!is.null(categorical_var) && length(selectors$categorical_var()$variables$selected) > 0) {
        teal::validate_input(
          inputId = "categorical_var-variables-selected",
          condition = !identical(
            selectors$outlier_var()$variables$selected,
            selectors$categorical_var()$variables$selected
          ),
          message = "`Variable` and `Categorical factor` cannot be the same"
        )
      }

      obj <- req(data_obj())
      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card("# Outliers Module"),
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's code")
      )
      teal.code::eval_code(obj, "library(dplyr);library(tidyr);library(tibble);library(ggplot2)")
    })

    merged <- teal.picks::merge_srv("merge", data = validated_q, selectors = selectors, output_name = "ANL")

    n_outlier_missing <- reactive({
      req(merged$data())
      outlier_var <- merged$variables()$outlier_var
      ANL <- merged$data()[["ANL"]]
      sum(is.na(ANL[[outlier_var]]))
    })

    common_code_q <- reactive({
      req(merged$data(), input$method)

      qenv <- merged$data()
      ANL <- qenv[["ANL"]]
      teal.reporter::teal_card(qenv) <-
        c(
          teal.reporter::teal_card(qenv),
          teal.reporter::teal_card("## Module's output(s)")
        )

      outlier_var <- merged$variables()$outlier_var
      categorical_var <- merged$variables()$categorical_var
      order_by_outlier <- input$order_by_outlier
      method <- input$method
      split_outliers <- input$split_outliers
      teal::validate_has_data(
        # missing values in the categorical variable may be used to form a category of its own
        `if`(
          length(categorical_var) == 0,
          ANL,
          ANL[, names(ANL) != categorical_var, drop = FALSE]
        ),
        min_nrow = 10,
        complete = TRUE,
        allow_inf = FALSE
      )
      validate(need(is.numeric(ANL[[outlier_var]]), "`Variable` is not numeric"))
      validate(need(length(unique(ANL[[outlier_var]])) > 1, "Variable has no variation, i.e. only one unique value"))

      # show/hide split_outliers
      if (length(categorical_var) == 0) {
        shinyjs::hide("split_outliers")
        if (n_outlier_missing() > 0) {
          qenv <- teal.code::eval_code(
            qenv,
            substitute(
              expr = ANL <- ANL %>% dplyr::filter(!is.na(outlier_var_name)),
              env = list(outlier_var_name = as.name(outlier_var))
            )
          )
        }
      } else {
        validate(need(
          is.factor(ANL[[categorical_var]]) ||
            is.character(ANL[[categorical_var]]) ||
            is.integer(ANL[[categorical_var]]),
          "`Categorical factor` must be `factor`, `character`, or `integer`"
        ))

        if (n_outlier_missing() > 0) {
          qenv <- teal.code::eval_code(
            qenv,
            substitute(
              expr = ANL <- ANL %>% dplyr::filter(!is.na(outlier_var_name)),
              env = list(outlier_var_name = as.name(outlier_var))
            )
          )
        }
        shinyjs::show("split_outliers")
      }

      # slider
      outlier_definition_param <- if (method == "IQR") {
        input$iqr_slider
      } else if (method == "Z-score") {
        input$zscore_slider
      } else if (method == "Percentile") {
        input$percentile_slider
      }

      # this is utils function that converts a %>% NULL %>% b into a %>% b
      remove_pipe_null <- function(x) {
        if (length(x) == 1) {
          x
        } else if (identical(x[[1]], as.name("%>%")) && is.null(x[[3]])) {
          remove_pipe_null(x[[2]])
        } else {
          as.call(c(x[[1]], lapply(x[-1], remove_pipe_null)))
        }
      }

      qenv <- teal.code::eval_code(
        qenv,
        substitute(
          expr = {
            ANL_OUTLIER <- ANL %>%
              group_expr %>% # styler: off
              dplyr::mutate(is_outlier = {
                q1_q3 <- stats::quantile(outlier_var_name, probs = c(0.25, 0.75))
                iqr <- q1_q3[2] - q1_q3[1]
                !(outlier_var_name >= q1_q3[1] - 1.5 * iqr & outlier_var_name <= q1_q3[2] + 1.5 * iqr)
              }) %>%
              calculate_outliers %>% # styler: off
              ungroup_expr %>% # styler: off
              dplyr::filter(is_outlier | is_outlier_selected) %>%
              dplyr::select(-is_outlier)
          },
          env = list(
            calculate_outliers = if (method == "IQR") {
              substitute(
                expr = dplyr::mutate(is_outlier_selected = {
                  q1_q3 <- stats::quantile(outlier_var_name, probs = c(0.25, 0.75))
                  iqr <- q1_q3[2] - q1_q3[1]
                  !(
                    outlier_var_name >= q1_q3[1] - outlier_definition_param * iqr &
                      outlier_var_name <= q1_q3[2] + outlier_definition_param * iqr
                  )
                }),
                env = list(
                  outlier_var_name = as.name(outlier_var),
                  outlier_definition_param = outlier_definition_param
                )
              )
            } else if (method == "Z-score") {
              substitute(
                expr = dplyr::mutate(
                  is_outlier_selected = abs(outlier_var_name - mean(outlier_var_name)) /
                    stats::sd(outlier_var_name) > outlier_definition_param
                ),
                env = list(
                  outlier_var_name = as.name(outlier_var),
                  outlier_definition_param = outlier_definition_param
                )
              )
            } else if (method == "Percentile") {
              substitute(
                expr = dplyr::mutate(
                  is_outlier_selected = outlier_var_name < stats::quantile(outlier_var_name, outlier_definition_param) |
                    outlier_var_name > stats::quantile(outlier_var_name, 1 - outlier_definition_param)
                ),
                env = list(
                  outlier_var_name = as.name(outlier_var),
                  outlier_definition_param = outlier_definition_param
                )
              )
            },
            outlier_var_name = as.name(outlier_var),
            group_expr = if (isTRUE(split_outliers) && length(categorical_var) != 0) {
              substitute(dplyr::group_by(x), list(x = as.name(categorical_var)))
            },
            ungroup_expr = if (isTRUE(split_outliers) && length(categorical_var) != 0) {
              substitute(dplyr::ungroup())
            }
          )
        ) %>%
          remove_pipe_null()
      )

      # ANL_OUTLIER_EXTENDED is the base table
      dataname_first <- outlier_dataname()
      join_keys <- as.character(teal.data::join_keys(data_obj())[dataname_first, dataname_first])

      if (length(join_keys) == 1 && join_keys == ".row_id") {
        # Dummy join key - single dataset, no join needed
        qenv <- teal.code::eval_code(qenv, quote(ANL_OUTLIER_EXTENDED <- ANL_OUTLIER))
      } else {
        # Join keys exist - perform left join
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = {
              ANL_OUTLIER_EXTENDED <- dplyr::left_join(
                ANL_OUTLIER,
                dplyr::select(
                  dataname,
                  dplyr::setdiff(names(dataname), dplyr::setdiff(names(ANL_OUTLIER), join_keys))
                ),
                by = join_keys
              )
            },
            env = list(
              dataname = as.name(dataname_first),
              join_keys = join_keys
            )
          )
        )
      }

      teal.reporter::teal_card(qenv) <- c(teal.reporter::teal_card(qenv), "### Summary Table")
      qenv <- if (length(categorical_var) > 0) {
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = summary_data_pre <- ANL_OUTLIER %>%
              dplyr::filter(is_outlier_selected) %>%
              dplyr::select(outlier_var_name, categorical_var_name) %>%
              dplyr::group_by(categorical_var_name) %>%
              dplyr::summarise(n_outliers = dplyr::n()) %>%
              dplyr::right_join(
                ANL %>%
                  dplyr::select(outlier_var_name, categorical_var_name) %>%
                  dplyr::group_by(categorical_var_name) %>%
                  dplyr::summarise(
                    total_in_cat = dplyr::n(),
                    n_na = sum(is.na(outlier_var_name) | is.na(categorical_var_name))
                  ),
                by = categorical_var
              ) %>%
              # This is important as there may be categorical variables with natural orderings, e.g. AGE.
              # The plots should be displayed by default in increasing order in these situations.
              # dplyr::arrange will sort integer, factor, and character data types in the expected way.
              dplyr::arrange(categorical_var_name) %>%
              dplyr::mutate(
                n_outliers = dplyr::if_else(is.na(n_outliers), 0, as.numeric(n_outliers)),
                display_str = dplyr::if_else(
                  n_outliers > 0,
                  sprintf("%d [%.02f%%]", n_outliers, 100 * n_outliers / total_in_cat),
                  "0"
                ),
                display_str_na = dplyr::if_else(
                  n_na > 0,
                  sprintf("%d [%.02f%%]", n_na, 100 * n_na / total_in_cat),
                  "0"
                ),
                order = seq_along(n_outliers)
              ),
            env = list(
              categorical_var = categorical_var,
              categorical_var_name = as.name(categorical_var),
              outlier_var_name = as.name(outlier_var)
            )
          )
        )
        # now to handle when user chooses to order based on amount of outliers
        if (order_by_outlier) {
          qenv <- teal.code::eval_code(
            qenv,
            quote(
              summary_data_pre <- summary_data_pre %>%
                dplyr::arrange(desc(n_outliers / total_in_cat)) %>%
                dplyr::mutate(order = seq_len(nrow(summary_data_pre)))
            )
          )
        }

        teal.code::eval_code(
          qenv,
          substitute(
            expr = {
              # In order for geom_rug to work properly when reordering takes place inside facet_grid,
              # all tables must have the column used for reording.
              # In this case, the column used for reordering is `order`.
              ANL_OUTLIER <- dplyr::left_join(
                ANL_OUTLIER,
                summary_data_pre[, c("order", categorical_var)],
                by = categorical_var
              )
              # so that x axis of plot aligns with columns of summary table, from most outliers to least by percentage
              ANL <- ANL %>%
                dplyr::left_join(
                  dplyr::select(summary_data_pre, categorical_var_name, order),
                  by = categorical_var
                ) %>%
                dplyr::arrange(order)
              summary_data <- summary_data_pre %>%
                dplyr::select(
                  categorical_var_name,
                  Outliers = display_str, Missings = display_str_na, Total = total_in_cat
                ) %>%
                dplyr::mutate_all(as.character) %>%
                tidyr::pivot_longer(-categorical_var_name) %>%
                tidyr::pivot_wider(names_from = categorical_var, values_from = value) %>%
                tibble::column_to_rownames("name")
            },
            env = list(
              categorical_var = categorical_var,
              categorical_var_name = as.name(categorical_var)
            )
          )
        ) |>
          within({
            table <- rtables::df_to_tt(summary_data)
            table
          })
      } else {
        msg <- "No categorical variable selected, summary table cannot be created."
        showNotification(msg,
          closeButton = FALSE, type = "warning",
          id = session$ns("no_summary_table")
        )
        within(qenv, cat(msg), msg = msg)
      }


      if (length(categorical_var) > 0 && nrow(qenv[["ANL_OUTLIER"]]) > 0) {
        shinyjs::show("order_by_outlier")
      } else {
        shinyjs::hide("order_by_outlier")
      }

      qenv
    })

    # boxplot/violinplot # nolint commented_code
    box_plot_q <- reactive({
      req(common_code_q())
      qenv <- common_code_q()
      teal.reporter::teal_card(qenv) <- c(teal.reporter::teal_card(qenv), "### Box Plot")

      ANL <- qenv[["ANL"]]
      ANL_OUTLIER <- qenv[["ANL_OUTLIER"]]

      outlier_var <- merged$variables()$outlier_var
      categorical_var <- merged$variables()$categorical_var

      # validation
      teal::validate_has_data(ANL, 1)

      # boxplot
      plot_call <- quote(ANL %>% ggplot())

      plot_call <- if (input$boxplot_alts == "Box plot") {
        substitute(expr = plot_call + ggplot2::geom_boxplot(outlier.shape = NA), env = list(plot_call = plot_call))
      } else if (input$boxplot_alts == "Violin plot") {
        substitute(expr = plot_call + ggplot2::geom_violin(), env = list(plot_call = plot_call))
      } else {
        NULL
      }

      plot_call <- if (identical(categorical_var, character(0)) || is.null(categorical_var)) {
        inner_call <- substitute(
          expr = plot_call +
            ggplot2::aes(x = "Entire dataset", y = outlier_var_name) +
            ggplot2::scale_x_discrete(),
          env = list(plot_call = plot_call, outlier_var_name = as.name(outlier_var))
        )
        if (nrow(ANL_OUTLIER) > 0) {
          substitute(
            expr = inner_call + ggplot2::geom_point(
              data = ANL_OUTLIER,
              ggplot2::aes(x = "Entire dataset", y = outlier_var_name, color = is_outlier_selected)
            ),
            env = list(inner_call = inner_call, outlier_var_name = as.name(outlier_var))
          )
        } else {
          inner_call
        }
      } else {
        substitute(
          expr = plot_call +
            ggplot2::aes(y = outlier_var_name, x = reorder(categorical_var_name, order)) +
            ggplot2::xlab(categorical_var) +
            ggplot2::scale_x_discrete() +
            ggplot2::geom_point(
              data = ANL_OUTLIER,
              ggplot2::aes(x = as.factor(categorical_var_name), y = outlier_var_name, color = is_outlier_selected)
            ),
          env = list(
            plot_call = plot_call,
            outlier_var_name = as.name(outlier_var),
            categorical_var_name = as.name(categorical_var),
            categorical_var = categorical_var
          )
        )
      }

      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        labs = list(color = "Is outlier?"),
        theme = list(legend.position = "top")
      )

      all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Boxplot"]],
        user_default = ggplot2_args$default,
        module_plot = dev_ggplot2_args
      )

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args,
        ggtheme = input$ggtheme
      )

      teal.code::eval_code(
        qenv,
        substitute(
          expr = box_plot <- plot_call +
            ggplot2::scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
            labs + ggthemes + themes,
          env = list(
            plot_call = plot_call,
            labs = parsed_ggplot2_args$labs,
            ggthemes = parsed_ggplot2_args$ggtheme,
            themes = parsed_ggplot2_args$theme
          )
        )
      )
    })

    # density plot
    density_plot_q <- reactive({
      qenv <- common_code_q()
      teal.reporter::teal_card(qenv) <- c(teal.reporter::teal_card(qenv), "### Density Plot")

      ANL <- qenv[["ANL"]]
      ANL_OUTLIER <- qenv[["ANL_OUTLIER"]]

      outlier_var <- merged$variables()$outlier_var
      categorical_var <- merged$variables()$categorical_var

      # validation
      teal::validate_has_data(ANL, 1)
      # plot
      plot_call <- substitute(
        expr = ANL %>%
          ggplot2::ggplot(ggplot2::aes(x = outlier_var_name)) +
          ggplot2::geom_density() +
          ggplot2::geom_rug(data = ANL_OUTLIER, ggplot2::aes(x = outlier_var_name, color = is_outlier_selected)) +
          ggplot2::scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")),
        env = list(outlier_var_name = as.name(outlier_var))
      )

      plot_call <- if (identical(categorical_var, character(0)) || is.null(categorical_var)) {
        substitute(expr = plot_call, env = list(plot_call = plot_call))
      } else {
        substitute(
          expr = plot_call + ggplot2::facet_grid(~ reorder(categorical_var_name, order)),
          env = list(plot_call = plot_call, categorical_var_name = as.name(categorical_var))
        )
      }

      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        labs = list(color = "Is outlier?"),
        theme = list(legend.position = "top")
      )

      all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Density Plot"]],
        user_default = ggplot2_args$default,
        module_plot = dev_ggplot2_args
      )

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args,
        ggtheme = input$ggtheme
      )

      teal.code::eval_code(
        qenv,
        substitute(
          expr = density_plot <- plot_call + labs + ggthemes + themes,
          env = list(
            plot_call = plot_call,
            labs = parsed_ggplot2_args$labs,
            themes = parsed_ggplot2_args$theme,
            ggthemes = parsed_ggplot2_args$ggtheme
          )
        )
      )
    })

    # Cumulative distribution plot
    cumulative_plot_q <- reactive({
      qenv <- common_code_q()
      teal.reporter::teal_card(qenv) <- c(teal.reporter::teal_card(qenv), "### Cumulative Distribution Plot")

      ANL <- qenv[["ANL"]]
      ANL_OUTLIER <- qenv[["ANL_OUTLIER"]]

      outlier_var <- merged$variables()$outlier_var
      categorical_var <- merged$variables()$categorical_var

      # validation
      teal::validate_has_data(ANL, 1)

      # plot
      plot_call <- substitute(
        expr = ANL %>% ggplot2::ggplot(ggplot2::aes(x = outlier_var_name)) +
          ggplot2::stat_ecdf(),
        env = list(outlier_var_name = as.name(outlier_var))
      )
      if (length(categorical_var) == 0) {
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = {
              ecdf_df <- ANL %>%
                dplyr::mutate(
                  y = stats::ecdf(ANL[[outlier_var]])(ANL[[outlier_var]])
                )

              outlier_points <- dplyr::left_join(
                ecdf_df,
                ANL_OUTLIER,
                by = dplyr::setdiff(names(ecdf_df), "y")
              ) %>%
                dplyr::filter(!is.na(is_outlier_selected))
            },
            env = list(outlier_var = outlier_var)
          )
        )
      } else {
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = {
              all_categories <- lapply(
                unique(ANL[[categorical_var]]),
                function(x) {
                  ANL <- ANL %>% dplyr::filter(get(categorical_var) == x)
                  anl_outlier2 <- ANL_OUTLIER %>% dplyr::filter(get(categorical_var) == x)
                  ecdf_df <- ANL %>%
                    dplyr::mutate(y = stats::ecdf(ANL[[outlier_var]])(ANL[[outlier_var]]))

                  dplyr::left_join(
                    ecdf_df,
                    anl_outlier2,
                    by = dplyr::setdiff(names(ecdf_df), "y")
                  ) %>%
                    dplyr::filter(!is.na(is_outlier_selected))
                }
              )
              outlier_points <- do.call(rbind, all_categories)
            },
            env = list(categorical_var = categorical_var, outlier_var = outlier_var)
          )
        )
        plot_call <- substitute(
          expr = plot_call + ggplot2::facet_grid(~ reorder(categorical_var_name, order)),
          env = list(plot_call = plot_call, categorical_var_name = as.name(categorical_var))
        )
      }

      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        labs = list(color = "Is outlier?"),
        theme = list(legend.position = "top")
      )

      all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Cumulative Distribution Plot"]],
        user_default = ggplot2_args$default,
        module_plot = dev_ggplot2_args
      )

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args,
        ggtheme = input$ggtheme
      )

      teal.code::eval_code(
        qenv,
        substitute(
          expr = cumulative_plot <- plot_call +
            ggplot2::geom_point(
              data = outlier_points,
              ggplot2::aes(x = outlier_var_name, y = y, color = is_outlier_selected)
            ) +
            ggplot2::scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
            labs + ggthemes + themes,
          env = list(
            plot_call = plot_call,
            outlier_var_name = as.name(outlier_var),
            labs = parsed_ggplot2_args$labs,
            themes = parsed_ggplot2_args$theme,
            ggthemes = parsed_ggplot2_args$ggtheme
          )
        )
      )
    })

    current_tab_r <- reactive({
      switch(req(input$tabs),
        "Boxplot" = "box_plot",
        "Density Plot" = "density_plot",
        "Cumulative Distribution Plot" = "cumulative_plot"
      )
    })

    decorated_q <- mapply(
      function(obj_name, q) {
        teal::srv_transform_teal_data(
          id = sprintf("d_%s", obj_name),
          data = q,
          transformators = select_decorators(decorators, obj_name),
          expr = reactive({
            substitute(
              expr = {
                columns_index <- union(
                  setdiff(names(ANL_OUTLIER), c("is_outlier_selected", "order")),
                  table_columns
                )
                ANL_OUTLIER_EXTENDED[ANL_OUTLIER_EXTENDED$is_outlier_selected, columns_index]
                print(.plot)
              },
              env = list(table_columns = input$table_ui_columns, .plot = as.name(obj_name))
            )
          })
        )
      },
      stats::setNames(nm = c("box_plot", "density_plot", "cumulative_plot")),
      c(box_plot_q, density_plot_q, cumulative_plot_q)
    )

    box_plot_r <- reactive({
      req(decorated_q$box_plot())[["box_plot"]]
    })
    density_plot_r <- reactive({
      req(decorated_q$density_plot())[["density_plot"]]
    })
    cumulative_plot_r <- reactive({
      req(decorated_q$cumulative_plot())[["cumulative_plot"]]
    })

    box_pws <- teal.widgets::plot_with_settings_srv(
      id = "box_plot",
      plot_r = box_plot_r,
      height = plot_height,
      width = plot_width,
      brushing = TRUE
    )

    density_pws <- teal.widgets::plot_with_settings_srv(
      id = "density_plot",
      plot_r = density_plot_r,
      height = plot_height,
      width = plot_width,
      brushing = TRUE
    )

    cum_density_pws <- teal.widgets::plot_with_settings_srv(
      id = "cum_density_plot",
      plot_r = cumulative_plot_r,
      height = plot_height,
      width = plot_width,
      brushing = TRUE
    )

    pws_list <- list(box_plot = box_pws, density_plot = density_pws, cumulative_plot = cum_density_pws)
    decorated_final_q <- reactive({
      pws <- pws_list[[req(current_tab_r())]]
      req(pws$dim())
      req(decorated_q[[current_tab_r()]]())
      set_chunk_dims(pws, decorated_q[[current_tab_r()]])()
    })

    summary_table_r <- reactive({
      q <- req(decorated_final_q())

      DT::datatable(
        data = {
          categorical_var <- merged$variables()$categorical_var
          if (!is.null(categorical_var)) q[["summary_data"]]
        },
        options = list(
          dom = "t",
          autoWidth = TRUE,
          columnDefs = list(list(width = "200px", targets = "_all"))
        )
      )
    })

    output$summary_table <- DT::renderDataTable(summary_table_r())

    # slider text
    output$ui_outlier_help <- renderUI({
      req(input$method)
      if (input$method == "IQR") {
        req(input$iqr_slider)
        tags$small(
          withMathJax(
            helpText(
              "Outlier data points (\\(x \\lt Q1 - ", input$iqr_slider, "\\times IQR\\) or \\(
            Q3 + ", input$iqr_slider, "\\times IQR \\lt x\\))
            are displayed in red on the plot and can be visualized in the table below."
            ),
            if (input$split_outliers) {
              withMathJax(helpText("Note: Quantiles are calculated per group."))
            }
          )
        )
      } else if (input$method == "Z-score") {
        req(input$zscore_slider)
        tags$small(
          withMathJax(
            helpText(
              "Outlier data points (\\(Zscore(x) < -", input$zscore_slider,
              "\\) or \\(", input$zscore_slider, "< Zscore(x) \\))
              are displayed in red on the plot and can be visualized in the table below."
            ),
            if (input$split_outliers) {
              withMathJax(helpText(" Note: Z-scores are calculated per group."))
            }
          )
        )
      } else if (input$method == "Percentile") {
        req(input$percentile_slider)
        tags$small(
          withMathJax(
            helpText(
              "Outlier/extreme data points (\\( Percentile(x) <", input$percentile_slider,
              "\\) or \\(", 1 - input$percentile_slider, " < Percentile(x) \\))
            are displayed in red on the plot and can be visualized in the table below."
            ),
            if (input$split_outliers) {
              withMathJax(helpText("Note: Percentiles are calculated per group."))
            }
          )
        )
      }
    })

    choices <- reactive(teal.transform::variable_choices(data_obj()[[outlier_dataname()]]))

    observeEvent(common_code_q(), {
      ANL_OUTLIER <- common_code_q()[["ANL_OUTLIER"]]
      teal.widgets::updateOptionalSelectInput(
        session,
        inputId = "table_ui_columns",
        choices = dplyr::setdiff(choices(), names(ANL_OUTLIER)),
        selected = restoreInput(ns("table_ui_columns"), isolate(input$table_ui_columns))
      )
    })

    output$table_ui <- DT::renderDataTable(
      expr = {
        tab <- input$tabs
        req(tab) # tab is NULL upon app launch, hence will crash without this statement
        req(common_code_q())
        outlier_var <- merged$variables()$outlier_var
        categorical_var <- merged$variables()$categorical_var

        ANL_OUTLIER <- common_code_q()[["ANL_OUTLIER"]]
        ANL_OUTLIER_EXTENDED <- common_code_q()[["ANL_OUTLIER_EXTENDED"]]
        ANL <- common_code_q()[["ANL"]]

        plot_brush <- switch(current_tab_r(),
          box_plot = {
            box_plot_r()
            box_pws$brush()
          },
          density_plot = {
            density_plot_r()
            density_pws$brush()
          },
          cumulative_plot = {
            cumulative_plot_r()
            cum_density_pws$brush()
          }
        )

        # removing unused column ASAP
        ANL_OUTLIER$order <- ANL$order <- NULL

        display_table <- if (!is.null(plot_brush)) {
          if (length(categorical_var) > 0) {
            # due to reordering, the x-axis label may be changed to something like "reorder(categorical_var, order)"
            if (tab == "Boxplot") {
              plot_brush$mapping$x <- categorical_var
            } else {
              # the other plots use facetting
              # so it is panelvar1 that gets relabelled to "reorder(categorical_var, order)"
              plot_brush$mapping$panelvar1 <- categorical_var
            }
          } else {
            if (tab == "Boxplot") {
              # in boxplot with no categorical variable, there is no column in ANL that would correspond to x-axis
              # so a column needs to be inserted with the value "Entire dataset" because that's the label used in plot
              ANL[[plot_brush$mapping$x]] <- "Entire dataset"
            }
          }

          # in density and cumulative plots, ANL does not have a column corresponding to y-axis.
          # so they need to be computed and attached to ANL
          if (tab == "Density Plot") {
            plot_brush$mapping$y <- "density"
            ANL$density <- plot_brush$ymin
            # either ymin or ymax will work
          } else if (tab == "Cumulative Distribution Plot") {
            plot_brush$mapping$y <- "cdf"
            if (length(categorical_var) > 0) {
              ANL <- ANL %>%
                dplyr::group_by(!!as.name(plot_brush$mapping$panelvar1)) %>%
                dplyr::mutate(cdf = stats::ecdf(!!as.name(outlier_var))(!!as.name(outlier_var)))
            } else {
              ANL$cdf <- stats::ecdf(ANL[[outlier_var]])(ANL[[outlier_var]])
            }
          }

          brushed_rows <- brushedPoints(ANL, plot_brush)
          if (nrow(brushed_rows) > 0) {
            # now we need to remove extra column from ANL so that it will have the same columns as ANL_OUTLIER
            # so that dplyr::intersect will work
            if (tab == "Density Plot") {
              brushed_rows$density <- NULL
            } else if (tab == "Cumulative Distribution Plot") {
              brushed_rows$cdf <- NULL
            } else if (tab == "Boxplot" && length(categorical_var) == 0) {
              brushed_rows[[plot_brush$mapping$x]] <- NULL
            }
            # is_outlier_selected is part of ANL_OUTLIER so needed here
            brushed_rows$is_outlier_selected <- TRUE
            dplyr::intersect(ANL_OUTLIER, brushed_rows)
          } else {
            ANL_OUTLIER[0, ]
          }
        } else {
          ANL_OUTLIER[ANL_OUTLIER$is_outlier_selected, ]
        }

        display_table$is_outlier_selected <- NULL

        # Extend the brushed ANL_OUTLIER with additional columns
        dplyr::left_join(
          display_table,
          dplyr::select(ANL_OUTLIER_EXTENDED, -"is_outlier_selected"),
          by = names(display_table)
        ) %>%
          dplyr::select(union(names(display_table), input$table_ui_columns))
      },
      options = list(
        searching = FALSE, language = list(
          zeroRecords = "The brushed area does not contain outlier observations for the currently defined threshold"
        ),
        pageLength = input$table_ui_rows
      )
    )

    output$total_outliers <- renderUI({
      req(common_code_q())
      ANL <- merged$data()[["ANL"]]
      ANL_OUTLIER <- common_code_q()[["ANL_OUTLIER"]]
      teal::validate_has_data(ANL, 1)
      ANL_OUTLIER_SELECTED <- ANL_OUTLIER[ANL_OUTLIER$is_outlier_selected, ]
      tags$h5(
        sprintf(
          "%s %d / %d [%.02f%%]",
          "Total number of outlier(s):",
          nrow(ANL_OUTLIER_SELECTED),
          nrow(ANL),
          100 * nrow(ANL_OUTLIER_SELECTED) / nrow(ANL)
        )
      )
    })

    output$total_missing <- renderUI({
      if (n_outlier_missing() > 0) {
        ANL <- merged$data()[["ANL"]]
        helpText(
          sprintf(
            "%s %d / %d [%.02f%%]",
            "Total number of row(s) with missing values:",
            n_outlier_missing(),
            nrow(ANL),
            100 * (n_outlier_missing()) / nrow(ANL)
          )
        )
      }
    })

    output$table_ui_wrap <- renderUI({
      req(common_code_q())
      tagList(
        teal.widgets::optionalSelectInput(
          inputId = ns("table_ui_columns"),
          label = "Choose additional columns",
          choices = NULL,
          selected = NULL,
          multiple = TRUE
        ),
        tags$h4("Outlier Table"),
        teal.widgets::get_dt_rows(ns("table_ui"), ns("table_ui_rows"))
      )
    })

    decorated_final_q
  })
}
