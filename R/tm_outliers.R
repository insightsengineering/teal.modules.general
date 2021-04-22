#' Outliers Module
#'
#' Module to analyze and identify outliers using different methods
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#'
#' @param outlier_var (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'  variable to consider for the outliers analysis.
#' @param categorical_var (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   categorical factor to split the selected outliers variable on.
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#'
#' fact_vars_adsl <- names(Filter(isTRUE, sapply(ADSL, is.factor)))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_outliers(
#'       outlier_var = list(
#'         data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             label = "Select variable:",
#'             choices = variable_choices(ADSL, c("AGE", "BMRKR1")),
#'             selected = "AGE",
#'             multiple = FALSE,
#'             fixed = FALSE
#'           )
#'         )
#'       ),
#'       categorical_var = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = variable_choices(ADSL, subset = fact_vars_adsl),
#'           selected = "RACE",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_outliers <- function(label = "Outliers Module",
                        outlier_var,
                        categorical_var = NULL,
                        plot_height = c(600, 200, 2000),
                        plot_width = NULL,
                        pre_output = NULL,
                        post_output = NULL) {
  if (!is_class_list("data_extract_spec")(outlier_var)) {
    outlier_var <- list(outlier_var)
  }
  if (!is.null(categorical_var) && !is_class_list("data_extract_spec")(categorical_var)) {
    categorical_var <- list(categorical_var)
  }

  stop_if_not(
    is_character_single(label),
    is_class_list("data_extract_spec")(outlier_var),
    is.null(categorical_var) || is_class_list("data_extract_spec")(categorical_var)
  )

  args <- as.list(environment())

  data_extract_list <- list(
    outlier_var = outlier_var,
    categorical_var = categorical_var
  )

  module(
    label = label,
    server = srv_outliers,
    server_args = c(data_extract_list, list(plot_height = plot_height, plot_width = plot_width)),
    ui = ui_outliers,
    ui_args = args,
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @importFrom DT datatable
ui_outliers <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  is_single_dataset_value <- is_single_dataset(args$outlier_var, args$categorical_var)

  standard_layout(
    output = white_small_well(
      uiOutput(ns("total_outliers")),
      DT::dataTableOutput(ns("summary_table")),
      uiOutput(ns("total_missing")),
      br(), hr(),
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("Boxplot", plot_with_settings_ui(id = ns("box_plot"))),
        tabPanel("Density plot", plot_with_settings_ui(id = ns("density_plot"))),
        tabPanel("Cumulative distribution plot", plot_with_settings_ui(id = ns("cum_density_plot")))
      ),
      br(), hr(),
      optionalSelectInput(
        inputId = ns("table_ui_columns"),
        label = "Choose additional columns",
        choices = NULL,
        selected = NULL,
        multiple = TRUE
      ),
      h4("Outlier Table"),
      get_dt_rows(ns("table_ui"), ns("table_ui_rows")),
      DT::dataTableOutput(ns("table_ui"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("outlier_var", "categorical_var")]),
      data_extract_input(
        id = ns("outlier_var"),
        label = "Variable",
        data_extract_spec = args$outlier_var,
        is_single_dataset = is_single_dataset_value
      ),
      if (!is.null(args$categorical_var)) {
        tagList(
          data_extract_input(
            id = ns("categorical_var"),
            label = "Categorial factor",
            data_extract_spec = args$categorical_var,
            is_single_dataset = is_single_dataset_value
          ),
          uiOutput(ns("categorical_var_levels_ui"))
        )
      },
      conditionalPanel(
        condition = paste0("input['", ns("tabs"), "'] == 'Boxplot'"),
        optionalSelectInput(
          inputId = ns("boxplot_alts"),
          label = "Plot type",
          choices = c("Box plot", "Violin plot"),
          selected = "Box plot",
          multiple = FALSE
        )
      ),
      shinyjs::hidden(checkboxInput(ns("split_outliers"), "Define outliers based on group splitting", value = FALSE)),
      shinyjs::hidden(checkboxInput(ns("order_by_outlier"), "Re-order categories by outliers [by %]", value = FALSE)),
      panel_group(
        panel_item(
          title = "Method parameters",
          collapsed = FALSE,
          optionalSelectInput(
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
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

#' @importFrom stats quantile sd
#' @importFrom dplyr left_join right_join filter group_by summarise n if_else arrange mutate select bind_rows setdiff
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
#' @importFrom shinyjs hide show
#' @importFrom stats ecdf
srv_outliers <- function(input, output, session, datasets, outlier_var,
                         categorical_var, plot_height, plot_width) {
  init_chunks()

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(outlier_var, categorical_var),
    input_id = c("outlier_var", "categorical_var"),
    # left_join is used instead of inner_join
    merge_function = "dplyr::left_join"
  )

  output$categorical_var_levels_ui <- renderUI({
    categorical_var <- merged_data()$columns_source$categorical_var
    req(categorical_var)
    data <- merged_data()$data()
    req(nrow(data) > 0)
    data_no_cat_na <- data[!is.na(data[[categorical_var]]), ]
    choices <- value_choices(data_no_cat_na, categorical_var)
    if (anyNA(data[[categorical_var]])) {
      choices <- c(choices, "(Missing)")
    }
    optionalSelectInput(
      inputId = session$ns("categorical_var_levels"),
      label = "Categories to include",
      choices = choices,
      selected = choices,
      multiple = TRUE
    )
  })

  common_code_chunks <- reactive({
    # Create a private stack for this function only.
    common_stack <- chunks$new()

    common_stack_push <- function(...) {
      chunks_push(..., chunks = common_stack)
    }

    chunks_push_data_merge(merged_data(), common_stack)
    outlier_var <- as.vector(merged_data()$columns_source$outlier_var)
    categorical_var <- as.vector(merged_data()$columns_source$categorical_var)
    order_by_outlier <- input$order_by_outlier # nolint
    method <- input$method
    split_outliers <- input$split_outliers

    validate(need(outlier_var, "Please select a variable"))
    chunks_validate_custom(
      bquote(length(unique(ANL[[.(outlier_var)]])) > 1),
      msg = "Variable has no variation, i.e. only one unique value",
      chunks = common_stack
    )
    validate(need(input$method, "Please select a method"))
    validate(need(is.numeric(merged_data()$data()[[outlier_var]]), "`Variable` is not numeric"))
    validate_has_data(merged_data()$data(), min_nrow = 10, complete = TRUE, allow_inf = FALSE)

    # show/hide split_outliers
    if (is_empty(categorical_var)) {
      shinyjs::hide("split_outliers")
      contains_na <- anyNA(merged_data()$data()[, outlier_var])
      if (contains_na) {
        common_stack_push(bquote({
          ANL_NO_NA <- ANL %>% dplyr::filter(!is.na(.(as.name(outlier_var)))) # nolint
        }))
      }
    } else {
      validate(need(input$categorical_var_levels, "Please select categories to include"))

      validate(need(
        is.factor(merged_data()$data()[[categorical_var]]) ||
          is.character(merged_data()$data()[[categorical_var]]) ||
          is.integer(merged_data()$data()[[categorical_var]]),
        "`Categorical factor` must be `factor`, `character`, or `integer`"
      ))
      validate(need(outlier_var != categorical_var, "`Variable` and `Categorical factor` cannot be the same"))

      if ("(Missing)" %in% input$categorical_var_levels) {
        common_stack_push(
          bquote({
            ANL[[.(categorical_var)]] <- dplyr::if_else(
              is.na(ANL[[.(categorical_var)]]),
              "(Missing)",
              as.character(ANL[[.(categorical_var)]])
            )
            ANL <- ANL %>% dplyr::filter(.(as.name(categorical_var)) %in% .(input$categorical_var_levels)) # nolint
          })
        )
      } else {
        common_stack_push(
          bquote(
            ANL <- ANL %>% dplyr::filter(.(as.name(categorical_var)) %in% .(input$categorical_var_levels)) # nolint
          )
        )
      }
      contains_na <- anyNA(merged_data()$data()[, outlier_var])
      if (contains_na) {
        common_stack_push(bquote({
          ANL_NO_NA <- ANL %>% dplyr::filter(!is.na(.(as.name(outlier_var)))) # nolint
        }))
      }
      shinyjs::show("split_outliers")
    }

    # slider
    outlier_definition_param <- if (method == "IQR") { # nolint
      input$iqr_slider
    } else if (method == "Z-score") {
      input$zscore_slider
    } else {
      input$percentile_slider
    }

    common_stack_push(
      quote(
        outliers_beyond_1.5_IQR <- function(ANL, outlier_var, categorical_var = NULL) { # nolint
          if (!utils.nest::is_empty(categorical_var)) {
            ANL %>%
              dplyr::group_by(!!as.name(categorical_var)) %>%
              dplyr::mutate(
                is_outlier = {
                  q1_q3 <- quantile(!!as.name(outlier_var), probs = c(0.25, 0.75))
                  iqr <- 1.5 * (q1_q3[2] - q1_q3[1])
                  !(!!as.name(outlier_var) >= q1_q3[1] - iqr & !!as.name(outlier_var) <= q1_q3[2] + iqr)
                }
              ) %>%
              dplyr::ungroup() %>%
              dplyr::filter(is_outlier | !!as.name("is_outlier_selected")) %>%
              dplyr::select(-is_outlier)
          } else {
            ANL %>%
              dplyr::mutate(
                is_outlier = {
                  q1_q3 <- quantile(!!as.name(outlier_var), probs = c(0.25, 0.75))
                  iqr <- 1.5 * (q1_q3[2] - q1_q3[1])
                  !(!!as.name(outlier_var) >= q1_q3[1] - iqr & !!as.name(outlier_var) <= q1_q3[2] + iqr)
                }
              ) %>%
              dplyr::filter(is_outlier | !!as.name("is_outlier_selected")) %>%
              dplyr::select(-is_outlier)
          }
        }
      )
    )

    # Define calculation function
    if (method == "IQR") {
      if (split_outliers && is_character_single(categorical_var)) {
        common_stack_push(
          bquote({
            calculate_outliers <- function(ANL, # nolint
                                           outlier_var,
                                           outlier_definition_param,
                                           categorical_var) {
              all_categories <- lapply( # nolint
                unique(ANL[[categorical_var]]),
                function(x) {
                  ANL_FILTERED <- ANL %>% dplyr::filter(get(categorical_var) == x) # nolint
                  outlier_var_value <- ANL_FILTERED[[outlier_var]]
                  q1_q3 <- quantile(outlier_var_value, probs = c(0.25, 0.75))
                  iqr <- q1_q3[2] - q1_q3[1]
                  ANL_FILTERED$is_outlier_selected <-
                    !(outlier_var_value >= q1_q3[1] - outlier_definition_param * iqr &
                      outlier_var_value <= q1_q3[2] + outlier_definition_param * iqr
                    )
                  ANL_FILTERED
                }
              )
              do.call(rbind, all_categories)
            }
          })
        )
      } else {
        common_stack_push(
          bquote({
            calculate_outliers <- function(ANL, # nolint
                                           outlier_var,
                                           outlier_definition_param,
                                           categorical_var = NA) {
              outlier_var_value <- ANL[[outlier_var]]
              q1_q3 <- quantile(outlier_var_value, probs = c(0.25, 0.75))
              iqr <- q1_q3[2] - q1_q3[1]
              ANL$is_outlier_selected <-
                !(outlier_var_value >= q1_q3[1] - outlier_definition_param * iqr &
                  outlier_var_value <= q1_q3[2] + outlier_definition_param * iqr
                )
              ANL
            }
          })
        )
      }
    } else if (method == "Z-score") {
      if (split_outliers && is_character_single(categorical_var)) {
        common_stack_push(
          bquote({
            calculate_outliers <- function(ANL, # nolint
                                           outlier_var,
                                           outlier_definition_param,
                                           categorical_var) {
              all_categories <- lapply(unique(ANL[[categorical_var]]), function(x) {
                ANL_FILTERED <- ANL %>% dplyr::filter(get(categorical_var) == x) # nolint
                if (nrow(ANL_FILTERED) >= 2) {
                  outlier_var_value <- ANL_FILTERED[[outlier_var]]
                  zscore <- abs(outlier_var_value - mean(outlier_var_value)) / sd(outlier_var_value)
                  ANL_FILTERED$is_outlier_selected <- zscore > outlier_definition_param
                  ANL_FILTERED
                } else {
                  dplyr::mutate(ANL_FILTERED[FALSE, , drop = FALSE], is_outlier_selected = logical(0))
                }
              })
              do.call(rbind, all_categories)
            }
          })
        )
      } else {
        common_stack_push(
          bquote({
            calculate_outliers <- function(ANL, # nolint
                                           outlier_var,
                                           outlier_definition_param,
                                           categorical_var = NA) {
              outlier_var_value <- ANL[[outlier_var]]
              zscore <- abs(outlier_var_value - mean(outlier_var_value)) / sd(outlier_var_value)
              ANL$is_outlier_selected <- zscore > outlier_definition_param
              ANL
            }
          })
        )
      }
    } else if (method == "Percentile") {
      if (split_outliers && is_character_single(categorical_var)) {
        common_stack_push(
          bquote({
            calculate_outliers <- function(ANL, # nolint
                                           outlier_var,
                                           outlier_definition_param,
                                           categorical_var) {
              all_categories <- lapply(
                unique(ANL[[categorical_var]]),
                function(x) {
                  ANL_FILTERED <- ANL %>% dplyr::filter(get(categorical_var) == x) # nolint
                  outlier_var_value <- ANL_FILTERED[[outlier_var]]
                  ANL_FILTERED$is_outlier_selected <-
                    outlier_var_value < quantile(outlier_var_value, outlier_definition_param) |
                    outlier_var_value > quantile(outlier_var_value, 1 - outlier_definition_param)
                  ANL_FILTERED
                }
              )
              do.call(rbind, all_categories)
            }
          })
        )
      } else {
        common_stack_push(
          bquote({
            calculate_outliers <- function(ANL, # nolint
                                           outlier_var,
                                           outlier_definition_param,
                                           categorical_var) {
              outlier_var_value <- ANL[[outlier_var]]
              ANL$is_outlier_selected <-
                outlier_var_value < quantile(outlier_var_value, outlier_definition_param) |
                outlier_var_value > quantile(outlier_var_value, 1 - outlier_definition_param)
              ANL
            }
          })
        )
      }
    }

    common_stack_push(
      bquote({
        ANL_OUTLIER <- calculate_outliers( # nolint
          .(if (contains_na) quote(ANL_NO_NA) else quote(ANL)),
          .(outlier_var),
          .(outlier_definition_param),
          .(categorical_var)
        ) %>%
          outliers_beyond_1.5_IQR(.(outlier_var), .(categorical_var))
        ANL_OUTLIER # used to display table when running show-r-code code
      })
    )

    if (!is_empty(categorical_var)) {
      common_stack_push(
        quote(ANL_SUMMARY <- dplyr::filter( # nolint
          ANL_OUTLIER,
          !!as.name("is_outlier_selected")
        ) %>% dplyr::select(-"is_outlier_selected"))
      )

      common_stack_push(
        bquote({
          summary_table_pre <- ANL_SUMMARY[, c(.(outlier_var), .(categorical_var))] %>%
            dplyr::group_by(.(as.name(categorical_var))) %>%
            dplyr::summarise(n_outliers = dplyr::n()) %>%
            dplyr::right_join(
              ANL[, c(.(outlier_var), .(categorical_var))] %>%
                dplyr::group_by(.(as.name(categorical_var))) %>%
                dplyr::summarise(
                  total_in_cat = dplyr::n(),
                  n_na = sum(is.na(.(as.name(outlier_var))) | is.na(.(as.name(categorical_var))))
                ),
              by = .(categorical_var)
            ) %>%
            # This is important as there may be categorical variables with natural orderings, e.g. AGE.
            # The plots should be displayed by default in increasing order in these situations.
            # dplyr::arrange will sort integer, factor, and character data types in the expected way.
            dplyr::arrange(.(as.name(categorical_var))) %>%
            dplyr::mutate(
              n_outliers = dplyr::if_else(is.na(n_outliers), 0L, n_outliers),
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
            )
        })
      )
      # now to handle when user chooses to order based on amount of outliers
      if (order_by_outlier) {
        common_stack_push(
          quote(
            summary_table_pre <- summary_table_pre %>%
              dplyr::arrange(desc(n_outliers / total_in_cat)) %>%
              dplyr::mutate(order = seq_len(nrow(summary_table_pre)))
          )
        )
      }
      common_stack_push(
        bquote({
          # In order for geom_rug to work properly when reordering takes place inside facet_grid,
          # all tables must have the column used for reording.
          # In this case, the column used for reordering is `order`.
          ANL_OUTLIER <- dplyr::left_join( # nolint
            ANL_OUTLIER,
            summary_table_pre[, c("order", .(categorical_var))],
            by = .(categorical_var)
          )
          # so that x axis of plot aligns with columns of summary table, from most outliers to least by percentage
          ANL <- ANL %>% # nolint
            dplyr::left_join(
              dplyr::select(summary_table_pre, .(as.name(categorical_var)), order),
              by = .(categorical_var)
            ) %>%
            dplyr::arrange(order)
          summary_table_wide <- summary_table_pre %>%
            dplyr::select(.(as.name(categorical_var)), display_str) %>%
            tidyr::pivot_wider(names_from = .(categorical_var), values_from = display_str) %>%
            dplyr::mutate(row_name = "Outlier(s)")
        })
      )
      if (contains_na) {
        common_stack_push(
          bquote({
            summary_table_na_wide <- summary_table_pre %>%
              dplyr::select(.(as.name(categorical_var)), display_str_na) %>%
              tidyr::pivot_wider(names_from = .(categorical_var), values_from = display_str_na) %>%
              dplyr::mutate(row_name = "Missing(s)")
          })
        )
      }
      common_stack_push(
        bquote({
          summary_table_total_wide <- summary_table_pre %>%
            dplyr::select(.(as.name(categorical_var)), total_in_cat) %>%
            dplyr::mutate(total_in_cat = as.character(total_in_cat)) %>%
            tidyr::pivot_wider(names_from = .(categorical_var), values_from = total_in_cat) %>%
            dplyr::mutate(row_name = "Total")
        })
      )
      if (contains_na) {
        common_stack_push(
          bquote(
            summary_table <- dplyr::bind_rows(summary_table_wide, summary_table_na_wide, summary_table_total_wide) %>%
              tibble::column_to_rownames("row_name")
          )
        )
      } else {
        common_stack_push(
          bquote(
            summary_table <- dplyr::bind_rows(summary_table_wide, summary_table_total_wide) %>%
              tibble::column_to_rownames("row_name")
          )
        )
      }
    }

    chunks_safe_eval(chunks = common_stack)
    if (!is_empty(categorical_var) && nrow(chunks_get_var("ANL_OUTLIER", common_stack)) > 0) {
      shinyjs::show("order_by_outlier")
    } else {
      shinyjs::hide("order_by_outlier")
    }
    list(common_stack = common_stack)
  })

  output$summary_table <- DT::renderDataTable({
    suppressWarnings(
      chunks_get_var("summary_table", common_code_chunks()$common_stack)
    )
  },
  options = list(
    dom = "t",
    autoWidth = TRUE,
    columnDefs = list(list(width = "200px", targets = "_all"))
  )
  )

  # boxplot/violinplot #nolint
  box_plot_r_chunks <- reactive({

    # Create a private stack for this function only.
    boxplot_r_stack <- chunks$new()
    boxplot_r_stack_push <- function(...) {
      chunks_push(..., chunks = boxplot_r_stack)
    }

    # Add common code into this chunk
    chunks_push_chunks(common_code_chunks()$common_stack, chunks = boxplot_r_stack)
    ANL <- chunks_get_var("ANL", boxplot_r_stack) # nolint
    ANL_OUTLIER <- chunks_get_var("ANL_OUTLIER", boxplot_r_stack) # nolint

    outlier_var <- as.vector(merged_data()$columns_source$outlier_var)
    categorical_var <- as.vector(merged_data()$columns_source$categorical_var)

    # validation
    validate_has_data(ANL, 1)
    validate(need(input$boxplot_alts, "Please select `Plot type`"))

    # boxplot
    plot_call <- bquote(ANL %>% ggplot()) # nolint

    plot_call <- if (input$boxplot_alts == "Box plot") {
      bquote(.(plot_call) + geom_boxplot(outlier.shape = NA))
    } else if (input$boxplot_alts == "Violin plot") {
      bquote(.(plot_call) + geom_violin())
    } else {
      NULL
    }

    plot_call <- if (is_character_empty(categorical_var) || is.null(categorical_var)) {
      inner_call <- bquote(
        .(plot_call) +
          aes(x = "Entire dataset", y = .(as.name(outlier_var))) +
          scale_x_discrete()
      )
      if (nrow(ANL_OUTLIER) > 0) {
        bquote(.(inner_call) + geom_point(
          data = ANL_OUTLIER,
          aes(x = "Entire dataset", y = .(as.name(outlier_var)), color = is_outlier_selected)
        ))
      } else {
        inner_call
      }
    } else {
      bquote(
        .(plot_call) +
          aes(y = .(as.name(outlier_var)), x = reorder(.(as.name(categorical_var)), order)) +
          xlab(.(categorical_var)) +
          scale_x_discrete() +
          geom_point(
            data = ANL_OUTLIER,
            aes(x = as.factor(.(as.name(categorical_var))), y = .(as.name(outlier_var)), color = is_outlier_selected)
          )
      )
    }
    boxplot_r_stack_push(bquote(g <- .(plot_call) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
      labs(color = "Is outlier?") +
      theme(legend.position = "top")))
    boxplot_r_stack_push(quote(print(g)))
    chunks_safe_eval(boxplot_r_stack)
    boxplot_r_stack
  })

  box_plot_plot_r <- reactive({
    chunks_reset()
    chunks_push_chunks(box_plot_r_chunks())
    chunks_get_var(var = "g", chunks = box_plot_r_chunks())
  })

  # density plot
  density_plot_r_chunks <- reactive({
    # Create a private stack for this function only.
    density_r_stack <- chunks$new()
    density_r_stack_push <- function(...) {
      chunks_push(..., chunks = density_r_stack)
    }

    # Add common code into this chunk
    chunks_push_chunks(common_code_chunks()$common_stack, chunks = density_r_stack)
    ANL <- chunks_get_var("ANL", density_r_stack) # nolint
    ANL_OUTLIER <- chunks_get_var("ANL_OUTLIER", density_r_stack) # nolint

    outlier_var <- as.vector(merged_data()$columns_source$outlier_var)
    categorical_var <- as.vector(merged_data()$columns_source$categorical_var)

    # validation
    validate_has_data(ANL, 1)
    # plot
    plot_call <- bquote(
      ANL %>% ggplot(aes(x = .(as.name(outlier_var)))) +
        geom_density() +
        geom_rug(
          data = ANL_OUTLIER,
          aes(x = .(as.name(outlier_var)),
              color = is_outlier_selected)
        ) +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
        labs(color = "Is outlier?") +
        theme(legend.position = "top")
    )

    plot_call <- if (is_character_empty(categorical_var) || is.null(categorical_var)) {
      bquote(
        .(plot_call)
      )
    } else {
      bquote(
        .(plot_call) +
          facet_grid(~ reorder(.(as.name(categorical_var)), order))
      )
    }

    density_r_stack_push(bquote(g <- .(plot_call)))
    density_r_stack_push(quote(print(g)))
    chunks_safe_eval(density_r_stack)
    density_r_stack
  })

  density_plot_plot_r <- reactive({
    chunks_reset()
    chunks_push_chunks(density_plot_r_chunks())
    chunks_get_var(var = "g", chunks = density_plot_r_chunks())
  })

  # Cumulative distribution plot
  cumulative_plot_r_chunks <- reactive({
    # Create a private stack for this function only.
    cumulative_r_stack <- chunks$new()
    cumulative_r_stack_push <- function(...) {
      chunks_push(..., chunks = cumulative_r_stack)
    }

    # Add common code into this chunk
    chunks_push_chunks(common_code_chunks()$common_stack, chunks = cumulative_r_stack)
    ANL <- chunks_get_var("ANL", cumulative_r_stack) # nolint
    ANL_OUTLIER <- chunks_get_var("ANL_OUTLIER", cumulative_r_stack) # nolint

    outlier_var <- as.vector(merged_data()$columns_source$outlier_var)
    categorical_var <- as.vector(merged_data()$columns_source$categorical_var)

    # validation
    validate_has_data(ANL, 1)

    # plot
    plot_call <- bquote(ANL %>% ggplot(
      aes(x = .(as.name(outlier_var)))
    ) +
      stat_ecdf())

    plot_call <- if (is_character_empty(categorical_var) || is.null(categorical_var)) {
      cumulative_r_stack_push(
        bquote({
          ecdf_df <- ANL %>%
            dplyr::mutate(
              y = ecdf(ANL[[.(outlier_var)]])(ANL[[.(outlier_var)]])
            )

          outlier_points <- dplyr::left_join(
            ecdf_df,
            ANL_OUTLIER,
            by = dplyr::setdiff(names(ecdf_df), "y")
          ) %>%
            dplyr::filter(!is.na(is_outlier_selected))
        })
      )
      plot_call <- bquote(
        .(plot_call)
      )
    } else {
      contains_na <- !is.null(suppressWarnings(chunks_get_var("ANL_NO_NA", cumulative_r_stack)))
      ANL <- if (contains_na) { # nolint
        cumulative_r_stack_push(
          bquote(
            ANL_NO_NA <- ANL_NO_NA %>% # nolint
              dplyr::left_join(
                dplyr::select(summary_table_pre, .(as.name(categorical_var)), order),
                by = .(categorical_var)
              ) %>%
              dplyr::arrange(order)
          )
        )
        quote(ANL_NO_NA)
      } else {
        quote(ANL)
      }
      cumulative_r_stack_push(
        bquote({
          all_categories <- lapply(
            unique(.(ANL)[[.(categorical_var)]]),
            function(x) {
              anl_filtered <- .(ANL) %>% dplyr::filter(get(.(categorical_var)) == x)
              anl_outlier2 <- ANL_OUTLIER %>% dplyr::filter(get(.(categorical_var)) == x)
              ecdf_df <- anl_filtered %>%
                dplyr::mutate(y = ecdf(anl_filtered[[.(outlier_var)]])(anl_filtered[[.(outlier_var)]]))

              dplyr::left_join(
                ecdf_df,
                anl_outlier2,
                by = dplyr::setdiff(names(ecdf_df), "y")) %>%
                dplyr::filter(!is.na(is_outlier_selected))
            }
          )
          outlier_points <- do.call(rbind, all_categories)
        })
      )
      plot_call <- bquote(
        .(plot_call) +
          facet_grid(~ reorder(.(as.name(categorical_var)), order))
      )
    }

    cumulative_r_stack_push(bquote(g <- .(plot_call) +
      geom_point(
        data = outlier_points,
        aes(x = .(as.name(outlier_var)), y = y, color = is_outlier_selected)
      ) +
      scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
      labs(color = "Is outlier?") +
      theme(legend.position = "top")))

    cumulative_r_stack_push(quote(print(g)))
    chunks_safe_eval(cumulative_r_stack)
    cumulative_r_stack
  })

  cumulative_plot_plot_r <- reactive({
    chunks_reset()
    chunks_push_chunks(cumulative_plot_r_chunks())
    chunks_get_var(var = "g", chunks = cumulative_plot_r_chunks())
  })

  observeEvent(input$tabs, {
    tab <- input$tabs
    req(tab) # tab is NULL upon app launch, hence will crash without this statement
    chunks_reset()
    if (tab == "Boxplot") {
      chunks_push_chunks(box_plot_r_chunks())
    } else if (tab == "Density plot") {
      chunks_push_chunks(density_plot_r_chunks())
    } else if (tab == "Cumulative distribution plot") {
      chunks_push_chunks(cumulative_plot_r_chunks())
    }
  })

  # slider text
  output$ui_outlier_help <- renderUI({
    validate(need(input$method, "Please select a method"))
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

  box_brush <- callModule(
    plot_with_settings_srv,
    id = "box_plot",
    plot_r = box_plot_plot_r,
    height = plot_height,
    width = plot_width,
    brushing = TRUE
  )

  density_brush <- callModule(
    plot_with_settings_srv,
    id = "density_plot",
    plot_r = density_plot_plot_r,
    height = plot_height,
    width = plot_width,
    brushing = TRUE
  )

  cum_density_brush <- callModule(
    plot_with_settings_srv,
    id = "cum_density_plot",
    plot_r = cumulative_plot_plot_r,
    height = plot_height,
    width = plot_width,
    brushing = TRUE
  )

  choices <- variable_choices(
    datasets$get_data(if_empty(datasets$get_parentname(datasets$datanames()[[1]]), datasets$datanames()[[1]])))

  observeEvent(common_code_chunks(), {
    ANL_OUTLIER <- chunks_get_var("ANL_OUTLIER", common_code_chunks()$common_stack) # nolint
    updateOptionalSelectInput(
      session,
      inputId = "table_ui_columns",
      choices = dplyr::setdiff(choices, names(ANL_OUTLIER)),
      selected = isolate(input$table_ui_columns))
  })

  output$table_ui <- DT::renderDataTable({
    tab <- input$tabs
    req(tab) # tab is NULL upon app launch, hence will crash without this statement
    outlier_var <- as.vector(merged_data()$columns_source$outlier_var)
    categorical_var <- as.vector(merged_data()$columns_source$categorical_var)

    ANL_OUTLIER <- chunks_get_var("ANL_OUTLIER", common_code_chunks()$common_stack) # nolint
    ANL <- chunks_get_var("ANL", common_code_chunks()$common_stack) # nolint
    ANL_NO_NA <- suppressWarnings(chunks_get_var("ANL_NO_NA", common_code_chunks()$common_stack)) # nolint
    if (!is.null(ANL_NO_NA)) {
      ANL <- ANL_NO_NA # nolint
    }
    plot_brush <-
      if (tab == "Boxplot") {
        box_plot_plot_r()
        box_brush$brush()
      } else if (tab == "Density plot") {
        density_plot_plot_r()
        density_brush$brush()
      } else if (tab == "Cumulative distribution plot") {
        cumulative_plot_plot_r()
        cum_density_brush$brush()
      }

    # removing unused column ASAP
    ANL_OUTLIER$order <- ANL$order <- NULL

    display_table <- if (!is.null(plot_brush)) {
      if (!is_empty(categorical_var)) {
        # due to reordering, the x-axis label may be changed to something like "reorder(categorical_var, order)"
        if (tab == "Boxplot") {
          plot_brush$mapping$x <- categorical_var
        } else {
          # the other plots use facetting, so it is panelvar1 that gets relabelled to "reorder(categorical_var, order)"
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
      if (tab == "Density plot") {
        plot_brush$mapping$y <- "density"
        ANL$density <- plot_brush$ymin # either ymin or ymax will work
      } else if (tab == "Cumulative distribution plot") {
        plot_brush$mapping$y <- "cdf"
        if (!is_empty(categorical_var)) {
          ANL <- ANL %>% # nolint
            dplyr::group_by(!!as.name(plot_brush$mapping$panelvar1)) %>%
            dplyr::mutate(cdf = ecdf(!!as.name(outlier_var))(!!as.name(outlier_var)))
        } else {
          ANL$cdf <- ecdf(ANL[[outlier_var]])(ANL[[outlier_var]])
        }
      }
      brushed_rows <- brushedPoints(ANL, plot_brush)
      if (nrow(brushed_rows) > 0) {
        # now we need to remove extra column from ANL so that it will have the same columns as ANL_OUTLIER
        # so that dplyr::intersect will work
        if (tab == "Density plot") {
          brushed_rows$density <- NULL
        } else if (tab == "Cumulative distribution plot") {
          brushed_rows$cdf <- NULL
        } else if (tab == "Boxplot" && is_empty(categorical_var)) {
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
    dataname <- if_empty(datasets$get_parentname(datasets$datanames()[[1]]), datasets$datanames()[[1]])
    keys <- datasets$get_keys(dataname)
    data <- datasets$get_data(dataname)
    dplyr::left_join(
      display_table,
      dplyr::select(data, dplyr::setdiff(names(data), dplyr::setdiff(names(display_table), keys))), by = keys) %>%
      dplyr::select(union(names(display_table), input$table_ui_columns))
  }, options = list(searching = FALSE, language = list(
    zeroRecords = "The highlighted area does not contain outlier points under the actual defined threshold"),
    pageLength = input$table_ui_rows
  )
  )

  output$total_outliers <- renderUI({
    ANL <- chunks_get_var("ANL", common_code_chunks()$common_stack) # nolint
    ANL_OUTLIER <- chunks_get_var("ANL_OUTLIER", common_code_chunks()$common_stack) # nolint
    validate_has_data(ANL, 1)
    ANL_OUTLIER_SELECTED <- ANL_OUTLIER[ANL_OUTLIER$is_outlier_selected, ] # nolint
    h5(
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
    ANL <- chunks_get_var("ANL", common_code_chunks()$common_stack) # nolint
    ANL_NO_NA <- suppressWarnings(chunks_get_var("ANL_NO_NA", common_code_chunks()$common_stack)) # nolint
    if (!is.null(ANL_NO_NA)) {
      helpText(
        sprintf(
          "%s %d / %d [%.02f%%]",
          "Total number of row(s) with missing values:",
          nrow(ANL) - nrow(ANL_NO_NA),
          nrow(ANL),
          100 * (nrow(ANL) - nrow(ANL_NO_NA)) / nrow(ANL)
        )
      )
    }
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(outlier_var, categorical_var)),
    modal_title = "R Code for outlier",
    code_header = "Outlier"
  )
}
