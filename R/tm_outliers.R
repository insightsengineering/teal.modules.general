#' Outlier Module
#'
#' Module to analyze and identify outliers using different methods
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#'
#' @param outlier_var (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Which variable to use for the outliers analysis.
#' @param categorical_var (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Categorical factor to split the analyzed variable on.
#' @param lineplot_param (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   x-axis variable to be used for the outlier line plot or `NULL` (default) if no line plot
#'   tab to be included.
#' @param key_identifier (`character`)
#'   Identifier key variable to be used in the cumulative distribution plot and the line plot.
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADLB <- radlb(cached = TRUE)
#'
#' fact_vars_adsl <- names(Filter(isTRUE, sapply(ADSL, is.factor)))
#' date_vars_adsl <- names(ADSL)[
#'   vapply(ADSL,
#'     function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")),
#'     logical(1)
#'   )
#' ]
#' adsl_extracted_col <- data_extract_spec(
#'   dataname = "ADSL",
#'   select = select_spec(
#'     choices = variable_choices(ADSL, subset = fact_vars_adsl),
#'     selected = NULL,
#'     multiple = FALSE,
#'     fixed = FALSE
#'   )
#' )
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADLB", ADLB, code = "ADLB <- radlb(cached = TRUE)"),
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
#'         ),
#'         data_extract_spec(
#'           dataname = "ADLB",
#'           select = select_spec(
#'             label = "Select variable:",
#'             choices = variable_choices(ADLB, c("AVAL", "CHG2")),
#'             selected = "AVAL",
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
#'       ),
#'       lineplot_param = data_extract_spec(
#'         dataname = "ADLB",
#'         select = select_spec(
#'           choices = c("ADTM"),
#'           selected = "ADTM",
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
tm_outliers <- function(label = "Outlier Module",
                        outlier_var,
                        categorical_var,
                        lineplot_param = NULL,
                        key_identifier = c("USUBJID"),
                        plot_height = c(600, 200, 2000),
                        plot_width = NULL,
                        pre_output = NULL,
                        post_output = NULL) {
  if (!is_class_list("data_extract_spec")(outlier_var)) {
    outlier_var <- list(outlier_var)
  }
  if (!is_class_list("data_extract_spec")(categorical_var)) {
    categorical_var <- list(categorical_var)
  }
  if (!is.null(lineplot_param) && !is_class_list("data_extract_spec")(lineplot_param)) {
    lineplot_param <- list(lineplot_param)
  }

  stop_if_not(
    is_character_single(label),
    is_class_list("data_extract_spec")(outlier_var),
    is_class_list("data_extract_spec")(categorical_var),
    is.null(lineplot_param) || is_class_list("data_extract_spec")(lineplot_param),
    !is.null(key_identifier)
  )

  args <- as.list(environment())

  data_extract_list <- list(
    outlier_var = outlier_var,
    categorical_var = categorical_var,
    lineplot_param = lineplot_param
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
#' @importFrom shinyjs hidden
ui_outliers <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  is_single_dataset_value <- is_single_dataset(args$outlier_var, args$categorical_var, args$lineplot_param)

  standard_layout(
    output = white_small_well(
      uiOutput(ns("total_outliers")),
      DT::dataTableOutput(ns("summary_table")),
      uiOutput(ns("total_missing")),
      br(), hr(),
      if (!is.null(args$lineplot_param)) {
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Boxplot", plot_with_settings_ui(id = ns("box_plot"))),
          tabPanel("Density plot", plot_with_settings_ui(id = ns("density_plot"))),
          tabPanel("Cumulative distribution plot", plot_with_settings_ui(id = ns("cum_density_plot"))),
          tabPanel("Line plot", plot_with_settings_ui(id = ns("line_plot")))
        )
      }
      else {
        tabsetPanel(
          id = ns("tabs"),
          tabPanel("Boxplot", plot_with_settings_ui(id = ns("box_plot"))),
          tabPanel("Density plot", plot_with_settings_ui(id = ns("density_plot"))),
          tabPanel("Cumulative distribution plot", plot_with_settings_ui(id = ns("cum_density_plot")))
        )
      },
      br(), hr(),
      h4("Data table"),
      shinyjs::hidden(DT::dataTableOutput(ns("table_ui"))),
      shinyjs::hidden(DT::dataTableOutput(ns("line_table_ui")))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("outlier_var", "categorical_var", "lineplot_param")]),
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
      checkboxInput(ns("split_outliers"), "Define outliers based on group splitting", value = FALSE),
      checkboxInput(ns("order_by_outlier"), "Re-arrange categories by share of outliers", value = TRUE),
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
              min = 0.01,
              max = 1,
              value = 0.01,
              step = 0.01
            )
          ),
          uiOutput(ns("ui_outlier_help")),
          conditionalPanel(
            condition =
              paste0(
              "input['", ns("tabs"), "'] == 'Cumulative distribution plot' ||
              input['", ns("tabs"), "'] == 'Line plot'"),
            optionalSelectInput(
              inputId = ns("key_identifier"),
              label = "Key Identifier",
              choices = args$key_identifier,
              selected = args$key_identifier[1],
              multiple = FALSE
            )
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == 'Line plot'"),
          panel_item(
            title = "Line plot parameters",
            collapsed = FALSE,
            data_extract_input(
              id = ns("lineplot_param"),
              label = "X-axis variable",
              data_extract_spec = args$lineplot_param,
              is_single_dataset = is_single_dataset_value
            )
          )
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

#' @importFrom stats quantile sd
#' @importFrom dplyr left_join right_join filter group_by summarise n if_else arrange mutate select bind_rows
#' @importFrom tidyr pivot_wider
#' @importFrom tibble column_to_rownames
#' @importFrom shinyjs hide show
#' @importFrom grid grid.draw
srv_outliers <- function(input, output, session, datasets, outlier_var,
                         categorical_var, lineplot_param, plot_height, plot_width) {
  init_chunks()

  merged_data_lineplot <- data_merge_module(
    datasets = datasets,
    data_extract = list(outlier_var, categorical_var, lineplot_param),
    input_id = c("outlier_var", "categorical_var", "lineplot_param"),
    # left_join is used instead of inner_join
    merge_function = "dplyr::left_join"
  )

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
    choices <- value_choices(merged_data()$data(), categorical_var)
    selected <- isolate(input$categorical_var_levels)
    selected <- if (!is.null(selected) && any(selected %in% choices)) {
      selected[selected %in% choices]
    } else {
      choices
    }
    optionalSelectInput(session$ns("categorical_var_levels"),
                        label = "Categories to include",
                        choices = choices,
                        selected = selected,
                        multiple = TRUE
    )
  })

  common_code_chunks <- reactive({
    # Create a private stack for this function only.
    common_stack <- chunks$new()
    line_stack <- chunks$new()

    common_line_stack_push <- function(...) {
      chunks_push(..., chunks = common_stack)
      chunks_push(..., chunks = line_stack)
    }

    chunks_push_data_merge(merged_data(), common_stack)
    chunks_push_data_merge(merged_data_lineplot(), line_stack)
    outlier_var <- as.vector(merged_data()$columns_source$outlier_var)
    categorical_var <- as.vector(merged_data()$columns_source$categorical_var)
    lineplot_param <- as.vector(merged_data_lineplot()$columns_source$lineplot_param)
    order_by_outlier <- input$order_by_outlier # nolint
    method <- input$method

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
      shinyjs::hide("order_by_outlier")
      shinyjs::hide("split_outliers")
      contains_na <- anyNA(merged_data()$data()[, outlier_var])
      if (contains_na) {
        common_line_stack_push(bquote({
          ANL_NO_NA <- ANL %>% dplyr::filter(!is.na(.(as.name(outlier_var)))) # nolint
        }))
      }
    } else {

      validate(need(input$categorical_var_levels, "Please select categories to include"))

      validate(need(
        is.factor(merged_data()$data()[[categorical_var]]) ||
          is.character(merged_data()$data()[[categorical_var]]) ||
          is.integer(merged_data()$data()[[categorical_var]]),
        "`Categorical factor` must be `factor`, `character`, or `integer`"))
      validate(need(outlier_var != categorical_var, "`Variable` and `Categorical factor` cannot be the same"))

      common_line_stack_push(
        bquote({
          ANL <- ANL %>% dplyr::filter(.(as.name(categorical_var)) %in% .(input$categorical_var_levels)) # nolint
        })
      )

      contains_na <- anyNA(merged_data()$data()[, c(outlier_var, categorical_var)])
      if (contains_na) {
        common_line_stack_push(bquote({
          ANL_NO_NA <- ANL %>% dplyr::filter(!is.na(.(as.name(outlier_var))) & !is.na(.(as.name(categorical_var)))) # nolint
        }))
      }
      shinyjs::show("order_by_outlier")
      shinyjs::show("split_outliers")
    }

    # slider
    if (method == "IQR") {
      outlier_definition_param <- input$iqr_slider # nolint
    } else if (method == "Z-score") {
      outlier_definition_param <- input$zscore_slider # nolint
    } else {
      outlier_definition_param <- input$percentile_slider # nolint
    }

    # Define calculation function
    if (method == "IQR") {
      if (input$split_outliers && is_character_single(categorical_var)) {
        common_line_stack_push(
          bquote({
            calculate_outliers <- function(ANL, outlier_var, outlier_definition_param, categorical_var) { # nolint
              all_categories <- lapply(unique(ANL[[.(categorical_var)]]), function(x) {
                ANL_FILTERED <- ANL %>% dplyr::filter(get(.(categorical_var)) == x) # nolint
                outlier_var_value <- ANL_FILTERED[[.(outlier_var)]]
                q1_q3 <- quantile(ANL_FILTERED[[.(outlier_var)]], probs = c(0.25, 0.75))
                iqr <- q1_q3[2] - q1_q3[1]
                outlier_var_outlier_indices <-
                  !(outlier_var_value >= q1_q3[1] - .(outlier_definition_param) * iqr &
                    outlier_var_value <= q1_q3[2] + .(outlier_definition_param) * iqr)
                  ANL_FILTERED[outlier_var_outlier_indices, ]
              })
              do.call(rbind, all_categories)
            }
          })
        )
      } else {
        common_line_stack_push(
          bquote({
            calculate_outliers <- function(ANL, outlier_var, outlier_definition_param, categorical_var = NA) { # nolint
              outlier_var_value <- ANL[[.(outlier_var)]]
              q1_q3 <- quantile(ANL[[.(outlier_var)]], probs = c(0.25, 0.75))
              iqr <- q1_q3[2] - q1_q3[1]
              outlier_var_outlier_indices <-
                !(outlier_var_value >= q1_q3[1] - .(outlier_definition_param) * iqr &
                  outlier_var_value <= q1_q3[2] + .(outlier_definition_param) * iqr)
                ANL[outlier_var_outlier_indices, ]
            }
          })
        )
      }
    } else if (method == "Z-score") {
      if (input$split_outliers && is_character_single(categorical_var)) {
        common_line_stack_push(
          bquote({
            calculate_outliers <- function(ANL, outlier_var, outlier_definition_param, categorical_var) { # nolint
              all_categories <- lapply(unique(ANL[[.(categorical_var)]]), function(x) {
                ANL_FILTERED <- ANL %>% dplyr::filter(get(.(categorical_var)) == x) # nolint
                if (nrow(ANL_FILTERED) >= 2) {
                  outlier_var_value <- ANL_FILTERED[[.(outlier_var)]]
                  zscore <- abs(outlier_var_value - mean(outlier_var_value)) / sd(outlier_var_value)
                  outlier_var_outlier_indices <- zscore > .(outlier_definition_param)
                  ANL_FILTERED[outlier_var_outlier_indices, ]
                } else {
                  ANL_FILTERED[FALSE, ]
                }
              })
              do.call(rbind, all_categories)
            }
          })
        )
      } else {
        common_line_stack_push(
          bquote({
            calculate_outliers <- function(ANL, outlier_var, outlier_definition_param, categorical_var = NA) { # nolint
              outlier_var_value <- ANL[[.(outlier_var)]]
              zscore <- abs(outlier_var_value - mean(outlier_var_value)) / sd(outlier_var_value)
              outlier_var_outlier_indices <- zscore > .(outlier_definition_param)
              ANL[outlier_var_outlier_indices, ]
            }
          })
        )
      }
    } else if (method == "Percentile") {
      if (input$split_outliers && is_character_single(categorical_var)) {
        common_line_stack_push(
          bquote({
            calculate_outliers <- function(ANL, outlier_var, outlier_definition_param, categorical_var) { # nolint
              all_categories <- lapply(unique(ANL[[.(categorical_var)]]), function(x) {
                ANL_FILTERED <- ANL %>% dplyr::filter(get(.(categorical_var)) == x) # nolint
                outlier_var_value <- ANL_FILTERED[[outlier_var]]
                lower_bound <- quantile(outlier_var_value, .(outlier_definition_param))
                upper_bound <- quantile(outlier_var_value, 1 - .(outlier_definition_param))
                outlier_var_outlier_indices <- which(outlier_var_value < lower_bound | outlier_var_value > upper_bound)
                ANL_FILTERED[outlier_var_outlier_indices, ]
              })
              do.call(rbind, all_categories)
            }
          })
        )
      } else {
        common_line_stack_push(
          bquote({
            calculate_outliers <- function(ANL, outlier_var, outlier_definition_param, categorical_var = NA) { # nolint
              outlier_var_value <- ANL[[.(outlier_var)]]
              lower_bound <- quantile(outlier_var_value, .(outlier_definition_param))
              upper_bound <- quantile(outlier_var_value, 1 - .(outlier_definition_param))
              outlier_var_outlier_indices <- which(outlier_var_value < lower_bound | outlier_var_value > upper_bound)
              ANL[outlier_var_outlier_indices, ]
            }
          })
        )
      }
    }
    common_line_stack_push(
      bquote({
        ANL_OUTLIER <- calculate_outliers( # nolint
          .(if (contains_na) quote(ANL_NO_NA) else quote(ANL)),
          .(outlier_var),
          .(outlier_definition_param),
          .(categorical_var)) # nolint
        ANL_OUTLIER # used to display table when running show-r-code code
      })
    )

    if (!is_empty(categorical_var)) {
      common_line_stack_push(quote(ANL_SUMMARY <- ANL_OUTLIER)) # nolint
      if (!is_empty(lineplot_param)) {
        chunks_push(
          bquote({
            ANL_SUMMARY[[.(lineplot_param)]] <- NULL
            ANL_SUMMARY <- unique(ANL_SUMMARY) # nolint
          }),
          chunks = line_stack
        )
      }
      common_line_stack_push(
        bquote({
          summary_table <- ANL_SUMMARY[, c(.(outlier_var), .(categorical_var))] %>%
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
            dplyr::mutate(
              n_outliers = dplyr::if_else(is.na(n_outliers), 0L, n_outliers),
              display_str = ifelse(
                n_outliers > 0,
                sprintf("%d [%.02f%%]", n_outliers, 100 * n_outliers / total_in_cat),
                sprintf("%d", n_outliers)),
              display_str_na = ifelse(
                n_na > 0,
                sprintf("%d [%.02f%%]", n_na, 100 * n_na / total_in_cat),
                sprintf("%d", n_na))
            )
          .(if (order_by_outlier) {
            quote(
              summary_table <- summary_table %>%
                dplyr::arrange(desc(n_outliers / total_in_cat)) %>%
                dplyr::mutate(order = seq_len(nrow(summary_table)))
            )} else {
              if (is.integer(merged_data()$data()[[categorical_var]])) {
                bquote(
                  summary_table <- summary_table %>%
                    dplyr::arrange(.(as.name(categorical_var))) %>%
                    dplyr::mutate(order = seq_len(nrow(summary_table)))
                )
              } else {
                quote(
                  summary_table <- summary_table %>%
                    dplyr::mutate(order = seq_len(nrow(summary_table)))
                )}
              }
          )
          # so that x axis of plot aligns with columns of summary table, from most outliers to least by percentage
          ANL <- ANL %>% # nolint
            dplyr::left_join(
              dplyr::select(summary_table, .(as.name(categorical_var)), order), by = .(categorical_var)) %>%
            dplyr::arrange(order)
          # In order for geom_rug to work properly when reordering takes place inside facet_grid,
          # all tables must have the same columns. ANL has column named order, hence so must ANL_OUTLIER
          ANL_OUTLIER <- dplyr::left_join( # nolint
            ANL_OUTLIER,
            summary_table[, c("order", .(categorical_var))],
            by = .(categorical_var)
          )
          summary_table_wide <- summary_table %>%
            dplyr::select(.(as.name(categorical_var)), display_str) %>%
            tidyr::pivot_wider(names_from = .(categorical_var), values_from = display_str) %>%
            dplyr::mutate(row_name = "Outlier(s)")
        })
      )
      if (contains_na) {
        common_line_stack_push(
          bquote({
            summary_table_wide_na <- summary_table %>%
              dplyr::select(.(as.name(categorical_var)), display_str_na) %>%
              tidyr::pivot_wider(names_from = .(categorical_var), values_from = display_str_na) %>%
              dplyr::mutate(row_name = "Missing(s)")
            summary_table_wide <- dplyr::bind_rows(summary_table_wide, summary_table_wide_na)
            ANL_NO_NA <- ANL_NO_NA %>% # nolint
              dplyr::left_join(
                dplyr::select(summary_table, .(as.name(categorical_var)), order), by = .(categorical_var)) %>%
              dplyr::arrange(order)
          })
        )
      }
      common_line_stack_push(
        bquote({
          summary_table_total_wide <- summary_table %>%
            dplyr::select(.(as.name(categorical_var)), total_in_cat) %>%
            dplyr::mutate(total_in_cat = as.character(total_in_cat)) %>%
            tidyr::pivot_wider(names_from = .(categorical_var), values_from = total_in_cat) %>%
            dplyr::mutate(row_name = "Total")
          summary_table_wide <- dplyr::bind_rows(summary_table_wide, summary_table_total_wide) %>%
            tibble::column_to_rownames("row_name")
        })
      )
    }

    chunks_safe_eval(chunks = common_stack)
    chunks_safe_eval(chunks = line_stack)
    list(common_stack = common_stack, line_stack = line_stack)
  })

  output$summary_table <- DT::renderDataTable({
    # same regardless of tab, i.e. no need to consider line_stack
    suppressWarnings(chunks_get_var("summary_table_wide", common_code_chunks()$common_stack))
  }, options = list(dom = "t", autoWidth = TRUE, columnDefs = list(list(width = "200px", targets = "_all"))))

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
      bquote(.(plot_call) + geom_violin(outlier.shape = NA))
    } else {
      NULL
    }

    plot_call <- if (is_character_empty(categorical_var)) {
      inner_call <- bquote(
        .(plot_call) +
          aes(x = "Entire dataset", y = .(as.name(outlier_var))) +
          scale_x_discrete()
        )
      if (nrow(ANL_OUTLIER) > 0) {
        bquote(.(inner_call) + geom_point(
          data = ANL_OUTLIER,
          aes(x = "Entire dataset", y = .(as.name(outlier_var))),
          color = "red"
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
            aes(x = as.factor(.(as.name(categorical_var))), y = .(as.name(outlier_var))),
            color = "red"
          )
      )
    }
    boxplot_r_stack_push(bquote(g <- .(plot_call)))
    boxplot_r_stack_push(quote(grid::grid.draw(g)))
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
    plot_call <- bquote({
      ANL %>% ggplot(aes(x = .(as.name(outlier_var)))) +
        geom_density() +
        geom_rug(
          data = ANL_OUTLIER,
          aes(x = .(as.name(outlier_var))),
          color = "red"
        )
    })

    plot_call <- if (is_character_empty(categorical_var)) {
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
    density_r_stack_push(quote(grid::grid.draw(g)))
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
    identifier <- input$key_identifier

    # validation
    validate_has_data(ANL, 1)
    validate(need(identifier, "Please select a unique identifier"))

    # plot
    plot_call <- bquote(ANL %>% ggplot(
      aes(x = .(as.name(outlier_var)))
    ) +
      stat_ecdf())

    plot_call <- if (is_character_empty(categorical_var)) {
      outlier_red_points <- bquote({
        ecdf_df <- ANL %>%
          mutate(
            y = ecdf(ANL[[.(outlier_var)]])(ANL[[.(outlier_var)]]),
            unique_identifier = paste0(ANL[[.(identifier)]], "_", ANL[[.(outlier_var)]])
          )
        anl_outlier2 <- ANL_OUTLIER %>%
          mutate(unique_identifier = paste0(ANL_OUTLIER[[.(identifier)]], "_", ANL_OUTLIER[[.(outlier_var)]]))
        red_points <- ecdf_df[match(anl_outlier2[["unique_identifier"]], ecdf_df[["unique_identifier"]]), ]
      })

      plot_call <- bquote(
        .(plot_call) +
          geom_point(
            data = .(outlier_red_points),
            aes(x = .(as.name(outlier_var)), y = y), color = "red"
          )
      )
    } else {
      contains_na <- !is.null(suppressWarnings(chunks_get_var("ANL_NO_NA", cumulative_r_stack)))
      ANL <- if (contains_na) quote(ANL_NO_NA) else quote(ANL) # nolint
      outlier_red_points <- bquote({
        all_categories <- lapply(unique(.(ANL)[[.(categorical_var)]]), function(x) {
          anl_filtered <- .(ANL) %>% dplyr::filter(get(.(categorical_var)) == x)
          anl_outlier2 <- ANL_OUTLIER %>% dplyr::filter(get(.(categorical_var)) == x)
          ecdf_df <- anl_filtered %>%
            mutate(y = ecdf(anl_filtered[[.(outlier_var)]])(anl_filtered[[.(outlier_var)]]))
          ecdf_df[["unique_identifier"]] <- paste0(ecdf_df[[.(identifier)]], "_", ecdf_df[[.(outlier_var)]])
          if (nrow(anl_outlier2) != 0) {
            anl_outlier2[["unique_identifier"]] <-
              paste0(anl_outlier2[[.(identifier)]], "_", anl_outlier2[[.(outlier_var)]])
            ecdf_df2 <- ecdf_df[match(anl_outlier2[["unique_identifier"]], ecdf_df[["unique_identifier"]]), ]
          } else {
            ecdf_df2 <- ecdf_df[FALSE, ]
          }
          ecdf_df2
        })
        do.call(rbind, all_categories)
      })
      plot_call <- bquote(
        .(plot_call) +
          facet_grid(~ reorder(.(as.name(categorical_var)), order)) +
          geom_point(data = .(outlier_red_points), aes(x = .(as.name(outlier_var)), y = y), color = "red")
      )
    }

    cumulative_r_stack_push(bquote(g <- .(plot_call)))
    cumulative_r_stack_push(quote(grid::grid.draw(g)))
    chunks_safe_eval(cumulative_r_stack)
    cumulative_r_stack
  })

  cumulative_plot_plot_r <- reactive({
    chunks_reset()
    chunks_push_chunks(cumulative_plot_r_chunks())
    chunks_get_var(var = "g", chunks = cumulative_plot_r_chunks())
  })

  # Lineplot
  line_plot_r_chunks <- reactive({
    # Create a private stack for this function only.
    line_r_stack <- chunks$new()
    line_r_stack_push <- function(...) {
      chunks_push(..., chunks = line_r_stack)
    }

    # Add common code into this chunk
    chunks_push_chunks(common_code_chunks()$line_stack, chunks = line_r_stack)
    ANL <- chunks_get_var("ANL", line_r_stack) # nolint
    ANL_OUTLIER <- chunks_get_var("ANL_OUTLIER", line_r_stack) # nolint

    outlier_var <- as.vector(merged_data_lineplot()$columns_source$outlier_var)
    categorical_var <- as.vector(merged_data_lineplot()$columns_source$categorical_var)
    lineplot_param <- as.vector(merged_data_lineplot()$columns_source$lineplot_param)
    identifier <- input$key_identifier


    # validation
    validate_has_data(ANL, 1)
    validate(need(lineplot_param, "Please select a lineplot parameter"))
    validate(need(identifier, "Please select a unique identifier"))

    plot_call <- bquote(ANL %>% ggplot(
      aes(x = .(as.name(lineplot_param)), y = .(as.name(outlier_var)), group = .(as.name(identifier)))
    ) +
      geom_line() +
      geom_point(
        data = ANL_OUTLIER,
        aes(y = .(as.name(outlier_var))),
        color = "red"
      ))

    plot_call <- if (is_character_empty(categorical_var)) {
      bquote(
        .(plot_call)
      )
    } else {
      bquote(
        .(plot_call) +
          facet_grid(~ reorder(.(as.name(categorical_var)), order))
      )
    }

    line_r_stack_push(bquote(g <- .(plot_call)))
    line_r_stack_push(quote(grid::grid.draw(g)))
    chunks_safe_eval(line_r_stack)
    line_r_stack
  })

  line_plot_plot_r <- reactive({
    chunks_reset()
    chunks_push_chunks(line_plot_r_chunks())
    chunks_get_var(var = "g", chunks = line_plot_r_chunks())
  })

  observeEvent(input$tabs, {
    tab <- input$tabs
    req(tab) # tab is NULL upon app launch, hence will crash without this statement
    chunks_reset()
    if (tab == "Line plot") {
      chunks_push_chunks(line_plot_r_chunks())
    } else if (tab == "Boxplot") {
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
      tags$small(helpText(
        withMathJax(paste0(
          "Outlier data points (\\(Q1 - ", input$iqr_slider, "\\times IQR \\gt x\\) and \\(
          Q3 + ", input$iqr_slider, "\\times IQR \\lt x\\))
          are displayed in red on the plot and can be visualized in the table below."
        ))
      ))
    } else if (input$method == "Z-score") {
      req(input$zscore_slider)
      tags$small(helpText(
        withMathJax(paste0(
          "Outlier data points (\\(Z-score(x) > ", input$zscore_slider,
          "\\) and \\(Z-score(x) < -", input$zscore_slider, "\\))
            are displayed in red on the plot and can be visualized in the table below."
        ))
      ))
    } else if (input$method == "Percentile") {
      req(input$percentile_slider)
      tags$small(helpText(
        withMathJax(paste0(
          "Outlier/extreme data points (\\(", input$percentile_slider,
          "-Percentile > x\\) and \\(",
          1 - input$percentile_slider, "-Percentile < x\\))
            are displayed in red on the plot and can be visualized in the table below."
        ))
      ))
    }
  })

  callModule(
    plot_with_settings_srv,
    id = "box_plot",
    plot_r = box_plot_plot_r,
    height = plot_height,
    width = plot_width
  )

  callModule(
    plot_with_settings_srv,
    id = "density_plot",
    plot_r = density_plot_plot_r,
    height = plot_height,
    width = plot_width
  )

  callModule(
    plot_with_settings_srv,
    id = "cum_density_plot",
    plot_r = cumulative_plot_plot_r,
    height = plot_height,
    width = plot_width
  )

  callModule(
    plot_with_settings_srv,
    id = "line_plot",
    plot_r = line_plot_plot_r,
    height = plot_height,
    width = plot_width
  )

  output$table_ui <- DT::renderDataTable({
    chunks_get_var("ANL_OUTLIER", common_code_chunks()$common_stack)
  })

  output$line_table_ui <- DT::renderDataTable({
    chunks_get_var("ANL_OUTLIER", common_code_chunks()$line_stack)
  })

  observe({
    req(input$tabs) # input$tabs does not exist upon app launch
    if (input$tabs == "Line plot") {
      shinyjs::hide("table_ui")
      shinyjs::show("line_table_ui")
    } else {
      shinyjs::show("table_ui")
      shinyjs::hide("line_table_ui")
    }
  })

  output$total_outliers <- shiny::renderUI({
    ANL <- chunks_get_var("ANL", common_code_chunks()$common_stack) # nolint
    ANL_OUTLIER <- chunks_get_var("ANL_OUTLIER", common_code_chunks()$common_stack) # nolint
    validate_has_data(ANL, 1)
    shiny::h5(
      sprintf(
        "%s %d / %d [%.02f%%]",
        "Total number of outlier(s):",
        nrow(ANL_OUTLIER),
        nrow(ANL),
        100 * nrow(ANL_OUTLIER) / nrow(ANL)))
  })

  output$total_missing <- shiny::renderUI({
    ANL <- chunks_get_var("ANL", common_code_chunks()$common_stack) # nolint
    ANL_NO_NA <- suppressWarnings(chunks_get_var("ANL_NO_NA", common_code_chunks()$common_stack)) # nolint
    if (!is.null(ANL_NO_NA)) {
      shiny::helpText(
        sprintf(
          "%s %d / %d [%.02f%%]",
          "Total number of row(s) with missing values:",
          nrow(ANL) - nrow(ANL_NO_NA),
          nrow(ANL),
          100 * nrow(ANL_NO_NA) / nrow(ANL)))
    }
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(outlier_var, categorical_var, lineplot_param)),
    modal_title = "R Code for outlier",
    code_header = "Outlier"
  )
}
