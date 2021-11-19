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
#'   categorical factor to split the selected outlier variables on.
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' fact_vars_adsl <- names(Filter(isTRUE, sapply(ADSL, is.factor)))
#' vars <- choices_selected(variable_choices(ADSL, fact_vars_adsl))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
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
#'         filter = filter_spec(
#'           vars = vars,
#'           choices = value_choices(ADSL, vars$selected),
#'           selected = value_choices(ADSL, vars$selected),
#'           multiple = TRUE
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
  logger::log_info("Initializing tm_outliers")
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
        data_extract_input(
          id = ns("categorical_var"),
          label = "Categorical factor",
          data_extract_spec = args$categorical_var,
          is_single_dataset = is_single_dataset_value
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

srv_outliers <- function(input, output, session, datasets, outlier_var,
                         categorical_var, plot_height, plot_width) {
  init_chunks()

  vars <- list(outlier_var = outlier_var, categorical_var = categorical_var)
  selector_list <- data_extract_multiple_srv(vars, datasets)

  reactive_select_input <- reactive({
    if (length(selector_list$categorical_var()$select) > 0) {
      selector_list
    } else {
      list(outlier_var = selector_list$outlier_var)
    }
  })

  merged_data <- data_merge_srv(
    selector_list = reactive_select_input,
    datasets = datasets,
    merge_function = "dplyr::left_join"
  )

  is_cat_filter_spec <- inherits(categorical_var[[1]]$filter[[1]], "filter_spec")
  cat_dataname <- categorical_var[[1]]$dataname

  common_code_chunks <- reactive({
    # Create a private stack for this function only.
    common_stack <- chunks$new()

    common_stack_push <- function(...) {
      chunks_push(..., chunks = common_stack)
    }

    input_catvar <- input[[extract_input(
      "categorical_var",
      cat_dataname,
      filter = is_cat_filter_spec
    )]]

    chunks_push_data_merge(merged_data(), common_stack)
    outlier_var <- as.vector(merged_data()$columns_source$outlier_var)
    categorical_var <- as.vector(merged_data()$columns_source$categorical_var)
    order_by_outlier <- input$order_by_outlier # nolint
    method <- input$method
    split_outliers <- input$split_outliers

    validate(need(outlier_var, "Please select a variable"))
    chunks_validate_custom(
      substitute(expr = length(unique(ANL[[outlier_var]])) > 1, env = list(outlier_var = outlier_var)),
      msg = "Variable has no variation, i.e. only one unique value",
      chunks = common_stack
    )
    validate(need(input$method, "Please select a method"))
    validate(need(is.numeric(merged_data()$data()[[outlier_var]]), "`Variable` is not numeric"))
    validate_has_data(
      # missing values in the categorical variable may be used to form a category of its own
      `if`(
        is_empty(categorical_var),
        merged_data()$data(),
        merged_data()$data()[, names(merged_data()$data()) != categorical_var]
      ),
      min_nrow = 10,
      complete = TRUE,
      allow_inf = FALSE
    )

    # show/hide split_outliers
    if (is_empty(categorical_var)) {
      shinyjs::hide("split_outliers")
      contains_na <- anyNA(merged_data()$data()[, outlier_var])
      if (contains_na) {
        common_stack_push(substitute(
          expr = ANL_NO_NA <- ANL %>% dplyr::filter(!is.na(outlier_var_name)), # nolint
          env = list(outlier_var_name = as.name(outlier_var))
        ))
      }
    } else {
      validate(need(input_catvar, "Please select categories to include"))

      validate(need(
        is.factor(merged_data()$data()[[categorical_var]]) ||
          is.character(merged_data()$data()[[categorical_var]]) ||
          is.integer(merged_data()$data()[[categorical_var]]),
        "`Categorical factor` must be `factor`, `character`, or `integer`"
      ))
      validate(need(outlier_var != categorical_var, "`Variable` and `Categorical factor` cannot be the same"))

      input_catlevels <- if (is_cat_filter_spec) {
        input_catvar
      } else {
        NULL
      }

      # If there are both string values "NA" and missing values NA, value_choices function should output a warning
      if ("NA" %in% input_catlevels) {
        common_stack_push(
          substitute(
            expr = {
              ANL[[categorical_var]] <- dplyr::if_else(
                is.na(ANL[[categorical_var]]),
                "NA",
                as.character(ANL[[categorical_var]])
              )
            },
            env = list(
              categorical_var = categorical_var,
              categorical_var_name = as.name(categorical_var)
            )
          )
        )
      }

      if (is_cat_filter_spec && !all(unique(merged_data()$data()[[categorical_var]]) %in% input_catlevels)) {
        common_stack_push(
          substitute(
            expr = ANL <- ANL %>% dplyr::filter(categorical_var_name %in% categorical_var_levels), # nolint
            env = list(
              categorical_var_name = as.name(categorical_var),
              categorical_var_levels = input_catlevels
            )
          )
        )
      }

      contains_na <- anyNA(merged_data()$data()[, outlier_var])
      if (contains_na) {
        common_stack_push(substitute(
          expr = ANL_NO_NA <- ANL %>% dplyr::filter(!is.na(outlier_var_name)), # nolint
          env = list(outlier_var_name = as.name(outlier_var))
        ))
      }
      shinyjs::show("split_outliers")
    }

    # slider
    outlier_definition_param <- if (method == "IQR") { # nolint
      input$iqr_slider
    } else if (method == "Z-score") {
      input$zscore_slider
    } else if (method == "Percentile") {
      input$percentile_slider
    }

    # this is utils function that converts a %>% NULL %>% b into a %>% b
    remove_pipe_null <- function(x) {
      if (length(x) == 1) {
        return(x)
      }
      if (identical(x[[1]], as.name("%>%")) && is.null(x[[3]])) {
        return(remove_pipe_null(x[[2]]))
      }
      return(as.call(c(x[[1]], lapply(x[-1], remove_pipe_null))))
    }

    common_stack_push(substitute(
      expr = {
        ANL_OUTLIER <- anl_call %>% # nolint
          group_expr %>%
          dplyr::mutate(is_outlier = {
            q1_q3 <- stats::quantile(outlier_var_name, probs = c(0.25, 0.75))
            iqr <- q1_q3[2] - q1_q3[1]
            !(outlier_var_name >= q1_q3[1] - 1.5 * iqr & outlier_var_name <= q1_q3[2] + 1.5 * iqr)
          }) %>%
          calculate_outliers %>%
          ungroup_expr %>%
          dplyr::filter(is_outlier | is_outlier_selected) %>%
          dplyr::select(-is_outlier)
        ANL_OUTLIER # used to display table when running show-r-code code
      },
      env = list(
        anl_call = if (contains_na) quote(ANL_NO_NA) else quote(ANL),
        calculate_outliers = if (method == "IQR") {
          substitute(
            expr = dplyr::mutate(is_outlier_selected = {
              q1_q3 <- stats::quantile(outlier_var_name, probs = c(0.25, 0.75))
              iqr <- q1_q3[2] - q1_q3[1]
              !(outlier_var_name >= q1_q3[1] - outlier_definition_param * iqr &
                outlier_var_name <= q1_q3[2] + outlier_definition_param * iqr)
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
                sd(outlier_var_name) > outlier_definition_param
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
        group_expr = if (isTRUE(split_outliers)) substitute(dplyr::group_by(x), list(x = as.name(categorical_var))),
        ungroup_expr = if (isTRUE(split_outliers)) substitute(dplyr::ungroup())
      )
    ) %>%
      remove_pipe_null())

    if (!is_empty(categorical_var)) {
      common_stack_push(substitute(
        expr = {
          summary_table_pre <- ANL_OUTLIER %>%
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
        },
        env = list(
          categorical_var = categorical_var,
          categorical_var_name = as.name(categorical_var),
          outlier_var_name = as.name(outlier_var)
        )
      ))
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
      common_stack_push(substitute(
        expr = {
          # In order for geom_rug to work properly when reordering takes place inside facet_grid,
          # all tables must have the column used for reording.
          # In this case, the column used for reordering is `order`.
          ANL_OUTLIER <- dplyr::left_join( # nolint
            ANL_OUTLIER,
            summary_table_pre[, c("order", categorical_var)],
            by = categorical_var
          )
          # so that x axis of plot aligns with columns of summary table, from most outliers to least by percentage
          ANL <- ANL %>% # nolint
            dplyr::left_join(
              dplyr::select(summary_table_pre, categorical_var_name, order),
              by = categorical_var
            ) %>%
            dplyr::arrange(order)
          summary_table <- summary_table_pre %>%
            dplyr::select(
              categorical_var_name, Outliers = display_str, Missings = display_str_na, Total = total_in_cat
            ) %>%
            dplyr::mutate_all(as.character) %>%
            tidyr::pivot_longer(-categorical_var_name) %>%
            tidyr::pivot_wider(names_from = categorical_var, values_from = value) %>%
            tibble::column_to_rownames("name")
          summary_table
        },
        env = list(
          categorical_var = categorical_var,
          categorical_var_name = as.name(categorical_var)
        )
      ))
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
    plot_call <- quote(ANL %>% ggplot()) # nolint

    plot_call <- if (input$boxplot_alts == "Box plot") {
      substitute(expr = plot_call + geom_boxplot(outlier.shape = NA), env = list(plot_call = plot_call))
    } else if (input$boxplot_alts == "Violin plot") {
      substitute(expr = plot_call + geom_violin(), env = list(plot_call = plot_call))
    } else {
      NULL
    }

    plot_call <- if (is_character_empty(categorical_var) || is.null(categorical_var)) {
      inner_call <- substitute(
        expr = plot_call +
          aes(x = "Entire dataset", y = outlier_var_name) +
          scale_x_discrete(),
        env = list(plot_call = plot_call, outlier_var_name = as.name(outlier_var))
      )
      if (nrow(ANL_OUTLIER) > 0) {
        substitute(
          expr = inner_call + geom_point(
            data = ANL_OUTLIER,
            aes(x = "Entire dataset", y = outlier_var_name, color = is_outlier_selected)
          ),
          env = list(inner_call = inner_call, outlier_var_name = as.name(outlier_var))
        )
      } else {
        inner_call
      }
    } else {
      substitute(
        expr = plot_call +
          aes(y = outlier_var_name, x = reorder(categorical_var_name, order)) +
          xlab(categorical_var) +
          scale_x_discrete() +
          geom_point(
            data = ANL_OUTLIER,
            aes(x = as.factor(categorical_var_name), y = outlier_var_name, color = is_outlier_selected)
          ),
        env = list(
          plot_call = plot_call,
          outlier_var_name = as.name(outlier_var),
          categorical_var_name = as.name(categorical_var),
          categorical_var = categorical_var
        )
      )
    }
    boxplot_r_stack_push(substitute(
      expr = g <- plot_call +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
        labs(color = "Is outlier?") +
        theme(legend.position = "top"),
      env = list(plot_call = plot_call)
    ))
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
    plot_call <- substitute(
      expr = ANL %>%
        ggplot(aes(x = outlier_var_name)) +
        geom_density() +
        geom_rug(data = ANL_OUTLIER, aes(x = outlier_var_name, color = is_outlier_selected)) +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
        labs(color = "Is outlier?") +
        theme(legend.position = "top"),
      env = list(outlier_var_name = as.name(outlier_var))
    )

    plot_call <- if (is_character_empty(categorical_var) || is.null(categorical_var)) {
      substitute(expr = plot_call, env = list(plot_call = plot_call))
    } else {
      substitute(
        expr = plot_call + facet_grid(~ reorder(categorical_var_name, order)),
        env = list(plot_call = plot_call, categorical_var_name = as.name(categorical_var))
      )
    }

    density_r_stack_push(substitute(expr = g <- plot_call, env = list(plot_call = plot_call)))
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
    plot_call <- substitute(
      expr = ANL %>% ggplot(aes(x = outlier_var_name)) +
        stat_ecdf(),
      env = list(outlier_var_name = as.name(outlier_var))
    )

    plot_call <- if (is_character_empty(categorical_var) || is.null(categorical_var)) {
      cumulative_r_stack_push(
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
          env = list(outlier_var = outlier_var))
      )
      plot_call <- substitute(expr = plot_call, env = list(plot_call = plot_call))
    } else {
      contains_na <- !is.null(suppressWarnings(chunks_get_var("ANL_NO_NA", cumulative_r_stack)))
      ANL <- if (contains_na) { # nolint
        cumulative_r_stack_push(
          substitute(
            expr = ANL_NO_NA <- ANL_NO_NA %>% # nolint
              dplyr::left_join(dplyr::select(summary_table_pre, categorical_var_name, order), by = categorical_var) %>%
              dplyr::arrange(order),
            env = list(categorical_var_name = as.name(categorical_var), categorical_var = categorical_var)
          )
        )
        quote(ANL_NO_NA)
      } else {
        quote(ANL)
      }
      cumulative_r_stack_push(
        substitute(
          expr = {
            all_categories <- lapply(
              unique(anl[[categorical_var]]),
              function(x) {
                anl_filtered <- anl %>% dplyr::filter(get(categorical_var) == x)
                anl_outlier2 <- ANL_OUTLIER %>% dplyr::filter(get(categorical_var) == x)
                ecdf_df <- anl_filtered %>%
                  dplyr::mutate(y = stats::ecdf(anl_filtered[[outlier_var]])(anl_filtered[[outlier_var]]))

                dplyr::left_join(
                  ecdf_df,
                  anl_outlier2,
                  by = dplyr::setdiff(names(ecdf_df), "y")) %>%
                  dplyr::filter(!is.na(is_outlier_selected))
              }
            )
            outlier_points <- do.call(rbind, all_categories)
          },
          env = list(anl = ANL, categorical_var = categorical_var, outlier_var = outlier_var))
      )
      plot_call <- substitute(
        expr = plot_call + facet_grid(~ reorder(categorical_var_name, order)),
        env = list(plot_call = plot_call, categorical_var_name = as.name(categorical_var))
      )
    }

    cumulative_r_stack_push(substitute(
      expr = g <- plot_call +
        geom_point(data = outlier_points, aes(x = outlier_var_name, y = y, color = is_outlier_selected)) +
        scale_color_manual(values = c("TRUE" = "red", "FALSE" = "black")) +
        labs(color = "Is outlier?") +
        theme(legend.position = "top"),
      env = list(plot_call = plot_call, outlier_var_name = as.name(outlier_var))
    ))

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

  dataname <- datasets$datanames()[[1]]
  dataname <- if (!is.function(datasets$get_parentname)) {
    dataname
  } else {
    if_empty(datasets$get_parentname(dataname), dataname)
  }
  choices <- variable_choices(datasets$get_data(dataname))

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
    plot_brush <- if (tab == "Boxplot") {
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
            dplyr::mutate(cdf = stats::ecdf(!!as.name(outlier_var))(!!as.name(outlier_var)))
        } else {
          ANL$cdf <- stats::ecdf(ANL[[outlier_var]])(ANL[[outlier_var]])
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
