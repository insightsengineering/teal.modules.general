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
#'   x-axis variable to be used for the outlier line plot.
#' @param method Vector of outlier analysis methods to be used `c(IQR, Z-score, Percentile)`.
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
#' date_vars_adsl <- names(ADSL)[vapply(ADSL, function(x) inherits(x, c("Date", "POSIXct", "POSIXlt")), logical(1))]
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
#'        data_extract_spec(
#'          dataname = "ADLB",
#'          select = select_spec(
#'            label = "Select variable:",
#'            choices = variable_choices(ADLB, c("AVAL", "CHG2")),
#'            selected = "AVAL",
#'            multiple = FALSE,
#'            fixed = FALSE
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
#'           )
#'         )
#'       )
#'     )
#'   )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_outliers <- function(label = "Outlier Module",
                        outlier_var,
                        categorical_var,
                        lineplot_param,
                        method = c("IQR", "Z-score", "Percentile"),
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
  if (!is_class_list("data_extract_spec")(lineplot_param)) {
    lineplot_param <- list(lineplot_param)
  }

  stop_if_not(
    is_character_single(label),
    is_class_list("data_extract_spec")(outlier_var),
    is_class_list("data_extract_spec")(categorical_var),
    is_class_list("data_extract_spec")(lineplot_param),
    !is.null(method)
  )
  method <- match.arg(method)

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
ui_outliers <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  is_single_dataset_value <- is_single_dataset(args$outlier_var, args$categorical_var, args$lineplot_param)

  standard_layout(
    output = white_small_well(
      tabsetPanel(
        id = ns("tabs"),
        tabPanel(
          "Boxplot",
          div(
            fluidRow(
              plot_with_settings_ui(
                id = ns("box_plot"),
                height = args$plot_height,
                width = args$plot_width
                )
              )
            )
          ),
        tabPanel(
          "Density plot",
          div(
            fluidRow(
              plot_with_settings_ui(
                id = ns("density_plot"),
                height = args$plot_height,
                width = args$plot_width
                )
              )
            )
          ),
        tabPanel(
          "Cumulative distribution plot",
          div(
            fluidRow(
              plot_with_settings_ui(
                id = ns("cum_density_plot"),
                height = args$plot_height,
                width = args$plot_width
                )
              )
            )
          ),
        tabPanel(
          "Line plot",
          div(
            fluidRow(
              plot_with_settings_ui(
                id = ns("line_plot"),
                height = args$plot_height,
                width = args$plot_width
                )
              )
            )
          )
        ),
      br(), hr(),
      h4("Data table"),
      DT::dataTableOutput(ns("table_ui"))
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
        data_extract_input(
          id = ns("categorical_var"),
          label = "Categorial factor",
          data_extract_spec = args$categorical_var,
          is_single_dataset = is_single_dataset_value
        )
      },
      optionalSelectInput(
        inputId = ns("method"),
        label = "Method",
        choices = c("IQR", "Z-score", "Percentile"),
        selected = args$method,
        multiple = FALSE
      ),
      checkboxInput(ns("split_outliers"), "Define outliers based on group splitting", value = FALSE),
      panel_group(
        panel_item(
          title = "Method paramteres",
          collapsed = FALSE,
          conditionalPanel(
            condition =
              paste0("input['", ns("method"), "'] == 'IQR'"),
            sliderInput(
              ns("iqr_slider"),
              "Outlier range:",
              min = 1,
              max = 5,
              value = 0.5,
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
              value = 0.5,
              step = 0.5
            )),
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
          uiOutput(ns("ui_outlier_help"))
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


srv_outliers <- function(input, output, session, datasets, outlier_var,
                         categorical_var, lineplot_param, plot_height, plot_width) {
  init_chunks()


  merged_data_lineplot <- data_merge_module(
    datasets = datasets,
    data_extract = list(outlier_var, categorical_var, lineplot_param),
    input_id = c("outlier_var", "categorical_var", "lineplot_param"),
    #left_join is used instead of inner_join
    merge_function = "dplyr::left_join"
  )

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(outlier_var, categorical_var),
    input_id = c("outlier_var", "categorical_var"),
    #left_join is used instead of inner_join
    merge_function = "dplyr::left_join"
  )

  common_code_chunks <- reactive({
    # Create a private stack for this function only.
    common_stack <- chunks$new()
    common_stack_push <- function(...) {
      chunks_push(..., chunks = common_stack)
    }

    if (input$tabs == "Line plot") {
      chunks_push_data_merge(merged_data_lineplot(), common_stack)
      outlier_var <- as.vector(merged_data_lineplot()$columns_source$outlier_var)
      categorical_var <- as.vector(merged_data_lineplot()$columns_source$categorical_var)
      lineplot_param <- as.vector(merged_data_lineplot()$columns_source$lineplot_param)
    }
    else {
      chunks_push_data_merge(merged_data(), common_stack)
      outlier_var <- as.vector(merged_data()$columns_source$outlier_var)
      categorical_var <- as.vector(merged_data()$columns_source$categorical_var)
    }


    method <- input$method
    validate(need(outlier_var, "Please select a variable"))
    validate(need(input$method, "Please select a method"))

    #show/hide split_outliers
    if (is_character_empty(categorical_var)) {
      shinyjs::hide("split_outliers")
    } else {
      shinyjs::show("split_outliers")
    }

    #slider
    if (method == "IQR") {
      slider_value <- input$iqr_slider # nolint
    } else if (method == "Z-score") {
      slider_value <- input$zscore_slider # nolint
    } else {
      slider_value <- input$percentile_slider # nolint
    }

    #Define calculation function
    if (method == "IQR") {
      if (input$split_outliers && is_character_single(categorical_var)) {
        common_stack_push(
          bquote({
            calculate_outliers <- function(ANL, outlier_var, slider_value, categorical_var) { # nolint
              all_categories <- lapply(unique(ANL[[.(categorical_var)]]), function(x) {
                ANL_FILTERED <- ANL %>% filter(get(.(categorical_var)) == x) # nolint
                outlier_var_value <- ANL_FILTERED[[.(outlier_var)]]
                q1_q3 <- quantile(ANL_FILTERED[[.(outlier_var)]], probs = c(0.25, 0.75))
                iqr <- q1_q3[2] - q1_q3[1]
                outlier_var_outlier_indices <-
                  !(outlier_var_value >= q1_q3[1] - .(slider_value) * iqr &
                    outlier_var_value <= q1_q3[2] + .(slider_value) * iqr)
                  ANL_FILTERED[outlier_var_outlier_indices, ]
              })
              do.call(rbind, all_categories)
            }
          })
        )
    } else {
      common_stack_push(
        bquote({
          calculate_outliers <- function(ANL, outlier_var, slider_value, categorical_var = NA) { # nolint
            outlier_var_value <- ANL[[.(outlier_var)]]
            q1_q3 <- quantile(ANL[[.(outlier_var)]], probs = c(0.25, 0.75))
            iqr <- q1_q3[2] - q1_q3[1]
            outlier_var_outlier_indices <-
              !(outlier_var_value >= q1_q3[1] - .(slider_value) * iqr &
                outlier_var_value <= q1_q3[2] + .(slider_value) * iqr)
              ANL[outlier_var_outlier_indices, ]
          }
        })
      )}
    } else if (method == "Z-score") {
      if (input$split_outliers && is_character_single(categorical_var)) {
        common_stack_push(
          bquote({
            calculate_outliers <- function(ANL, outlier_var, slider_value, categorical_var) { # nolint
              all_categories <- lapply(unique(ANL[[.(categorical_var)]]), function(x) {
                ANL_FILTERED <- ANL %>% filter(get(.(categorical_var)) == x) # nolint
                if (nrow(ANL_FILTERED) >= 2) {
                  outlier_var_value <- ANL_FILTERED[[.(outlier_var)]]
                  zscore <- abs(outlier_var_value - mean(outlier_var_value)) / sd(outlier_var_value)
                  outlier_var_outlier_indices <- zscore > .(slider_value)
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
      common_stack_push(
        bquote({
          calculate_outliers <- function(ANL, outlier_var, slider_value, categorical_var = NA) { # nolint
            outlier_var_value <- ANL[[.(outlier_var)]]
            zscore <- abs(outlier_var_value - mean(outlier_var_value)) / sd(outlier_var_value)
            outlier_var_outlier_indices <- zscore > .(slider_value)
            ANL[outlier_var_outlier_indices, ]
          }
        })
      )}
    } else if (method == "Percentile") {
      if (input$split_outliers && is_character_single(categorical_var)) {
        common_stack_push(
          bquote({
            calculate_outliers <- function(ANL, outlier_var, slider_value, categorical_var) { # nolint
              all_categories <- lapply(unique(ANL[[.(categorical_var)]]), function(x) {
                ANL_FILTERED <- ANL %>% filter(get(.(categorical_var)) == x) # nolint
                outlier_var_value <- ANL_FILTERED[[outlier_var]]
                lower_bound <- quantile(outlier_var_value, .(slider_value))
                upper_bound <- quantile(outlier_var_value, 1 - .(slider_value))
                outlier_var_outlier_indices <- which(outlier_var_value < lower_bound | outlier_var_value > upper_bound)
                ANL_FILTERED[outlier_var_outlier_indices, ]
              })
              do.call(rbind, all_categories)
            }
            })
          )
        } else {
          common_stack_push(
            bquote({
              calculate_outliers <- function(ANL, outlier_var, slider_value, categorical_var = NA) { # nolint
                outlier_var_value <- ANL[[.(outlier_var)]]
                lower_bound <- quantile(outlier_var_value, .(slider_value))
                upper_bound <- quantile(outlier_var_value, 1 - .(slider_value))
                outlier_var_outlier_indices <- which(outlier_var_value < lower_bound | outlier_var_value > upper_bound)
                ANL[outlier_var_outlier_indices, ]
              }
            })
          )
        }
      }
    common_stack_push(
      bquote({
        ANL_OUTLIER <- calculate_outliers(ANL, .(outlier_var), .(slider_value), .(categorical_var)) # nolint
      })
    )

    chunks_safe_eval(chunks = common_stack)
    common_stack
  })

  # boxplot/violinplot #nolint
  box_plot_r_chunks <- reactive({

    # Create a private stack for this function only.
    boxplot_r_stack <- chunks$new()
    boxplot_r_stack_push <- function(...) {
      chunks_push(..., chunks = boxplot_r_stack)
    }

    # Add common code into this chunk
    chunks_push_chunks(common_code_chunks(), chunks = boxplot_r_stack)
    ANL <- chunks_get_var("ANL", boxplot_r_stack) # nolint
    ANL_OUTLIER <- chunks_get_var("ANL_OUTLIER", boxplot_r_stack) # nolint

    outlier_var <- as.vector(merged_data()$columns_source$outlier_var)
    categorical_var <- as.vector(merged_data()$columns_source$categorical_var)

    # validation
    validate_has_data(ANL, 1)

    # boxplot
    plot_call <- bquote(ANL %>% ggplot()) # nolint

    plot_call <- if (input$boxplot_alts == "Box plot") {
      bquote(.(plot_call) + geom_boxplot(outlier.shape = NA))
    } else if (input$boxplot_alts == "Violin plot") {
      bquote(.(plot_call) + geom_violin(outlier.shape = NA))
    } else
      NULL

    plot_call <- if (is_character_empty(categorical_var)) {
      bquote(
        .(plot_call) +
          aes(x = .(outlier_var), y = .(as.name(outlier_var))) +
          scale_x_discrete() +
          geom_point(
            data = ANL_OUTLIER,
            aes(y = .(as.name(outlier_var))),
            color = "red"
          )
      )
    } else {
      bquote(
        .(plot_call) +
          aes(y = .(as.name(outlier_var)), x = .(as.name(categorical_var))) +
          scale_x_discrete() +
          geom_point(
            data = ANL_OUTLIER,
            aes(x = .(as.name(categorical_var)), y = .(as.name(outlier_var))),
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
    chunks_push_chunks(common_code_chunks(), chunks = density_r_stack)
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
      )} else {
        bquote(
          .(plot_call) +
            facet_grid(~.(as.name(categorical_var)))
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
    chunks_push_chunks(common_code_chunks(), chunks = cumulative_r_stack)
    ANL <- chunks_get_var("ANL", cumulative_r_stack) # nolint
    ANL_OUTLIER <- chunks_get_var("ANL_OUTLIER", cumulative_r_stack) # nolint

    outlier_var <- as.vector(merged_data()$columns_source$outlier_var)
    categorical_var <- as.vector(merged_data()$columns_source$categorical_var)

    # validation
    validate_has_data(ANL, 1)

    # plot
    plot_call <- bquote(ANL %>% ggplot(
      aes(x = .(as.name(outlier_var)))) +
        stat_ecdf(geom = "point")
      )

    plot_call <- if (is_character_empty(categorical_var)) {
      bquote(
        .(plot_call)
      )} else {
        bquote(
          .(plot_call) +
            facet_grid(~.(as.name(categorical_var)))
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
    chunks_push_chunks(common_code_chunks(), chunks = line_r_stack)
    ANL <- chunks_get_var("ANL", line_r_stack) # nolint
    ANL_OUTLIER <- chunks_get_var("ANL_OUTLIER", line_r_stack) # nolint

    outlier_var <- as.vector(merged_data_lineplot()$columns_source$outlier_var)
    categorical_var <- as.vector(merged_data_lineplot()$columns_source$categorical_var)
    lineplot_param <- as.vector(merged_data_lineplot()$columns_source$lineplot_param)

    validate(need(lineplot_param, "Please select a lineplot parameter"))

    # validation
    validate_has_data(ANL, 1)

    plot_call <- bquote(ANL %>% ggplot(
      aes(x = .(as.name(lineplot_param)), y = .(as.name(outlier_var)), group = USUBJID)) +
      geom_line() +
        geom_point(
          data = ANL_OUTLIER,
          aes(y = .(as.name(outlier_var))),
          color = "red"
        )
    )

    plot_call <- if (is_character_empty(categorical_var)) {
      bquote(
        .(plot_call)
      )} else {
        bquote(
          .(plot_call) +
            facet_grid(~.(as.name(categorical_var)))
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

  # slider text
  output$ui_outlier_help <- renderUI({
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

  # table
  table_chunks <- reactive({

    # Create a private stack for this function only.
    table_stack <- chunks$new()
    table_stack_push <- function(...) {
      chunks_push(..., chunks = table_stack)
    }

    chunks_push_chunks(common_code_chunks(), chunks = table_stack)
    ANL_OUTLIER <- chunks_get_var("ANL_OUTLIER", table_stack) # nolint

    table_stack_push(quote({
      tbl <- ANL_OUTLIER
      })
    )
    chunks_safe_eval(table_stack)
    table_stack
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
    chunks_reset()
    chunks_push_chunks(table_chunks())
    chunks_get_var("tbl")
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
