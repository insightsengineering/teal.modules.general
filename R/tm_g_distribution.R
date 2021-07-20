#' Distribution Module
#'
#' Module to analyze and explore univariate variable distribution
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#'
#' @param dist_var (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'  variable to consider for the distribution analysis.
#' @param group_var (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   categorical variable to split the selected distribution variable on.
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#' vars <- choices_selected(
#'   choices = variable_choices(ADSL)
#' )
#'
#' dists <- choices_selected(
#'   choices = c("Exponential", "Gaussian", "Gamma", "Beta")
#' )
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- radsl(cached = TRUE)",
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_distribution(
#'       dist_var = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = variable_choices(ADSL, c("AGE", "BMRKR1")),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       group_var = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = variable_choices(ADSL, c("SEX", "COUNTRY", "ARM")),
#'           selected = NULL,
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
tm_g_distribution <- function(label = "Distribution Module",
                              dist_var,
                              group_var = NULL,
                              plot_height = c(600, 200, 2000),
                              plot_width = NULL,
                              pre_output = NULL,
                              post_output = NULL) {
  if (!is.null(dist_var) && !is_class_list("data_extract_spec")(dist_var)) {
    dist_var <- list(dist_var)
  }

  if (!is_class_list("data_extract_spec")(group_var)) {
    group_var <- list(group_var)
  }

  stop_if_not(
    is_character_single(label),
    is_class_list("data_extract_spec")(dist_var),
    is.null(group_var) || is_class_list("data_extract_spec")(group_var)
  )

  args <- as.list(environment())

  data_extract_list <- list(
    dist_var = dist_var,
    group_var = group_var
  )

  module(
    label = label,
    server = srv_distribution,
    server_args = c(data_extract_list, list(plot_height = plot_height, plot_width = plot_width)),
    ui = ui_distribution,
    ui_args = args,
    filters = get_extract_datanames(data_extract_list)
  )
}


ui_distribution <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  is_single_dataset_value <- is_single_dataset(args$dist_var, args$group_var)

  standard_layout(
    output = tagList(
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("Histogram", plot_with_settings_ui(id = ns("hist_plot"))),
        tabPanel("QQplot", plot_with_settings_ui(id = ns("qq_plot")))
      ),
      h3("Statistics:"),
      DT::dataTableOutput(ns("summary_table")),
      tags$br(),
      shiny::verbatimTextOutput(ns("t_stats"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("dist_var", "group_var")]),
      data_extract_input(
        id = ns("dist_i"),
        label = "Variable",
        data_extract_spec = args$dist_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("group_i"),
        label = "Group by",
        data_extract_spec = args$group_var,
        is_single_dataset = is_single_dataset_value
      ),
      panel_group(
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == 'Histogram'"),
          ns = NS(NULL),
          panel_item(
            "Histogram",
            sliderInput(ns("bins"), label = "number of bins:", min = 1, max = 100, value = 30, step = 1),
            shinyWidgets::radioGroupButtons(
              ns("main_type"),
              label = "Plot Type:",
              choices = c("Density", "Frequency"),
              justified = TRUE, status = "primary",
              checkIcon = list(yes = icon("ok", lib = "glyphicon"), no = icon("remove", lib = "glyphicon"))
            ),
            shinyWidgets::awesomeCheckbox(ns("add_dens"), label = "Overlay Density", value = TRUE),
            shinyWidgets::sliderTextInput(
              inputId = ns("ndensity"),
              label = "n density points:",
              choices = 2**seq(1, 10, 1),
              selected = 2**9
            ),
            conditionalPanel(
              condition = paste0(
                "input['", ns("main_type"), "'] == 'Density'",
                " && ",
                "input['", extract_input(
                  ns("group_i"),
                  args$group_var[[1]]$dataname
                ),
                "'].length == 0"
              ),
              sliderInput(ns("boot_iters"), label = div(
                class = "teal-tooltip",
                tagList(
                  "Number of bootstrap:",
                  icon("info-circle"),
                  span(
                    class = "tooltiptext",
                    paste(
                      "Choose the the number of bootstrap samples.\n",
                      "Use random sampling with replacement to get intervals for distribution."
                    )
                  )
                )
              ), 0, 100, 0, 5)
            ),
            collapsed = FALSE
          )
        ),
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == 'QQplot'"),
          panel_item(
            "QQ Plot",
            checkboxInput(ns("qq_line"), label = "Add diagonal line(s)", TRUE),
            collapsed = FALSE
          )
        )
      ),
      panel_item(
        "Theoretical Distribution",
        optionalSelectInput(ns("fitdistr_dist"),
                            "Distribution:",
                            choices = c("normal", "lognormal", "gamma", "unif"),
                            selected = NULL, multiple = FALSE
        ),
        checkboxInput(ns("params_manual"),
                      "manual",
                      value = FALSE
        ),
        numericInput(ns("dist_param1"), "parameter 1", value = NULL),
        numericInput(ns("dist_param2"), "parameter 2", value = NULL),
        span(actionButton(ns("params_update"), "Update params")),
        collapsed = FALSE
      ),
      panel_item(
        "Tests",
        optionalSelectInput(ns("dist_tests"),
                            "Test:",
                            choices = c("Kolmogorov-Smirnov", "Shapiro-Wilk", "Fligner-Killeen", "t-test", "var-test"),
                            selected = NULL
        )
      ),
      panel_item(
        "Statictics Table",
        sliderInput(ns("roundn"), "Round n digits", min = 0, max = 10, value = 2),
        shinyWidgets::awesomeCheckbox(ns("add_stats_plot"),
                                      label =
                                        "Overlay params table", value = TRUE
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

srv_distribution <- function(input,
                             output,
                             session,
                             datasets,
                             dist_var,
                             group_var,
                             plot_height,
                             plot_width) {
  init_chunks()

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(dist_var, group_var),
    input_id = c("dist_i", "group_i")
  )

  common_code_chunks <- reactive({
    # Create a private stack for this function only.
    common_stack <- chunks$new()

    common_stack_push <- function(...) {
      chunks_push(..., chunks = common_stack)
    }

    chunks_push_data_merge(merged_data(), common_stack)
    ANL <- chunks_get_var("ANL", common_stack) # nolint

    dist_var <- as.vector(merged_data()$columns_source$dist_i)
    g_var <- as.vector(merged_data()$columns_source$group_i)
    main_type_var <- input$main_type
    bins_var <- input$bins
    roundn <- input$roundn
    fitdistr_dist <- input$fitdistr_dist
    dist_param1 <- isolate(input$dist_param1)
    dist_param2 <- isolate(input$dist_param2)
    params_manual <- isolate(input$params_manual)
    test_var <- input$dist_tests
    input$tabs
    input$params_update

    validate(need(dist_var, "Please select a variable."))
    validate(need(is.numeric(ANL[[dist_var]]), "Please select a numeric variable."))
    validate_has_data(ANL, 1, complete = TRUE)


    common_stack_push(substitute(variable <- as.numeric(ANL[[dist_var]]), env = list(dist_var = dist_var)))

    if (length(g_var) == 0) {
      common_stack_push(
        substitute(
          expr = {
            summary_table <- data.frame(
              mean = round(mean(variable, na.rm = TRUE), roundn),
              median = round(median(variable, na.rm = TRUE), roundn),
              sd = round(sd(variable, na.rm = TRUE), roundn),
              min = round(min(variable, na.rm = TRUE), roundn),
              max = round(max(variable, na.rm = TRUE), roundn),
              count = length(variable)
            )
          },
          env = list(
            dist_var = dist_var,
            roundn = roundn
          )
        )
      )
    } else {
      common_stack_push(
        substitute(
          expr = {
            summary_table <- ANL %>%
              dplyr::group_by(g_var_name) %>%
              dplyr::summarise(
                mean = round(mean(dist_var_name, na.rm = TRUE), roundn),
                median = round(median(dist_var_name, na.rm = TRUE), roundn),
                sd = round(sd(dist_var_name, na.rm = TRUE), roundn),
                min = round(min(dist_var_name, na.rm = TRUE), roundn),
                max = round(max(dist_var_name, na.rm = TRUE), roundn),
                count = dplyr::n()
              )
          },
          env = list(
            dist_var_name = as.name(dist_var),
            g_var_name = as.name(g_var),
            roundn = roundn
          )
        )
      )
    }

    common_stack_push(substitute({
      test_stats <- NULL
      params <- NULL
      params_vec <- NULL
      params_names <- c("param1", "param2")
    }))

    if (length(fitdistr_dist) != 0 && isFALSE(params_manual)) {
      common_stack_push(
        substitute(
          expr = {
            get_dist_params <- function(x, dist) {
              if (dist == "poisson") {
                x <- as.integer(x)
              }

              if (dist == "unif") {
                res <- as.list(range(x))
                names(res) <- c("min", "max")
                return(res)
              }

              tryCatch(as.list(MASS::fitdistr(x, densfun = dist)$estimate),
                       error = function(e) list(param1 = NA, param2 = NA)
              )
            }

            params <- get_dist_params(ANL[[dist_var]], fitdistr_dist)
            params_vec <- round(unname(unlist(params)), 2)
            params_names <- names(params)
            df_params <- as.data.frame(matrix(params_vec, nrow = 1))
            colnames(df_params) <- params_names
            df_params$name <- fitdistr_dist
          },
          env = list(
            dist_var_name = as.name(dist_var),
            dist_var = dist_var,
            fitdistr_dist = fitdistr_dist
          )
        )
      )
    } else if (length(fitdistr_dist) != 0) {
      map_distr_nams <- data.frame(
        distr = c("normal", "lognormal", "t", "poisson", "gamma", "unif"),
        namparam = I(list(
          c("mean", "sd"),
          c("meanlog", "sdlog"),
          c("m", "s", "df"),
          c("lambda"),
          c("shape", "rate"),
          c("min", "max")
        )),
        stringsAsFactors = FALSE
      )
      params_names <- map_distr_nams$namparam[match(fitdistr_dist, map_distr_nams$distr)][[1]]

      params_raw <- as.list(c(dist_param1, dist_param2))

      common_stack_push(
        substitute(
          expr = {
            params <- params_raw
            names(params) <- params_names
            params_vec <- round(unname(unlist(params)), 2)
            df_params <- as.data.frame(matrix(params_vec, nrow = 1))
            colnames(df_params) <- params_names
            df_params$name <- fitdistr_dist
          },
          env = list(
            dist_param1 = dist_param1,
            dist_param2 = dist_param2,
            fitdistr_dist = fitdistr_dist,
            params_raw = params_raw,
            params_names = params_names
          )
        )
      )
    }


    if (length(test_var) > 0 && test_var == "Kolmogorov-Smirnov") {
      validate(need(fitdistr_dist, "Please select the theoretical distribution."))
      common_stack_push(substitute(
        expr = {
          map_dist <- stats::setNames(
            c("pnorm", "plnorm", "pt", "ppois", "pgamma", "punif"),
            c("normal", "lognormal", "t", "poisson", "gamma", "unif")
          )

          test_stats <- do.call(ks.test, append(list(quote(variable), map_dist[[ks_var]]), params), quote = FALSE)
        },
        env = list(ks_var = fitdistr_dist, dist_var = dist_var)
      ))
    } else if (length(test_var) > 0 && test_var == "Shapiro-Wilk") {
      common_stack_push(substitute(
        expr = {
          test_stats <- stats::shapiro.test(variable)
        },
        env = list(dist_var = dist_var)
      ))
    } else if (length(test_var) > 0 && test_var == "Fligner-Killeen") {
      validate(need(g_var, "select grouping variable"))
      common_stack_push(substitute(test_stats <- stats::fligner.test(variable, ANL[[g_var]]),
                                   env = list(g_var = g_var)
      ))
    } else if (length(test_var) > 0 && test_var == "t-test") {
      validate(need(g_var, "select grouping variable"))
      validate(need(length(unique(ANL[[g_var]])) == 2, "select grouping variable with 2 levels."))
      common_stack_push(substitute(expr = {
        variable_group <- split(variable, ANL[[g_var]])
        test_stats <- stats::t.test(variable_group[[1]], variable_group[[2]])
      }, env = list(g_var = g_var)))
    } else if (length(test_var) > 0 && test_var == "var-test") {
      validate(need(g_var, "select grouping variable"))
      validate(need(length(unique(ANL[[g_var]])) == 2, "select grouping variable with 2 levels."))
      common_stack_push(substitute({
        variable_group <- split(variable, ANL[[g_var]])
        test_stats <- stats::var.test(variable_group[[1]], variable_group[[2]])
      },
      env = list(g_var = g_var)
      ))
    }

    chunks_safe_eval(chunks = common_stack)

    list(common_stack = common_stack)
  })

  observeEvent(list(input$fitdistr_dist, input$params_update, input$params_manual), {
    params_names <- tryCatch(
      chunks_get_var("params_names", common_code_chunks()$common_stack),
      error = function(e) NULL
    )
    params_vec <- tryCatch(chunks_get_var("params_vec", common_code_chunks()$common_stack), error = function(e) NULL)
    updateNumericInput(session = session, inputId = "dist_param1", label = params_names[1], value = params_vec[1])
    updateNumericInput(session = session, inputId = "dist_param2", label = params_names[2], value = params_vec[2])
  }, ignoreInit = TRUE
  )

  dist_plot_r_chunks <- reactive({
    # Create a private stack for this function only.
    distplot_r_stack <- chunks$new()
    distplot_r_stack_push <- function(...) {
      chunks_push(..., chunks = distplot_r_stack)
    }

    # Add common code into this chunk
    chunks_push_chunks(common_code_chunks()$common_stack, chunks = distplot_r_stack)

    dist_var <- as.vector(merged_data()$columns_source$dist_i)
    g_var <- as.vector(merged_data()$columns_source$group_i)
    main_type_var <- input$main_type
    bins_var <- input$bins
    add_dens_var <- input$add_dens
    boot_iters_var <- input$boot_iters
    fitdistr_dist <- input$fitdistr_dist
    add_stats_plot <- input$add_stats_plot
    ndensity <- input$ndensity

    validate(need(dist_var, "Please select a variable."))

    m_type <- if (main_type_var == "Density") "..density.." else "..count.."
    m_type2 <- if (main_type_var == "Density") {
      "..density.."
    } else {
      paste(diff(range(merged_data()$data()[[dist_var]], na.rm = TRUE)) / bins_var, "* ..count..")
    }

    plot_call <- if (length(g_var) == 0) {
      substitute(
        expr = ggplot(ANL, aes(dist_var_name)) +
          geom_histogram(position = "identity", aes_string(y = m_type), bins = bins_var, alpha = 0.3),
        env = list(
          m_type = m_type,
          bins_var = bins_var,
          dist_var_name = as.name(dist_var)
        )
      )
    } else {
      substitute(
        expr = ggplot(ANL, aes(dist_var_name, col = g_var_name)) +
          geom_histogram(position = "identity", aes_string(y = m_type, fill = g_var), bins = bins_var, alpha = 0.3),
        env = list(
          m_type = m_type,
          bins_var = bins_var,
          dist_var_name = as.name(dist_var),
          g_var = g_var,
          g_var_name = as.name(g_var)
        )
      )
    }


    if (add_dens_var) {
      plot_call <- substitute(
        expr = plot_call +
          stat_density(
            aes_string(y = m_type2),
            geom = "line",
            position = "identity",
            alpha = 0.5,
            size = 2,
            n = ndensity
          ),
        env = list(
          plot_call = plot_call,
          m_type2 = m_type2,
          ndensity = ndensity
        )
      )
    }
    # nolint start
    # if (add_stats_plot) {
    #   datas <- if (length(fitdistr_dist) != 0 && m_type == "..density..") {
    #     bquote(data.frame(
    #       x = c(0.7, 0), y = c(1, 1),
    #       tb = I(c(list(df_params), list(summary_table)))
    #     ))
    #   } else {
    #     quote(data.frame(
    #       x = 0, y = 1,
    #       tb = I(list(summary_table))
    #     ))
    #   }
    #
    #   plot_call <- substitute(
    #     expr = plot_call + ggpp::geom_table_npc(
    #       data = data,
    #       aes(npcx = x, npcy = y, label = tb),
    #       hjust = 0, vjust = 1, size = 4
    #     ),
    #     env = list(plot_call = plot_call, data = datas)
    #   )
    # }
    # nolint end

    if (add_stats_plot) {
      if (length(fitdistr_dist) != 0 && length(g_var) == 0) {
        plot_call <- substitute(
          expr = {
            plot_data <- ggplot2::ggplot_build(plot_call)
            y_range <- plot_data$layout$panel_scales_y[[1]]$range$range
            x_range <- plot_data$layout$panel_scales_x[[1]]$range$range
            x_diff <- diff(x_range)

            plot_call +
              annotation_custom(
                gridExtra::tableGrob(df_params, rows = NULL),
                xmin = x_range[2] - 0.25 * x_diff, xmax = x_range[2] - 0.15 * x_diff,
                ymin = sum(y_range) / 1.2, ymax = y_range[2]
              )
          },
          env = list(plot_call = plot_call)
        )
      }
    }

    if (length(g_var) == 0 && m_type == "..density..") {
      if (length(boot_iters_var) != 0 && boot_iters_var != 0) {
        plot_call <- substitute(
          expr = {
            gg_boot <- function(data, iters = 100, seed = 1234, type = "..density..") {
              if (iters == 0 || type != "..density..") {
                return(data.frame(obs = NA, size = NA, x = NA, group = NA, name = NA))
              }

              set.seed(seed)
              len_data <- length(data)

              data_boot <- data.frame(
                obs = rep(1:len_data, iters), x = sample(data, size = len_data * iters, replace = TRUE),
                group = as.character(rep(1:iters, each = len_data))
              )

              data_boot$name <- "boot"
              data_boot <- data_boot[order(data_boot$obs), ]
              cols <- c("obs", "x", "group", "name")
              data_boot <- data_boot[, cols]

              data_boot
            }

            data_boot <- gg_boot(na.omit(ANL[[dist_var]]), boot_iters_var, type = m_type)
            plot_call +
              geom_line(
                data = data_boot,
                aes_string("x", group = "group"),
                stat = "density",
                alpha = 0.1,
                size = 1,
                col = "red"
              ) +
              theme(legend.position = "right")
          },
          env = list(
            plot_call = plot_call,
            dist_var = dist_var,
            boot_iters_var = boot_iters_var,
            m_type = m_type
          )
        )
      }

      if (length(fitdistr_dist) != 0 && m_type == "..density..") {
        plot_call <- substitute(
          expr = {
            map_dist <- stats::setNames(
              c("dnorm", "dlnorm", "dt", "dpois", "dgamma", "dunif"),
              c("normal", "lognormal", "t", "poisson", "gamma", "unif")
            )
            ddist <- unname(map_dist[fitdistr_dist])
            plot_call + stat_function(
              data = data.frame(x = range(ANL[[dist_var]]), color = ddist),
              aes(x, color = color),
              fun = ddist,
              n = ndensity,
              size = 2,
              args = params
            ) +
              scale_color_manual(values = stats::setNames("blue", ddist), aesthetics = "color") +
              theme(legend.position = "right")
          },
          env = list(
            plot_call = plot_call,
            dist_var = dist_var,
            fitdistr_dist = fitdistr_dist,
            boot_iters_var = boot_iters_var,
            m_type = m_type,
            ndensity = ndensity
          )
        )
      }
    }

    distplot_r_stack_push(substitute(
      expr = {
        g <- plot_call
      },
      env = list(plot_call = plot_call)
    ))

    distplot_r_stack_push(quote(print(g)))
    chunks_safe_eval(distplot_r_stack)
    distplot_r_stack
  })

  dist_plot_plot_r <- reactive({
    chunks_reset()
    chunks_push_chunks(dist_plot_r_chunks())
    chunks_get_var(var = "g", chunks = dist_plot_r_chunks())
  })

  qq_plot_r_chunks <- reactive({

    # Create a private stack for this function only.
    qqplot_r_stack <- chunks$new()
    qqplot_r_stack_push <- function(...) {
      chunks_push(..., chunks = qqplot_r_stack)
    }

    # Add common code into this chunk
    chunks_push_chunks(common_code_chunks()$common_stack, chunks = qqplot_r_stack)
    ANL <- chunks_get_var("ANL", qqplot_r_stack) # nolint

    dist_var <- as.vector(merged_data()$columns_source$dist_i)
    g_var <- as.vector(merged_data()$columns_source$group_i)
    dist <- input$fitdistr_dist
    add_stats_plot <- input$add_stats_plot

    validate(need(dist_var, "Please select a variable."))
    validate(need(dist, "Please select the theoretical distribution."))
    validate_has_data(ANL, 1)

    if (length(g_var) == 0) {
      plot_call <- substitute(
        expr = ggplot(ANL, aes_string(sample = dist_var)),
        env = list(
          dist_var = dist_var
        )
      )
    } else {
      plot_call <- substitute(
        expr = ggplot(ANL, aes_string(sample = dist_var, color = g_var)),
        env = list(
          dist_var = dist_var,
          g_var = g_var
        )
      )
    }

    plot_call <- substitute(
      expr = {
        map_dist <- stats::setNames(
          c("qnorm", "qlnorm", "qt", "qpois", "qgamma", "qunif"),
          c("normal", "lognormal", "t", "poisson", "gamma", "unif")
        )
        plot_call +
          stat_qq(distribution = map_dist[dist], dparams = params) +
          xlab("theoretical") +
          ylab("sample")
      },
      env = list(
        plot_call = plot_call,
        dist = dist,
        dist_var = dist_var,
        g_var = g_var
      )
    )

    # nolint start
    # if (add_stats_plot) {
    #   datas <- if (length(dist) != 0) {
    #     bquote(data.frame(
    #       x = c(0.7, 0), y = c(1, 1),
    #       tb = I(c(
    #         list(df_params),
    #         list(summary_table)
    #       ))
    #     ))
    #   } else {
    #     quote(data.frame(
    #       x = 0, y = 1,
    #       tb = I(list(summary_table))
    #     ))
    #   }
    #
    #   plot_call <- substitute(
    #     expr = plot_call +
    #       ggpp::geom_table_npc(
    #         data = data,
    #         aes(npcx = x, npcy = y, label = tb),
    #         hjust = 0,
    #         vjust = 1,
    #         size = 4
    #       ),
    #     env = list(
    #       plot_call = plot_call,
    #       data = datas
    #     )
    #   )
    # }
    # nolint end

    if (add_stats_plot) {
      if (length(dist) != 0) {
        plot_call <- substitute(
          expr = {
            plot_data <- ggplot2::ggplot_build(plot_call)
            y_range <- plot_data$layout$panel_scales_y[[1]]$range$range
            x_range <- plot_data$layout$panel_scales_x[[1]]$range$range
            x_diff <- diff(x_range)

            plot_call +
              annotation_custom(
                gridExtra::tableGrob(df_params, rows = NULL),
                xmin = x_range[2] - 0.25 * x_diff, xmax = x_range[2] - 0.15 * x_diff,
                ymin = sum(y_range) / 1.2, ymax = y_range[2]
              )
          },
          env = list(plot_call = plot_call)
        )
      }
    }

    if (isTRUE(input$qq_line)) {
      plot_call <- substitute(
        expr = {
          plot_call +
            stat_qq_line(distribution = map_dist[dist], dparams = params)
        },
        env = list(
          plot_call = plot_call,
          dist = dist
        )
      )
    }

    qqplot_r_stack_push(substitute(
      expr = {
        g <- plot_call
      },
      env = list(plot_call = plot_call)
    ))

    qqplot_r_stack_push(quote(print(g)))
    chunks_safe_eval(qqplot_r_stack)
    qqplot_r_stack
  })

  qq_plot_plot_r <- reactive({
    chunks_reset()
    chunks_push_chunks(qq_plot_r_chunks())
    chunks_get_var(var = "g", chunks = qq_plot_r_chunks())
  })

  output$t_stats <- renderText({
    res <- tryCatch(chunks_get_var(var = "test_stats", chunks = common_code_chunks()$common_stack),
                    error = function(e) NULL
    )
    if (is.null(res)) {
      return(NULL)
    }
    suppressWarnings(
      paste(capture.output(res)[-1], collapse = "\n")
    )
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
  ),
  rownames = FALSE
  )

  dist_brush <- callModule(
    plot_with_settings_srv,
    id = "hist_plot",
    plot_r = dist_plot_plot_r,
    height = plot_height,
    width = plot_width,
    brushing = FALSE
  )

  qq_brush <- callModule(
    plot_with_settings_srv,
    id = "qq_plot",
    plot_r = qq_plot_plot_r,
    height = plot_height,
    width = plot_width,
    brushing = FALSE
  )

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(dist_var, group_var)),
    modal_title = "R Code for distribution",
    code_header = "Distribution"
  )
}
