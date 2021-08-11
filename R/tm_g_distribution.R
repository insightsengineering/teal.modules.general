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
#' @param facet_var  optional, (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Which data columns to use for faceting rows.
#' @param freq optional, (`logical`) Whether to display frequency (`TRUE`) or density (`FALSE`).
#'   Defaults to density (`FALSE`).
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
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
#'     code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl",
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
#'       ),
#'       facet_var = data_extract_spec(
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
                              facet_var = NULL,
                              freq = FALSE,
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

  if (!is_class_list("data_extract_spec")(facet_var)) {
    facet_var <- list(facet_var)
  }

  stop_if_not(
    is_character_single(label),
    is_class_list("data_extract_spec")(dist_var) && isFALSE(dist_var[[1]]$select$multiple),
    is.null(group_var) || (is_class_list("data_extract_spec")(group_var) && isFALSE(group_var[[1]]$select$multiple)),
    is.null(facet_var) || (is_class_list("data_extract_spec")(facet_var) && isFALSE(facet_var[[1]]$select$multiple)),
    is_logical_single(freq)
  )

  args <- as.list(environment())

  data_extract_list <- list(
    dist_var = dist_var,
    group_var = group_var,
    facet_var = facet_var
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
  is_single_dataset_value <- is_single_dataset(args$dist_var, args$group_var, args$facet_var)

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
        label = "Stratify by",
        data_extract_spec = args$group_var,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("facet_i"),
        label = "Facet by",
        data_extract_spec = args$facet_var,
        is_single_dataset = is_single_dataset_value
      ),
      panel_group(
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == 'Histogram'"),
          ns = NS(NULL),
          panel_item(
            "Histogram",
            sliderInput(ns("bins"), label = "number of bins:", min = 1, max = 100, value = 30, step = 1),
            shinyWidgets::prettyRadioButtons(
              ns("main_type"),
              label = "Plot Type:",
              choices = c("Density", "Frequency"),
              selected = if (!args$freq) "Density" else "Frequency",
              bigger = TRUE,
              inline = TRUE
            ),
            shinyWidgets::awesomeCheckbox(ns("add_dens"), label = "Overlay Density", value = TRUE),
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
        optionalSelectInput(
          ns("t_dist"),
          div(
            class = "teal-tooltip",
            tagList(
              "Distribution:",
              icon("info-circle"),
              span(
                class = "tooltiptext",
                "Default parameters are optimized with MASS::fitdistr function."
              )
            )
          ),
          choices = c("normal", "lognormal", "gamma", "unif"),
          selected = NULL, multiple = FALSE
        ),
        numericInput(ns("dist_param1"), label = "param1", value = NULL),
        numericInput(ns("dist_param2"), label = "param2", value = NULL),
        span(actionButton(ns("params_reset"), "Reset params")),
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
        "Statistics Table",
        sliderInput(ns("roundn"), "Round to n digits", min = 0, max = 10, value = 2),
        shinyWidgets::awesomeCheckbox(
          ns("add_stats_plot"),
          label = "Overlay params table",
          value = TRUE
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
                             facet_var,
                             plot_height,
                             plot_width) {
  init_chunks()

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(dist_var, group_var, facet_var),
    input_id = c("dist_i", "group_i", "facet_i")
  )

  observeEvent(list(
    input$t_dist,
    input$params_reset,
    input[[extract_input("dist_i", dist_var[[1]]$dataname)]]
  ), {
    if (length(input$t_dist) != 0) {
      dist_var2 <- as.vector(merged_data()$columns_source$dist_i)

      get_dist_params <- function(x, dist) {
        if (dist == "unif") {
          res <- as.list(range(x))
          names(res) <- c("min", "max")
          return(res)
        }
        tryCatch(
          as.list(MASS::fitdistr(x, densfun = dist)$estimate),
          error = function(e) list(param1 = NA, param2 = NA)
        )
      }
      ANL <- datasets$get_data(as.character(dist_var[[1]]$dataname), filtered = TRUE) # nolint
      params <- get_dist_params(ANL[[dist_var2]], input$t_dist)
      params_vec <- round(unname(unlist(params)), 2)
      params_names <- names(params)

      updateNumericInput(session, "dist_param1", label = params_names[1], value = params_vec[1])
      updateNumericInput(session, "dist_param2", label = params_names[2], value = params_vec[2])
    } else {
      updateNumericInput(session, "dist_param1", label = "param1", value = NA)
      updateNumericInput(session, "dist_param2", label = "param2", value = NA)
    }
  },
  ignoreInit = TRUE
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
    s_var <- as.vector(merged_data()$columns_source$group_i)
    f_var <- as.vector(merged_data()$columns_source$facet_i)

    main_type_var <- input$main_type
    roundn <- input$roundn
    t_dist <- isolate(input$t_dist)
    dist_param1 <- input$dist_param1
    dist_param2 <- input$dist_param2
    test_var <- input$dist_tests
    input$tabs

    validate(need(dist_var, "Please select a variable."))
    validate(need(is.numeric(ANL[[dist_var]]), "Please select a numeric variable."))
    validate_has_data(ANL, 1, complete = TRUE)
    common_stack_push(substitute(variable <- ANL[[dist_var]], env = list(dist_var = dist_var)))

    if (length(t_dist) != 0) {
      map_distr_nams <- data.frame(
        distr = c("normal", "lognormal", "gamma", "unif"),
        namparam = I(list(
          c("mean", "sd"),
          c("meanlog", "sdlog"),
          c("shape", "rate"),
          c("min", "max")
        )),
        stringsAsFactors = FALSE
      )
      params_names_raw <- map_distr_nams$namparam[match(t_dist, map_distr_nams$distr)][[1]]

      common_stack_push(substitute({
        params <- as.list(c(dist_param1, dist_param2))
        names(params) <- params_names_raw
      },
      env = list(
        dist_param1 = dist_param1,
        dist_param2 = dist_param2,
        t_dist = t_dist,
        params_names_raw = params_names_raw
      )
      ))
    }

    if (length(s_var) == 0 && length(f_var) == 0) {
      common_stack_push(
        substitute(
          expr = {
            summary_table <- ANL %>%
              dplyr::summarise(
                min = round(min(dist_var_name, na.rm = TRUE), roundn),
                median = round(median(dist_var_name, na.rm = TRUE), roundn),
                mean = round(mean(dist_var_name, na.rm = TRUE), roundn),
                max = round(max(dist_var_name, na.rm = TRUE), roundn),
                sd = round(sd(dist_var_name, na.rm = TRUE), roundn),
                count = dplyr::n()
              )
          },
          env = list(
            dist_var_name = as.name(dist_var),
            roundn = roundn
          )
        )
      )
    } else {
      common_stack_push(
        substitute(
          expr = {
            group_vars <- group_vars_raw
            summary_table <- ANL %>%
              dplyr::group_by_at(dplyr::vars(dplyr::one_of(group_vars))) %>%
              dplyr::summarise(
                min = round(min(dist_var_name, na.rm = TRUE), roundn),
                median = round(median(dist_var_name, na.rm = TRUE), roundn),
                mean = round(mean(dist_var_name, na.rm = TRUE), roundn),
                max = round(max(dist_var_name, na.rm = TRUE), roundn),
                sd = round(sd(dist_var_name, na.rm = TRUE), roundn),
                count = dplyr::n()
              )
          },
          env = list(
            dist_var_name = as.name(dist_var),
            group_vars_raw = c(s_var, f_var),
            roundn = roundn
          )
        )
      )
    }

    if (length(test_var) > 0 && test_var == "Kolmogorov-Smirnov") {
      validate(need(t_dist, "Please select the theoretical distribution."))
      map_dist <- stats::setNames(
        c("pnorm", "plnorm", "pgamma", "punif"),
        c("normal", "lognormal", "gamma", "unif")
      )

      common_stack_push(substitute(
        expr = test_stats <- do.call(ks.test, append(list(quote(variable), mapped_dist), params), quote = FALSE),
        env = list(mapped_dist = unname(map_dist[[t_dist]]))
      ))
    } else if (length(test_var) > 0 && test_var == "Shapiro-Wilk") {
      common_stack_push(substitute(
        expr = {
          test_stats <- stats::shapiro.test(variable)
        },
        env = list(dist_var = dist_var)
      ))
    } else if (length(test_var) > 0 && test_var == "Fligner-Killeen") {
      validate(need(s_var, "select stratify variable"))
      common_stack_push(substitute(test_stats <- stats::fligner.test(variable, ANL[[s_var]]),
                                   env = list(s_var = s_var)
      ))
    } else if (length(test_var) > 0 && test_var == "t-test") {
      validate(need(s_var, "select stratify variable"))
      validate(need(length(unique(ANL[[s_var]])) == 2, "select stratify variable with 2 levels."))
      common_stack_push(substitute(expr = {
        variable_group <- split(variable, ANL[[s_var]])
        test_stats <- stats::t.test(variable_group[[1]], variable_group[[2]])
      }, env = list(s_var = s_var)))
    } else if (length(test_var) > 0 && test_var == "var-test") {
      validate(need(s_var, "select stratify variable"))
      validate(need(length(unique(ANL[[s_var]])) == 2, "select stratify variable with 2 levels."))
      common_stack_push(substitute({
        variable_group <- split(variable, ANL[[s_var]])
        test_stats <- stats::var.test(variable_group[[1]], variable_group[[2]])
      },
      env = list(s_var = s_var)
      ))
    }

    chunks_safe_eval(chunks = common_stack)

    list(common_stack = common_stack)
  })

  dist_plot_r_chunks <- reactive({
    # Create a private stack for this function only.
    distplot_r_stack <- chunks$new()
    distplot_r_stack_push <- function(...) {
      chunks_push(..., chunks = distplot_r_stack)
    }

    # Add common code into this chunk
    chunks_push_chunks(common_code_chunks()$common_stack, chunks = distplot_r_stack)

    dist_var <- as.vector(merged_data()$columns_source$dist_i)
    s_var <- as.vector(merged_data()$columns_source$group_i)
    f_var <- as.vector(merged_data()$columns_source$facet_i)

    main_type_var <- input$main_type
    bins_var <- input$bins
    add_dens_var <- input$add_dens
    t_dist <- isolate(input$t_dist)
    add_stats_plot <- input$add_stats_plot
    ndensity <- 512
    dist_param1 <- isolate(input$dist_param1)
    dist_param2 <- isolate(input$dist_param2)

    validate(need(dist_var, "Please select a variable."))

    m_type <- if (main_type_var == "Density") "..density.." else "..count.."
    m_type2 <- if (main_type_var == "Density") {
      "..density.."
    } else {
      paste(diff(range(merged_data()$data()[[dist_var]], na.rm = TRUE)) / bins_var, "* ..count..")
    }

    plot_call <- if (length(s_var) == 0 && length(f_var) == 0) {
      substitute(
        expr = ggplot(ANL, aes(dist_var_name)) +
          geom_histogram(position = "identity", aes_string(y = m_type), bins = bins_var, alpha = 0.3),
        env = list(
          m_type = m_type,
          bins_var = bins_var,
          dist_var_name = as.name(dist_var)
        )
      )
    } else if (length(s_var) != 0 && length(f_var) == 0) {
      substitute(
        expr = ggplot(ANL, aes(dist_var_name, col = s_var_name)) +
          geom_histogram(position = "identity", aes_string(y = m_type, fill = s_var), bins = bins_var, alpha = 0.3),
        env = list(
          m_type = m_type,
          bins_var = bins_var,
          dist_var_name = as.name(dist_var),
          s_var = s_var,
          s_var_name = as.name(s_var)
        )
      )
    } else if (length(s_var) == 0 && length(f_var) != 0) {
      substitute(
        expr = ggplot(ANL, aes(dist_var_name)) +
          geom_histogram(position = "identity", aes_string(y = m_type), bins = bins_var, alpha = 0.3) +
          facet_wrap(~f_var_name, ncol = 1),
        env = list(
          m_type = m_type,
          bins_var = bins_var,
          dist_var_name = as.name(dist_var),
          f_var_name = as.name(f_var)
        )
      )
    } else {
      substitute(
        expr = ggplot(ANL, aes(dist_var_name, col = s_var_name)) +
          geom_histogram(position = "identity", aes_string(y = m_type, fill = s_var), bins = bins_var, alpha = 0.3) +
          facet_wrap(~f_var_name, ncol = 1),
        env = list(
          m_type = m_type,
          bins_var = bins_var,
          dist_var_name = as.name(dist_var),
          s_var = s_var,
          f_var_name = if (length(f_var) > 0) as.name(f_var) else NULL,
          s_var_name = if (length(s_var) > 0) as.name(s_var) else NULL
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

    if (add_stats_plot) {
      datas <- if (length(t_dist) != 0 && m_type == "..density.." && length(f_var) == 0 && length(s_var) == 0) {
        distplot_r_stack_push(substitute(
          expr = {
            df_params <- as.data.frame(append(params, list(name = t_dist)))
          },
          env = list(t_dist = t_dist)
          ))

        quote(data.frame(
          x = c(0.7, 0), y = c(1, 1),
          tb = I(c(list(df_params = df_params), list(summary_table = summary_table)))
        ))
      } else {
        quote(data.frame(
          x = 0, y = 1,
          tb = I(list(summary_table = summary_table))
        ))
      }

      label <-  if (!is_empty(f_var)) {
        substitute(
          expr = split(tb$summary_table, tb$summary_table$f_var_name, drop = TRUE),
          env = list(f_var = f_var, f_var_name = if (is_empty(f_var)) f_var else as.name(f_var)))
        } else {
          substitute(expr = tb, env = list())
        }

      plot_call <- substitute(
        expr = plot_call + ggpp::geom_table_npc(
          data = data,
          aes(npcx = x, npcy = y, label = label),
          hjust = 0, vjust = 1, size = 4
        ),
        env = list(plot_call = plot_call, data = datas, label = label)
      )
    }

    if (length(s_var) == 0 && length(f_var) == 0 && m_type == "..density..") {
      if (length(t_dist) != 0 && m_type == "..density..") {
        map_dist <- stats::setNames(
          c("dnorm", "dlnorm", "dgamma", "dunif"),
          c("normal", "lognormal", "gamma", "unif")
        )

        plot_call <- substitute(
          expr = plot_call + stat_function(
            data = data.frame(x = range(ANL[[dist_var]]), color = mapped_dist),
            aes(x, color = color),
            fun = mapped_dist,
            n = ndensity,
            size = 2,
            args = params
            ) +
            scale_color_manual(values = stats::setNames("blue", mapped_dist), aesthetics = "color") +
            theme(legend.position = "right"),
          env = list(
            plot_call = plot_call,
            dist_var = dist_var,
            ndensity = ndensity,
            mapped_dist = unname(map_dist[t_dist])
          )
        )
      }
    }

    distplot_r_stack_push(substitute(
      expr = g <- plot_call,
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
    s_var <- as.vector(merged_data()$columns_source$group_i)
    t_dist <- isolate(input$t_dist)
    add_stats_plot <- input$add_stats_plot
    f_var <- as.vector(merged_data()$columns_source$facet_i)
    dist_param1 <- isolate(input$dist_param1)
    dist_param2 <- isolate(input$dist_param2)

    validate(need(dist_var, "Please select a variable."))
    validate(need(t_dist, "Please select the theoretical distribution."))
    validate_has_data(ANL, 1)

    plot_call <- if (length(s_var) == 0 && length(f_var) == 0) {
      substitute(
        expr = ggplot(ANL, aes_string(sample = dist_var)),
        env = list(
          dist_var = dist_var
        )
      )
    } else if (length(s_var) != 0 && length(f_var) == 0) {
      substitute(
        expr = ggplot(ANL, aes_string(sample = dist_var, color = s_var)),
        env = list(
          dist_var = dist_var,
          s_var = s_var
        )
      )
    } else if (length(s_var) == 0 && length(f_var) != 0) {
      substitute(
        expr = ggplot(ANL, aes_string(sample = dist_var)) +
          facet_wrap(~f_var_name, ncol = 1),
        env = list(
          dist_var = dist_var,
          f_var_name = as.name(f_var)
        )
      )
    } else {
      substitute(
        expr = ggplot(ANL, aes_string(sample = dist_var, color = s_var)) +
          facet_wrap(~f_var_name, ncol = 1),
        env = list(
          dist_var = dist_var,
          s_var = s_var,
          f_var_name = as.name(f_var)
        )
      )
    }

    map_dist <- stats::setNames(
      c("qnorm", "qlnorm", "qgamma", "qunif"),
      c("normal", "lognormal", "gamma", "unif")
    )

    plot_call <- substitute(
      expr = plot_call +
        stat_qq(distribution = mapped_dist, dparams = params) +
        xlab("theoretical") +
        ylab("sample"),
      env = list(plot_call = plot_call, mapped_dist = unname(map_dist[t_dist]))
    )

    if (add_stats_plot) {
      datas <- if (length(dist) != 0 && length(f_var) == 0 && length(s_var) == 0) {
        qqplot_r_stack_push(substitute(
          expr = {
            df_params <- as.data.frame(append(params, list(name = t_dist)))
          },
          env = list(t_dist = t_dist)
        ))

        quote(data.frame(
          x = c(0.7, 0), y = c(1, 1),
          tb = I(c(
            list(df_params = df_params),
            list(summary_table = summary_table)
          ))
        ))
      } else {
        quote(data.frame(
          x = 0, y = 1,
          tb = I(list(summary_table = summary_table))
        ))
      }

      label <-  if (!is_empty(f_var)) {
        substitute(
          expr = split(tb$summary_table, tb$summary_table$f_var_name, drop = TRUE),
          env = list(f_var = f_var, f_var_name = if (is_empty(f_var)) f_var else as.name(f_var)))
      } else {
        substitute(expr = tb, env = list())
      }

      plot_call <- substitute(
        expr = plot_call +
          ggpp::geom_table_npc(
            data = data,
            aes(npcx = x, npcy = y, label = label),
            hjust = 0,
            vjust = 1,
            size = 4
          ),
        env = list(
          plot_call = plot_call,
          data = datas,
          label = label
        )
      )
    }

    if (isTRUE(input$qq_line)) {
      plot_call <- substitute(
        expr = plot_call +
          stat_qq_line(distribution = mapped_dist, dparams = params),
        env = list(
          plot_call = plot_call,
          mapped_dist = unname(map_dist[t_dist])
        )
      )
    }

    qqplot_r_stack_push(substitute(
      expr = g <- plot_call,
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
    res <- tryCatch(
      suppressWarnings(chunks_get_var(var = "test_stats", chunks = common_code_chunks()$common_stack)),
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
    tryCatch(suppressWarnings(
      chunks_get_var("summary_table", common_code_chunks()$common_stack)
    ),
    error = function(e) NULL
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
    datanames = get_extract_datanames(list(dist_var, group_var, facet_var)),
    modal_title = "R Code for distribution",
    code_header = "Distribution"
  )
}
