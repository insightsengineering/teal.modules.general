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
#' @param strata_var (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   categorical variable to split the selected distribution variable on.
#' @param group_var  optional, (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Which data columns to use for faceting rows.
#' @param freq optional, (`logical`) Whether to display frequency (`TRUE`) or density (`FALSE`).
#'   Defaults to density (`FALSE`).
#' @param bins optional, (`integer`) If scalar then the histogram bins will have a fixed size.
#'   If a slider should be presented to adjust the number of histogram bins dynamically then it can be a
#'   vector of length three with `c(value, min, max)`.
#'   Defaults to `c(30L, 1L, 100L)`.
#'
#' @export
#'
#' @examples
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' vars1 <- choices_selected(variable_choices(ADSL, c("ARM", "COUNTRY", "SEX")), selected = NULL)
#'
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
#'       strata_var = data_extract_spec(
#'         dataname = "ADSL",
#'         filter = filter_spec(
#'           vars = vars1,
#'           multiple = TRUE
#'         )
#'       ),
#'       group_var = data_extract_spec(
#'         dataname = "ADSL",
#'         filter = filter_spec(
#'           vars = vars1,
#'           multiple = TRUE
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
                              strata_var = NULL,
                              group_var = NULL,
                              freq = FALSE,
                              bins = c(30L, 1L, 100L),
                              plot_height = c(600, 200, 2000),
                              plot_width = NULL,
                              pre_output = NULL,
                              post_output = NULL) {

  if (!is.null(dist_var) && !is_class_list("data_extract_spec")(dist_var)) {
    dist_var <- list(dist_var)
  }

  if (!is_class_list("data_extract_spec")(strata_var)) {
    strata_var <- list_or_null(strata_var)
  }

  if (!is_class_list("data_extract_spec")(group_var)) {
    group_var <- list_or_null(group_var)
  }

  stop_if_not(
    is_character_single(label),
    is_class_list("data_extract_spec")(dist_var) && isFALSE(dist_var[[1]]$select$multiple),
    is.null(strata_var) || (is_class_list("data_extract_spec")(strata_var)),
    is.null(group_var) || (is_class_list("data_extract_spec")(group_var)),
    is_logical_single(freq),
    (is_integer_vector(bins, 3, 3) && is.null(check_slider_input(bins))) || is_integer_single(bins)
  )

  args <- as.list(environment())

  data_extract_list <- list(
    dist_var = dist_var,
    strata_var = strata_var,
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
  is_single_dataset_value <- is_single_dataset(args$dist_var, args$strata_var, args$group_var)

  standard_layout(
    output = tagList(
      tabsetPanel(
        id = ns("tabs"),
        tabPanel("Histogram", plot_with_settings_ui(id = ns("hist_plot"))),
        tabPanel("QQplot", plot_with_settings_ui(id = ns("qq_plot")))
      ),
      h3("Summary statistics"),
      DT::dataTableOutput(ns("summary_table")),
      h3("Tests"),
      DT::dataTableOutput(ns("t_stats"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("dist_var", "strata_var")]),
      data_extract_input(
        id = ns("dist_i"),
        label = "Variable",
        data_extract_spec = args$dist_var,
        is_single_dataset = is_single_dataset_value
      ),
      if (!is.null(args$strata_var)) {
        data_extract_input(
          id = ns("strata_i"),
          label = "Stratify by",
          data_extract_spec = args$strata_var,
          is_single_dataset = is_single_dataset_value
        )
      },
      if (!is.null(args$group_var)) {
        tagList(
          data_extract_input(
            id = ns("group_i"),
            label = "Group by",
            data_extract_spec = args$group_var,
            is_single_dataset = is_single_dataset_value
          ),
          conditionalPanel(
            condition = paste0(
              "input['", extract_input(ns("group_i"), args$group_var[[1]]$dataname, filter = TRUE), "'].length != 0"
            ),
            shinyWidgets::prettyRadioButtons(
              ns("scales_type"),
              label = "Scales:",
              choices = c("Fixed", "Free"),
              selected = "Fixed",
              bigger = FALSE,
              inline = TRUE
            )
          )
        )
      },
      panel_group(
        conditionalPanel(
          condition = paste0("input['", ns("tabs"), "'] == 'Histogram'"),
          panel_item(
            "Histogram",
            optionalSliderInputValMinMax(ns("bins"), "Bins", args$bins, ticks = FALSE, step = 1),
            shinyWidgets::prettyRadioButtons(
              ns("main_type"),
              label = "Plot Type:",
              choices = c("Density", "Frequency"),
              selected = if (!args$freq) "Density" else "Frequency",
              bigger = FALSE,
              inline = TRUE
            ),
            checkboxInput(ns("add_dens"), label = "Overlay Density", value = TRUE),
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
                            "Tests:",
                            choices = c(
                              "Shapiro-Wilk",
                              "t-test (two-samples, not paired)",
                              "one-way ANOVA",
                              "Fligner-Killeen",
                              "F-test",
                              "Kolmogorov-Smirnov (one-sample)",
                              "Anderson-Darling (one-sample)",
                              "Cramer-von Mises (one-sample)",
                              "Kolmogorov-Smirnov (two-samples)"
                            ),
                            selected = NULL
        )
      ),
      panel_item(
        "Statistics Table",
        sliderInput(ns("roundn"), "Round to n digits", min = 0, max = 10, value = 2),
        checkboxInput(
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
                             strata_var,
                             group_var,
                             plot_height,
                             plot_width) {

  init_chunks()

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(dist_var, strata_var, group_var),
    input_id = c("dist_i", "strata_i", "group_i")
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

  merge_vars <- reactive({
    dist_var <- as.vector(merged_data()$columns_source$dist_i)
    s_var <- as.vector(merged_data()$columns_source$strata_i)
    g_var <- as.vector(merged_data()$columns_source$group_i)

    dist_var_name <- if (length(dist_var)) as.name(dist_var) else NULL
    s_var_name <- if (length(s_var)) as.name(s_var) else NULL
    g_var_name <- if (length(g_var)) as.name(g_var) else NULL

    list(dist_var = dist_var,
         s_var = s_var,
         g_var = g_var,
         dist_var_name = dist_var_name,
         s_var_name = s_var_name,
         g_var_name = g_var_name)
  })

  # common chunks ----
  common_code_chunks <- reactive({
    # Create a private stack for this function only.
    common_stack <- chunks$new()

    common_stack_push <- function(...) {
      chunks_push(..., chunks = common_stack)
    }

    chunks_push_data_merge(isolate(merged_data()), common_stack)
    ANL <- chunks_get_var("ANL", common_stack) # nolint

    dist_var <- merge_vars()$dist_var
    s_var <- merge_vars()$s_var
    g_var <- merge_vars()$g_var

    dist_var_name <- merge_vars()$dist_var_name
    s_var_name <- merge_vars()$s_var_name
    g_var_name <- merge_vars()$g_var_name

    roundn <- input$roundn
    dist_param1 <- input$dist_param1
    dist_param2 <- input$dist_param2
    # isolated as dist_param1/dist_param2 already triggered the reactivity
    t_dist <- isolate(input$t_dist)

    if (!is_empty(g_var)) {
      common_stack_push(substitute(
        expr = ANL <- ANL %>% dplyr::mutate(g_var_name := forcats::fct_explicit_na(g_var_name)), # nolint
        env = list(g_var_name = g_var_name)
      ))
    }

    if (!is_empty(s_var)) {
      common_stack_push(substitute(
        expr = ANL <- ANL %>% dplyr::mutate(s_var_name := forcats::fct_explicit_na(s_var_name)), # nolint
        env = list(s_var_name = s_var_name)
      ))
    }

    validate(need(dist_var, "Please select a variable."))
    validate(need(is.numeric(ANL[[dist_var]]), "Please select a numeric variable."))
    validate_has_data(ANL, 1, complete = TRUE)

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

    if (length(s_var) == 0 && length(g_var) == 0) {
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
            strata_vars <- strata_vars_raw
            summary_table <- ANL %>%
              dplyr::group_by_at(dplyr::vars(dplyr::any_of(strata_vars))) %>%
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
            dist_var_name = dist_var_name,
            strata_vars_raw = c(g_var, s_var),
            roundn = roundn
          )
        )
      )
    }

    chunks_safe_eval(chunks = common_stack)

    common_stack
  })

  # distplot chunks ----
  dist_plot_r_chunks <- eventReactive(
    eventExpr = {
      common_code_chunks()
      input$add_stats_plot
      input$scales_type
      input$main_type
      input$bins
      input$add_dens
    },
    valueExpr = {
      # Create a private stack for this function only.
      distplot_stack <- chunks$new()

      ANL <- chunks_get_var("ANL", common_code_chunks()) # nolint
      if ("params" %in% common_code_chunks()$ls()) {
        params <- chunks_get_var("params", common_code_chunks())
      }
      summary_table <- chunks_get_var("summary_table", common_code_chunks()) # nolint

      chunks_reset(chunks = distplot_stack)

      distplot_stack_push <- function(...) {
        chunks_push(..., chunks = distplot_stack)
      }

      # isolated as common chunks already triggered the reactivity
      dist_var <- merge_vars()$dist_var
      s_var <- merge_vars()$s_var
      g_var <- merge_vars()$g_var
      dist_var_name <- merge_vars()$dist_var_name
      s_var_name <- merge_vars()$s_var_name
      g_var_name <- merge_vars()$g_var_name
      t_dist <- input$t_dist
      dist_param1 <- input$dist_param1
      dist_param2 <- input$dist_param2

      add_stats_plot <- input$add_stats_plot # nolint
      scales_type <- input$scales_type
      ndensity <- 512
      main_type_var <- input$main_type
      bins_var <- input$bins
      add_dens_var <- input$add_dens

      m_type <- if (main_type_var == "Density") "..density.." else "..count.."
      m_type2 <- if (main_type_var == "Density") {
        "..density.."
      } else {
        paste(diff(range(merged_data()$data()[[dist_var]], na.rm = TRUE)) / bins_var, "* ..count..")
      }

      plot_call <- if (length(s_var) == 0 && length(g_var) == 0) {
        substitute(
          expr = ggplot(ANL, aes(dist_var_name)) +
            geom_histogram(position = "identity", aes_string(y = m_type), bins = bins_var, alpha = 0.3),
          env = list(
            m_type = m_type,
            bins_var = bins_var,
            dist_var_name = as.name(dist_var)
          )
        )
      } else if (length(s_var) != 0 && length(g_var) == 0) {
        substitute(
          expr = ggplot(ANL, aes(dist_var_name, col = s_var_name)) +
            geom_histogram(position = "identity", aes_string(y = m_type, fill = s_var), bins = bins_var, alpha = 0.3),
          env = list(
            m_type = m_type,
            bins_var = bins_var,
            dist_var_name = dist_var_name,
            s_var = s_var,
            s_var_name = s_var_name
          )
        )
      } else if (length(s_var) == 0 && length(g_var) != 0) {
        substitute(
          expr = ggplot(ANL, aes(dist_var_name)) +
            geom_histogram(position = "identity", aes_string(y = m_type), bins = bins_var, alpha = 0.3) +
            facet_wrap(~g_var_name, ncol = 1, scales = scales_raw),
          env = list(
            m_type = m_type,
            bins_var = bins_var,
            dist_var_name = dist_var_name,
            g_var_name = g_var_name,
            scales_raw = tolower(scales_type)
          )
        )
      } else {
        substitute(
          expr = ggplot(ANL, aes(dist_var_name, col = s_var_name)) +
            geom_histogram(position = "identity", aes_string(y = m_type, fill = s_var), bins = bins_var, alpha = 0.3) +
            facet_wrap(~g_var_name, ncol = 1, scales = scales_raw),
          env = list(
            m_type = m_type,
            bins_var = bins_var,
            dist_var_name = dist_var_name,
            s_var = s_var,
            g_var_name = g_var_name,
            s_var_name = s_var_name,
            scales_raw = tolower(scales_type)
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
        datas <- if (length(t_dist) != 0 && m_type == "..density.." && length(g_var) == 0 && length(s_var) == 0) {
          distplot_stack_push(substitute(
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

        label <-  if (!is_empty(g_var)) {
          substitute(
            expr = split(tb$summary_table, tb$summary_table$g_var_name, drop = TRUE),
            env = list(g_var = g_var, g_var_name = g_var_name))
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

      if (length(s_var) == 0 && length(g_var) == 0 && m_type == "..density..") {
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

      distplot_stack_push(substitute(
        expr = g <- plot_call,
        env = list(plot_call = plot_call)
      ))

      distplot_stack_push(quote(print(g)))

      chunks_safe_eval(distplot_stack)

      distplot_stack
    }
  )

  # qqplot chunks ----
  qq_plot_r_chunks <- eventReactive(
    eventExpr = {
      common_code_chunks()
      input$scales_type
      input$add_stats_plot
      input$qq_line
    },
    valueExpr = {
      # Create a private stack for this function only.
      qqplot_stack <- chunks$new()

      ANL <- chunks_get_var("ANL", common_code_chunks()) # nolint
      if ("params" %in% common_code_chunks()$ls()) {
        params <- chunks_get_var("params", common_code_chunks())
      }
      summary_table <- chunks_get_var("summary_table", common_code_chunks())

      chunks_reset(chunks = qqplot_stack)

      qqplot_stack_push <- function(...) {
        chunks_push(..., chunks = qqplot_stack)
      }

      # isolated as common chunks already triggered the reactivity
      dist_var <- merge_vars()$dist_var
      s_var <- merge_vars()$s_var
      g_var <- merge_vars()$g_var
      dist_var_name <- merge_vars()$dist_var_name
      s_var_name <- merge_vars()$s_var_name
      g_var_name <- merge_vars()$g_var_name
      t_dist <- input$t_dist
      dist_param1 <- input$dist_param1
      dist_param2 <- input$dist_param2

      scales_type <- input$scales_type
      add_stats_plot <- input$add_stats_plot

      validate(need(t_dist, "Please select the theoretical distribution."))
      validate_dist_parameters(t_dist, dist_param1, dist_param2)

      plot_call <- if (length(s_var) == 0 && length(g_var) == 0) {
        substitute(
          expr = ggplot(ANL, aes_string(sample = dist_var)),
          env = list(
            dist_var = dist_var
          )
        )
      } else if (length(s_var) != 0 && length(g_var) == 0) {
        substitute(
          expr = ggplot(ANL, aes_string(sample = dist_var, color = s_var)),
          env = list(
            dist_var = dist_var,
            s_var = s_var
          )
        )
      } else if (length(s_var) == 0 && length(g_var) != 0) {
        substitute(
          expr = ggplot(ANL, aes_string(sample = dist_var)) +
            facet_wrap(~g_var_name, ncol = 1, scales = scales_raw),
          env = list(
            dist_var = dist_var,
            g_var_name = g_var_name,
            scales_raw = tolower(scales_type)
          )
        )
      } else {
        substitute(
          expr = ggplot(ANL, aes_string(sample = dist_var, color = s_var)) +
            facet_wrap(~g_var_name, ncol = 1, scales = scales_raw),
          env = list(
            dist_var = dist_var,
            s_var = s_var,
            g_var_name = g_var_name,
            scales_raw = tolower(scales_type)
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
        datas <- if (length(t_dist) != 0 && length(g_var) == 0 && length(s_var) == 0) {
          qqplot_stack_push(substitute(
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

        label <-  if (!is_empty(g_var)) {
          substitute(
            expr = split(tb$summary_table, tb$summary_table$g_var_name, drop = TRUE),
            env = list(g_var = g_var, g_var_name = g_var_name))
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

      qqplot_stack_push(substitute(
        expr = g <- plot_call,
        env = list(plot_call = plot_call)
      ))

      qqplot_stack_push(quote(print(g)))

      chunks_safe_eval(qqplot_stack)

      qqplot_stack
    }
  )

  # test chunks ----
  test_r_chunks <- eventReactive(
    ignoreNULL = FALSE,
    eventExpr = {
      common_code_chunks()
      input$dist_param1
      input$dist_param2
      input$dist_tests
    },
    valueExpr = {
      # Create a private stack for this function only.
      test_stack <- chunks$new()

      ANL <- chunks_get_var("ANL", common_code_chunks()) # nolint
      if ("params" %in% common_code_chunks()$ls()) {
        params <- chunks_get_var("params", common_code_chunks())
      }

      chunks_reset(chunks = test_stack)

      test_stack_push <- function(...) {
        chunks_push(..., chunks = test_stack)
      }

      dist_var <- merge_vars()$dist_var
      s_var <- merge_vars()$s_var
      g_var <- merge_vars()$g_var

      dist_var_name <- merge_vars()$dist_var_name
      s_var_name <- merge_vars()$s_var_name
      g_var_name <- merge_vars()$g_var_name

      dist_param1 <- input$dist_param1
      dist_param2 <- input$dist_param2
      dist_tests <- input$dist_tests
      t_dist <- input$t_dist

      validate(need(dist_tests, "Please select a test"))

      if ((!is_empty(s_var) || !is_empty(g_var))) {
        counts <- ANL %>%
          dplyr::group_by_at(dplyr::vars(dplyr::any_of(c(s_var, g_var)))) %>%
          dplyr::summarise(n = dplyr::n())

        validate(need(all(counts$n > 5), "Please select strata*group with at least 5 observation each."))
      }

      if (dist_tests %in% c(
        "Kolmogorov-Smirnov (one-sample)",
        "Anderson-Darling (one-sample)",
        "Cramer-von Mises (one-sample)"
      )) {
        validate(need(t_dist, "Please select the theoretical distribution."))
      } else if (dist_tests == "Fligner-Killeen") {
        validate(need(s_var, "Please select stratify variable."))
        validate(need(!identical(s_var, g_var), "Please select different variables for strata and group."))
      } else if (dist_tests %in% c(
        "t-test (two-samples, not paired)",
        "F-test",
        "Kolmogorov-Smirnov (two-samples)"
      )) {
        validate(need(s_var, "Please select stratify variable."))
        if (is_empty(g_var) && !is_empty(s_var)) {
          validate(need(length(unique(ANL[[s_var]])) == 2,
                        "Please select stratify variable with 2 levels."
          ))
        }
        if (!is_empty(g_var) && !is_empty(s_var)) {
          validate(need(all(stats::na.omit(as.vector(tapply(
            ANL[[s_var]], list(ANL[[g_var]]), function(x) length(unique(x))) == 2))),
            "Please select stratify variable with 2 levels, per each group."
          ))
        }
      } else if (dist_tests == "one-way ANOVA") {
        validate(need(s_var, "Please select stratify variable."))
      }

      map_dist <- stats::setNames(
        c("pnorm", "plnorm", "pgamma", "punif"),
        c("normal", "lognormal", "gamma", "unif")
      )
      sks_args <- list(
        test = quote(stats::ks.test),
        args = bquote(append(list(.[[.(dist_var)]], .(map_dist[t_dist])), params)),
        groups = c(g_var, s_var)
      )
      ssw_args <- list(
        test = quote(stats::shapiro.test),
        args = bquote(list(.[[.(dist_var)]])),
        groups = c(g_var, s_var)
      )
      mfil_args <- list(
        test = quote(stats::fligner.test),
        args = bquote(list(.[[.(dist_var)]], .[[.(s_var)]])),
        groups = c(g_var)
      )
      sad_args <- list(
        test = quote(goftest::ad.test),
        args = bquote(append(list(.[[.(dist_var)]], .(map_dist[t_dist])), params)),
        groups = c(g_var, s_var)
      )
      scvm_args <- list(
        test = quote(goftest::cvm.test),
        args = bquote(append(list(.[[.(dist_var)]], .(map_dist[t_dist])), params)),
        groups = c(g_var, s_var)
      )
      manov_args <- list(
        test = quote(stats::aov),
        args = bquote(list(formula(.(dist_var_name) ~ .(s_var_name)), .)),
        groups = c(g_var)
      )
      mt_args <- list(
        test = quote(stats::t.test),
        args = bquote(unname(split(.[[.(dist_var)]], .[[.(s_var)]], drop = TRUE))),
        groups = c(g_var)
      )
      mv_args <- list(
        test = quote(stats::var.test),
        args = bquote(unname(split(.[[.(dist_var)]], .[[.(s_var)]], drop = TRUE))),
        groups = c(g_var)
      )
      mks_args <- list(
        test = quote(stats::ks.test),
        args = bquote(unname(split(.[[.(dist_var)]], .[[.(s_var)]], drop = TRUE))),
        groups = c(g_var)
      )

      tests_base <- switch(
        dist_tests,
        "Kolmogorov-Smirnov (one-sample)" = sks_args,
        "Shapiro-Wilk" = ssw_args,
        "Fligner-Killeen" = mfil_args,
        "one-way ANOVA" = manov_args,
        "t-test (two-samples, not paired)" = mt_args,
        "F-test" = mv_args,
        "Kolmogorov-Smirnov (two-samples)" = mks_args,
        "Anderson-Darling (one-sample)" = sad_args,
        "Cramer-von Mises (one-sample)" = scvm_args
      )

      env <- list(
        t_test = t_dist,
        dist_var = dist_var,
        g_var = g_var,
        s_var = s_var,
        args = tests_base$args,
        groups = tests_base$groups,
        test = tests_base$test,
        dist_var_name = dist_var_name,
        g_var_name = g_var_name,
        s_var_name = s_var_name
      )

      if ((is_empty(s_var) && is_empty(g_var))) {
        test_stack_push(
          substitute(
            expr = {
              test_stats <- ANL %>%
                dplyr::select(dist_var) %>%
                with(., broom::glance(do.call(test, args))) %>%
                dplyr::mutate_if(is.numeric, round, 3)
            },
            env = env
          )
        )
      } else {
        test_stack_push(
          substitute(
            expr = {
              test_stats <- ANL %>%
                dplyr::select(dist_var, s_var, g_var) %>%
                dplyr::group_by_at(dplyr::vars(dplyr::any_of(groups))) %>%
                dplyr::do(tests = broom::glance(do.call(test, args))) %>%
                tidyr::unnest(tests) %>%
                dplyr::mutate_if(is.numeric, round, 3)
            },
            env = env
          )
        )
      }

      chunks_safe_eval(test_stack)

      test_stack
    }
  )

  # outputs ----
  ## building main chunk
  observe({
    tab <- input$tabs
    req(tab) # tab is NULL upon app launch, hence will crash without this statement

    chunks_reset()
    chunks_push_chunks(common_code_chunks())
    # wrapped in if since test chunk could lead into validate error - we do want to continue
    `if`(!is_error(test_r_chunks()), chunks_push_chunks(test_r_chunks()))
    if (tab == "Histogram") {
      chunks_push_chunks(dist_plot_r_chunks())
    } else if (tab == "QQplot") {
      chunks_push_chunks(qq_plot_r_chunks())
    }
  })


  dist_r <- reactive({
    chunks_get_var(var = "g", chunks = dist_plot_r_chunks())
  })

  qq_r <- reactive({
    chunks_get_var(var = "g", chunks = qq_plot_r_chunks())
  })

  tests_r <- reactive({
    chunks_get_var(var = "test_stats", chunks = test_r_chunks())
  })


  output$summary_table <- DT::renderDataTable(
    expr = chunks_get_var("summary_table", chunks = common_code_chunks()),
    options = list(
      autoWidth = TRUE,
      columnDefs = list(list(width = "200px", targets = "_all"))
    ),
    rownames = FALSE
  )

  callModule(
    plot_with_settings_srv,
    id = "hist_plot",
    plot_r = dist_r,
    height = plot_height,
    width = plot_width,
    brushing = FALSE
  )

  callModule(
    plot_with_settings_srv,
    id = "qq_plot",
    plot_r = qq_r,
    height = plot_height,
    width = plot_width,
    brushing = FALSE
  )

  output$t_stats <- DT::renderDataTable(
    expr = tests_r(),
    options = list(scrollX = TRUE),
    rownames = FALSE
  )

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(dist_var, strata_var, group_var)),
    modal_title = "R Code for distribution",
    code_header = "Distribution"
  )
}

#' @description
#' Validates the parameters of the given theoretical distribution.
#'
#' @note Returns a Shiny validation error if the parameters don't meet
#' the assumptions of the theoretical distribution.
#'
#' @param dist_type (`character(1)`) the family of a distribution
#' @param dist_param1 (`numeric(1)`) the first parameter of the distribution
#' @param dist_param2 (`numeric(1)`) the second parameter of the distribution
#' @return NULL
#' @noRd
validate_dist_parameters <- function(dist_type, dist_param1, dist_param2) {
  switch(dist_type,
    "normal" =  {
      validate(need(dist_param2 >= 0, "Variance of the normal distribution needs to be nonnegative"))
    },
    "lognormal" = {
      validate(need(dist_param2 >= 0, "Sigma parameter of the log-normal distribution needs to be nonnegative"))
    },
    "gamma" = {
      validate(need(
        dist_param1 > 0 && dist_param2 > 0,
        "k and theta parameters of the gamma distribution need to be positive"
      ))
    },
    "unif" = NULL
  )
  NULL
}
