#' Missing data module
#'
#' Present analysis of missing observations and patients.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#'
#' @param ggtheme optional, (`character`) `ggplot2` theme to be used by default.
#'   One of `c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test")`.
#'   Each theme can be chosen by the user during the session. Defaults to `"classic"`.
#'
#' @templateVar ggnames "Summary Obs", "Summary Patients", "Combinations Main", "Combinations Hist", "By Subject"
#' @template ggplot2_args_multi
#'
#' @export
#'
#' @examples
#' library(nestcolor)
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' ADRS <- synthetic_cdisc_data("latest")$adrs
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     cdisc_dataset("ADRS", ADRS, code = "ADRS <- synthetic_cdisc_data(\"latest\")$adrs"),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_missing_data(
#'       ggplot2_args = list(
#'         "Combinations Hist" =
#'           teal.widgets::ggplot2_args(labs = list(subtitle = "Plot produced by Missing Data Module", caption = NULL)),
#'         "Combinations Main" = teal.widgets::ggplot2_args(labs = list(title = NULL))
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_missing_data <- function(label = "Missing data",
                            plot_height = c(600, 400, 5000),
                            plot_width = NULL,
                            ggtheme = c(
                              "classic", "gray", "bw", "linedraw",
                              "light", "dark", "minimal", "void", "test"
                            ),
                            ggplot2_args = list(
                              "Combinations Hist" = teal.widgets::ggplot2_args(labs = list(caption = NULL)),
                              "Combinations Main" = teal.widgets::ggplot2_args(labs = list(title = NULL))
                            ),
                            pre_output = NULL,
                            post_output = NULL) {
  logger::log_info("Initializing tm_missing_data")
  if (inherits(ggplot2_args, "ggplot2_args")) ggplot2_args <- list(default = ggplot2_args)

  checkmate::assert_string(label)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )
  ggtheme <- match.arg(ggtheme)
  plot_choices <- c("Summary Obs", "Summary Patients", "Combinations Main", "Combinations Hist", "By Subject")
  checkmate::assert_list(ggplot2_args, types = "ggplot2_args")
  checkmate::assert_subset(names(ggplot2_args), c("default", plot_choices))

  module(
    label,
    server = srv_page_missing_data,
    server_args = list(plot_height = plot_height, plot_width = plot_width, ggplot2_args = ggplot2_args),
    ui = ui_page_missing_data,
    filters = "all",
    ui_args = list(pre_output = pre_output, post_output = post_output, ggtheme = ggtheme)
  )
}

ui_page_missing_data <- function(id, data, pre_output = NULL, post_output = NULL, ggtheme) {
  ns <- NS(id)
  datanames <- names(data)

  if_subject_plot <- !is.null(attr(data, "join_keys"))

  shiny::tagList(
    include_css_files("custom"),
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        div(
          class = "flex",
          column(
            width = 12,
            do.call(
              tabsetPanel,
              c(
                id = ns("dataname_tab"),
                lapply(
                  datanames,
                  function(x) {
                    tabPanel(
                      title = x,
                      column(
                        width = 12,
                        div(
                          class = "mt-4",
                          ui_missing_data(id = ns(x), by_subject_plot = if_subject_plot)
                        )
                      )
                    )
                  }
                )
              )
            )
          )
        )
      ),
      encoding = div(
        tagList(
          lapply(
            datanames,
            function(x) {
              conditionalPanel(
                sprintf("$(\"#%s > li.active\").text().trim() == \"%s\"", ns("dataname_tab"), x),
                encoding_missing_data(
                  id = ns(x),
                  summary_per_patient = if_subject_plot,
                  ggtheme = ggtheme,
                  datanames = datanames
                )
              )
            }
          )
        )
      ),
      pre_output = pre_output,
      post_output = post_output
    )
  )
}

srv_page_missing_data <- function(id, data, reporter, filter_panel_api, plot_height, plot_width, ggplot2_args) {
  moduleServer(id, function(input, output, session) {
    lapply(
      names(data),
      function(x) {
        srv_missing_data(
          id = x,
          data = data,
          reporter = reporter,
          filter_panel_api = filter_panel_api,
          dataname = x,
          plot_height = plot_height,
          plot_width = plot_width,
          ggplot2_args = ggplot2_args
        )
      }
    )
  })
}

ui_missing_data <- function(id, by_subject_plot = FALSE) {
  ns <- NS(id)

  tab_list <- list(
    tabPanel(
      "Summary",
      teal.widgets::plot_with_settings_ui(id = ns("summary_plot")),
      helpText(
        p(paste(
          'The "Summary" graph shows the number of missing values per variable (both absolute and percentage),',
          "sorted by magnitude."
        )),
        p(
          'The "summary per patients" graph is showing how many subjects have at least one missing observation',
          "for each variable. It will be most useful for panel datasets."
        )
      )
    ),
    tabPanel(
      "Combinations",
      teal.widgets::plot_with_settings_ui(id = ns("combination_plot")),
      helpText(
        p(paste(
          'The "Combinations" graph is used to explore the relationship between the missing data within',
          "different columns of the dataset.",
          "It shows the different patterns of missingness in the rows of the data.",
          'For example, suppose that 70 rows of the data have exactly columns "A" and "B" missing.',
          "In this case there would be a bar of height 70 in the top graph and",
          'the column below this in the second graph would have rows "A" and "B" cells shaded red.'
        )),
        p(paste(
          "Due to the large number of missing data patterns possible, only those with a large set of observations",
          'are shown in the graph and the "Combination cut-off" slider can be used to adjust the number shown.'
        ))
      )
    ),
    tabPanel(
      "By Variable Levels",
      teal.widgets::get_dt_rows(ns("levels_table"), ns("levels_table_rows")),
      DT::dataTableOutput(ns("levels_table"))
    )
  )
  if (isTRUE(by_subject_plot)) {
    tab_list <- append(
      tab_list,
      list(tabPanel(
        "Grouped by Subject",
        teal.widgets::plot_with_settings_ui(id = ns("by_subject_plot")),
        helpText(
          p(paste(
            "This graph shows the missingness with respect to subjects rather than individual rows of the",
            "dataset. Each row represents one dataset variable and each column a single subject. Only subjects",
            "with at least one record in this dataset are shown. For a given subject, if they have any missing",
            "values of a specific variable then the appropriate cell in the graph is marked as missing."
          ))
        )
      ))
    )
  }

  do.call(
    tabsetPanel,
    c(
      id = ns("summary_type"),
      tab_list
    )
  )
}

encoding_missing_data <- function(id, summary_per_patient = FALSE, ggtheme, datanames) {
  ns <- NS(id)

  tagList(
    ### Reporter
    teal.reporter::simple_reporter_ui(ns("simple_reporter")),
    ###
    tags$label("Encodings", class = "text-primary"),
    helpText(
      paste0("Dataset", `if`(length(datanames) > 1, "s", ""), ":"),
      tags$code(paste(datanames, collapse = ", "))
    ),
    uiOutput(ns("variables")),
    actionButton(
      ns("filter_na"),
      span("Select only vars with missings", class = "whitespace-normal"),
      width = "100%",
      class = "mb-4"
    ),
    conditionalPanel(
      sprintf("$(\"#%s > li.active\").text().trim() == \"Summary\"", ns("summary_type")),
      checkboxInput(
        ns("any_na"),
        div(
          class = "teal-tooltip",
          tagList(
            "Add **anyna** variable",
            icon("circle-info"),
            span(
              class = "tooltiptext",
              "Describes the number of observations with at least one missing value in any variable."
            )
          )
        ),
        value = FALSE
      ),
      if (summary_per_patient) {
        checkboxInput(
          ns("if_patients_plot"),
          div(
            class = "teal-tooltip",
            tagList(
              "Add summary per patients",
              icon("circle-info"),
              span(
                class = "tooltiptext",
                paste(
                  "Displays the number of missing values per observation,",
                  "where the x-axis is sorted by observation appearance in the table."
                )
              )
            )
          ),
          value = FALSE
        )
      }
    ),
    conditionalPanel(
      sprintf("$(\"#%s > li.active\").text().trim() == \"Combinations\"", ns("summary_type")),
      uiOutput(ns("cutoff"))
    ),
    conditionalPanel(
      sprintf("$(\"#%s > li.active\").text().trim() == \"By Variable Levels\"", ns("summary_type")),
      tagList(
        uiOutput(ns("group_by_var_ui")),
        uiOutput(ns("group_by_vals_ui")),
        radioButtons(
          ns("count_type"),
          label = "Display missing as",
          choices = c("counts", "proportions"),
          selected = "counts",
          inline = TRUE
        )
      )
    ),
    teal.widgets::panel_item(
      title = "Plot settings",
      teal.widgets::optionalSelectInput(
        inputId = ns("ggtheme"),
        label = "Theme (by ggplot):",
        choices = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test"),
        selected = ggtheme,
        multiple = FALSE
      )
    ),
    hr(),
    teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
  )
}

#' @importFrom rlang .data
srv_missing_data <- function(id, data, reporter, filter_panel_api, dataname, plot_height, plot_width, ggplot2_args) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  moduleServer(id, function(input, output, session) {
    prev_group_by_var <- reactiveVal("")

    data_r <- reactive({data[[dataname]]()})

    data_keys <- reactive(attr(data, "join_keys")$get(dataname)[[dataname]])

    # chunks needed by all three outputs stored here
    common_code <- reactive({
      anl_name <- dataname
      anl <- data[[dataname]]()
      assign(anl_name, anl)
      group_var <- input$group_by_var

      nenv <- list()
      nenv[["ANL"]] <- anl

      quosure <- if (!is.null(selected_vars()) && length(selected_vars()) != ncol(anl)) {
        teal.code::new_quosure(
          env = list2env(nenv),
          code = substitute(
            expr = ANL <- anl_name[, selected_vars], # nolint
            env = list(anl_name = as.name(anl_name), selected_vars = selected_vars())
          )
        )
      } else {
        teal.code::new_quosure(
          env = list2env(nenv),
          code = substitute(expr = ANL <- anl_name, env = list(anl_name = as.name(anl_name))) # nolint
        )
      }

      if (input$summary_type == "By Variable Levels" && !is.null(group_var) && !(group_var %in% selected_vars())) {
        quosure <- tea.code::eval_code(
          quosure,
          substitute(
            expr = ANL[[group_var]] <- anl_name[[group_var]], # nolint
            env = list(group_var = group_var, anl_name = as.name(anl_name))
          ),
          name = "ANL_group_var_call"
        )
      }

      new_col_name <- "**anyna**" # nolint variable assigned and used

      quosure <- teal.code::eval_code(
        quosure,
        substitute(
          expr =
            create_cols_labels <- function(cols, just_label = FALSE) {
              column_labels <- column_labels_value
              column_labels[is.na(column_labels) | length(column_labels) == 0] <- ""
              if (just_label) {
                labels <- column_labels[cols]
              } else {
                labels <- ifelse(cols == new_col_name | cols == "", cols, paste0(column_labels[cols], " [", cols, "]"))
              }
              return(labels)
            },
          env = list(
            new_col_name = new_col_name,
            column_labels_value = c(formatters::var_labels(data[[dataname]]())[selected_vars()],
                                    new_col_name = new_col_name)
          )
        ),
        name = "create_cols_labels_function_call"
      )
      quosure
    })

    selected_vars <- reactive({
      req(input$variables_select)
      keys <- data_keys()
      vars <- unique(c(keys, input$variables_select))
      validate(need(length(setdiff(vars, keys)) >= 1, "Please also select non-key columns."))
      vars
    })

    vars_summary <- reactive({

      na_count <- data_r() %>%
        sapply(function(x) mean(is.na(x)), USE.NAMES = TRUE) %>%
        sort(decreasing = TRUE)

      tibble::tibble(
        key = names(na_count),
        value = unname(na_count),
        label = cut(na_count, breaks = seq(from = 0, to = 1, by = 0.1), include.lowest = TRUE)
      )
    })

    output$variables <- renderUI({
      choices <- split(x = vars_summary()$key, f = vars_summary()$label, drop = TRUE) %>% rev()
      selected <- choices <- unname(unlist(choices))

      teal.widgets::optionalSelectInput(
        session$ns("variables_select"),
        label = "Select variables",
        label_help = HTML(paste0("Dataset: ", tags$code(dataname))),
        choices = teal.transform::variable_choices(data_r(), choices),
        selected = selected,
        multiple = TRUE
      )
    })

    observeEvent(input$filter_na, {
      choices <- vars_summary() %>%
        dplyr::select(.data$key) %>%
        getElement(name = 1)

      selected <- vars_summary() %>%
        dplyr::filter(.data$value > 0) %>%
        dplyr::select(.data$key) %>%
        getElement(name = 1)

      teal.widgets::updateOptionalSelectInput(
        session = session,
        inputId = "variables_select",
        choices = teal.transform::variable_choices(data_r()),
        selected = selected
      )
    })

    output$group_by_var_ui <- renderUI({
      all_choices <- teal.transform::variable_choices(data_r())
      cat_choices <- all_choices[!sapply(data_r(), function(x) is.numeric(x) || inherits(x, "POSIXct"))]
      validate(
        need(cat_choices, "Dataset does not have any non-numeric or non-datetime variables to use to group data with")
      )
      teal.widgets::optionalSelectInput(
        session$ns("group_by_var"),
        label = "Group by variable",
        choices = cat_choices,
        selected = `if`(
          is.null(isolate(input$group_by_var)),
          cat_choices[1],
          isolate(input$group_by_var)
        ),
        multiple = FALSE,
        label_help = paste0("Dataset: ", dataname)
      )
    })

    output$group_by_vals_ui <- renderUI({
      req(input$group_by_var)

      choices <- teal.transform::value_choices(data_r(), input$group_by_var, input$group_by_var)
      prev_choices <- isolate(input$group_by_vals)

      # determine selected value based on filtered data
      # display those previously selected values that are still available
      selected <- if (!is.null(prev_choices) && any(prev_choices %in% choices)) {
        prev_choices[match(choices[choices %in% prev_choices], prev_choices)]
      } else if (!is.null(prev_choices) &&
        !any(prev_choices %in% choices) &&
        isolate(prev_group_by_var()) == input$group_by_var) {
        # if not any previously selected value is available and the grouping variable is the same,
        # then display NULL
        NULL
      } else {
        # if new grouping variable (i.e. not any previously selected value is available),
        # then display all choices
        choices
      }

      prev_group_by_var(input$group_by_var) # set current group_by_var
      validate(need(length(choices) < 100, "Please select group-by variable with fewer than 100 unique values"))

      teal.widgets::optionalSelectInput(
        session$ns("group_by_vals"),
        label = "Filter levels",
        choices = choices,
        selected = selected,
        multiple = TRUE,
        label_help = paste0("Dataset: ", dataname)
      )
    })

    summary_plot_r <- reactive({
      req(input$summary_type == "Summary") # needed to trigger show r code update on tab change
      teal::validate_has_data(data_r(), 1)
      validate(need(length(input$variables_select) > 0, "No variables selected"))

      quosure <- common_code()

      if (input$any_na) {
        new_col_name <- "**anyna**" # nolint (local variable is assigned and used)
        quosure <- teal.code::eval_code(
          quosure,
          substitute(
            expr = ANL[[new_col_name]] <- ifelse(rowSums(is.na(ANL)) > 0, NA, FALSE), # nolint
            env = list(new_col_name = new_col_name)
          )
        )
      }

      quosure <- teal.code::eval_code(
        quosure,
        substitute(
          expr = analysis_vars <- setdiff(colnames(ANL), data_keys),
          env = list(data_keys = data_keys())
        ),
        name = "analysis_vars_call"
      ) %>%
        teal.code::eval_code(
        substitute(
          expr = summary_plot_obs <- data_frame_call[, analysis_vars] %>%
            dplyr::summarise_all(list(function(x) sum(is.na(x)))) %>%
            tidyr::pivot_longer(tidyselect::everything(), names_to = "col", values_to = "n_na") %>%
            dplyr::mutate(n_not_na = nrow(ANL) - n_na) %>%
            tidyr::pivot_longer(-col, names_to = "isna", values_to = "n") %>%
            dplyr::mutate(isna = isna == "n_na", n_pct = n / nrow(ANL) * 100),
          env = list(data_frame_call = if (!inherits(data[[dataname]](), "tbl_df")) {
            quote(tibble::as_tibble(ANL))
          } else {
            quote(ANL)
          })
        ),
        name = "summary_plot_obs_call"
      ) %>%
      # x axis ordering according to number of missing values and alphabet
      teal.code::eval_code(
        quote(
          expr = x_levels <- dplyr::filter(summary_plot_obs, isna) %>%
            dplyr::arrange(n_pct, dplyr::desc(col)) %>%
            dplyr::pull(col) %>%
            create_cols_labels()
        ),
        name = "x_levels_call"
      )

      # always set "**anyna**" level as the last one
      if (isolate(input$any_na)) {
        quosure <- teal.code::eval_code(
          quosure,
          quote(x_levels <- c(setdiff(x_levels, "**anyna**"), "**anyna**")),
          name = "x_levels_anyna_call"
        )
      }

      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        labs = list(x = "Variable", y = "Missing observations"),
        theme = list(legend.position = "bottom", axis.text.x = quote(element_text(angle = 45, hjust = 1)))
      )

      all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Summary Obs"]],
        user_default = ggplot2_args$default,
        module_plot = dev_ggplot2_args
      )

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args,
        ggtheme = input$ggtheme
      )

      quosure <- teal.code::eval_code(
        quosure,
        substitute(
          p1 <- summary_plot_obs %>%
            ggplot() +
            aes(
              x = factor(create_cols_labels(col), levels = x_levels),
              y = n_pct,
              fill = isna
            ) +
            geom_bar(position = "fill", stat = "identity") +
            scale_fill_manual(
              name = "",
              values = c("grey90", c(getOption("ggplot2.discrete.colour")[2], "#ff2951ff")[1]),
              labels = c("Present", "Missing")
            ) +
            scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
            geom_text(
              aes(label = ifelse(isna == TRUE, sprintf("%d [%.02f%%]", n, n_pct), ""), y = 1),
              hjust = 1,
              color = "black"
            ) +
            labs +
            ggthemes +
            themes +
            coord_flip(),
          env = list(
            labs = parsed_ggplot2_args$labs,
            themes = parsed_ggplot2_args$theme,
            ggthemes = parsed_ggplot2_args$ggtheme
          )
        ),
        name = "p1_plot_call"
      )

      if (isTRUE(input$if_patients_plot)) {
        keys <- data_keys()
        quosure <- teal.code::eval_code(
          quosure,
          substitute(
            expr = parent_keys <- keys,
            env = list(keys = keys)
          ),
          name = "parent_keys_call"
        ) %>% teal.code::eval_code(
          quote(ndistinct_subjects <- dplyr::n_distinct(ANL[, parent_keys])),
          name = "ndistinct_subjects_call"
        ) %>%
          teal.code::eval_code(
          quote(
            summary_plot_patients <- ANL[, c(parent_keys, analysis_vars)] %>%
              dplyr::group_by_at(parent_keys) %>%
              dplyr::summarise_all(anyNA) %>%
              tidyr::pivot_longer(cols = !tidyselect::all_of(parent_keys), names_to = "col", values_to = "anyna") %>%
              dplyr::group_by_at(c("col")) %>%
              dplyr::summarise(count_na = sum(anyna)) %>%
              dplyr::mutate(count_not_na = ndistinct_subjects - count_na) %>%
              tidyr::pivot_longer(-c(col), names_to = "isna", values_to = "n") %>%
              dplyr::mutate(isna = isna == "count_na", n_pct = n / ndistinct_subjects * 100) %>%
              dplyr::arrange_at(c("isna", "n"), .funs = dplyr::desc)
          ),
          name = "summary_plot_patients_call"
        )

        dev_ggplot2_args <- teal.widgets::ggplot2_args(
          labs = list(x = "", y = "Missing patients"),
          theme = list(
            legend.position = "bottom",
            axis.text.x = quote(element_text(angle = 45, hjust = 1)),
            axis.text.y = quote(element_blank())
          )
        )

        all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Summary Patients"]],
          user_default = ggplot2_args$default,
          module_plot = dev_ggplot2_args
        )

        parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
          all_ggplot2_args,
          ggtheme = input$ggtheme
        )

        quosure <- teal.code::eval_code(
          quosure,
          substitute(
            p2 <- summary_plot_patients %>%
              ggplot() +
              aes_(
                x = ~ factor(create_cols_labels(col), levels = x_levels),
                y = ~n_pct,
                fill = ~isna
              ) +
              geom_bar(alpha = 1, stat = "identity", position = "fill") +
              scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
              scale_fill_manual(
                name = "",
                values = c("grey90", c(getOption("ggplot2.discrete.colour")[2], "#ff2951ff")[1]),
                labels = c("Present", "Missing")
              ) +
              geom_text(
                aes(label = ifelse(isna == TRUE, sprintf("%d [%.02f%%]", n, n_pct), ""), y = 1),
                hjust = 1,
                color = "black"
              ) +
              labs +
              ggthemes +
              themes +
              coord_flip(),
            env = list(
              labs = parsed_ggplot2_args$labs,
              themes = parsed_ggplot2_args$theme,
              ggthemes = parsed_ggplot2_args$ggtheme
            )
          ),
          name = "p2_plot_call"
        ) %>% teal.code::eval_code(
          quote({
            g1 <- ggplotGrob(p1)
            g2 <- ggplotGrob(p2)
            g <- gridExtra::gtable_cbind(g1, g2, size = "first")
            g$heights <- grid::unit.pmax(g1$heights, g2$heights)
            grid::grid.newpage()
          }),
          name = "final_plot_call"
        )
      } else {
        quosure <- teal.code::eval_code(
          quosure,
          quote({
            g <- ggplotGrob(p1)
            grid::grid.newpage()
          }),
          name = "final_plot_call"
        )
      }

      teal.code::eval_code(
        quosure,
        quote(grid::grid.draw(g)),
        name = "grid_draw_call"
      )
    })

    g_summary_plot_r <- reactive(summary_plot_r()[["g"]])

    combination_cutoff_r <- reactive({
      teal.code::eval_code(
        common_code(),
        quote({
          combination_cutoff <- ANL %>%
            dplyr::mutate_all(is.na) %>%
            dplyr::group_by_all() %>%
            dplyr::tally() %>%
            dplyr::ungroup()
        }),
        name = "combination_cutoff_call"
      )
    })

    output$cutoff <- renderUI({
      x <- combination_cutoff_r()[["combination_cutoff"]]$n

      # select 10-th from the top
      n <- length(x)
      idx <- max(1, n - 10)
      prev_value <- isolate(input$combination_cutoff)
      value <- `if`(
        is.null(prev_value) || prev_value > max(x) || prev_value < min(x),
        sort(x, partial = idx)[idx], prev_value
      )

      teal.widgets::optionalSliderInputValMinMax(
        session$ns("combination_cutoff"),
        "Combination cut-off",
        c(value, range(x))
      )
    })

    combination_plot_r <- reactive({
      req(input$summary_type == "Combinations") # needed to trigger show r code update on tab change
      teal::validate_has_data(data_r(), 1)
      validate(need(length(input$variables_select) > 0, "No variables selected"))
      req(input$combination_cutoff)

      quosure <- teal.code::eval_code(
        combination_cutoff_r(),
        substitute(
          expr = data_combination_plot_cutoff <- combination_cutoff %>%
            dplyr::filter(n >= combination_cutoff_value) %>%
            dplyr::mutate(id = rank(-n, ties.method = "first")) %>%
            tidyr::pivot_longer(-c(n, id), names_to = "key", values_to = "value") %>%
            dplyr::arrange(n),
          env = list(combination_cutoff_value = input$combination_cutoff)
        ),
        name = "data_combination_plot_cutoff_call_1"
      )

      # find keys in dataset not selected in the UI and remove them from dataset
      keys_not_selected <- setdiff(data_keys(), input$variables_select)
      if (length(keys_not_selected) > 0) {
        quosure <- teal.code::eval_code(
          quosure,
          substitute(
            expr = data_combination_plot_cutoff <- data_combination_plot_cutoff %>%
              dplyr::filter(!key %in% keys_not_selected),
            env = list(keys_not_selected = keys_not_selected)
          ),
          name = "data_combination_plot_cutoff_call_2"
        )
      }

      quosure <- teal.code::eval_code(
        quosure,
        quote(
          labels <- data_combination_plot_cutoff %>%
            dplyr::filter(key == key[[1]]) %>%
            getElement(name = 1)
        ),
        name = "labels_call"
      )

      dev_ggplot2_args1 <- teal.widgets::ggplot2_args(
        labs = list(x = "", y = ""),
        theme = list(
          legend.position = "bottom",
          axis.text.x = quote(element_blank())
        )
      )

      all_ggplot2_args1 <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Combinations Hist"]],
        user_default = ggplot2_args$default,
        module_plot = dev_ggplot2_args1
      )

      parsed_ggplot2_args1 <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args1,
        ggtheme = "void"
      )

      dev_ggplot2_args2 <- teal.widgets::ggplot2_args(
        labs = list(x = "", y = ""),
        theme = list(
          legend.position = "bottom",
          axis.text.x = quote(element_blank()),
          axis.ticks = quote(element_blank()),
          panel.grid.major = quote(element_blank())
        )
      )

      all_ggplot2_args2 <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Combinations Main"]],
        user_default = ggplot2_args$default,
        module_plot = dev_ggplot2_args2
      )

      parsed_ggplot2_args2 <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args2,
        ggtheme = input$ggtheme
      )

      teal.code::eval_code(
        quosure,
        substitute(
          expr = {
            p1 <- data_combination_plot_cutoff %>%
              dplyr::select(id, n) %>%
              dplyr::distinct() %>%
              ggplot(aes(x = id, y = n)) +
              geom_bar(stat = "identity", fill = c(getOption("ggplot2.discrete.colour")[2], "#ff2951ff")[1]) +
              geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.25) +
              ylim(c(0, max(data_combination_plot_cutoff$n) * 1.5)) +
              labs1 +
              ggthemes1 +
              themes1

            graph_number_rows <- length(unique(data_combination_plot_cutoff$id))
            graph_number_cols <- nrow(data_combination_plot_cutoff) / graph_number_rows

            p2 <- data_combination_plot_cutoff %>% ggplot() +
              aes(x = create_cols_labels(key), y = id - 0.5, fill = value) +
              geom_tile(alpha = 0.85, height = 0.95) +
              scale_fill_manual(
                name = "",
                values = c("grey90", c(getOption("ggplot2.discrete.colour")[2], "#ff2951ff")[1]),
                labels = c("Present", "Missing")
              ) +
              geom_hline(yintercept = seq_len(1 + graph_number_rows) - 1) +
              geom_vline(xintercept = seq_len(1 + graph_number_cols) - 0.5, linetype = "dotted") +
              coord_flip() +
              labs2 +
              ggthemes2 +
              themes2

            g1 <- ggplotGrob(p1)
            g2 <- ggplotGrob(p2)

            g <- gridExtra::gtable_rbind(g1, g2, size = "last")
            g$heights[7] <- grid::unit(0.2, "null") # rescale to get the bar chart smaller
            grid::grid.newpage()
            grid::grid.draw(g)
          },
          env = list(
            labs1 = parsed_ggplot2_args1$labs,
            themes1 = parsed_ggplot2_args1$theme,
            ggthemes1 = parsed_ggplot2_args1$ggtheme,
            labs2 = parsed_ggplot2_args2$labs,
            themes2 = parsed_ggplot2_args2$theme,
            ggthemes2 = parsed_ggplot2_args2$ggtheme
          )
        ),
        name = "grid_draw_call"
      )
    })

    g_combination_plot_r <- reactive(combination_plot_r()[["g"]])

    table_r <- reactive({
      req(input$summary_type == "By Variable Levels") # needed to trigger show r code update on tab change
      teal::validate_has_data(data_r(), 1)

      # extract the ANL dataset for use in further validation
      anl <- common_code()[["ANL"]]

      validate(need(input$count_type, "Please select type of counts"))
      if (!is.null(input$group_by_var)) {
        validate(need(!is.null(input$group_by_vals), "Please select both group-by variable and values"))
      }

      group_var <- input$group_by_var

      validate(
        need(
          is.null(group_var) ||
            nrow(unique(anl[, group_var])) < 100,
          "Please select group-by variable with fewer than 100 unique values"
        )
      )

      group_vals <- input$group_by_vals # nolint (local variable is assigned and used)
      variables_select <- input$variables_select
      vars <- unique(variables_select, group_var)
      count_type <- input$count_type # nolint (local variable is assigned and used)

      if (!is.null(selected_vars()) && length(selected_vars()) != ncol(anl)) {
        variables <- selected_vars() # nolint (local variable is assigned and used)
      } else {
        variables <- colnames(anl)
      }

      summ_fn <- if (input$count_type == "counts") {
        function(x) sum(is.na(x))
      } else {
        function(x) round(sum(is.na(x)) / length(x), 4)
      }

      quosure <- common_code()

      if (!is.null(group_var)) {
        quosure <- teal.code::eval_code(
          quosure,
          substitute(
            expr = {
              summary_data <- ANL %>%
                dplyr::mutate(group_var_name := forcats::fct_explicit_na(as.factor(group_var_name), "NA")) %>%
                dplyr::group_by_at(group_var) %>%
                dplyr::filter(group_var_name %in% group_vals)

              count_data <- dplyr::summarise(summary_data, n = dplyr::n())

              summary_data <- dplyr::summarise_all(summary_data, summ_fn) %>%
                dplyr::mutate(group_var_name := paste0(group_var, ":", group_var_name, "(N=", count_data$n, ")")) %>%
                tidyr::pivot_longer(!tidyselect::all_of(group_var), names_to = "Variable", values_to = "out") %>%
                tidyr::pivot_wider(names_from = group_var, values_from = "out") %>%
                dplyr::mutate(`Variable label` = create_cols_labels(Variable, just_label = TRUE), .after = Variable)
            },
            env = list(
              group_var = group_var, group_var_name = as.name(group_var), group_vals = group_vals, summ_fn = summ_fn
            )
          ),
          name = "summary_data_call_1"
        )
      } else {
        quosure <- teal.code::eval_code(
          quosure,
          substitute(
            expr = summary_data <- ANL %>%
              dplyr::summarise_all(summ_fn) %>%
              tidyr::pivot_longer(tidyselect::everything(),
                names_to = "Variable",
                values_to = paste0("Missing (N=", nrow(ANL), ")")
              ) %>%
              dplyr::mutate(`Variable label` = create_cols_labels(Variable), .after = Variable),
            env = list(summ_fn = summ_fn)
          ),
          name = "summary_data_call_1"
        )
      }

      teal.code::eval_code(quosure, quote(summary_data), name = "summary_data_call_2")
    })

    by_subject_plot_r <- reactive({
      req(input$summary_type == "Grouped by Subject") # needed to trigger show r code update on tab change
      teal::validate_has_data(data_r(), 1)
      validate(need(length(input$variables_select) > 0, "No variables selected"))

      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        labs = list(x = "", y = ""),
        theme = list(legend.position = "bottom", axis.text.x = quote(element_blank()))
      )

      all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["By Subject"]],
        user_default = ggplot2_args$default,
        module_plot = dev_ggplot2_args
      )

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args,
        ggtheme = input$ggtheme
      )

      keys <- data_keys()

      teal.code::eval_code(
        common_code(),
        substitute(
          expr = parent_keys <- keys,
          env = list(keys = keys)
        ),
        name = "parent_keys_call"
      ) %>%
teal.code::eval_code(
        substitute(
          expr = analysis_vars <- setdiff(colnames(ANL), data_keys),
          env = list(data_keys = data_keys())
        ),
        name = "analysis_vars_call"
      ) %>% teal.code::eval_code(
        quote({
          summary_plot_patients <- ANL[, c(parent_keys, analysis_vars)] %>%
            dplyr::group_by_at(parent_keys) %>%
            dplyr::mutate(id = dplyr::cur_group_id()) %>%
            dplyr::ungroup() %>%
            dplyr::group_by_at(c(parent_keys, "id")) %>%
            dplyr::summarise_all(anyNA) %>%
            dplyr::ungroup()

          # order subjects by decreasing number of missing and then by
          # missingness pattern (defined using sha1)
          order_subjects <- summary_plot_patients %>%
            dplyr::select(-"id", -tidyselect::all_of(parent_keys)) %>%
            dplyr::transmute(
              id = dplyr::row_number(),
              number_NA = apply(., 1, sum),
              sha = apply(., 1, digest::sha1)
            ) %>%
            dplyr::arrange(dplyr::desc(number_NA), sha) %>%
            getElement(name = "id")

          # order columns by decreasing percent of missing values
          ordered_columns <- summary_plot_patients %>%
            dplyr::select(-"id", -tidyselect::all_of(parent_keys)) %>%
            dplyr::summarise(
              column = create_cols_labels(colnames(.)),
              na_count = apply(., MARGIN = 2, FUN = sum),
              na_percent = na_count / nrow(.) * 100
            ) %>%
            dplyr::arrange(na_percent, dplyr::desc(column))

          summary_plot_patients <- summary_plot_patients %>%
            tidyr::gather("col", "isna", -"id", -tidyselect::all_of(parent_keys)) %>%
            dplyr::mutate(col = create_cols_labels(col))
        }),
        name = "summary_plot_patients_call"
      ) %>% teal.code::eval_code(
        substitute(
          expr = {
            g <- ggplot(summary_plot_patients, aes(
              x = factor(id, levels = order_subjects),
              y = factor(col, levels = ordered_columns[["column"]]),
              fill = isna
            )) +
              geom_raster() +
              annotate(
                "text",
                x = length(order_subjects),
                y = seq_len(nrow(ordered_columns)),
                hjust = 1,
                label = sprintf("%d [%.02f%%]", ordered_columns[["na_count"]], ordered_columns[["na_percent"]])
              ) +
              scale_fill_manual(
                name = "",
                values = c("grey90", c(getOption("ggplot2.discrete.colour")[2], "#ff2951ff")[1]),
                labels = c("Present", "Missing (at least one)")
              ) +
              labs +
              ggthemes +
              themes
            print(g)
          },
          env = list(
            labs = parsed_ggplot2_args$labs,
            themes = parsed_ggplot2_args$theme,
            ggthemes = parsed_ggplot2_args$ggtheme
          )
        ),
        name = "plot_call"
      )
    })

    g_by_subject_plot_r <- reactive(by_subject_plot_r()[["g"]])

    output$levels_table <- DT::renderDataTable(
      expr = {
        if (length(input$variables_select) == 0) {
          # so that zeroRecords message gets printed
          # using tibble as it supports weird column names, such as " "
          tibble::tibble(` ` = logical(0))
        } else {
          table_r()[["summary_data"]]
        }
      },
      options = list(language = list(zeroRecords = "No variable selected"), pageLength = input$levels_table_rows)
    )

    pws1 <- teal.widgets::plot_with_settings_srv(
      id = "summary_plot",
      plot_r = g_summary_plot_r,
      height = plot_height,
      width = plot_width
    )

    pws2 <- teal.widgets::plot_with_settings_srv(
      id = "combination_plot",
      plot_r = g_combination_plot_r,
      height = plot_height,
      width = plot_width
    )

    pws3 <- teal.widgets::plot_with_settings_srv(
      id = "by_subject_plot",
      plot_r = g_by_subject_plot_r,
      height = plot_height,
      width = plot_width
    )

    final_reactive <- reactive({
      sum_type <- input$summary_type
      if (sum_type == "Summary") {
        summary_plot_r()
      } else if (sum_type == "Combinations") {
        combination_plot_r()
      } else if (sum_type == "By Variable Levels") {
        table_r()
      } else if (sum_type == "Grouped by Subject") {
        by_subject_plot_r()
      }
    })

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = reactive(teal.code::get_code(final_reactive())),
      title = "Show R Code for Missing Data"
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment) {
        card <- teal.reporter::TealReportCard$new()
        sum_type <- input$summary_type
        title <- if (sum_type == "By Variable Levels") paste0(sum_type, " Table") else paste0(sum_type, " Plot")
        card$set_name(paste0("Missing Data - ", sum_type))
        card$append_text(title, "header2")
        card$append_fs(filter_panel_api$get_filter_state())
        if (sum_type == "Summary") {
          card$append_text("Plot", "header3")
          card$append_plot(g_summary_plot_r(), dim = pws1$dim())
        } else if (sum_type == "Combinations") {
          card$append_text("Plot", "header3")
          card$append_plot(g_combination_plot_r(), dim = pws2$dim())
        } else if (sum_type == "By Variable Levels") {
          card$append_text("Table", "header3")
          card$append_table(table_r[["summary_data"]])
        } else if (sum_type == "Grouped by Subject") {
          card$append_text("Plot", "header3")
          card$append_plot(g_by_subject_plot_r(), dim = pws3$dim())
        }
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(paste(teal.code::get_code(final_reactive()), collapse = "\n"))
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}
