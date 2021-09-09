#' Missing data module
#'
#' Present analysis of missing observations and patients.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @inheritParams teal.devel::standard_layout
#'
#'
#' @export
#'
#' @examples
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
#'   modules = root_modules(
#'     tm_missing_data()
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_missing_data <- function(label = "Missing data",
                            plot_height = c(600, 400, 5000),
                            plot_width = NULL,
                            pre_output = NULL,
                            post_output = NULL) {
  stopifnot(is_character_single(label))
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  module(
    label,
    server = srv_page_missing_data,
    server_args = list(plot_height = plot_height, plot_width = plot_width),
    ui = ui_page_missing_data,
    filters = "all",
    ui_args = list(pre_output = pre_output, post_output = post_output)
  )
}

ui_page_missing_data <- function(id, datasets, pre_output = NULL, post_output = NULL) {
  ns <- NS(id)
  datanames <- datasets$datanames()

  if_subject_plot <- is(datasets, "CDISCFilteredData")

  standard_layout(
    output = white_small_well(
      div(
        style = "display: flex;",
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
                      div(style = "height:10px;"),
                      ui_missing_data(id = ns(x), by_subject_plot = if_subject_plot)
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
      tags$label("Encodings", class = "text-primary"),
      helpText(
        paste0("Dataset", `if`(length(datanames) > 1, "s", ""), ":"),
        tags$code(paste(datanames, collapse = ", "))
      ),
      tagList(
        lapply(
          datanames,
          function(x) {
            conditionalPanel(
              sprintf("$(\"#%s > li.active\").text().trim() == \"%s\"", ns("dataname_tab"), x),
              encoding_missing_data(id = ns(x), summary_per_patient = if_subject_plot)
            )
          }
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

srv_page_missing_data <- function(input,
                                  output,
                                  session,
                                  datasets,
                                  plot_height,
                                  plot_width) {
  lapply(
    datasets$datanames(),
    function(x) {
      callModule(
        module = srv_missing_data,
        id = x,
        datasets = datasets,
        dataname = x,
        plot_height = plot_height,
        plot_width = plot_width
      )
    }
  )
}

ui_missing_data <- function(id, by_subject_plot = FALSE) {
  ns <- NS(id)

  tab_list <- list(
    tabPanel(
      "Summary",
      plot_with_settings_ui(id = ns("summary_plot")),
      helpText(
        p(paste(
          'The "Summary" graph shows the number of missing values per variable (both absolute and percentage),',
          "sorted by magnitude."
        )),
        p('The "summary per patients" graph is showing how many subjects have at least one missing observation',
          "for each variable. It will be most useful for panel datasets.")
      )
    ),
    tabPanel(
      "Combinations",
      plot_with_settings_ui(id = ns("combination_plot")),
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
      "By variable levels",
      get_dt_rows(ns("levels_table"), ns("levels_table_rows")),
      DT::dataTableOutput(ns("levels_table"))
    )
  )
  if (isTRUE(by_subject_plot)) {
    tab_list <- append(
      tab_list,
      list(tabPanel(
        "Grouped by Subject",
        plot_with_settings_ui(id = ns("by_subject_plot")),
        helpText(
          p(paste(
            "This graph shows the missingness with respect to subjects rather than individual rows of the",
            "dataset. Each row represents one dataset variable and each column a single subject. Only subjects",
            "with at least one record in this dataset are shown. For a given subject, if they have any missing",
            "values of a specific variable then the appropriate cell in the graph is marked as missing."
          ))
        )
      )
    ))
  }

  do.call(
    tabsetPanel,
    c(
      id = ns("summary_type"),
      tab_list
    )
  )
}

#' @importFrom shinyWidgets checkboxGroupButtons
encoding_missing_data <- function(id, summary_per_patient = FALSE) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("variables")),
    actionButton(
      ns("filter_na"),
      span(style = "white-space: normal;", "Select only vars with missings"),
      width = "100%"),
    conditionalPanel(
      sprintf("$(\"#%s > li.active\").text().trim() == \"Summary\"", ns("summary_type")),
      checkboxInput(
        ns("any_na"),
        div(
          class = "teal-tooltip",
          tagList(
            "Add **anyna** variable",
            icon("info-circle"),
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
              icon("info-circle"),
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
      sprintf("$(\"#%s > li.active\").text().trim() == \"By variable levels\"", ns("summary_type")),
      tagList(
        uiOutput(ns("group_by_var_ui")),
        uiOutput(ns("group_by_vals_ui")),
        shinyWidgets::checkboxGroupButtons(
          inputId = ns("count_type"),
          label = "Type of counts",
          choices = c("Missing", "All", "Fraction"),
          selected = c("Missing", "All", "Fraction"),
          justified = TRUE
        )
      )
    ),
    hr(),
    get_rcode_ui(ns("rcode"))
  )
}

#' @importFrom rlang .data
srv_missing_data <- function(input,
                             output,
                             session,
                             datasets,
                             dataname,
                             plot_height,
                             plot_width) {
  init_chunks()

  prev_group_by_var <- reactiveVal("")

  raw_data <- reactive({
    # unfiltered data maintains all attributes such as labels
    datasets$get_data(dataname, filtered = FALSE)
  })

  data <- reactive({
    datasets$get_data(dataname, filtered = TRUE)
  })

  data_keys <- reactive({
    datasets$get_keys(dataname)
  })

  # chunks needed by all three outputs stored here
  common_code_chunks <- reactive({
    # Create a private stack for this function only.
    common_stack <- chunks$new()
    common_stack_push <- function(...) {
      chunks_push(..., chunks = common_stack)
    }

    anl_name <- paste0(dataname, "_FILTERED")
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE)
    assign(anl_name, anl_filtered)
    chunks_reset(chunks = common_stack)
    group_var <- input$group_by_var

    if (!is.null(selected_vars()) && length(selected_vars()) != ncol(anl_filtered)) {
      common_stack_push(substitute(
        expr = ANL_FILTERED <- anl_name[, selected_vars], # nolint
        env = list(anl_name = as.name(anl_name), selected_vars = selected_vars())
      ))
    } else {
      common_stack_push(substitute(expr = ANL_FILTERED <- anl_name, env = list(anl_name = as.name(anl_name)))) # nolint
    }

    if (input$summary_type == "By variable levels" && !is.null(group_var) && !(group_var %in% selected_vars())) {
      common_stack_push(substitute(
        expr = ANL_FILTERED[[group_var]] <- anl_name[[group_var]],
        env = list(group_var = group_var, anl_name = as.name(anl_name))
      ))
    }

    new_col_name <- "**anyna**" # nolint variable assigned and used

    common_stack_push(substitute(
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
        column_labels_value = c(datasets$get_varlabels(dataname)[selected_vars()], new_col_name = new_col_name)
      )
    ))
    chunks_safe_eval(chunks = common_stack)
    common_stack
  })

  selected_vars <- reactive({
    req(input$variables_select)
    keys <- data_keys()
    vars <- unique(c(keys, input$variables_select))
    validate(need(length(setdiff(vars, keys)) >= 1, "Please also select non-key columns."))
    vars
  })

  vars_summary <- reactive({
    na_count <- data() %>%
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

    optionalSelectInput(
      session$ns("variables_select"),
      label = "Select variables",
      label_help = HTML(paste0("Dataset: ", tags$code(dataname))),
      choices = variable_choices(raw_data(), choices),
      selected = selected,
      multiple = TRUE
    )
  })

  observeEvent(input$filter_na, {
    choices <- vars_summary() %>%
      dplyr::select(.data$key) %>%
      extract2(1)

    selected <- vars_summary() %>%
      dplyr::filter(.data$value > 0) %>%
      dplyr::select(.data$key) %>%
      extract2(1)

    updateOptionalSelectInput(
      session = session,
      inputId = "variables_select",
      choices = variable_choices(raw_data()),
      selected = selected
    )
  })

  output$group_by_var_ui <- renderUI({
    all_choices <- variable_choices(raw_data())
    cat_choices <- all_choices[!sapply(raw_data(), function(x) is.numeric(x) || inherits(x, "POSIXct"))]
    validate(
      need(cat_choices, "Dataset does not have any non-numeric or non-datetime variables to use to group data with"))
    optionalSelectInput(
      session$ns("group_by_var"),
      label = "Group by variable",
      choices = cat_choices,
      selected = if_null(
        isolate(input$group_by_var),
        cat_choices[1]
      ),
      multiple = FALSE,
      label_help = paste0("Dataset: ", dataname)
    )
  })

  output$group_by_vals_ui <- renderUI({
    req(input$group_by_var)

    choices <- value_choices(raw_data(), input$group_by_var, input$group_by_var)
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

    optionalSelectInput(
      session$ns("group_by_vals"),
      label = "Filter levels",
      choices = choices,
      selected = selected,
      multiple = TRUE,
      label_help = paste0("Dataset: ", dataname)
    )
  })

  summary_plot_chunks <- reactive({
    req(input$summary_type == "Summary") # needed to trigger show r code update on tab change
    validate_has_data(data(), 1)
    validate(need(!is_empty(input$variables_select), "No variables selected"))

    # Create a private stack for this function only.
    summary_stack <- chunks$new()
    summary_stack_push <- function(...) {
      chunks_push(..., chunks = summary_stack)
    }

    # Add common code into this chunk
    chunks_push_chunks(common_code_chunks(), chunks = summary_stack)

    if (input$any_na) {
      new_col_name <- "**anyna**" # nolint (local variable is assigned and used)
      summary_stack_push(substitute(
        expr = ANL_FILTERED[[new_col_name]] <- ifelse(rowSums(is.na(ANL_FILTERED)) > 0, NA, FALSE),
        env = list(new_col_name = new_col_name)
      ))
    }

    summary_stack_push(substitute(
      expr = analysis_vars <- setdiff(colnames(ANL_FILTERED), data_keys),
      env = list(data_keys = data_keys())
    ))

    summary_stack_push(substitute(
      expr = summary_plot_obs <- data_frame_call[, analysis_vars] %>%
        dplyr::summarise_all(list(function(x) sum(is.na(x)))) %>%
        tidyr::pivot_longer(tidyselect::everything(), names_to = "col", values_to = "n_na") %>%
        dplyr::mutate(n_not_na = nrow(ANL_FILTERED) - n_na) %>%
        tidyr::pivot_longer(-col, names_to = "isna", values_to = "n") %>%
        dplyr::mutate(isna = isna == "n_na", n_pct = n / nrow(ANL_FILTERED) * 100),
      env = list(data_frame_call = if (!inherits(datasets$get_data(dataname, filtered = TRUE), "tbl_df")) {
        quote(tibble::as_tibble(ANL_FILTERED))
        } else quote(ANL_FILTERED)
      )
    ))

    # x axis ordering according to number of missing values and alphabet
    summary_stack_push(quote(
      expr = x_levels <- dplyr::filter(summary_plot_obs, isna) %>%
        dplyr::arrange(n_pct, dplyr::desc(col)) %>%
        dplyr::pull(col) %>%
        create_cols_labels()
    ))

    # always set "**anyna**" level as the last one
    if (isolate(input$any_na)) {
      summary_stack_push(quote(x_levels <- c(setdiff(x_levels, "**anyna**"), "**anyna**")))
    }

    summary_stack_push(quote(
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
          values = c("grey90", "#ff2951ff"),
          labels = c("Present", "Missing")
        ) +
        scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
        geom_text(
          aes(label = ifelse(isna == TRUE, sprintf("%d [%.02f%%]", n, n_pct), ""), y = 1),
          hjust = 1,
          color = "black"
        ) +
        labs(
          x = "Variable",
          y = "Missing observations"
        ) +
        theme_classic() +
        theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
        coord_flip()
    ))

    if (isTRUE(input$if_patients_plot)) {
      keys <- data_keys()
      summary_stack_push(substitute(
        expr = parent_keys <- keys,
        env = list(keys = datasets$get_keys(if_empty(datasets$get_parentname(dataname), dataname)))
      ))
      summary_stack_push(quote(ndistinct_subjects <- dplyr::n_distinct(ANL_FILTERED[, parent_keys])))
      summary_stack_push(
        quote(
          summary_plot_patients <- ANL_FILTERED[, c(parent_keys, analysis_vars)] %>%
            dplyr::group_by_at(parent_keys) %>%
            dplyr::summarise_all(anyNA) %>%
            tidyr::pivot_longer(cols = !tidyselect::all_of(parent_keys), names_to = "col", values_to = "anyna") %>%
            dplyr::group_by_at(c("col")) %>%
            dplyr::summarise(count_na = sum(anyna)) %>%
            dplyr::mutate(count_not_na = ndistinct_subjects - count_na) %>%
            tidyr::pivot_longer(-c(col), names_to = "isna", values_to = "n") %>%
            dplyr::mutate(isna = isna == "count_na", n_pct = n / ndistinct_subjects * 100) %>%
            dplyr::arrange_at(c("isna", "n"), .funs = dplyr::desc)
        )
      )

      summary_stack_push(quote(
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
            values = c("grey90", "#ff2951ff"),
            labels = c("Present", "Missing")
          ) +
          labs(
            x = "",
            y = "Missing patients"
          ) +
          geom_text(
            aes(label = ifelse(isna == TRUE, sprintf("%d [%.02f%%]", n, n_pct), ""), y = 1),
            hjust = 1,
            color = "black"
          ) +
          theme_classic() +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1),
            axis.text.y = element_blank()
          ) +
          coord_flip()
      ))

      summary_stack_push(quote({
        g1 <- ggplotGrob(p1)
        g2 <- ggplotGrob(p2)
        g <- gridExtra::gtable_cbind(g1, g2, size = "first")
        g$heights <- grid::unit.pmax(g1$heights, g2$heights)
        grid::grid.newpage()
      }))
    } else {
      summary_stack_push(quote({
        g <- ggplotGrob(p1)
        grid::grid.newpage()
      }))
    }

    summary_stack_push(quote(grid::grid.draw(g)))
    chunks_safe_eval(summary_stack)
    summary_stack
  })

  summary_plot_r <- reactive({
    chunks_reset()
    chunks_push_chunks(summary_plot_chunks())
    chunks_get_var(var = "g")
  })

  combination_cutoff_chunks <- reactive({
    combination_cutoff_stack <- chunks$new()

    # Add common code into this chunk
    chunks_push_chunks(common_code_chunks(), chunks = combination_cutoff_stack)

    chunks_push(
      quote({
        combination_cutoff <- ANL_FILTERED %>%
          dplyr::mutate_all(is.na) %>%
          dplyr::group_by_all() %>%
          dplyr::tally() %>%
          dplyr::ungroup()
      }),
      chunks = combination_cutoff_stack
    )

    chunks_safe_eval(combination_cutoff_stack)
    combination_cutoff_stack
  })

  output$cutoff <- renderUI({
    x <- chunks_get_var("combination_cutoff", combination_cutoff_chunks())$n

    # select 10-th from the top
    n <- length(x)
    idx <- max(1, n - 10)
    prev_value <- isolate(input$combination_cutoff)
    value <- `if`(
      is.null(prev_value) || prev_value > max(x) || prev_value < min(x),
      sort(x, partial = idx)[idx], prev_value
    )

    optionalSliderInputValMinMax(
      session$ns("combination_cutoff"),
      "Combination cut-off",
      c(value, range(x))
    )
  })

  combination_plot_chunks <- reactive({
    req(input$summary_type == "Combinations") # needed to trigger show r code update on tab change
    validate_has_data(data(), 1)
    validate(need(!is_empty(input$variables_select), "No variables selected"))
    req(input$combination_cutoff)

    # Create a private stack for this function only.
    combination_stack <- chunks$new()
    combination_stack_push <- function(...) {
      chunks_push(..., chunks = combination_stack)
    }

    chunks_push_chunks(combination_cutoff_chunks(), chunks = combination_stack)

    combination_stack_push(substitute(
      expr = data_combination_plot_cutoff <- combination_cutoff %>%
        dplyr::filter(n >= combination_cutoff_value) %>%
        dplyr::mutate(id = rank(n, ties.method = "first")) %>%
        tidyr::pivot_longer(-c(n, id), names_to = "key", values_to = "value") %>%
        dplyr::arrange(n),
      env = list(combination_cutoff_value = input$combination_cutoff)
    ))

    # find keys in dataset not selected in the UI and remove them from dataset
    keys_not_selected <- setdiff(data_keys(), input$variables_select)
    if (length(keys_not_selected) > 0) {
      combination_stack_push(substitute(
        expr = data_combination_plot_cutoff <- data_combination_plot_cutoff %>%
          dplyr::filter(!key %in% keys_not_selected),
        env = list(keys_not_selected = keys_not_selected)
      ))
    }

    combination_stack_push(quote(
      labels <- data_combination_plot_cutoff %>%
        dplyr::filter(key == key[[1]]) %>%
        extract2(1)
    ))

    combination_stack_push(quote({
      p1 <- data_combination_plot_cutoff %>%
        dplyr::select(id, n) %>%
        dplyr::distinct() %>%
        ggplot(aes(x = id, y = n)) +
        geom_bar(stat = "identity", fill = "#ff2951ff") +
        ylab("") + xlab("") + theme_void() +
        theme(axis.text.x = element_blank()) +
        geom_text(aes(label = n), position = position_dodge(width = 0.9), vjust = -0.25) +
        ylim(c(0, max(data_combination_plot_cutoff$n) * 1.5))

      graph_number_rows <- length(unique(data_combination_plot_cutoff$id))
      graph_number_cols <- nrow(data_combination_plot_cutoff) / graph_number_rows

      p2 <- data_combination_plot_cutoff %>% ggplot() +
        aes(x = create_cols_labels(key), y = id - 0.5, fill = value) +
        geom_tile(alpha = 0.85, height = 0.95) +
        scale_fill_manual(
          name = "",
          values = c("grey90", "#ff2951ff"),
          labels = c("Present", "Missing")
        ) +
        labs(x = "", y = "") +
        theme_classic() +
        theme(
          legend.position = "bottom",
          axis.text.x = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major = element_blank()
        ) +
        geom_hline(yintercept = seq_len(1 + graph_number_rows) - 1) +
        geom_vline(xintercept = seq_len(1 + graph_number_cols) - 0.5, linetype = "dotted") +
        coord_flip()

      g1 <- ggplotGrob(p1)
      g2 <- ggplotGrob(p2)

      g <- gridExtra::gtable_rbind(g1, g2, size = "last")
      g$heights[7] <- grid::unit(0.2, "null") #rescale to get the bar chart smaller
      grid::grid.newpage()
      grid::grid.draw(g)
    }))

    chunks_safe_eval(combination_stack)
    combination_stack
  })

  combination_plot_r <- reactive({
    chunks_reset()
    chunks_push_chunks(combination_plot_chunks())
    chunks_get_var(var = "g")
  })

  table_chunks <- reactive({
    req(input$summary_type == "By variable levels") # needed to trigger show r code update on tab change
    validate_has_data(data(), 1)

    # Create a private stack for this function only.
    table_stack <- chunks$new()
    table_stack_push <- function(...) {
      chunks_push(..., chunks = table_stack)
    }

    # Add common code into this stack
    chunks_push_chunks(common_code_chunks(), chunks = table_stack)

    # extract the ANL_FILTERED dataset for use in further validation
    anl_filtered <- chunks_get_var("ANL_FILTERED", chunks = table_stack)

    validate(need(input$count_type, "Please select type of counts"))
    if (!is.null(input$group_by_var)) {
      validate(need(!is.null(input$group_by_vals), "Please select both group-by variable and values"))
    }

    group_var <- input$group_by_var

    validate(
      need(is.null(group_var) ||
        nrow(unique(anl_filtered[, group_var])) < 100,
        "Please select group-by variable with fewer than 100 unique values")
    )

    group_vals <- input$group_by_vals # nolint (local variable is assigned and used)
    variables_select <- input$variables_select
    vars <- unique(variables_select, group_var)
    count_type <- input$count_type # nolint (local variable is assigned and used)

    table_stack_push(substitute(
      expr = cell_values <- function(x) {
        nas_string <- if ("Missing" %in% count_type) sum(is.na(x)) else ""
        sum_string <- if ("All" %in% count_type) length(x) else ""
        fra_string <- if ("Fraction" %in% count_type) sprintf("[%.2f%%]", mean(is.na(x)) * 100) else ""

        trimws(
          paste(
            nas_string,
            if (all(c("Missing", "All") %in% count_type)) "/",
            sum_string,
            fra_string
          )
        )
      },
      env = list(count_type = count_type)
    ))

    if (!is.null(selected_vars()) && length(selected_vars()) != ncol(anl_filtered)) {
      variables <- selected_vars() # nolint (local variable is assigned and used)
    } else {
      variables <- colnames(anl_filtered)
    }

    if (!is.null(group_var)) {
      table_stack_push(substitute(
        expr = summary_data <- ANL_FILTERED %>%
          dplyr::group_by_at(group_var) %>%
          dplyr::filter(group_var_name %in% group_vals) %>%
          dplyr::summarise_all(cell_values) %>%
          tidyr::pivot_longer(!tidyselect::all_of(group_var), names_to = "Variable", values_to = "out") %>%
          dplyr::mutate(
            `Variable label` = "Placeholder",
            altered_group_var = dplyr::if_else(group_var_name == "", "''", group_var)
          ) %>%
          dplyr::select(altered_group_var, Variable, `Variable label`, out) %>%
          tidyr::pivot_wider(names_from = altered_group_var, values_from = out) %>%
          dplyr::mutate(`Variable label` = create_cols_labels(Variable, just_label = TRUE)),
        env = list(group_var = group_var, group_var_name = as.name(group_var), group_vals = group_vals)
      ))
    } else {
      table_stack_push(quote(
        summary_data <- ANL_FILTERED %>%
          dplyr::summarise_all(cell_values) %>%
          tidyr::pivot_longer(tidyselect::everything(), names_to = "Variable", values_to = "out") %>%
          dplyr::mutate(`Variable label` = create_cols_labels(Variable), just_label = TRUE) %>%
          dplyr::select(Variable, `Variable label`, `Overall missing` = out)
      ))
    }
    table_stack_push(quote({
      summary_data
    }))
    chunks_safe_eval(table_stack)
    table_stack
  })

  by_subject_plot_chunks <- reactive({

    req(input$summary_type == "Grouped by Subject") # needed to trigger show r code update on tab change
    validate_has_data(data(), 1)
    validate(need(!is_empty(input$variables_select), "No variables selected"))
    # Create a private stack for this function only.
    by_subject_stack <- chunks$new()
    by_subject_stack_push <- function(...) {
      chunks_push(..., chunks = by_subject_stack)
    }

    # Add common code into this chunk
    chunks_push_chunks(common_code_chunks(), chunks = by_subject_stack)

    keys <- data_keys()
    by_subject_stack_push(substitute(
      expr = parent_keys <- keys,
      env = list(keys = `if`(
        is_empty(datasets$get_parentname(dataname)),
        keys,
        datasets$get_keys(datasets$get_parentname(dataname))
      ))
    ))

    by_subject_stack_push(
      substitute(
        expr = analysis_vars <- setdiff(colnames(ANL_FILTERED), data_keys),
        env = list(data_keys = data_keys())
      )
    )

    by_subject_stack_push(
      quote({
        summary_plot_patients <- ANL_FILTERED[, c(parent_keys, analysis_vars)] %>%
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
          dplyr::transmute(id = dplyr::row_number(), number_NA = apply(., 1, sum), sha = apply(., 1, digest::sha1)) %>%
          dplyr::arrange(dplyr::desc(number_NA), sha) %>%
          extract2("id")

        summary_plot_patients <- summary_plot_patients %>%
          tidyr::gather("col", "isna", -"id", -tidyselect::all_of(parent_keys))

      })
    )

    by_subject_stack_push(
      quote({
        g <- ggplot(summary_plot_patients, aes(
          x = factor(id, levels = order_subjects),
          y = create_cols_labels(col), fill = isna)
        ) +
          geom_raster() +
          theme_classic() +
          scale_fill_manual(
            name = "",
            values = c("grey90", "#ff2951ff"),
            labels = c("Present", "Missing (at least one)")
          ) +
          ylab("") +
          xlab("") +
          theme(
            legend.position = "bottom",
            axis.text.x = element_blank()
          )
        print(g)
      })
    )

    chunks_safe_eval(by_subject_stack)
    by_subject_stack
  })

  by_subject_plot_r <- reactive({
    chunks_reset()
    chunks_push_chunks(by_subject_plot_chunks())
    chunks_get_var(var = "g")
  })

  output$levels_table <- DT::renderDataTable({
    if (is_empty(input$variables_select)) {
      # so that zeroRecords message gets printed
      # using tibble as it supports weird column names, such as " "
      tibble::tibble(` ` = logical(0))
    } else {
      chunks_reset()
      chunks_push_chunks(table_chunks())
      chunks_get_var("summary_data")
    }
  }, options =  list(language = list(zeroRecords = "No variable selected"), pageLength = input$levels_table_rows)
  )

  callModule(
    plot_with_settings_srv,
    id = "summary_plot",
    plot_r = summary_plot_r,
    height = plot_height,
    width = plot_width
  )
  callModule(
    plot_with_settings_srv,
    id = "combination_plot",
    plot_r = combination_plot_r,
    height = plot_height,
    width = plot_width
  )

  callModule(
    plot_with_settings_srv,
    id = "by_subject_plot",
    plot_r = by_subject_plot_r,
    height = plot_height,
    width = plot_width
  )

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = dataname,
    modal_title = "R code for missing data "
  )
}
