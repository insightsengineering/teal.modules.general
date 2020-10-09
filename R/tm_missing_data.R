#' Missing data module
#'
#' Present analysis of missing observations and patients.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#'
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADRS <- radrs(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     code = "ADSL <- radsl(cached = TRUE); ADRS <- radrs(cached = TRUE)",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_missing_data()
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_missing_data <- function(label = "Missing data", plot_height = c(600, 400, 5000), plot_width = NULL) {
  stopifnot(is_character_single(label))
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  module(
    label,
    server = srv_page_missing_data,
    server_args = list(plot_height = plot_height, plot_width = plot_width),
    ui = ui_page_missing_data,
    ui_args = list(plot_height = plot_height, plot_width = plot_width),
    filters = "all"
  )
}

ui_page_missing_data <- function(id, datasets, plot_height, plot_width) {
  ns <- NS(id)
  datanames <- datasets$datanames()

  standard_layout(
    output = white_small_well(
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
                    ui_missing_data(
                      id = ns(x), plot_height = plot_height, plot_width = plot_width
                    )
                  )
                )
              }
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
              encoding_missing_data(id = ns(x))
            )
          }
        )
      )
    )
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

ui_missing_data <- function(id, plot_height, plot_width) {
  ns <- NS(id)
  tabsetPanel(
    id = ns("summary_type"),
    tabPanel(
      "Summary",
      plot_with_settings_ui(id = ns("summary_plot"), height = plot_height, width = plot_width),
      helpText(
        p(paste(
          'The "Summary" graph shows the number of missing values per variable (both absolute and percentage),',
          "sorted by magnitude."
        ))
      )
    ),
    tabPanel(
      "Combinations",
      plot_with_settings_ui(id = ns("combination_plot"), height = plot_height, width = plot_width),
      helpText(
        p(paste(
          'The "Combinations" graph is used to explore the relationship between the missing data within',
          "different columns of the dataset.",
          "It shows the different patterns of missingness in the rows of the data.",
          'For example, suppose that 70 rows of the dataset had missing data in columns "A" and "B" (in each column).',
          'In this case, at N=70 on the y-axis and columns "A" and "B" on the x-axis, the cells would be shaded red.'
        )),
        p(paste(
          "Due to the large number of missing data patterns possible, only those with a large set of observations",
          'are shown in the graph and the "Combination cut-off" slider can be used to adjust the number shown.'
        ))
      )
    ),
    tabPanel(
      "By variable levels",
      DT::dataTableOutput(ns("levels_table"))
    )
  )
}

encoding_missing_data <- function(id) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("variables")),
    actionButton(ns("filter_na"), span(style = "white-space: normal;", "Select only vars with missings"), width = "100%"), # nolint
    conditionalPanel(
      sprintf("$(\"#%s > li.active\").text().trim() == \"Summary\"", ns("summary_type")),
      checkboxInput(ns("any_na"),
        div(
          "Add **anyna** variable",
          title = "Describes the number of observations with at least one missing value in any variable.",
          icon("info-circle")
        ),
        value = FALSE),
      checkboxInput(ns("if_patients_plot"),
                    div(
                      "Add summary per patients",
                      title = paste("Displays the number of missing values per observation,",
                        "where the x-axis is sorted by observation appearance in the table."),
                      icon("info-circle")
                    ),
                    value = FALSE)
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

#' @importFrom dplyr arrange arrange_at desc filter group_by_all group_by_at mutate mutate_all
#'   pull select summarise_all row_number ungroup tally tibble transmute
#' @import ggplot2
#' @importFrom grid grid.newpage grid.draw unit.pmax
#' @importFrom gridExtra gtable_cbind
#' @importFrom magrittr %>% extract2
#' @importFrom scales percent_format
#' @importFrom stats reorder
#' @importFrom rlang .data sym
#' @importFrom tidyr gather spread
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
    datasets$get_data_attr(dataname, "keys")$primary
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

    if (!is.null(selected_vars()) && length(selected_vars()) != ncol(anl_filtered)) {
      common_stack_push(bquote(ANL_FILTERED <- .(as.name(anl_name))[, .(selected_vars())])) # nolint
    } else {
      common_stack_push(bquote(ANL_FILTERED <- .(as.name(anl_name)))) # nolint
    }

    new_col_name <- "**anyna**" # nolint variable assigned and used

    common_stack_push(
      bquote({
        create_cols_labels <- function(cols, just_label = FALSE) {
          column_labels <- .(c(datasets$get_variable_labels(dataname)[selected_vars()], new_col_name = new_col_name))
          column_labels[is.na(column_labels) | length(column_labels) == 0] <- ""
          if (just_label) {
            labels <- column_labels[cols]
          } else{
            labels <- ifelse(cols == .(new_col_name) | cols == "", cols, paste0(column_labels[cols], " [", cols, "]"))
          }
          return(labels)
        }
      })
    )
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

    tibble(
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
      label_help = paste0("Dataset: ", dataname),
      choices = variable_choices(raw_data(), choices),
      selected = selected,
      multiple = TRUE
    )
  })

  observeEvent(input$filter_na, {
    choices <- vars_summary() %>%
      select(.data$key) %>%
      extract2(1)

    selected <- vars_summary() %>%
      filter(.data$value > 0) %>%
      select(.data$key) %>%
      extract2(1)

    updateOptionalSelectInput(
      session = session,
      inputId = "variables_select",
      choices = variable_choices(raw_data()),
      selected = selected
    )

  })

  output$group_by_var_ui <- renderUI({
    optionalSelectInput(
      session$ns("group_by_var"),
      label = "Group by variable",
      choices = variable_choices(raw_data()),
      selected = if_null(
        isolate(input$group_by_var),
        names(raw_data())[!sapply(raw_data(), is.numeric)][1]),
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
    validate(need(length(choices) < 100, "Please select variable with no more than 100 unique values"))

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
    validate_has_data(data(), 1)

    # Create a private stack for this function only.
    summary_stack <- chunks$new()
    summary_stack_push <- function(...) {
      chunks_push(..., chunks = summary_stack)
    }

    #Add common code into this chunk
    chunks_push_chunks(common_code_chunks(), chunks = summary_stack)

    if (input$any_na) {
      new_col_name <- "**anyna**" # nolint (local variable is assigned and used)
      summary_stack_push(
        bquote(
          ANL_FILTERED[[.(new_col_name)]] <- ifelse(rowSums(is.na(ANL_FILTERED)) > 0, NA, FALSE)
        )
      )
    }

    summary_stack_push(
      bquote({
        summary_plot_obs <- ANL_FILTERED[, setdiff(colnames(ANL_FILTERED), .(data_keys()))] %>%
          dplyr::summarise_all(list(function(x) sum(is.na(x)))) %>%
          tidyr::gather("col", "n_na") %>%
          dplyr::mutate(n_not_na = nrow(ANL_FILTERED) - n_na) %>%
          tidyr::gather("isna", "n", -col) %>%
          dplyr::mutate(isna = isna == "n_na", n_pct = n / nrow(ANL_FILTERED) * 100)
      })
    )

    # x axis ordering according to number of missing values and alphabet
    summary_stack_push(
      bquote(
        x_levels <- dplyr::filter(summary_plot_obs, isna) %>%
          dplyr::arrange(n_pct, dplyr::desc(col)) %>%
          dplyr::pull(col) %>%
          create_cols_labels()
      )
    )

    # always set "**anyna**" level as the last one
    if (isolate(input$any_na)) {
      summary_stack_push(
        quote(
          x_levels <- c(setdiff(x_levels, "**anyna**"), "**anyna**")
        )
      )
    }

    summary_stack_push(
      bquote(
        p1 <- summary_plot_obs %>%
          ggplot() +
          aes(x = factor(create_cols_labels(col), levels = x_levels),
            y = n_pct,
            fill = isna) +
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
      )
    )

    if (input$if_patients_plot) {

      keys <- data_keys()
      summary_stack_push(
        bquote(
          summary_plot_patients <- ANL_FILTERED %>%
            dplyr::mutate(id = dplyr::row_number()) %>%
            dplyr::group_by_at(.(c(keys, "id"))) %>%
            dplyr::summarise_all(anyNA) %>%
            tidyr::gather("col", "isna", -id, -.(keys)) %>%
            dplyr::mutate(count_na = sum(isna)) %>%
            dplyr::arrange_at(c("count_na", "col"), .funs = dplyr::desc)
        )
      )

      summary_stack_push(
        bquote(
          p2 <- summary_plot_patients %>%
            ggplot() +
            aes_(x = ~factor(create_cols_labels(col), levels = x_levels),
              y = ~reorder(id, order(-count_na)),
              fill = ~isna) +
            geom_bar(alpha = 1, stat = "identity") +
            scale_fill_manual(
              name = "",
              values = c("grey90", "#ff2951ff"),
              labels = c("Present", "Missing")
            ) +
            labs(
              x = "",
              y = "Missing patients (sorted)"
            ) +
            theme_classic() +
            theme(
              legend.position = "bottom",
              axis.text = element_blank(),
              axis.ticks.x = element_blank()
            ) +
            coord_flip()
        )
      )

      summary_stack_push(
        bquote({
          g1 <- ggplotGrob(p1)
          g2 <- ggplotGrob(p2)
          g <- gridExtra::gtable_cbind(g1, g2, size = "first")
          g$heights <- grid::unit.pmax(g1$heights, g2$heights)
          grid::grid.newpage()
        })
      )
    } else {
      summary_stack_push(
        bquote({
          g <- ggplotGrob(p1)
          grid::grid.newpage()
        })
      )
    }

    summary_stack_push(quote(grid::grid.draw(g)))
    chunks_safe_eval(summary_stack)
    summary_stack
  })

  summary_plot_r <- reactive({
    chunks_reset()
    chunks_push_chunks(summary_plot_chunks())
    summary_plot_chunks()
  })

  combination_cutoff_chunks <- reactive({

    combination_cutoff_stack <- chunks$new()

    #Add common code into this chunk
    chunks_push_chunks(common_code_chunks(), chunks = combination_cutoff_stack)

    chunks_push(
      bquote({
        combination_cutoff <- ANL_FILTERED %>%
          dplyr::mutate_all(is.na) %>%
          dplyr::group_by_all() %>%
          dplyr::tally() %>%
          dplyr::ungroup()
      }), chunks = combination_cutoff_stack
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
    value <- `if`(is.null(prev_value) || prev_value > max(x) || prev_value < min(x),
      sort(x, partial = idx)[idx], prev_value)

    optionalSliderInputValMinMax(session$ns("combination_cutoff"),
      "Combination cut-off",
      c(value, range(x))
    )
  })

  combination_plot_chunks <- reactive({
    validate_has_data(data(), 1)
    req(input$combination_cutoff)

    # Create a private stack for this function only.
    combination_stack <- chunks$new()
    combination_stack_push <- function(...) {
      chunks_push(..., chunks = combination_stack)
    }

    chunks_push_chunks(combination_cutoff_chunks(), chunks = combination_stack)

    combination_stack_push(
      bquote(
        data_combination_plot_cutoff <- combination_cutoff %>%
          dplyr::filter(n >= .(input$combination_cutoff)) %>%
          dplyr::mutate(id = rank(n, ties.method = "first")) %>%
          tidyr::gather("key", "value", -n, -id) %>%
          dplyr::arrange(n)
      )
    )

    combination_stack_push(
      bquote(
        labels <- data_combination_plot_cutoff %>%
          dplyr::filter(key == key[[1]]) %>%
          dplyr::transmute(paste0("N=", n)) %>%
          magrittr::extract2(1)
      )
    )

    combination_stack_push(
      bquote(
        p <- data_combination_plot_cutoff %>%
          ggplot() +
          aes(x = create_cols_labels(key), y = id, fill = value) +
          geom_tile(alpha = 0.85) +
          scale_y_continuous(breaks = seq_along(labels), labels = labels) +
          scale_fill_manual(
            name = "",
            values = c("grey90", "#ff2951ff"),
            labels = c("Present", "Missing")
          ) +
          labs(
            x = "Variable",
            y = "Combinations"
          ) +
          theme_classic() +
          theme(
            legend.position = "bottom",
            axis.text.x = element_text(angle = 90, hjust = 1),
            panel.grid.major = element_line(colour = "black", linetype = "dotted", size = 0.5)
          )
      )
    )

    combination_stack_push(bquote({
      g <- ggplotGrob(p)
      grid::grid.newpage()
      grid::grid.draw(g)
    }))

    chunks_safe_eval(combination_stack)
    combination_stack
  })

  combination_plot_r <- reactive({
    chunks_reset()
    chunks_push_chunks(combination_plot_chunks())
    combination_plot_chunks()
  })

  table_chunks <- reactive({
    validate_has_data(data(), 1)

    # Create a private stack for this function only.
    table_stack <- chunks$new()
    table_stack_push <- function(...) {
      chunks_push(..., chunks = table_stack)
    }

    #Add common code into this stack
    chunks_push_chunks(common_code_chunks(), chunks = table_stack)

    #extract the ANL_FILTERED dataset for use in further validation
    anl_filtered <- chunks_get_var("ANL_FILTERED", chunks = table_stack)

    validate(need(input$count_type, "Please select type of counts"))
    if (!is.null(input$group_by_var)) {
      validate(need(input$group_by_vals, "Please select grouping variable values"))
    }

    group_var <- input$group_by_var

    validate(
      need(is.null(group_var) ||
        group_var %in% selected_vars(),
        "Grouping variable has been already filtered out")
    )

    validate(
      need(is.null(group_var) ||
        nrow(unique(anl_filtered[, group_var])) < 100,
        "Please select variable with no more than 100 unique values")
    )

    group_vals <- input$group_by_vals # nolint (local variable is assigned and used)
    variables_select <- input$variables_select
    vars <- unique(variables_select, group_var)
    count_type <- input$count_type # nolint (local variable is assigned and used)

    table_stack_push(bquote(cell_values <- function(x) {
      nas_string <- if ("Missing" %in% .(count_type)) sum(is.na(x)) else ""
      sum_string <- if ("All" %in% .(count_type)) length(x) else ""
      fra_string <- if ("Fraction" %in% .(count_type)) sprintf("[%.2f%%]", mean(is.na(x)) * 100) else ""

      trimws(
        paste(nas_string,
              if (all(c("Missing", "All") %in% .(count_type))) "/",
              sum_string,
              fra_string)
      )
    }))

    if (!is.null(selected_vars()) && length(selected_vars()) != ncol(anl_filtered)) {
      variables <- selected_vars() # nolint (local variable is assigned and used)
    } else {
      variables <- colnames(anl_filtered)
    }

    if (!is.null(group_var)) {
      table_stack_push(
        bquote(
          summary_data <- ANL_FILTERED %>%
            dplyr::group_by_at(.(group_var)) %>%
            dplyr::filter(.(sym(group_var)) %in% .(group_vals)) %>%
            dplyr::summarise_all(cell_values) %>%
            tidyr::gather("Variable", "out", -.(sym(group_var))) %>%
            dplyr::mutate(`Variable label` = "Placeholder") %>%
            dplyr::select(.(sym(group_var)), Variable, `Variable label`, out) %>%
            tidyr::spread(.(sym(group_var)), out) %>%
            dplyr::mutate(`Variable label` = create_cols_labels(Variable, just_label  = TRUE))
        )
      )} else {
        table_stack_push(
          bquote(
            summary_data <- ANL_FILTERED %>%
              dplyr::summarise_all(cell_values) %>%
              tidyr::gather("Variable", "out") %>%
              dplyr::mutate(`Variable label` = create_cols_labels(Variable), just_label = TRUE) %>%
              dplyr::select(Variable, `Variable label`, `Overall missing` = out)
          )
        )
      }
    table_stack_push(quote({
      summary_data
    }))
    chunks_safe_eval(table_stack)
    table_stack
  })

  output$levels_table <- DT::renderDataTable({
    chunks_reset()
    chunks_push_chunks(table_chunks())
    chunks_get_var("summary_data")
  })

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
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = dataname,
    modal_title = "R code for missing data "
  )

}
