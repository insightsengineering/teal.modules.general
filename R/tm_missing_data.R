#' Missing data module
#'
#' Present analysis of missing observations and patients.
#'
#' @inheritParams teal::module
#' @param plot_height (\code{numeric}) Vector of length three with \code{c(value, min and max)}.
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
#'   shinyApp(app$ui, app$server)
#' }
tm_missing_data <- function(label = "Missing data", plot_height = c(600, 400, 5000)) {
  stopifnot(is_character_single(label))
  stopifnot(is_numeric_vector(plot_height, 3, 3))
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])

  module(
    label,
    server = srv_page_missing_data,
    ui = ui_page_missing_data,
    ui_args = list(plot_height = plot_height),
    filters = "all"
  )
}

ui_page_missing_data <- function(id, datasets, plot_height) {
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
                      id = ns(x)
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
              encoding_missing_data(id = ns(x), plot_height = plot_height)
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
                                  datasets) {

  lapply(
    datasets$datanames(),
    function(x) {
      callModule(
        module = srv_missing_data,
        id = x,
        datasets = datasets,
        dataname = x
      )
    }
  )
}

ui_missing_data <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    id = ns("summary_type"),
    tabPanel(
      "Summary",
      plot_height_output(id = ns("summary_plot"))
    ),
    tabPanel(
      "Combinations",
      plot_height_output(id = ns("combination_plot"))
    ),
    tabPanel(
      "By variable levels",
      DT::dataTableOutput(ns("levels_table"))
    )
  )
}

encoding_missing_data <- function(id, plot_height) {
  ns <- NS(id)

  tagList(
    uiOutput(ns("variables")),
    actionButton(ns("filter_na"), "Select only vars with missings", width = "100%"),
    checkboxInput(ns("any_na"), "Add any_NA variable", value = FALSE),
    conditionalPanel(
      sprintf("$(\"#%s > li.active\").text().trim() == \"Summary\"", ns("summary_type")),
      checkboxInput(ns("if_patients_plot"), "Add summary per patients", value = FALSE),
      plot_height_input(id = ns("plot_height_summary"), value = plot_height)
    ),
    conditionalPanel(
      sprintf("$(\"#%s > li.active\").text().trim() == \"Combinations\"", ns("summary_type")),
      uiOutput(ns("cutoff")),
      plot_height_input(id = ns("plot_height_combinations"), value = plot_height)
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
    hr()
  )
}

#' @importFrom digest sha1
#' @importFrom dplyr arrange arrange_at desc filter group_by group_by_all group_by_at if_else mutate mutate_all
#'   n select summarise_all sym row_number ungroup tally tibble transmute
#' @import ggplot2
#' @importFrom grid grid.newpage grid.draw unit.pmax
#' @importFrom gridExtra gtable_cbind
#' @importFrom magrittr %>% extract2
#' @importFrom scales percent_format
#' @importFrom stats reorder
#' @importFrom rlang .data !!
#' @importFrom tidyr gather spread
srv_missing_data <- function(input,
                             output,
                             session,
                             datasets,
                             dataname) {

  callModule(
    plot_with_height,
    id = "summary_plot",
    plot_height = reactive(input$plot_height_summary),
    plot_id = session$ns("summary_plot")
  )
  callModule(
    plot_with_height,
    id = "combination_plot",
    plot_height = reactive(input$plot_height_combinations),
    plot_id = session$ns("combination_plot")
  )

  data <- reactive({
    datasets$get_data(dataname, filtered = FALSE, reactive = FALSE)
  })

  data_keys <- reactive({
    datasets$get_data_attr(dataname, "keys")$primary
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
      choices = variable_choices(data(), choices),
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
      choices = variable_choices(data()),
      selected = selected
    )

  })

  output$group_by_var_ui <- renderUI({
    optionalSelectInput(
      session$ns("group_by_var"),
      label = "Group by variable",
      choices = variable_choices(data()),
      selected = names(data())[!sapply(data(), is.numeric)][1],
      multiple = FALSE,
      label_help = paste0("Dataset: ", dataname)
    )
  })

  output$group_by_vals_ui <- renderUI({
    req(input$group_by_var)

    choices <- selected <- value_choices(data(), input$group_by_var, input$group_by_var)

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

  data_selected <- reactive({
    req(input$variables_select)

    keys <- data_keys()
    vars <- unique(c(keys, input$variables_select))

    data() %>%
      select(!!vars)
  })

  data_summary_plot <- reactive({
    ANL_FILTERED <- data_selected() # nolint

    if (input$any_na) {
      new_col_name <- "**anyna**"
      ANL_FILTERED[[new_col_name]] <- ifelse(rowSums(is.na(ANL_FILTERED)) > 0, NA, FALSE)
    }

    ANL_FILTERED
  })

  data_summary_plot_obs <- reactive({
    res <- data_summary_plot() %>%
      select(-c(data_keys())) %>%
      summarise_all(list(function(x) sum(is.na(x)))) %>%
      gather("col", "n_na") %>%
      mutate(n_not_na = nrow(data_summary_plot()) - .data$n_na) %>%
      gather("isna", "n", -.data$col) %>%
      mutate(isna = .data$isna == "n_na", n_pct = n / nrow(data_summary_plot()) * 100)
    res
  })

  data_summary_plot_patients <- reactive({
    keys <- data_keys()
    res <- data_summary_plot() %>%
      mutate(id = row_number()) %>%
      group_by_at(c(keys, "id")) %>%
      summarise_all(anyNA) %>%
      gather("col", "isna", -.data$id, -keys) %>%
      mutate(count_na = sum(.data$isna)) %>%
      arrange_at(c("count_na", "col"), .funs = desc)
    res
  })

  output$summary_plot <- renderPlot({
    p1 <- data_summary_plot_obs() %>%
      ggplot() +
      aes_(x = ~reorder(col, desc(if_else(isna == TRUE, -n_pct, 0))), y = ~n_pct, fill = ~isna) +
      geom_bar(position = "fill", stat = "identity") +
      scale_fill_manual(
        name = "",
        values = c("grey", "black"),
        labels = c("Present", "Missing")
      ) +
      scale_y_continuous(labels = scales::percent_format(), breaks = seq(0, 1, by = 0.1), expand = c(0, 0)) +
      geom_text(aes_(label = ~ifelse(isna == TRUE, sprintf("%d [%.02f%%]", n, n_pct), ""), y = 1.1), hjust = 1) +
      labs(
        x = "Variable",
        y = "Missing observations"
      ) +
      theme_classic() +
      theme(legend.position = "bottom") +
      coord_flip()

    p2 <- if (input$if_patients_plot) {
      data_summary_plot_patients() %>%
        ggplot() +
        aes_(x = ~reorder(col, desc(-isna)),
             y = ~reorder(id, order(-count_na)),
             fill = ~isna) +
        geom_raster(alpha = 1) +
        scale_fill_manual(
          name = "",
          values = c("grey", "black"),
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
    } else {
      NULL
    }

    if (!is.null(p2)) {
      g1 <- ggplotGrob(p1)
      g2 <- ggplotGrob(p2)
      g <- gridExtra::gtable_cbind(g1, g2, size = "first")
      g$heights <- grid::unit.pmax(g1$heights, g2$heights)
      grid::grid.newpage()
      return(grid::grid.draw(g))
    } else {
      return(p1)
    }

  })

  data_combination_plot <- reactive({
    res <- data_selected() %>%
      mutate_all(is.na) %>%
      group_by_all() %>%
      tally() %>%
      ungroup()
    res
  })

  output$cutoff <- renderUI({
    x <- data_combination_plot()$n

    # select 10-th from the top
    n <- length(x)
    idx <- max(1, n - 10)
    value <- sort(x, partial = idx)[idx]

    optionalSliderInputValMinMax(session$ns("combination_cutoff"), "Combination cut-off", c(value, range(x)))
  })

  data_combination_plot_cutoff <- reactive({
    req(input$combination_cutoff)
    res <- data_combination_plot() %>%
      filter(.data$n >= input$combination_cutoff) %>%
      mutate(id = order(.data$n)) %>%
      gather("key", "value", -.data$n, -.data$id) %>%
      arrange(.data$n)
    res
  })

  output$combination_plot <- renderPlot({
    labels <- data_combination_plot_cutoff() %>%
      filter(.data$key == .data$key[[1]]) %>%
      transmute(paste0("N=", .data$n)) %>%
      extract2(1)

    p <- data_combination_plot_cutoff() %>%
      ggplot() +
      aes_string(x = "key", y = "id", fill = "value") +
      geom_tile(alpha = 1) +
      scale_y_continuous(breaks = seq_along(labels), labels = labels) +
      scale_fill_manual(
        name = "",
        values = c("grey", "black"),
        labels = c(
          "Present",
          "Missing"
        )
      ) +
      labs(
        x = "Variable",
        y = "Combinations"
      ) +
      theme_classic() +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 90, hjust = 1)
      )
    p
  })

  data_plot3 <- reactive({
    validate(need(input$group_by_var, "Please select groupping variable"))
    validate(need(input$group_by_vals, "Please select groupping variable values"))
    validate(need(input$count_type, "Please select type of counts"))

    group_var <- input$group_by_var
    validate(need(group_var %in% names(data_selected()), "Groupping variable has been already filtered out"))

    group_vals <- input$group_by_vals

    vars <- unique(c(input$variables_select, group_var))

    cell_values <- function(x) {
      nas_string <- if ("Missing" %in% input$count_type) sum(is.na(x)) else ""
      sum_string <- if ("All" %in% input$count_type) length(x) else ""
      fra_string <- if ("Fraction" %in% input$count_type) sprintf("[%.2f%%]", mean(is.na(x)) * 100) else ""

      trimws(
        paste(nas_string,
              if (all(c("Missing", "All") %in% input$count_type)) "/",
              sum_string,
              fra_string)
      )
    }

    data_selected() %>%
      group_by_at(group_var) %>%
      filter(!!sym(group_var) %in% group_vals) %>%
      summarise_all(cell_values) %>%
      gather("Variable", "out", -!!sym(group_var)) %>%
      mutate(`Variable label` = vapply(if_empty(datasets$get_data_labels(dataname, .data$Variable), ""),
                                       if_empty,
                                       character(1),
                                       "")) %>%
      select(!!sym(group_var), .data$Variable, .data$`Variable label`, .data$out) %>%
      spread(!!sym(group_var), .data$out)
  })

  output$levels_table <- DT::renderDataTable({
    data_plot3()
  })
}
