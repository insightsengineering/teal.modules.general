#' `teal` module: Variable browser
#'
#' Module provides provides a detailed summary and visualization of variable distributions
#' for `data.frame` objects, with interactive features to customize analysis.
#'
#' Numeric columns with fewer than 30 distinct values can be treated as either discrete
#' or continuous with a checkbox allowing users to switch how they are treated(if < 6 unique values
#' then the default is discrete, otherwise it is continuous).
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param parent_dataname (`character(1)`) string specifying a parent dataset.
#' If it exists in `datanames` then an extra checkbox will be shown to
#' allow users to not show variables in other datasets which exist in this `dataname`.
#' This is typically used to remove `ADSL` columns in `CDISC` data.
#' In non `CDISC` data this can be ignored. Defaults to `"ADSL"`.
#' @param datasets_selected (`character`) `r lifecycle::badge("deprecated")` vector of datasets to show, please
#' use the `datanames` argument.
#'
#' @inherit shared_params return
#'
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
# nolint start: line_length_linter.
#' @examples
# nolint end: line_length_linter.
#' # general data example
#' data <- teal_data()
#' data <- within(data, {
#'   iris <- iris
#'   mtcars <- mtcars
#'   women <- women
#'   faithful <- faithful
#'   CO2 <- CO2
#' })
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_variable_browser(
#'       label = "Variable browser"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
# nolint start: line_length_linter.
#' @examples
# nolint end: line_length_linter.
#' # CDISC example data
#' library(sparkline)
#' data <- teal_data()
#' data <- within(data, {
#'   ADSL <- teal.data::rADSL
#'   ADTTE <- teal.data::rADTTE
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_variable_browser(
#'       label = "Variable browser"
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
#'
tm_variable_browser <- function(label = "Variable Browser",
                                datasets_selected = deprecated(),
                                datanames = if (missing(datasets_selected)) "all" else datasets_selected,
                                parent_dataname = "ADSL",
                                pre_output = NULL,
                                post_output = NULL,
                                ggplot2_args = teal.widgets::ggplot2_args(),
                                transformators = list()) {
  message("Initializing tm_variable_browser")

  # Start of assertions
  checkmate::assert_string(label)
  if (!missing(datasets_selected)) {
    lifecycle::deprecate_stop(
      when = "0.4.0",
      what = "tm_variable_browser(datasets_selected)",
      with = "tm_variable_browser(datanames)",
      details = c(
        "If both `datasets_selected` and `datanames` are set `datasets_selected` will be silently ignored.",
        i = 'Use `tm_variable_browser(datanames = "all")` to keep the previous behavior and avoid this warning.'
      )
    )
  }
  checkmate::assert_character(datanames, min.len = 0, min.chars = 1, null.ok = TRUE)
  checkmate::assert_character(parent_dataname, min.len = 0, max.len = 1)
  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_class(ggplot2_args, "ggplot2_args")
  # End of assertions

  datanames_module <- if (identical(datanames, "all") || is.null(datanames)) {
    datanames
  } else {
    union(datanames, parent_dataname)
  }

  ans <- module(
    label,
    server = srv_variable_browser,
    ui = ui_variable_browser,
    datanames = datanames_module,
    server_args = list(
      datanames = if (is.null(datanames)) "all" else datanames,
      parent_dataname = parent_dataname,
      ggplot2_args = ggplot2_args
    ),
    ui_args = list(
      pre_output = pre_output,
      post_output = post_output
    ),
    transformators = transformators
  )
  # `shiny` inputs are stored properly but the majority of the module is state of `datatable` which is not stored.
  attr(ans, "teal_bookmarkable") <- NULL
  ans
}

# UI function for the variable browser module
ui_variable_browser <- function(id,
                                pre_output = NULL,
                                post_output = NULL) {
  ns <- NS(id)

  tags$div(
    shinyjs::useShinyjs(),
    teal.widgets::standard_layout(
      output = tags$div(
        htmlwidgets::getDependency("sparkline"), # needed for sparklines to work
        bslib::layout_column_wrap(
          width = 0.5,
          teal.widgets::white_small_well(
            uiOutput(ns("ui_variable_browser")),
            shinyjs::hidden({
              checkboxInput(ns("show_parent_vars"), "Show parent dataset variables", value = FALSE)
            })
          ),
          teal.widgets::white_small_well(
            uiOutput(ns("ui_histogram_display")),
            uiOutput(ns("ui_numeric_display")),
            teal.widgets::plot_with_settings_ui(ns("variable_plot")),
            tags$br(),
            bslib::accordion(
              open = TRUE,
              bslib::accordion_panel(
                title = "Plot settings",
                collapsed = TRUE,
                selectInput(
                  inputId = ns("ggplot_theme"), label = "ggplot2 theme",
                  choices = ggplot_themes,
                  selected = "grey"
                ),
                bslib::layout_columns(
                  col_widths = c(6, 6),
                  sliderInput(
                    inputId = ns("font_size"), label = "font size",
                    min = 5L, max = 30L, value = 15L, step = 1L, ticks = FALSE
                  ),
                  sliderInput(
                    inputId = ns("label_rotation"), label = "rotate x labels",
                    min = 0L, max = 90L, value = 45L, step = 1, ticks = FALSE
                  )
                )
              )
            ),
            tags$br(),
            teal.widgets::get_dt_rows(ns("variable_summary_table"), ns("variable_summary_table_rows")),
            DT::dataTableOutput(ns("variable_summary_table"))
          )
        )
      ),
      pre_output = pre_output,
      post_output = post_output
    )
  )
}

# Server function for the variable browser module
srv_variable_browser <- function(id,
                                 data,
                                 datanames, parent_dataname, ggplot2_args) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    # if there are < this number of unique records then a numeric
    # variable can be treated as a factor and all factors with < this groups
    # have their values plotted
    .unique_records_for_factor <- 30
    # if there are < this number of unique records then a numeric
    # variable is by default treated as a factor
    .unique_records_default_as_factor <- 6 # nolint: object_length.

    varname_numeric_as_factor <- reactiveValues()

    datanames <- Filter(function(name) {
      is.data.frame(isolate(data())[[name]])
    }, if (identical(datanames, "all")) names(isolate(data())) else datanames)

    output$ui_variable_browser <- renderUI({
      ns <- session$ns
      do.call(
        tabsetPanel,
        c(
          id = ns("tabset_panel"),
          do.call(
            tagList,
            lapply(datanames, function(dataname) {
              tabPanel(
                dataname,
                tags$div(
                  style = "margin-top: 1rem;",
                  textOutput(ns(paste0("dataset_summary_", dataname)))
                ),
                tags$div(
                  style = "margin-top: 1rem;",
                  teal.widgets::get_dt_rows(
                    ns(paste0("variable_browser_", dataname)),
                    ns(paste0("variable_browser_", dataname, "_rows"))
                  ),
                  DT::dataTableOutput(ns(paste0("variable_browser_", dataname)), width = "100%")
                )
              )
            })
          )
        )
      )
    })

    # conditionally display checkbox
    shinyjs::toggle(
      id = "show_parent_vars",
      condition = length(parent_dataname) > 0 && parent_dataname %in% datanames
    )

    columns_names <- new.env()

    # plot_var$data holds the name of the currently selected dataset
    # plot_var$variable[[<dataset_name>]] holds the name of the currently selected
    # variable for dataset <dataset_name>
    plot_var <- reactiveValues(data = NULL, variable = list())

    establish_updating_selection(datanames, input, plot_var, columns_names)

    # validations
    validation_checks <- validate_input(req(input), req(plot_var), data)

    # data_for_analysis is a list with two elements: a column from a dataset and the column label
    plotted_data <- reactive({
      req(input, plot_var, data())
      validation_checks()
      get_plotted_data(input, plot_var, data)
    })

    treat_numeric_as_factor <- reactive({
      if (is_num_var_short(.unique_records_for_factor, input, plotted_data)) {
        input$numeric_as_factor
      } else {
        FALSE
      }
    })

    render_tabset_panel_content(
      input = input,
      output = output,
      data = data,
      datanames = datanames,
      parent_dataname = parent_dataname,
      columns_names = columns_names,
      plot_var = plot_var
    )
    # add used-defined text size to ggplot arguments passed from caller frame
    all_ggplot2_args <- reactive({
      user_text <- teal.widgets::ggplot2_args(
        theme = list(
          "text" = ggplot2::element_text(size = input[["font_size"]]),
          "axis.text.x" = ggplot2::element_text(angle = input[["label_rotation"]], hjust = 1)
        )
      )
      user_theme <- utils::getFromNamespace(sprintf("theme_%s", input[["ggplot_theme"]]), ns = "ggplot2")
      user_theme <- user_theme()
      # temporary fix to circumvent assertion issue with resolve_ggplot2_args
      # drop problematic elements
      user_theme <- user_theme[grep("strip.text.y.left", names(user_theme), fixed = TRUE, invert = TRUE)]

      teal.widgets::resolve_ggplot2_args(
        user_plot = user_text,
        user_default = teal.widgets::ggplot2_args(theme = user_theme),
        module_plot = ggplot2_args
      )
    })

    output$ui_numeric_display <- renderUI({
      dataname <- req(input$tabset_panel)
      varname <- req(plot_var$variable)[[dataname]]
      df <- req(data())[[dataname]]
      validation_checks()

      numeric_ui <- bslib::page_fluid(
        bslib::layout_columns(
          col_widths = c(8, 4),
          bslib::layout_columns(
            col_widths = c(6, 6, 12),
            style = bslib::css(grid_row_gap = 0),
            bslib::input_switch(
              id = session$ns("display_density"),
              label = tags$div(
                "Show density:",
                bslib::tooltip(
                  trigger = icon("circle-info"),
                  tags$span(
                    "Show kernel density estimation with gaussian kernel and bandwidth function bw.nrd0 (R default)"
                  )
                )
              ),
              value = `if`(is.null(isolate(input$display_density)), TRUE, isolate(input$display_density)),
              width = "100%"
            ),
            bslib::input_switch(
              id = session$ns("remove_outliers"),
              label = "Remove outliers",
              value = `if`(is.null(isolate(input$remove_outliers)), FALSE, isolate(input$remove_outliers)),
              width = "100%"
            ),
            uiOutput(session$ns("ui_outlier_help"))
          ),
          uiOutput(session$ns("outlier_definition_slider_ui"))
        )
      )

      observeEvent(input$numeric_as_factor, ignoreInit = TRUE, {
        varname_numeric_as_factor[[plot_var$variable[[dataname]]]] <- input$numeric_as_factor
      })

      if (is.numeric(df[[varname]])) {
        unique_entries <- length(unique(df[[varname]]))
        if (unique_entries < .unique_records_for_factor && unique_entries > 0) {
          list(
            checkboxInput(
              session$ns("numeric_as_factor"),
              "Treat variable as factor",
              value = `if`(
                is.null(varname_numeric_as_factor[[varname]]),
                unique_entries < .unique_records_default_as_factor,
                varname_numeric_as_factor[[varname]]
              )
            ),
            conditionalPanel("!input.numeric_as_factor", ns = session$ns, numeric_ui)
          )
        } else if (unique_entries > 0) {
          numeric_ui
        }
      } else {
        NULL
      }
    })

    output$ui_histogram_display <- renderUI({
      validation_checks()
      dataname <- req(input$tabset_panel)
      varname <- req(plot_var$variable)[[dataname]]
      df <- req(data())[[dataname]]

      numeric_ui <- bslib::input_switch(
        id = session$ns("remove_NA_hist"),
        label = "Remove NA values",
        value = FALSE,
        width = "100%"
      )

      var <- df[[varname]]
      if (anyNA(var) && (is.factor(var) || is.character(var) || is.logical(var))) {
        groups <- unique(as.character(var))
        len_groups <- length(groups)
        if (len_groups >= .unique_records_for_factor) {
          NULL
        } else {
          numeric_ui
        }
      } else {
        NULL
      }
    })

    output$outlier_definition_slider_ui <- renderUI({
      req(input$remove_outliers)
      sliderInput(
        inputId = session$ns("outlier_definition_slider"),
        tags$div(
          tagList(
            "Outlier definition:",
            bslib::tooltip(
              icon("circle-info"),
              tags$span(
                paste(
                  "Use the slider to choose the cut-off value to define outliers; the larger the value the",
                  "further below Q1/above Q3 points have to be in order to be classed as outliers"
                )
              )
            )
          )
        ),
        min = 1,
        max = 5,
        value = 3,
        step = 0.5
      )
    })

    output$ui_outlier_help <- renderUI({
      req(is.logical(input$remove_outliers), input$outlier_definition_slider)
      if (input$remove_outliers) {
        tags$small(
          helpText(
            withMathJax(paste0(
              "Outlier data points (\\( X \\lt Q1 - ", input$outlier_definition_slider, "\\times IQR \\) or
            \\(Q3 + ", input$outlier_definition_slider, "\\times IQR \\lt X\\))
            have not been displayed on the graph and will not be used for any kernel density estimations, ",
              "although their values remain in the statisics table below."
            ))
          )
        )
      } else {
        NULL
      }
    })

    output$variable_summary_table <- DT::renderDataTable({
      var_summary_table(
        plotted_data()$ANL[, 1, drop = TRUE],
        treat_numeric_as_factor(),
        input$variable_summary_table_rows,
        if (!is.null(input$remove_outliers) && input$remove_outliers) {
          req(input$outlier_definition_slider)
          as.numeric(input$outlier_definition_slider)
        } else {
          0
        }
      )
    })

    variable_plot_r <- reactive({
      display_density <- `if`(is.null(input$display_density), FALSE, input$display_density)
      remove_outliers <- `if`(is.null(input$remove_outliers), FALSE, input$remove_outliers)

      if (remove_outliers) {
        req(input$outlier_definition_slider)
        outlier_definition <- as.numeric(input$outlier_definition_slider)
      } else {
        outlier_definition <- 0
      }

      plot_var_summary(
        qenv = req(plotted_data()),
        wrap_character = 15,
        numeric_as_factor = treat_numeric_as_factor(),
        remove_NA_hist = input$remove_NA_hist,
        display_density = display_density,
        outlier_definition = outlier_definition,
        records_for_factor = .unique_records_for_factor,
        ggplot2_args = all_ggplot2_args()
      )
    })

    plot_r <- reactive({
      validation_checks()
      req(variable_plot_r())[["plot"]]
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id = "variable_plot",
      plot_r = plot_r,
      height = c(500, 200, 2000)
    )

    set_chunk_dims(pws, variable_plot_r)
  })
}

#' Summarize NAs.
#'
#' Summarizes occurrence of missing values in vector.
#' @param x vector of any type and length
#' @return Character string describing `NA` occurrence.
#' @keywords internal
var_missings_info <- function(x) {
  sprintf("%s [%s%%]", sum(is.na(x)), round(mean(is.na(x) * 100), 2))
}

#' Summarizes variable
#'
#' Creates html summary with statistics relevant to data type. For numeric values it returns central
#' tendency measures, for factor returns level counts, for Date  date range, for other just
#' number of levels.
#'
#' @param x vector of any type
#' @param numeric_as_factor `logical` should the numeric variable be treated as a factor
#' @param dt_rows `numeric` current/latest `DT` page length
#' @param outlier_definition If 0 no outliers are removed, otherwise
#'   outliers (those more than `outlier_definition*IQR below/above Q1/Q3` be removed)
#' @return text with simple statistics.
#' @keywords internal
var_summary_table <- function(x, numeric_as_factor, dt_rows, outlier_definition) {
  if (is.null(dt_rows)) {
    dt_rows <- 10
  }
  if (is.numeric(x) && !numeric_as_factor) {
    req(!any(is.infinite(x)))

    x <- remove_outliers_from(x, outlier_definition)

    qvals <- round(stats::quantile(x, na.rm = TRUE, probs = c(0.25, 0.5, 0.75), type = 2), 2)
    # classical central tendency measures

    summary <-
      data.frame(
        Statistic = c("min", "Q1", "median", "mean", "Q3", "max", "sd", "n"),
        Value = c(
          round(min(x, na.rm = TRUE), 2),
          qvals[1],
          qvals[2],
          round(mean(x, na.rm = TRUE), 2),
          qvals[3],
          round(max(x, na.rm = TRUE), 2),
          round(stats::sd(x, na.rm = TRUE), 2),
          length(x[!is.na(x)])
        )
      )

    DT::datatable(summary, rownames = FALSE, options = list(dom = "<t>", pageLength = dt_rows))
  } else if (is.factor(x) || is.character(x) || (is.numeric(x) && numeric_as_factor) || is.logical(x)) {
    # make sure factor is ordered numeric
    if (is.numeric(x)) {
      x <- factor(x, levels = sort(unique(x)))
    }

    level_counts <- table(x)
    max_levels_signif <- nchar(level_counts)

    if (!all(is.na(x))) {
      levels <- names(level_counts)
      counts <- sprintf(
        "%s [%.2f%%]",
        format(level_counts, width = max_levels_signif), prop.table(level_counts) * 100
      )
    } else {
      levels <- character(0)
      counts <- numeric(0)
    }

    summary <- data.frame(
      Level = levels,
      Count = counts,
      stringsAsFactors = FALSE
    )

    # sort the dataset in decreasing order of counts (needed as character variables default to alphabetical)
    summary <- summary[order(summary$Count, decreasing = TRUE), ]

    dom_opts <- if (nrow(summary) <= 10) {
      "<t>"
    } else {
      "<lf<t>ip>"
    }
    DT::datatable(summary, rownames = FALSE, options = list(dom = dom_opts, pageLength = dt_rows))
  } else if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    summary <-
      data.frame(
        Statistic = c("min", "median", "max"),
        Value = c(
          min(x, na.rm = TRUE),
          stats::median(x, na.rm = TRUE),
          max(x, na.rm = TRUE)
        )
      )
    DT::datatable(summary, rownames = FALSE, options = list(dom = "<t>", pageLength = dt_rows))
  } else {
    NULL
  }
}

#' Plot variable
#'
#' Creates summary plot with statistics relevant to data type.
#'
#' @inheritParams shared_params
#' @param qenv teal_data object where code should be evaluated.
#' @param wrap_character (`numeric`) number of characters at which to wrap text values of `var`
#' @param numeric_as_factor (`logical`) should the numeric variable be treated as a factor
#' @param display_density (`logical`) should density estimation be displayed for numeric values
#' @param remove_NA_hist (`logical`) should `NA` values be removed for histogram of factor like variables
#' @param outlier_definition if 0 no outliers are removed, otherwise
#'   outliers (those more than outlier_definition*IQR below/above Q1/Q3 be removed)
#' @param records_for_factor (`numeric`) if the number of factor levels is >= than this value then
#'   a graph of the factors isn't shown, only a list of values
#'
#' @return plot
#' @keywords internal
plot_var_summary <- function(qenv,
                             wrap_character = NULL,
                             numeric_as_factor,
                             display_density = FALSE,
                             remove_NA_hist = FALSE, # nolint: object_name.
                             outlier_definition,
                             records_for_factor,
                             ggplot2_args) {
  checkmate::assert_numeric(wrap_character, null.ok = TRUE)
  checkmate::assert_flag(numeric_as_factor)
  checkmate::assert_flag(display_density)
  checkmate::assert_logical(remove_NA_hist, null.ok = TRUE)
  checkmate::assert_number(outlier_definition, lower = 0, finite = TRUE)
  checkmate::assert_integerish(records_for_factor, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_class(ggplot2_args, "ggplot2_args")

  var_name <- names(qenv$ANL)

  teal.reporter::teal_card(qenv) <- c(
    teal.reporter::teal_card(qenv),
    teal.reporter::teal_card("### Histogram plot")
  )

  var <- qenv$ANL[[var_name]]
  qenv_plot <- if (is.factor(var) || is.character(var) || is.logical(var)) {
    groups <- unique(as.character(var))
    len_groups <- length(groups)
    if (len_groups >= records_for_factor) {
      qenv_plot <- within(qenv,
        {
          groups <- unique(as.character(ANL[[var]]))
          len_groups <- length(groups)
          text <- sprintf(
            "%s unique values\n%s:\n %s\n ...\n %s",
            len_groups,
            teal.data::col_labels(ANL),
            paste(utils::head(groups), collapse = ",\n "),
            paste(utils::tail(groups), collapse = ",\n ")
          )
          plot <- gridExtra::arrangeGrob(
            grid::textGrob(
              text,
              x = grid::unit(1, "line"),
              y = grid::unit(1, "npc") - grid::unit(1, "line"),
              just = c("left", "top")
            ),
            ncol = 1
          )
        },
        var = var_name
      )
    } else {
      if (!is.null(wrap_character)) {
        qenv <- within(qenv,
          {
            col_label <- attr(ANL[[var]], "label")
            ANL[[var]] <- stringr::str_wrap(ANL[[var]], width = wrap_character)
            attr(ANL[[var]], "label") <- col_label
          },
          var = var_name,
          wrap_character = wrap_character
        )
      }

      if (isTRUE(remove_NA_hist)) {
        qenv <- within(qenv,
          {
            ANL <- filter(ANL, !is.na(var))
          },
          var = as.name(var_name)
        )
      }
      qenv_plot <- within(qenv,
        {
          plot <- ANL %>%
            ggplot2::ggplot(ggplot2::aes(x = forcats::fct_infreq(var_name))) +
            ggplot2::geom_bar(
              stat = "count", ggplot2::aes(fill = ifelse(is.na(var_name), "withcolor", "")), show.legend = FALSE
            ) +
            ggplot2::scale_fill_manual(values = c("gray50", "tan"))
        },
        var = var_name,
        var_name = as.name(var_name)
      )
    }
  } else if (is.numeric(var)) {
    # Validate input
    validate(need(any(!is.na(var)), "No data left to visualize."))
    var <- var[which(!is.na(var))] # Filter out NA
    validate(need(!any(is.infinite(var)), "Cannot display graph when data includes infinite values"))

    if (numeric_as_factor) {
      var <- factor(var)
      qenv_plot <- within(qenv,
        {
          col_label <- attr(ANL[[var]], "label")
          ANL[[var]] <- as.factor(ANL[[var]])
          attr(ANL[[var]], "label") <- col_label
          p <- ANL %>%
            ggplot2::ggplot(ggplot2::aes(x = var_name)) +
            ggplot2::geom_histogram(stat = "count")
        },
        var = var_name,
        var_name = as.name(var_name)
      )
    } else {
      # remove outliers
      if (outlier_definition != 0) {
        number_records <- length(var)
        var <- remove_outliers_from(var, outlier_definition)
        number_outliers <- number_records - length(var)
        outlier_text <- paste0(
          number_outliers, " outliers (",
          round(number_outliers / number_records * 100, 2),
          "% of non-missing records) not shown"
        )
        validate(need(
          length(var) > 1,
          "At least two data points must remain after removing outliers for this graph to be displayed"
        ))
        qenv <- within(qenv,
          {
            filter_outliers <- filter_outliers
            ANL <- filter(ANL, filter_outliers(var_name, outlier_definition))
          },
          filter_outliers = filter_outliers,
          var_name = as.name(var_name),
          outlier_definition = outlier_definition
        )
      }

      ## histogram
      binwidth <- get_bin_width(var)
      qenv_plot <- within(qenv,
        {
          plot <- ggplot2::ggplot(data = ANL, ggplot2::aes(x = var_name, y = ggplot2::after_stat(count))) +
            ggplot2::geom_histogram(binwidth = binwidth) +
            ggplot2::scale_y_continuous(
              sec.axis = ggplot2::sec_axis(
                trans = ~ . / nrow(ANL),
                labels = scales::percent,
                name = "proportion (in %)"
              )
            )
        },
        var_name = as.name(var_name),
        binwidth = binwidth
      )

      if (display_density) {
        qenv_plot <- within(qenv_plot,
          {
            plot <- plot + ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(count * binwidth)))
          },
          binwidth = binwidth
        )
      }
      if (outlier_definition != 0) {
        qenv_plot <- within(qenv_plot,
          {
            plot <- plot + ggplot2::annotate(
              geom = "text",
              label = outlier_text,
              x = Inf, y = Inf,
              hjust = 1.02, vjust = 1.2,
              color = "black",
              # explicitly modify geom text size according
              size = size
            )
          },
          outlier_text = outlier_text,
          size = ggplot2_args[["theme"]][["text"]][["size"]] / 3.5
        )
      }
      qenv_plot
    }
    qenv_plot
  } else if (inherits(var, "Date") || inherits(var, "POSIXct") || inherits(var, "POSIXlt")) {
    var_num <- as.numeric(var)
    binwidth <- get_bin_width(var_num, 1)
    qenv_plot <- within(qenv,
      {
        col_label <- attr(ANL[[var]], "label")
        ANL[[var]] <- as.numeric(ANL[[var]])
        attr(ANL[[var]], "label") <- col_label
        plot <- ANL %>%
          ggplot2::ggplot(ggplot2::aes(x = var_name, y = ggplot2::after_stat(count))) +
          ggplot2::geom_histogram(binwidth = binwidth)
      },
      binwidth = binwidth,
      var = var_name,
      var_name = as.name(var_name)
    )
  } else {
    qenv_plot <- within(qenv,
      {
        plot <- gridExtra::arrangeGrob(
          grid::textGrob(
            paste(strwrap(
              utils::capture.output(utils::str(ANL[[var]])),
              width = .9 * grid::convertWidth(grid::unit(1, "npc"), "char", TRUE)
            ), collapse = "\n"),
            x = grid::unit(1, "line"), y = grid::unit(1, "npc") - grid::unit(1, "line"), just = c("left", "top")
          ),
          ncol = 1
        )
      },
      var = var_name
    )
  }

  dev_ggplot2_args <- teal.widgets::ggplot2_args(
    labs = list(x = teal.data::col_labels(qenv$ANL))
  )

  all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
    ggplot2_args,
    module_plot = dev_ggplot2_args
  )

  if (is.ggplot(qenv_plot$plot)) {
    qenv_plot <- within(qenv_plot,
      {
        plot <- plot +
          theme_light() +
          labs
      },
      labs = do.call("labs", all_ggplot2_args$labs)
    )
  }
  qenv_plot <- within(qenv_plot, {
    plot
  })
}

is_num_var_short <- function(.unique_records_for_factor, input, data_for_analysis) {
  length(unique(data_for_analysis()$data)) < .unique_records_for_factor && !is.null(input$numeric_as_factor)
}

#' Validates the variable browser inputs
#'
#' @param input (`session$input`) the `shiny` session input
#' @param plot_var (`list`) list of a data frame and an array of variable names
#' @param data (`teal_data`) the datasets passed to the module
#'
#' @returns `logical` TRUE if validations pass; a `shiny` validation error otherwise
#' @keywords internal
validate_input <- function(input, plot_var, data) {
  reactive({
    dataset_name <- req(input$tabset_panel)
    varname <- plot_var$variable[[dataset_name]]

    validate(need(dataset_name, "No data selected"))
    validate(need(varname, "No variable selected"))
    df <- data()[[dataset_name]]
    teal::validate_has_data(df, 1)
    teal::validate_has_variable(varname = varname, data = df, "Variable not available")

    TRUE
  })
}

get_plotted_data <- function(input, plot_var, data) {
  dataset_name <- req(input$tabset_panel)
  varname <- plot_var$variable[[dataset_name]]
  obj <- data()
  teal.reporter::teal_card(obj) <-
    c(
      teal.reporter::teal_card(obj),
      teal.reporter::teal_card("## Module's output(s)")
    )
  teal.code::eval_code(obj, "library(ggplot2)") |>
    within(
      {
        ANL <- select(dataset_name, varname)
      },
      dataset_name = as.name(dataset_name),
      varname = as.name(varname)
    )
}

#' Renders the left-hand side `tabset` panel of the module
#'
#' @param datanames (`character`) the name of the dataset
#' @param parent_dataname (`character`) the name of a parent `dataname` to filter out variables from
#' @param data (`teal_data`) the object containing all datasets
#' @param input (`session$input`) the `shiny` session input
#' @param output (`session$output`) the `shiny` session output
#' @param columns_names (`environment`) the environment containing bindings for each dataset
#' @param plot_var (`list`) the list containing the currently selected dataset (tab) and its column names
#' @keywords internal
render_tabset_panel_content <- function(datanames, parent_dataname, output, data, input, columns_names, plot_var) {
  lapply(datanames, render_single_tab,
    input = input,
    output = output,
    data = data,
    parent_dataname = parent_dataname,
    columns_names = columns_names,
    plot_var = plot_var
  )
}

#' Renders a single tab in the left-hand side tabset panel
#'
#' Renders a single tab in the left-hand side tabset panel. The rendered tab contains
#' information about one dataset out of many presented in the module.
#'
#' @param dataset_name (`character`) the name of the dataset contained in the rendered tab
#' @param parent_dataname (`character`) the name of a parent `dataname` to filter out variables from
#' @inheritParams render_tabset_panel_content
#' @keywords internal
render_single_tab <- function(dataset_name, parent_dataname, output, data, input, columns_names, plot_var) {
  render_tab_header(dataset_name, output, data)

  render_tab_table(
    dataset_name = dataset_name,
    parent_dataname = parent_dataname,
    output = output,
    data = data,
    input = input,
    columns_names = columns_names,
    plot_var = plot_var
  )
}

#' Renders the text headlining a single tab in the left-hand side tabset panel
#'
#' @param dataset_name (`character`) the name of the dataset of the tab
#' @inheritParams render_tabset_panel_content
#' @keywords internal
render_tab_header <- function(dataset_name, output, data) {
  dataset_ui_id <- paste0("dataset_summary_", dataset_name)
  output[[dataset_ui_id]] <- renderText({
    df <- data()[[dataset_name]]
    join_keys <- teal.data::join_keys(data())
    if (!is.null(join_keys)) {
      key <- teal.data::join_keys(data())[dataset_name, dataset_name]
    } else {
      key <- NULL
    }
    sprintf(
      "Dataset with %s unique key rows and %s variables",
      nrow(unique(`if`(length(key) > 0, df[, key, drop = FALSE], df))),
      ncol(df)
    )
  })
}

#' Renders the table for a single dataset in the left-hand side tabset panel
#'
#' The table contains column names, column labels,
#' small summary about NA values and `sparkline` (if appropriate).
#'
#' @param dataset_name (`character`) the name of the dataset
#' @param parent_dataname (`character`) the name of a parent `dataname` to filter out variables from
#' @inheritParams render_tabset_panel_content
#' @keywords internal
render_tab_table <- function(dataset_name, parent_dataname, output, data, input, columns_names, plot_var) {
  table_ui_id <- paste0("variable_browser_", dataset_name)

  output[[table_ui_id]] <- DT::renderDataTable({
    df <- data()[[dataset_name]]

    get_vars_df <- function(input, dataset_name, parent_name, data) {
      data_cols <- colnames(df)
      if (isTRUE(input$show_parent_vars)) {
        data_cols
      } else if (dataset_name != parent_name && parent_name %in% names(data)) {
        setdiff(data_cols, colnames(data()[[parent_name]]))
      } else {
        data_cols
      }
    }

    if (length(parent_dataname) > 0) {
      df_vars <- get_vars_df(input, dataset_name, parent_dataname, data)
      df <- df[df_vars]
    }

    if (is.null(df) || ncol(df) == 0) {
      columns_names[[dataset_name]] <- character(0)
      df_output <- data.frame(
        Type = character(0),
        Variable = character(0),
        Label = character(0),
        Missings = character(0),
        Sparklines = character(0),
        stringsAsFactors = FALSE
      )
    } else {
      # extract data variable labels
      labels <- teal.data::col_labels(df)

      columns_names[[dataset_name]] <- names(labels)

      # calculate number of missing values
      missings <- vapply(
        df,
        var_missings_info,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE
      )

      # get icons proper for the data types
      icons <- vapply(df, function(x) class(x)[1L], character(1L))

      join_keys <- teal.data::join_keys(data())
      if (!is.null(join_keys)) {
        icons[intersect(join_keys[dataset_name, dataset_name], colnames(df))] <- "primary_key"
      }
      icons <- variable_type_icons(icons)

      # generate sparklines
      sparklines_html <- vapply(
        df,
        create_sparklines,
        FUN.VALUE = character(1),
        USE.NAMES = FALSE
      )

      df_output <- data.frame(
        Type = icons,
        Variable = names(labels),
        Label = labels,
        Missings = missings,
        Sparklines = sparklines_html,
        stringsAsFactors = FALSE
      )
    }

    # Select row 1 as default / fallback
    selected_ix <- 1
    # Define starting page index (base-0 index of the first item on page
    #  note: in many cases it's not the item itself
    selected_page_ix <- 0

    # Retrieve current selected variable if any
    isolated_variable <- isolate(plot_var$variable[[dataset_name]])

    if (!is.null(isolated_variable)) {
      index <- which(columns_names[[dataset_name]] == isolated_variable)[1]
      if (!is.null(index) && !is.na(index) && length(index) > 0) selected_ix <- index
    }

    # Retrieve the index of the first item of the current page
    #  it works with varying number of entries on the page (10, 25, ...)
    table_id_sel <- paste0("variable_browser_", dataset_name, "_state")
    dt_state <- isolate(input[[table_id_sel]])
    if (selected_ix != 1 && !is.null(dt_state)) {
      selected_page_ix <- floor(selected_ix / dt_state$length) * dt_state$length
    }

    DT::datatable(
      df_output,
      escape = FALSE,
      rownames = FALSE,
      selection = list(mode = "single", target = "row", selected = selected_ix),
      options = list(
        fnDrawCallback = htmlwidgets::JS("function() { HTMLWidgets.staticRender(); }"),
        pageLength = input[[paste0(table_ui_id, "_rows")]],
        displayStart = selected_page_ix
      )
    )
  })
}

#' Creates observers updating the currently selected column
#'
#' The created observers update the column currently selected in the left-hand side
#' tabset panel.
#'
#' @note
#' Creates an observer for each dataset (each tab in the tabset panel).
#'
#' @inheritParams render_tabset_panel_content
#' @keywords internal
establish_updating_selection <- function(datanames, input, plot_var, columns_names) {
  lapply(datanames, function(dataset_name) {
    table_ui_id <- paste0("variable_browser_", dataset_name)
    table_id_sel <- paste0(table_ui_id, "_rows_selected")
    observeEvent(input[[table_id_sel]], {
      plot_var$data <- dataset_name
      plot_var$variable[[dataset_name]] <- columns_names[[dataset_name]][input[[table_id_sel]]]
    })
  })
}

get_bin_width <- function(x_vec, scaling_factor = 2) {
  x_vec <- x_vec[!is.na(x_vec)]
  qntls <- stats::quantile(x_vec, probs = c(0.1, 0.25, 0.75, 0.9), type = 2)
  iqr <- qntls[3] - qntls[2]
  binwidth <- max(scaling_factor * iqr / length(x_vec) ^ (1 / 3), sqrt(qntls[4] - qntls[1])) # styler: off
  binwidth <- ifelse(binwidth == 0, 1, binwidth)
  # to ensure at least two bins when variable span is very small
  x_span <- diff(range(x_vec))
  if (isTRUE(x_span / binwidth >= 2)) binwidth else x_span / 2
}

#' Removes the outlier observation from an array
#'
#' @param var (`numeric`) a numeric vector
#' @param outlier_definition (`numeric`) if `0` then no outliers are removed, otherwise
#'   outliers (those more than `outlier_definition*IQR below/above Q1/Q3`) are removed
#' @returns (`numeric`) vector without the outlier values
#' @keywords internal
remove_outliers_from <- function(var, outlier_definition) {
  var[filter_outliers(var, outlier_definition)]
}


#' Logical vector
#'
#' Returns a logical vector.
#' Suitable for `dplyr::filter()` and data.frames.
#'
#' @inheritParams remove_outliers_from
#'
#' @keywords internal
filter_outliers <- function(var, outlier_definition) {
  if (outlier_definition == 0) {
    return(rep(TRUE, length.out = length(var)))
  }
  q1_q3 <- stats::quantile(var, probs = c(0.25, 0.75), type = 2, na.rm = TRUE)
  iqr <- q1_q3[2] - q1_q3[1]
  var >= q1_q3[1] - outlier_definition * iqr & var <= q1_q3[2] + outlier_definition * iqr
}


# sparklines ----

#' S3 generic for `sparkline` widget HTML
#'
#' Generates the `sparkline` HTML code corresponding to the input array.
#' For numeric variables creates a box plot, for character and factors - bar plot.
#' Produces an empty string for variables of other types.
#'
#' @param arr vector of any type and length
#' @param width `numeric` the width of the `sparkline` widget (pixels)
#' @param bar_spacing `numeric` the spacing between the bars (in pixels)
#' @param bar_width `numeric` the width of the bars (in pixels)
#' @param ... `list` additional options passed to bar plots of `jquery.sparkline`;
#'                   see [`jquery.sparkline docs`](https://omnipotent.net/jquery.sparkline/#common)
#'
#' @return Character string containing HTML code of the `sparkline` HTML widget.
#' @keywords internal
create_sparklines <- function(arr, width = 150, ...) {
  if (all(is.null(arr))) {
    return("")
  }
  UseMethod("create_sparklines")
}

#' @rdname create_sparklines
#' @keywords internal
#' @export
create_sparklines.logical <- function(arr, ...) {
  create_sparklines(as.factor(arr))
}

#' @rdname create_sparklines
#' @keywords internal
#' @export
create_sparklines.numeric <- function(arr, width = 150, ...) {
  if (any(is.infinite(arr))) {
    return(as.character(tags$code("infinite values", class = "text-blue")))
  }
  if (length(arr) > 100000) {
    return(as.character(tags$code("Too many rows (>100000)", class = "text-blue")))
  }

  arr <- arr[!is.na(arr)]
  sparkline::spk_chr(unname(arr), type = "box", width = width, ...)
}

#' @rdname create_sparklines
#' @keywords internal
#' @export
create_sparklines.character <- function(arr, ...) {
  create_sparklines(as.factor(arr))
}


#' @rdname create_sparklines
#' @keywords internal
#' @export
create_sparklines.factor <- function(arr, width = 150, bar_spacing = 5, bar_width = 20, ...) {
  decreasing_order <- TRUE

  counts <- table(arr)
  if (length(counts) >= 100) {
    return(as.character(tags$code("> 99 levels", class = "text-blue")))
  } else if (length(counts) == 0) {
    return(as.character(tags$code("no levels", class = "text-blue")))
  } else if (length(counts) == 1) {
    return(as.character(tags$code("one level", class = "text-blue")))
  }

  # Summarize the occurences of different levels
  # and get the maximum and minimum number of occurences
  # This is needed for the sparkline to correctly display the bar plots
  # Otherwise they are cropped
  counts <- sort(counts, decreasing = decreasing_order, method = "radix")
  max_value <- if (decreasing_order) counts[1] else counts[length[counts]]
  max_value <- unname(max_value)

  sparkline::spk_chr(
    unname(counts),
    type = "bar",
    chartRangeMin = 0,
    chartRangeMax = max_value,
    width = width,
    barWidth = bar_width,
    barSpacing = bar_spacing,
    tooltipFormatter = custom_sparkline_formatter(names(counts), as.vector(counts))
  )
}

#' @rdname create_sparklines
#' @keywords internal
#' @export
create_sparklines.Date <- function(arr, width = 150, bar_spacing = 5, bar_width = 20, ...) {
  arr_num <- as.numeric(arr)
  arr_num <- sort(arr_num, decreasing = FALSE, method = "radix")
  binwidth <- get_bin_width(arr_num, 1)
  bins <- floor(diff(range(arr_num)) / binwidth) + 1
  if (all(is.na(bins))) {
    return(as.character(tags$code("only NA", class = "text-blue")))
  } else if (bins == 1) {
    return(as.character(tags$code("one date", class = "text-blue")))
  }
  counts <- as.vector(unname(base::table(cut(arr_num, breaks = bins))))
  max_value <- max(counts)

  start_bins <- as.integer(seq(1, length(arr_num), length.out = bins))
  labels_start <- as.character(as.Date(arr_num[start_bins], origin = as.Date("1970-01-01")))
  labels <- paste("Start:", labels_start)

  sparkline::spk_chr(
    unname(counts),
    type = "bar",
    chartRangeMin = 0,
    chartRangeMax = max_value,
    width = width,
    barWidth = bar_width,
    barSpacing = bar_spacing,
    tooltipFormatter = custom_sparkline_formatter(labels, counts)
  )
}

#' @rdname create_sparklines
#' @keywords internal
#' @export
create_sparklines.POSIXct <- function(arr, width = 150, bar_spacing = 5, bar_width = 20, ...) {
  arr_num <- as.numeric(arr)
  arr_num <- sort(arr_num, decreasing = FALSE, method = "radix")
  binwidth <- get_bin_width(arr_num, 1)
  bins <- floor(diff(range(arr_num)) / binwidth) + 1
  if (all(is.na(bins))) {
    return(as.character(tags$code("only NA", class = "text-blue")))
  } else if (bins == 1) {
    return(as.character(tags$code("one date-time", class = "text-blue")))
  }
  counts <- as.vector(unname(base::table(cut(arr_num, breaks = bins))))
  max_value <- max(counts)

  start_bins <- as.integer(seq(1, length(arr_num), length.out = bins))
  labels_start <- as.character(format(as.POSIXct(arr_num[start_bins], origin = as.Date("1970-01-01")), "%Y-%m-%d"))
  labels <- paste("Start:", labels_start)

  sparkline::spk_chr(
    unname(counts),
    type = "bar",
    chartRangeMin = 0,
    chartRangeMax = max_value,
    width = width,
    barWidth = bar_width,
    barSpacing = bar_spacing,
    tooltipFormatter = custom_sparkline_formatter(labels, counts)
  )
}

#' @rdname create_sparklines
#' @keywords internal
#' @export
create_sparklines.POSIXlt <- function(arr, width = 150, bar_spacing = 5, bar_width = 20, ...) {
  arr_num <- as.numeric(arr)
  arr_num <- sort(arr_num, decreasing = FALSE, method = "radix")
  binwidth <- get_bin_width(arr_num, 1)
  bins <- floor(diff(range(arr_num)) / binwidth) + 1
  if (all(is.na(bins))) {
    return(as.character(tags$code("only NA", class = "text-blue")))
  } else if (bins == 1) {
    return(as.character(tags$code("one date-time", class = "text-blue")))
  }
  counts <- as.vector(unname(base::table(cut(arr_num, breaks = bins))))
  max_value <- max(counts)

  start_bins <- as.integer(seq(1, length(arr_num), length.out = bins))
  labels_start <- as.character(format(as.POSIXct(arr_num[start_bins], origin = as.Date("1970-01-01")), "%Y-%m-%d"))
  labels <- paste("Start:", labels_start)

  sparkline::spk_chr(
    unname(counts),
    type = "bar",
    chartRangeMin = 0,
    chartRangeMax = max_value,
    width = width,
    barWidth = bar_width,
    barSpacing = bar_spacing,
    tooltipFormatter = custom_sparkline_formatter(labels, counts)
  )
}

#' @rdname create_sparklines
#' @keywords internal
#' @export
create_sparklines.default <- function(arr, width = 150, ...) {
  as.character(tags$code("unsupported variable type", class = "text-blue"))
}

custom_sparkline_formatter <- function(labels, counts) {
  htmlwidgets::JS(
    sprintf(
      "function(sparkline, options, field) {
        return 'ID: ' + %s[field[0].offset] + '<br>' + 'Count: ' + %s[field[0].offset];
        }",
      jsonlite::toJSON(labels),
      jsonlite::toJSON(counts)
    )
  )
}
