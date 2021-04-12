#' Variable Browser Teal Module
#'
#' The variable browser provides a table with variable names and labels and a
#' plot that visualizes the content of a particular variable.
#'
#' @details Numeric columns with fewer than 30 distinct values can be treated as either factors
#' or numbers with a checkbox allowing users to switch how they are treated (if < 6 unique values
#' then the default is categorical, otherwise it is numeric).
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param datasets_selected (`character`) A vector of datasets which should be
#'   shown and in what order. Names in the vector have to correspond with datasets names.
#'   If vector of length zero (default) then all datasets are shown.
#'
#' @importFrom stats quantile sd
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADTTE <- radtte(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADTTE", ADTTE, code = "ADTTE <- radtte(cached = TRUE)"),
#'     check = TRUE
#'   ),
#'   root_modules(
#'     tm_variable_browser(label = "Variable browser")
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_variable_browser <- function(label = "Variable Browser",
                                datasets_selected = character(0),
                                pre_output = NULL,
                                post_output = NULL) {
  stop_if_not(is_character_single(label),
              is_character_empty(datasets_selected) || is_character_vector(datasets_selected)
  )

  datasets_selected <- unique(datasets_selected)

  module(
    label,
    server = srv_variable_browser,
    ui = ui_variable_browser,
    filters = "all",
    server_args = list(datasets_selected = datasets_selected),
    ui_args = list(
      datasets_selected = datasets_selected,
      pre_output = pre_output,
      post_output = post_output
    )
  )
}

# ui function
#' @importFrom stats setNames
#' @importFrom shinyWidgets switchInput
ui_variable_browser <- function(id,
                                datasets,
                                datasets_selected,
                                pre_output = NULL,
                                post_output = NULL) {
  ns <- NS(id)

  datanames <- get_datanames_selected(datasets, datasets_selected)

  standard_layout(
    output = fluidRow(
      htmlwidgets::getDependency("sparkline"), # needed for sparklines to work
      column(
        6,
        # variable browser
        white_small_well(
          do.call(
            tabsetPanel,
            c(
              id = ns("tsp"),
              do.call(
                tagList,
                setNames(
                  lapply(
                    datanames,
                    function(dataname) {
                      tabPanel(
                        dataname,
                        div(
                          style = "margin-top: 15px;",
                          textOutput(ns(paste0("dataset_summary_", dataname)))
                        ),
                        div(style = "margin-top: 15px;",
                          get_dt_rows(ns(paste0(
                            "variable_browser_", dataname
                          )),
                          ns(
                            paste0("variable_browser_", dataname, "_rows")
                          )),
                          DT::dataTableOutput(ns(paste0(
                            "variable_browser_", dataname
                          )), width = "100%"))
                      )
                    }
                  ),
                  NULL
                )
              )
            )
          ),
          { # nolint
            x <- checkboxInput(ns("show_parent_vars"), "Show parent dataset variables", value = FALSE)
            if (is(datasets, "CDISCFilteredData")) {
              x
            } else {
              shinyjs::hidden(x)
            }
          }
        )
      ),
      column(
        6,
        white_small_well(
          div(
            class = "clearfix",
            style = "margin: 15px 15px 0px 15px;",
            div(
              class = "pull-left",
              shinyWidgets::switchInput(
                inputId = ns("raw_or_filtered"),
                label = "Use filtered data",
                value = TRUE,
                width = "100%",
                labelWidth = "130px",
                handleWidth = "50px"
              )
            )
          ),
          div(
            class = "clearfix;",
            style = "margin: 0px 15px 15px 15px;",
            uiOutput(ns("ui_numeric_display"))
          ),
          plot_with_settings_ui(ns("variable_plot")),
          br(),
          get_dt_rows(ns("variable_summary_table"), ns("variable_summary_table_rows")),
          DT::dataTableOutput(ns("variable_summary_table"))
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}


#' @importFrom grid convertWidth grid.draw grid.newpage textGrob unit
#' @importFrom utils capture.output str
#' @importFrom shinyWidgets switchInput
srv_variable_browser <- function(input, output, session, datasets, datasets_selected) {

  # if there are < this number of unique records then a numeric
  # variable can be treated as a factor and all factors with < this groups
  # have their values plotted
  .unique_records_for_factor <- 30
  # if there are < this number of unique records then a numeric
  # variable is by default treated as a factor
  .unique_records_default_as_factor <- 6 # nolint

  # useful to pass on to parent program
  plot_var <- reactiveValues(data = NULL, variable = list())



  # reactive contain list of the column of data to be analyzed (data) and
  # the variable label for this column (d_var_name)
  data_for_analysis <- reactive({
    data <- input$tsp
    varname <- plot_var$variable[[input$tsp]]
    type <- input$raw_or_filtered

    validate(need(data, "no data selected"))
    validate(need(varname, "no variable selected"))
    validate(need(is.logical(type), "select what type of data to plot"))

    df <- datasets$get_data(data, filtered = type)
    validate_has_data(df, 1)
    validate_has_variable(varname = varname, data = df, "variable not available")
    .log("plot/summarize variable", varname, "for data", data, "(", `if`(type, "filtered", "raw"), ")")

    varlabel <- datasets$get_varlabels(dataname = data, varname)
    d_var_name <- paste0(if_na(varlabel, varname), " [", data, ".", varname, "]")
    list(data = df[[varname]], d_var_name = d_var_name)
  })


  treat_numeric_as_factor <- reactive({
    if (length(unique(data_for_analysis()$data)) < .unique_records_for_factor && !is.null(input$numeric_as_factor)) {
      input$numeric_as_factor
    } else {
      FALSE
    }
  })


  current_rows <- new.env() # nolint
  # subset certain datasets
  datanames <- get_datanames_selected(datasets, datasets_selected)

  lapply(datanames, function(name) {
    .log("variable label table:", name)

    dataset_ui_id <- paste0("dataset_summary_", name)
    output[[dataset_ui_id]] <- renderText({
      df <- datasets$get_data(name, filtered = FALSE)
      key <- datasets$get_keys(name)
      sprintf(
        "Dataset with %s unique key rows and %s variables",
        nrow(unique(`if`(!is_empty(key), df[, key, drop = FALSE], df))),
        ncol(df)
      )
    })

    table_ui_id <- paste0("variable_browser_", name)

    output[[table_ui_id]] <- DT::renderDataTable({
      df <- datasets$get_data(name, filtered = FALSE)

      df_vars <- if (isFALSE(input$show_parent_vars)) {
        datasets$get_filterable_varnames(name)
      } else {
        datasets$get_varnames(name)
      }
      df <- df[df_vars]

      if (is.null(df) || ncol(df) == 0) {
        current_rows[[name]] <- character(0)
        data.frame(
          Variable = character(0),
          Label = character(0),
          Missings = character(0),
          Sparklines = character(0),
          stringsAsFactors = FALSE)
      } else {
        # extract data variable labels
        labels <- setNames(
          ulapply(
            df,
            function(x) {
              if_null(attr(x, "label"), "")
            }
          ),
          names(df)
        )

        current_rows[[name]] <- names(labels)

        # calculate number of missing values
        missings <- vapply(
          df,
          var_missings_info,
          FUN.VALUE = character(1),
          USE.NAMES = FALSE
        )

        # get icons proper for the data types
        icons <- setNames(teal:::variable_types(df), colnames(df))
        icons[intersect(datasets$get_keys(name), colnames(df))] <- "primary_key"
        icons <- teal:::variable_type_icons(icons)

        # generate sparklines
        sparklines_html <- vapply(
          df,
          create_sparklines,
          FUN.VALUE = character(1),
          USE.NAMES = FALSE)

        # join labels, missings and icons
        dt <- DT::datatable(
          data.frame(
            Variable = paste(icons, names(labels)),
            Label = labels,
            Missings = missings,
            Sparklines = sparklines_html,
            stringsAsFactors = FALSE
          ),
          escape = FALSE,
          rownames = FALSE,
          selection = list(mode = "single", target = "row", selected = 1),
          options = list(
            columnDefs = list(
              list(orderable = FALSE, className = "details-control", targets = 0)
            ),
            fnDrawCallback = htmlwidgets::JS("function() { HTMLWidgets.staticRender(); }")
          )
        )
      }
    },
    server = TRUE,
    options = list(pageLength = input$table_ui_id_rows)
    )

    table_id_sel <- paste0(table_ui_id, "_rows_selected")
    observeEvent(input[[table_id_sel]], {
      plot_var$data <- name
      plot_var$variable[[name]] <- current_rows[[name]][input[[table_id_sel]]]
    })
  })

  output$ui_numeric_display <- renderUI({
    data <- input$tsp
    varname <- plot_var$variable[[input$tsp]]
    type <- input$raw_or_filtered
    req(data, varname, is.logical(type))

    df <- datasets$get_data(data, filtered = type)

    numeric_ui <- tagList(
      fluidRow(
        div(
          class = "col-md-4",
          shinyWidgets::switchInput(
            inputId = session$ns("display_density"),
            label = "Show density",
            value = if_null(isolate(input$display_density), TRUE),
            width = "50%",
            labelWidth = "100px",
            handleWidth = "50px"
          )
        ),
        div(
          class = "col-md-4",
          shinyWidgets::switchInput(
            inputId = session$ns("remove_outliers"),
            label = "Remove outliers",
            value = if_null(isolate(input$remove_outliers), FALSE),
            width = "50%",
            labelWidth = "100px",
            handleWidth = "50px"
          )
        ),
        div(
          class = "col-md-4",
          uiOutput(session$ns("outlier_definition_slider_ui"))
        )
      ),
      div(
        style = "margin-left: 15px;",
        uiOutput(session$ns("ui_density_help")),
        uiOutput(session$ns("ui_outlier_help"))
      )
    )

    if (is.numeric(df[[varname]])) {
      unique_entries <- length(unique(df[[varname]]))
      if (unique_entries < .unique_records_for_factor) {
        list(
          checkboxInput(session$ns("numeric_as_factor"),
            "Treat variable as factor",
            value = if_null(isolate(input$numeric_as_factor), unique_entries < .unique_records_default_as_factor)),
          conditionalPanel("!input.numeric_as_factor", ns = session$ns, numeric_ui)
        )
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
      div(
        "Outlier definition:",
        title = paste("Use the slider to choose the cut-off value to define outliers;\nthe larger the value the",
          "further below Q1/above Q3 points have\nto be in order to be classed as outliers"),
        icon("info-circle")
      ),
      min = 1,
      max = 5,
      value = 3,
      step = 0.5
    )
  })

  output$ui_density_help <- renderUI({
    req(is.logical(input$display_density))
    if (input$display_density) {
      tags$small(helpText(paste(
        "Kernel density estimation with gaussian kernel",
        "and bandwidth function bw.nrd0 (R default)"
      )))
    } else {
      NULL
    }
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


  variable_plot_r <- reactive({
    display_density <- if_null(input$display_density, FALSE)
    remove_outliers <- if_null(input$remove_outliers, FALSE)

    if (remove_outliers) {
      req(input$outlier_definition_slider)
      outlier_definition <- as.numeric(input$outlier_definition_slider)
    } else {
      outlier_definition <- 0
    }

    plot_var_summary(
      var = data_for_analysis()$data,
      var_lab = data_for_analysis()$d_var_name,
      numeric_as_factor = treat_numeric_as_factor(),
      display_density = display_density,
      outlier_definition = outlier_definition,
      records_for_factor = .unique_records_for_factor
    )
  })

  callModule(
    plot_with_settings_srv,
    id = "variable_plot",
    plot_r = variable_plot_r,
    height =  c(500, 200, 2000)
  )

  output$variable_summary_table <- DT::renderDataTable({
    var_summary_table(data_for_analysis()$data, treat_numeric_as_factor(), input$variable_summary_table_rows)
  })
}

#' Summarizes missings occurrence
#'
#' Summarizes missings occurrence in vector
#' @param x vector of any type and length
#' @return text describing \code{NA} occurrence.
var_missings_info <- function(x) {
  return(sprintf("%s [%s%%]", sum(is.na(x)), round(mean(is.na(x) * 100), 2)))
}

#' S3 generic for \code{sparkline} widget HTML
#'
#' Generates the \code{sparkline} HTML code corresponding to the input array.
#' For numeric variables creates a box plot, for character and factors - bar plot.
#' Produces an empty string for variables of other types.
#'
#' @param arr vector of any type and length
#' @param width \code{numeric} the width of the \code{sparkline} widget (pixels)
#' @param ... \code{list} additional options passed to bar plots of \code{jquery.sparkline};  see
#' \href{http://omnipotent.net/jquery.sparkline/#common}{\code{jquery.sparkline docs}}
#'
#' @return character variable containing the HTML code of the \code{sparkline} HTML widget
#'
#' @export
#'
#' @importFrom sparkline spk_chr
create_sparklines <- function(arr, width = 150, ...) {
  if (all(is.null(arr))) {
    return("")
  }
  UseMethod("create_sparklines")
}

#' Default method for \code{\link{create_sparklines}}
#'
#' @inheritParams create_sparklines
#'
#' @export
#'
#' @seealso \code{\link{create_sparklines}}
create_sparklines.default <- function(arr, width = 150, ...) { # nousage # nolint
  return(as.character(tags$code("unsupported variable type", style = "color:blue")))
}

#' Generates the HTML code for the \code{sparkline} widget
#'
#' @inheritParams create_sparklines
#'
#' @return \code{character} with HTML code for the \code{sparkline} widget
#'
#' @export
#'
#' @seealso \code{\link{create_sparklines}}
create_sparklines.Date <- function(arr, ...) { # nousage # nolint
  return(as.character(tags$code("Date variable", style = "color:blue")))
}

#' Generates the HTML code for the \code{sparkline} widget
#'
#'
#' @inheritParams create_sparklines
#'
#' @return \code{character} with HTML code for the \code{sparkline} widget
#'
#' @export
#'
#' @seealso \code{\link{create_sparklines}}
create_sparklines.POSIXct <- function(arr, ...) { # nousage # nolint
  return(as.character(tags$code("POSIXct variable", style = "color:blue")))
}

#' Generates the HTML code for the \code{sparkline} widget
#'
#'
#' @inheritParams create_sparklines
#'
#' @return \code{character} with HTML code for the \code{sparkline} widget
#'
#' @export
#'
#' @seealso \code{\link{create_sparklines}}
create_sparklines.POSIXt <- function(arr, ...) { # nousage # nolint
  return(as.character(tags$code("POSIXt variable", style = "color:blue")))
}

#' Generates the HTML code for the \code{sparkline} widget
#'
#' Coerces character vector to factor and delegates to the \code{create_sparklines.factor}
#'
#' @inheritParams create_sparklines
#'
#' @return \code{character} with HTML code for the \code{sparkline} widget
#'
#' @export
#'
#' @seealso \code{\link{create_sparklines}}
create_sparklines.character <- function(arr, ...) { # nousage # nolint
  arr <- as.factor(arr)
  return(create_sparklines(arr))
}

#' Generates the \code{sparkline} HTML code
#'
#' @inheritParams create_sparklines
#' @param bar_spacing \code{numeric} spacing between the bars (in pixels)
#' @param bar_width \code{numeric} width of the bars (in pixels)
#'
#' @return \code{character} with HTML code for the \code{sparkline} widget
#'
#' @importFrom sparkline spk_chr
#' @importFrom jsonlite toJSON
#' @importFrom htmlwidgets JS
#'
#' @export
#'
#' @seealso \code{\link{create_sparklines}}
create_sparklines.factor <- function(arr, width = 150, bar_spacing = 5, bar_width = 20, ...) { # nousage # nolint
  decreasing_order <- TRUE

  counts <- table(droplevels(arr))
  if (length(counts) >= 100) {
    return(as.character(tags$code("> 99 levels", style = "color:blue")))
  } else if (length(counts) == 0) {
    return(as.character(tags$code("no levels", style = "color:blue")))
  } else if (length(counts) == 1) {
    return(as.character(tags$code("one level", style = "color:blue")))
  }

  # Summarize the occurences of different levels
  # and get the maximum and minimum number of occurences
  # This is needed for the sparkline to correctly display the bar plots
  # Otherwise they are cropped
  counts <- sort(counts, decreasing = decreasing_order)
  max_value <- if (decreasing_order) counts[1] else counts[length[counts]]
  max_value <- unname(max_value)

  custom_formatter <- function(labels, counts) {
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

  res <- do.call(
    spk_chr,
    c(
      list(
        unname(counts),
        type = "bar",
        chartRangeMin = 0,
        chartRangeMax = max_value,
        width = width,
        barWidth = bar_width,
        barSpacing = bar_spacing,
        tooltipFormatter = custom_formatter(names(counts), as.vector(counts))
      ),
      ...
    )
  )
  return(res)
}

#' Generates the \code{sparkline} HTML code
#'
#' @inheritParams create_sparklines
#'
#' @return \code{character} with HTML code for the \code{sparkline} widget
#'
#' @importFrom sparkline spk_chr
#'
#' @export
#'
#' @seealso \code{\link{create_sparklines}}

create_sparklines.numeric <- function(arr, width = 150, ...) { # nousage # nolint
  if (any(is.infinite(arr))) {
    return(as.character(tags$code("infinite values", style = "color:blue")))
  }
  if (length(arr) > 100000) {
    return(as.character(tags$code("Too many rows (>100000)", style = "color:blue")))
  }

  res <- sparkline::spk_chr(arr, type = "box", width = width, ...)
  return(res)
}

#' Summarizes variable
#'
#' Creates html summary with statistics relevant to data type. For numeric values it returns central
#' tendency measures, for factor returns level counts, for Date  date range, for other just
#' number of levels.
#' @param x vector of any type
#' @param numeric_as_factor \code{logical} should the numeric variable be treated as a factor
#' @param dt_rows \code{numeric} current/latest DT page length
#' @return text with simple statistics.
#' @importFrom stats median
var_summary_table <- function(x, numeric_as_factor, dt_rows) {
  if (is.null(dt_rows))
    dt_rows <- 10
  if (is.numeric(x) && !numeric_as_factor) {

    req(!any(is.infinite(x)))

    qvals <- round(quantile(x, na.rm = TRUE, probs = c(0.25, 0.5, 0.75)), 2)
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
          round(sd(x, na.rm = TRUE), 2),
          length(x[!is.na(x)])
        )
      )

    DT::datatable(summary, rownames = FALSE, options = list(dom = "<t>", pageLength = dt_rows))
  } else if (is.factor(x) || is.character(x) || (is.numeric(x) && numeric_as_factor)) {

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
          median(x, na.rm = TRUE),
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
#' @param var vector of any type to be plotted. For numeric variables it produces histogram with
#' density line, for factors it creates frequency plot
#' @param var_lab text describing selected variable to be displayed on the plot
#' @param numeric_as_factor \code{logical} should the numeric variable be treated as a factor
#' @param display_density \code{logical} Should density estimation be displayed for numeric values?
#' @param outlier_definition If 0 no outliers are removed, otherwise
#'   outliers (those more than outlier_definition*IQR below/above Q1/Q3 be removed)
#' @param records_for_factor \code{numeric} if the number of factor levels is >= than this value then
#'   a graph of the factors isn't shown, only a list of values.
#' @return plot
#'
#' @importFrom stats IQR
plot_var_summary <- function(var,
  var_lab,
  numeric_as_factor,
  display_density = is.numeric(var),
  outlier_definition,
  records_for_factor) {

  stopifnot(is_logical_single(display_density))

  get_bin_width <- function(x, scaling_factor = 2) {
    x <- x[!is.na(x)]
    qntls <- quantile(x, probs = c(0.1, 0.25, 0.75, 0.9))
    iqr <- qntls[3] - qntls[2]
    binwidth <- max(scaling_factor * iqr / length(x) ^ (1 / 3), sqrt(qntls[4] - qntls[1]))
    binwidth <- ifelse(binwidth == 0, 1, binwidth)
  }

  grid::grid.newpage()

  plot_grob <- if (is.factor(var) || is.character(var)) {
    groups <- unique(as.character(var))
    len_groups <- length(groups)
    if (len_groups >= records_for_factor) {
      len_groups_even <- ifelse(!len_groups %% 2, len_groups, len_groups - 1)
      groups_df <- apply(matrix(groups[1:min(50, len_groups_even)], nrow = 2), 2, paste, collapse = ",  ")
      grid::textGrob(
        sprintf(
          "%s:\n  %s\n   ... other %s values",
          var_lab,
          paste(groups_df, collapse = ",\n  "),
          len_groups - min(50, len_groups_even)
          ),
        x = grid::unit(1, "line"),
        y = grid::unit(1, "npc") - grid::unit(1, "line"),
        just = c("left", "top")
      )
    } else {
      p <- qplot(var) +
        xlab(var_lab) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

      ggplotGrob(p)
    }
  } else if (is.numeric(var)) {
    validate(need(any(!is.na(var)), "No data left to visualize."))

    # Filter out NA
    var <- var[which(!is.na(var))]

    validate(need(!any(is.infinite(var)), "Cannot display graph when data includes infinite values"))

    if (numeric_as_factor) {
      var <- factor(var, levels = sort(unique(var)))
      p <- qplot(var) +
        xlab(var_lab) +
        theme_light() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    else {
      # remove outliers
      if (outlier_definition != 0) {
        q1_q3 <- quantile(var, probs = c(0.25, 0.75))
        iqr <- q1_q3[2] - q1_q3[1]
        number_records <- length(var)
        var <- var[var >= q1_q3[1] - outlier_definition * iqr & var <= q1_q3[2] + outlier_definition * iqr]
        number_outliers <- number_records - length(var)
        outlier_text <- paste0(number_outliers, " outliers (",
          round(number_outliers / number_records * 100, 2),
          "% of non-missing records) not shown")
        validate(need(length(var) > 1,
          "At least two data points must remain after removing outliers for this graph to be displayed"))
      }


      ## histogram
      binwidth <- get_bin_width(var)
      p <- ggplot(data = data.frame(var = var), aes_string(x = "var", y = "..count..")) +
        geom_histogram(binwidth = binwidth) +
        scale_y_continuous(
          sec.axis = sec_axis(
            trans = ~ . / nrow(data.frame(var = var)),
            labels = scales::percent,
            name = "proportion (in %)"
          )
        ) +
        xlab(var_lab) +
        theme_light()

      if (display_density) {
        p <- p + geom_density(aes_string(y = "..count.. * binwidth"))
      }

      if (outlier_definition != 0) {
        p <- p + annotate(
          geom = "text",
          label = outlier_text,
          x = Inf, y = Inf,
          hjust = 1.02, vjust = 1.2,
          color = "black"
        )
      }
    }
    ggplotGrob(p)
  } else if (inherits(var, "Date") || inherits(var, "POSIXct") || inherits(var, "POSIXlt")) {

    var_num <- as.numeric(var)
    binwidth <- get_bin_width(var_num, 1)

    p <- ggplot(data = data.frame(var = var), aes_string(x = "var", y = "..count..")) +
      geom_histogram(binwidth = binwidth) +
      xlab(var_lab) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotGrob(p)
  } else {
    grid::textGrob(
      paste(strwrap(
        utils::capture.output(utils::str(var)),
        width = .9 * grid::convertWidth(grid::unit(1, "npc"), "char", TRUE)
      ), collapse = "\n"),
      x = grid::unit(1, "line"), y = grid::unit(1, "npc") - grid::unit(1, "line"), just = c("left", "top")
    )
  }

  grid::grid.draw(plot_grob)
  plot_grob
}
