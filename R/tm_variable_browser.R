#' Variable Browser Teal Module
#'
#' The variable browser provides a table with variable names and labels and a
#' plot that visualizes the content of a particular variable.
#'
#' @inheritParams teal::module
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
tm_variable_browser <- function(label = "variable browser") {
  stopifnot(is_character_single(label))

  module(
    label,
    server = srv_variable_browser,
    ui = ui_variable_browser,
    filters = "all",
    ui_args = list()
  )
}

# ui function
#' @importFrom stats setNames
#' @importFrom shinyWidgets switchInput
ui_variable_browser <- function(id, datasets) {
  ns <- NS(id)

  fluidRow(
    column(
      6,
      # variable browser
      white_small_well(
        do.call(
          tabsetPanel,
          c(
            id = ns("tsp"),
            do.call(tagList, setNames(lapply(datasets$datanames(), function(dataname) {
              tabPanel(
                dataname,
                div(
                  style = "margin-top: 15px;",
                  textOutput(ns(paste0("dataset_summary_", dataname)))
                ),
                div(
                  style = "margin-top: 15px;",
                  DT::dataTableOutput(ns(paste0("variable_browser_", dataname)), width = "100%")
                )
              )
            }), NULL))
          )
        ),
        checkboxInput(ns("show_adsl_vars"), "Show ADSL variables in datasets other than ADSL", value = FALSE)
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
        plotOutput(ns("variable_plot"), height = "500px"),
        br(),
        DT::dataTableOutput(ns("variable_summary_table"))
      )
    )
  )
}


#' @importFrom grid convertWidth grid.draw grid.newpage textGrob unit
#' @importFrom utils capture.output str
#' @importFrom shinyWidgets switchInput
srv_variable_browser <- function(input, output, session, datasets) {
  # useful to pass on to parent program
  plot_var <- reactiveValues(data = NULL, variable = list())

  current_rows <- new.env() # nolint

  lapply(datasets$datanames(), function(name) {
    .log("variable label table:", name)

    dataset_ui_id <- paste0("dataset_summary_", name)
    output[[dataset_ui_id]] <- renderText({
      df <- datasets$get_data(name, filtered = FALSE)
      sprintf(
        "Dataset with %s unique subjects IDs and %s variables",
        length(unique(df$USUBJID)),
        ncol(df)
      )
    })

    table_ui_id <- paste0("variable_browser_", name)

    output[[table_ui_id]] <- DT::renderDataTable({
      df <- datasets$get_data(name, filtered = FALSE)
      show_adsl_vars <- input$show_adsl_vars

      if (!show_adsl_vars && name != "ADSL") {
        adsl_vars <- names(datasets$get_data("ADSL", filtered = FALSE))
        df <- df[!(names(df) %in% adsl_vars)]
        }

      if (is.null(df)) {
        current_rows[[name]] <- character(0)
        data.frame(Variable = character(0), Label = character(0), stringsAsFactors = FALSE)
      } else {
        labels <- setNames(unlist(lapply(df, function(x) {
          lab <- attr(x, "label")
          if (is.null(lab)) "" else lab
          }), use.names = FALSE), names(df))

        current_rows[[name]] <- names(labels)
        missings <- vapply(df, var_missings_info, FUN.VALUE = character(1), USE.NAMES = FALSE)
        icons <- vapply(
          df,
          function(x) teal:::variable_type_icons(class(x)[1]),
          FUN.VALUE = character(1),
          USE.NAMES = FALSE
          )

        dt <- DT::datatable(
          data.frame(
            Variable = paste(icons, names(labels)),
            Label = labels,
            Missings = missings,
            stringsAsFactors = FALSE
            ),
          escape = FALSE,
          rownames = FALSE,
          selection = list(mode = "single", target = "row", selected = 1),
          options = list(
            columnDefs = list(
              list(orderable = FALSE, className = "details-control", targets = 0)
              )
            )
          )
        }
      },
      server = TRUE
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

    if (is.numeric(df[[varname]])) {
      list(
        fluidRow(
          column(4,
            shinyWidgets::switchInput(
              inputId = session$ns("display_density"),
              label = "Show density",
              value = if_null(isolate(input$display_density), TRUE),
              width = "100%",
              labelWidth = "130px",
              handleWidth = "50px"
            )
          ),
          column(4,
            shinyWidgets::switchInput(
              inputId = session$ns("remove_outliers"),
              label = "Remove outliers",
              value = if_null(isolate(input$remove_outliers), FALSE),
              width = "100%",
              labelWidth = "130px",
              handleWidth = "50px"
            )
          ),
          column(4,
            uiOutput(session$ns("outlier_definition_slider_ui"))
          )
        ),
        div(
          style = "margin-left: 15px;",
          uiOutput(session$ns("ui_density_help")),
          uiOutput(session$ns("ui_outlier_help"))
        )
      )
    } else {
      NULL
    }
  })

  output$outlier_definition_slider_ui <- renderUI({
    req(input$remove_outliers)
    sliderInput(inputId = session$ns("outlier_definition_slider"),
      div(
        "Outlier definition:",
        title = paste("Use the slider to choose the cut-off value to define outliers;\nthe larger the value the",
          "further below Q1/above Q3 points have\nto be in order to be classed as outliers"),
        icon("info-circle")
      ),
      min = 1,
      max = 5,
      value = 3,
      step = 0.5)
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
      tags$small(helpText(paste0(
        "Outlier data points (those less than Q1 -", input$outlier_definition_slider,
        "*IQR and those greater than Q3 + ", input$outlier_definition_slider, "*IQR) ",
        "have not been displayed on the graph and will not be used for any kernel density estimations, ",
        "although their values remain in the statisics table below"
      )))
    } else {
      NULL
    }
  })

  output$variable_plot <- renderPlot({
    data <- input$tsp
    varname <- plot_var$variable[[input$tsp]]
    type <- input$raw_or_filtered
    display_density <- input$display_density
    remove_outliers <- if_null(input$remove_outliers, FALSE)

    if (remove_outliers) {
      req(input$outlier_definition_slider)
      outlier_definition <- as.numeric(input$outlier_definition_slider)
    } else {
      outlier_definition <- 0
    }

    validate(need(data, "no data selected"))
    validate(need(varname, "no variable selected"))
    validate(need(is.logical(type), "select what type of data to plot"))

    .log("plot variable", varname, "for data", data, "(", `if`(type, "filtered", "raw"), ")")


    if (is.null(varname)) {
      validate(need(NULL, "no valid variable was selected"))
    } else {
      df <- datasets$get_data(data, filtered = type)
      display_density <- if_null(display_density, FALSE)

      validate_has_data(df, 1)
      validate_has_variable(varname = varname, data = df, "variable not available")

      varlabel <- datasets$get_variable_labels(dataname = data, varname)
      var <- df[[varname]]
      d_var_name <- paste0(varlabel, " [", data, ".", varname, "]")

      plot_var_summary(var = var, var_lab = d_var_name, display_density, outlier_definition = outlier_definition)
    }
  })

  output$variable_summary_table <- DT::renderDataTable({
    data <- input$tsp
    varname <- plot_var$variable[[input$tsp]]
    type <- input$raw_or_filtered

    validate(need(data, "no data selected"))
    validate(need(varname, "no variable selected"))
    validate(need(is.logical(type), "select what type of data to plot"))

    df <- datasets$get_data(data, filtered = type)
    validate_has_data(df, 1)

    x <- df[[varname]]
    var_summary_table(x)
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

#' Summarizes variable
#'
#' Creates html summary with statistics relevant to data type. For numeric values it returns central
#' tendency measures, for factor returns level counts, for Date  date range, for other just
#' number of levels.
#' @param x vector of any type
#' @return text with simple statistics.
var_summary_table <- function(x) {
  if (is.numeric(x)) {

    req(!any(is.infinite(x)))

    qvals <- round(quantile(x, na.rm = TRUE, probs = c(0.25, 0.5, 0.75)), 2)
    # classical central tendency measures

    summary <-
      data.frame(
        Statistic = c("min", "Q1", "median", "mean", "Q3", "max", "sd"),
        Value = c(
          round(min(x, na.rm = TRUE), 2),
          qvals[1],
          qvals[2],
          round(mean(x, na.rm = TRUE), 2),
          qvals[3],
          round(max(x, na.rm = TRUE), 2),
          round(sd(x, na.rm = TRUE), 2)
        )
      )

    DT::datatable(summary, rownames = FALSE, options = list(dom = "<t>"))
  } else if (is.factor(x) || is.character(x)) {

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

    dom_opts <- if (nrow(summary) <= 10) {
      "<t>"
    } else {
      "<lf<t>ip>"
    }
    DT::datatable(summary, rownames = FALSE, options = list(dom = dom_opts))
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
#' @param display_density \code{logical} Should density estimation be displayed for numeric values?
#' @param outlier_definition If 0 no outliers are removed, otherwise
#'   outliers (those more than outlier_definition*IQR below/above Q1/Q3 be removed)
#' @return plot
plot_var_summary <- function(var, var_lab, display_density = is.numeric(var), outlier_definition) {
  stopifnot(is_logical_single(display_density))

  grid::grid.newpage()

  plot_grob <- if (is.factor(var) || is.character(var)) {
    groups <- unique(as.character(var))
    len_groups <- length(groups)
    if (len_groups > 30) {
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
    binwidth <- max(
      2 * IQR(var, na.rm = TRUE) / length(var) ^ (1 / 3),
      sqrt(quantile(var, 0.9) - quantile(var, 0.1))
    )

    binwidth <- ifelse(binwidth == 0, 1, binwidth)

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
}
