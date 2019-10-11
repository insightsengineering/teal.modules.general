#' Variable Browser Teal Module
#'
#' The variable browser provides a table with variable names and labels and a
#' plot the visualizes the content of a particular variable.
#'
#' @inheritParams teal::module
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
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADTTE", ADTTE)
#'   ),
#'   root_modules(
#'     tm_variable_browser(label = "Variable browser")
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_variable_browser <- function(label = "variable browser") {
  stopifnot(is.character.single(label))

  module(
    label,
    server = srv_page_variable_browser,
    ui = ui_page_variable_browser,
    filters = "all",
    ui_args = list()
  )
}

# ui function
#' @importFrom stats setNames
ui_page_variable_browser <- function(id, datasets) {
  ns <- NS(id)

  div(
    class = "row",
    div(
      class = "col-md-6",
      # variable browser
      div(
        class = "well", style = "background: transparent;",
        do.call(
          tabsetPanel,
          c(
            id = ns("tsp"),
            do.call(tagList, setNames(lapply(datasets$datanames(), function(domain) {
              ui_id <- paste0("variable_browser_", domain)
              tabPanel(domain, div(
                style = "margin-top: 15px;",
                DT::dataTableOutput(ns(ui_id), width = "100%")
              ))
            }), NULL))
          )
        ),
        checkboxInput(ns("show_adsl_vars"), "Show ADSL variables in datasets other than ADSL", value = FALSE)
      )
    ),
    div(
      class = "col-md-6",
      div(
        class = "well",
        style = "padding-bottom: 0px",
        plotOutput(ns("variable_plot"), height = "500px"),
        div(
          class = "clearfix",
          style = "margin-top: 15px;",
          div(
            class = "pull-left",
            radioButtons(ns("raw_or_filtered"), NULL,
                         choices = c("unfiltered data" = "raw", "filtered data" = "filtered"),
                         selected = "filtered", inline = TRUE)
          ),
          uiOutput(ns("action_link"))

        ),
        uiOutput(ns("warning"))
      )
    )
  )
}


#' @importFrom grid convertWidth grid.draw grid.newpage textGrob unit
#' @importFrom utils capture.output str
srv_page_variable_browser <- function(input, output, session, datasets) {
  # useful to pass on to parent program
  plot_var <- reactiveValues(data = NULL, variable = list())

  action_link <- reactiveValues(vars = list())

  current_rows <- new.env() # nolint

  adsl_vars <- names(datasets$get_data("ADSL"))


  lapply(datasets$datanames(), function(name) {
    .log("variable label table:", name)

    ui_id <- paste0("variable_browser_", name)

    output[[ui_id]] <- DT::renderDataTable({
      df <- datasets$get_data(name, filtered = FALSE, reactive = TRUE)

      show_adsl_vars <- input$show_adsl_vars

      if (is.null(df)) {
        current_rows[[name]] <- character(0)
        data.frame(Variable = character(0), Label = character(0), stringsAsFactors = FALSE)
      } else {
        labels <- setNames(unlist(lapply(df, function(x) {
          lab <- attr(x, "label")
          if (is.null(lab)) "" else lab
        }), use.names = FALSE), names(df))

        if (!show_adsl_vars && name != "ADSL") {
          adsl_vars <- names(datasets$get_data("ADSL", filtered = FALSE, reactive = FALSE))
          labels <- labels[!(names(labels) %in% adsl_vars)]
        }

        current_rows[[name]] <- names(labels)
        data.frame(Variable = names(labels), Label = labels, stringsAsFactors = FALSE)
      }
    }, rownames = FALSE, selection = list(mode = "single", target = "row", selected = 1), server = TRUE)


    ui_id_sel <- paste0(ui_id, "_rows_selected")
    observeEvent(input[[ui_id_sel]], {
      plot_var$data <- name
      plot_var$variable[[name]] <- current_rows[[name]][input[[ui_id_sel]]]
    })
  })

  output$variable_plot <- renderPlot({
    data <- input$tsp
    varname <- plot_var$variable[[input$tsp]]
    active <- input$tsp
    type <- input$raw_or_filtered

    validate(
      need(data, "no data selected"),
      need(varname, "no variable selected"),
      need(active, "no tab active"),
      need(type, "select what type of data to plot")
    )

    .log("plot variable", varname, "for data", data, "(", type, ")", " | active:", active)

    df <- datasets$get_data(data, filtered = (type == "filtered"), reactive = TRUE)
    varlabel <- as.list(datasets$get_data_attr(dataname = data, attr = "labels")$column_labels)[[varname]]

    if (is.null(varname)) {
      validate(need(NULL, "no valid variable was selected"))
    } else {
      validate(need(datasets$has_variable(data, varname), "variable not available"))

      var <- df[[varname]]
      d_var_name <- paste0(varlabel, " [", data, ".", varname, "]")

      grid::grid.newpage()

      plot_grob <- if (is.factor(var) || is.character(var)) {
        groups <- unique(as.character(var))
        if (length(groups) > 30) {
          grid::textGrob(
            paste0(d_var_name, ":\n  ", paste(groups[1:min(10, length(groups))], collapse = "\n  "), "\n   ..."),
            x = grid::unit(1, "line"), y = grid::unit(1, "npc") - grid::unit(1, "line"),
            just = c("left", "top")
          )
        } else {
          p <- qplot(var) + xlab(d_var_name) + theme_light() + coord_flip()
          ggplotGrob(p)
        }
      } else if (is.numeric(var)) {
        ## histogram
        p <- qplot(var) + xlab(d_var_name) + theme_light() + coord_flip()
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
  })

  warning_messages <- reactiveValues(varinfo = "", i = 0)

  observeEvent(input$add_filter_variable, {

    dataname <- input$tsp
    varname <- plot_var$variable[[input$tsp]]

    .log("add filter variable", dataname, "and", varname)

    if (!is.null(dataname)) {
      if (!is.null(varname)) {
        if (dataname != "ADSL" && varname %in% adsl_vars) {
          warning_messages$varinfo <- paste("You can not add an ADSL variable from any dataset other than ADSL.
                                            Switch to the ADSL data and add the variable from there.")
        } else if (datasets$get_filter_type(dataname, varname) == "unknown") {
          warning_messages$varinfo <- paste("variable", paste(dataname, varname, sep = "."),
                                            "can currently not be used as a filter variable.")
        } else {
          if (!is.null(datasets$get_filter_execution(dataname, varname))) {
            datasets$remove_filter(dataname, varname)
            .log("filter removed:", varname)
            action_link$vars[[dataname]][[varname]] <- FALSE
          } else {
            datasets$set_default_filter_state(dataname, varname)
            .log("filter added:", varname)
            action_link$vars[[dataname]] <- list(varname = TRUE)
          }
          warning_messages$varinfo <- ""
        }
        warning_messages$i <- warning_messages$i + 1
      }
    }
  })

  output$action_link <- renderUI({
    varname <- plot_var$variable[[input$tsp]]
    dataname <- input$tsp
    if (!is.null(action_link$vars[[dataname]][[varname]]) && action_link$vars[[dataname]][[varname]]) {
      actionLink(session$ns("add_filter_variable"), "remove as filter variable", class = "pull-right")
    } else {
      actionLink(session$ns("add_filter_variable"), "add as filter variable", class = "pull-right")
    }
  })

  output$warning <- renderUI({
    warning_messages$i
    msg <- warning_messages$varinfo

    if (is.null(msg) || msg == "") {
      div(style = "display: none;")
    } else {
      div(class = "text-warning", style = "margin-bottom: 15px;", msg)
    }
  })
}
