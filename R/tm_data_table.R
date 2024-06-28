#' `teal` module: Data table viewer
#'
#' Module provides a dynamic and interactive way to view `data.frame`s in a `teal` application.
#' It uses the `DT` package to display data tables in a paginated, searchable, and sortable format,
#' which helps to enhance data exploration and analysis.
#'
#' The `DT` package has an option `DT.TOJSON_ARGS` to show `Inf` and `NA` in data tables.
#' Configure the `DT.TOJSON_ARGS` option via
#' `options(DT.TOJSON_ARGS = list(na = "string"))` before running the module.
#' Note though that sorting of numeric columns with `NA`/`Inf` will be lexicographic not numerical.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param variables_selected (`named list`) Character vectors of the variables (i.e. columns)
#' which should be initially shown for each dataset.
#' Names of list elements should correspond to the names of the datasets available in the app.
#' If no entry is specified for a dataset, the first six variables from that
#' dataset will initially be shown.
#' @param datasets_selected (`character`) A vector of datasets which should be
#' shown and in what order. Names in the vector have to correspond with datasets names.
#' If vector of `length == 0` (default) then all datasets are shown.
#' Note: Only datasets of the `data.frame` class are compatible.
#' @param dt_args (`named list`) Additional arguments to be passed to [DT::datatable()]
#' (must not include `data` or `options`).
#' @param dt_options (`named list`) The `options` argument to `DT::datatable`. By default
#' `list(searching = FALSE, pageLength = 30, lengthMenu = c(5, 15, 30, 100), scrollX = TRUE)`
#' @param server_rendering (`logical`) should the data table be rendered server side
#' (see `server` argument of [DT::renderDataTable()])
#'
#' @inherit shared_params return
#'
#' @examples
#' # general data example
#' data <- teal_data()
#' data <- within(data, {
#'   require(nestcolor)
#'   iris <- iris
#' })
#' datanames(data) <- c("iris")
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_data_table(
#'       variables_selected = list(
#'         iris = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
#'       ),
#'       dt_args = list(caption = "IRIS Table Caption")
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' # CDISC data example
#' data <- teal_data()
#' data <- within(data, {
#'   require(nestcolor)
#'   ADSL <- rADSL
#' })
#' datanames(data) <- "ADSL"
#' join_keys(data) <- default_cdisc_join_keys[datanames(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_data_table(
#'       variables_selected = list(ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX")),
#'       dt_args = list(caption = "ADSL Table Caption")
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
#'
tm_data_table <- function(label = "Data Table",
                          variables_selected = list(),
                          datasets_selected = character(0),
                          dt_args = list(),
                          dt_options = list(
                            searching = FALSE,
                            pageLength = 30,
                            lengthMenu = c(5, 15, 30, 100),
                            scrollX = TRUE
                          ),
                          server_rendering = FALSE,
                          pre_output = NULL,
                          post_output = NULL) {
  message("Initializing tm_data_table")

  # Start of assertions
  checkmate::assert_string(label)

  checkmate::assert_list(variables_selected, min.len = 0, types = "character", names = "named")
  if (length(variables_selected) > 0) {
    lapply(seq_along(variables_selected), function(i) {
      checkmate::assert_character(variables_selected[[i]], min.chars = 1, min.len = 1)
      if (!is.null(names(variables_selected[[i]]))) {
        checkmate::assert_names(names(variables_selected[[i]]))
      }
    })
  }

  checkmate::assert_character(datasets_selected, min.len = 0, min.chars = 1)
  checkmate::assert(
    checkmate::check_list(dt_args, len = 0),
    checkmate::check_subset(names(dt_args), choices = names(formals(DT::datatable)))
  )
  checkmate::assert_list(dt_options, names = "named")
  checkmate::assert_flag(server_rendering)
  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  # End of assertions

  ans <- module(
    label,
    server = srv_page_data_table,
    ui = ui_page_data_table,
    datanames = if (length(datasets_selected) == 0) "all" else datasets_selected,
    server_args = list(
      variables_selected = variables_selected,
      datasets_selected = datasets_selected,
      dt_args = dt_args,
      dt_options = dt_options,
      server_rendering = server_rendering
    ),
    ui_args = list(
      pre_output = pre_output,
      post_output = post_output
    )
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI page module
ui_page_data_table <- function(id,
                               pre_output = NULL,
                               post_output = NULL) {
  ns <- NS(id)

  tagList(
    include_css_files("custom"),
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        fluidRow(
          column(
            width = 12,
            checkboxInput(
              ns("if_distinct"),
              "Show only distinct rows:",
              value = FALSE
            )
          )
        ),
        fluidRow(
          class = "mb-8",
          column(
            width = 12,
            uiOutput(ns("dataset_table"))
          )
        )
      ),
      pre_output = pre_output,
      post_output = post_output
    )
  )
}

# Server page module
srv_page_data_table <- function(id,
                                data,
                                datasets_selected,
                                variables_selected,
                                dt_args,
                                dt_options,
                                server_rendering) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    if_filtered <- reactive(as.logical(input$if_filtered))
    if_distinct <- reactive(as.logical(input$if_distinct))

    datanames <- isolate(teal.data::datanames(data()))
    datanames <- Filter(function(name) {
      is.data.frame(isolate(data())[[name]])
    }, datanames)

    if (!identical(datasets_selected, character(0))) {
      checkmate::assert_subset(datasets_selected, datanames)
      datanames <- datasets_selected
    }

    output$dataset_table <- renderUI({
      do.call(
        tabsetPanel,
        c(
          list(id = session$ns("dataname_tab")),
          lapply(
            datanames,
            function(x) {
              dataset <- isolate(data()[[x]])
              choices <- names(dataset)
              labels <- vapply(
                dataset,
                function(x) ifelse(is.null(attr(x, "label")), "", attr(x, "label")),
                character(1)
              )
              names(choices) <- ifelse(
                is.na(labels) | labels == "",
                choices,
                paste(choices, labels, sep = ": ")
              )
              variables_selected <- if (!is.null(variables_selected[[x]])) {
                variables_selected[[x]]
              } else {
                utils::head(choices)
              }
              tabPanel(
                title = x,
                column(
                  width = 12,
                  div(
                    class = "mt-4",
                    ui_data_table(
                      id = session$ns(x),
                      choices = choices,
                      selected = variables_selected
                    )
                  )
                )
              )
            }
          )
        )
      )
    })

    lapply(
      datanames,
      function(x) {
        srv_data_table(
          id = x,
          data = data,
          dataname = x,
          if_filtered = if_filtered,
          if_distinct = if_distinct,
          dt_args = dt_args,
          dt_options = dt_options,
          server_rendering = server_rendering
        )
      }
    )
  })
}

# UI function for the data_table module
ui_data_table <- function(id,
                          choices,
                          selected) {
  ns <- NS(id)

  if (!is.null(selected)) {
    all_choices <- choices
    choices <- c(selected, setdiff(choices, selected))
    names(choices) <- names(all_choices)[match(choices, all_choices)]
  }

  tagList(
    teal.widgets::get_dt_rows(ns("data_table"), ns("dt_rows")),
    fluidRow(
      teal.widgets::optionalSelectInput(
        ns("variables"),
        "Select variables:",
        choices = choices,
        selected = selected,
        multiple = TRUE,
        width = "100%"
      )
    ),
    fluidRow(
      DT::dataTableOutput(ns("data_table"), width = "100%")
    )
  )
}

# Server function for the data_table module
srv_data_table <- function(id,
                           data,
                           dataname,
                           if_filtered,
                           if_distinct,
                           dt_args,
                           dt_options,
                           server_rendering) {
  moduleServer(id, function(input, output, session) {
    iv <- shinyvalidate::InputValidator$new()
    iv$add_rule("variables", shinyvalidate::sv_required("Please select valid variable names"))
    iv$add_rule("variables", shinyvalidate::sv_in_set(
      set = names(isolate(data())[[dataname]]), message_fmt = "Not all selected variables exist in the data"
    ))
    iv$enable()

    output$data_table <- DT::renderDataTable(server = server_rendering, {
      teal::validate_inputs(iv)

      df <- data()[[dataname]]
      variables <- input$variables

      teal::validate_has_data(df, min_nrow = 1L, msg = paste("data", dataname, "is empty"))

      dataframe_selected <- if (if_distinct()) {
        dplyr::count(df, dplyr::across(dplyr::all_of(variables)))
      } else {
        df[variables]
      }

      dt_args$options <- dt_options
      if (!is.null(input$dt_rows)) {
        dt_args$options$pageLength <- input$dt_rows
      }
      dt_args$data <- dataframe_selected

      do.call(DT::datatable, dt_args)
    })
  })
}
