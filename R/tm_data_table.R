#' Data Table Viewer Teal Module
#'
#' A data table viewer shows the data using a paginated table.
#'
#' @param label (\code{character})
#' @param variables_selected (\code{list}) a named list that says which variables should be
#'   initially shown for particular dataset. Names in list should correspond with names provided in list `data()`.
#'   If not specified for any dataset - first six variables from dataset will be shown.
#'
#' @export
#'
#' @examples
#'
#' library(random.cdisc.data)
#'
#' asl <- radsl()
#'
#' x <- teal::init(
#'   data = list(ASL = asl),
#'   modules = root_modules(
#'     tm_data_table()
#'   )
#' )
#' \dontrun{
#' shinyApp(x$ui, x$server)
#' }
#'
#' # two-datasets example
#' library(random.cdisc.data)
#' asl <- radsl()
#' adte <- radaette(asl)
#'
#' x <- teal::init(
#'   data = list(ASL = asl, ADTE = adte),
#'   modules = root_modules(
#'     tm_data_table(
#'       variables_selected = list(ASL  = c("SEX", "AGE","RACE"),
#'                                 ADTE = c("STUDYID","AGE")))
#'   )
#' )
#' \dontrun{
#' shinyApp(x$ui, x$server)
#' }
tm_data_table <- function(label = "Data table", variables_selected = NULL) {
  teal::module(
    label,
    server = srv_page_data_table,
    ui = ui_page_data_table,
    filters = "all",
    server_args = list(cache_selected = if (is.null(variables_selected)) list() else variables_selected),
    ui_args = list(datasets = "teal_datasets")
  )
}


#' ui function of Data Table Viewer
#'
#' @param id (\code{character}) UI id
#' @param datasets (\code{FilteredData}) object
#'
#' @import stats
ui_page_data_table <- function(id, datasets) {
  ns <- NS(id)

  datanames <- datasets$datanames()
  sel_varnames <- names(datasets$get_data(datanames[1]))

  # Create a tab list for each dataset
  datatset_tabs <- lapply(datanames, function(x) tabPanel(title = x, value = x))

  tagList(
    fluidRow(
      div(
        class = "col-md-3",
        radioButtons(ns("data_raw_or_filtered"), NULL,
          choices = c("unfiltered data" = "raw", "filtered data" = "filtered"),
          selected = "filtered", inline = TRUE
        ),
        checkboxInput(ns("distinct"), "show only distinct rows", value = FALSE)
      ),
      div(class = "col-md-9", selectInput(ns("variables"), "select variables",
        choices = sel_varnames, selected = head(sel_varnames),
        multiple = TRUE, width = "100%"
      ))
    ),
    tags$hr(),
    fluidRow(
      div(style = "margin-left:15px", do.call(tabsetPanel, append(datatset_tabs, list(id = ns("dataset")))))
    ),
    fluidRow(
      div(style = "height:10px;"),
      div(class = "col-md-12", DT::dataTableOutput(ns("tbl"), width = "100%"))
    ),
    div(style = "height:30px;")
  )
}


## data table
#' @import utils
#' @importFrom dplyr count_
srv_page_data_table <- function(input,
                                output,
                                session,
                                datasets,
                                cache_selected = list()) {

  # This function uses session$userData to store the choices made by the user for select variables.
  #


  # select first 6 variables for each dataset if not otherwise specified
  for (name in setdiff(datasets$datanames(), names(cache_selected))) {
    cache_selected[[name]] <- datasets$get_data(name, filtered = FALSE, reactive = FALSE) %>%
      names() %>%
      head(6)
  }

  cache_selected_reactive <-  reactiveVal(cache_selected)

  observeEvent(input$dataset, {

    dataname <- input$dataset

    validate(
      need(dataname, "need valid dataset name"),
      need(dataname %in% datasets$datanames(), paste("data", dataname, "was not specified"))
    )

    choices <- datasets$get_data(dataname, filtered = FALSE, reactive = FALSE) %>% names()

    variables_cached_all <- cache_selected_reactive()
    variables_cached <- variables_cached_all[[dataname]]

    selected <- if (is.null(variables_cached)) {
      head(choices, 6)
    } else {
      intersect(variables_cached, choices)
    }

    .log("data table, update variables for", dataname)

    updateSelectInput(session, "variables",
      choices = c(selected, setdiff(choices, selected)),
      selected = selected
    )
    variables_cached_all[[dataname]] <- selected
    cache_selected_reactive(variables_cached_all)
  })

  observeEvent(input$variables, {
    variables_cached <- cache_selected_reactive()
    variables_cached[[input$dataset]] <- input$variables
    cache_selected_reactive(variables_cached)
  })

  output$tbl <- DT::renderDataTable({
    dataname <- input$dataset

    validate(need(dataname, "need valid dataname"))

    variables <- input$variables

    validate(need(variables, "need valid variable names"))

    .log("data table update", dataname)

    df <- datasets$get_data(
      dataname,
      filtered = input$data_raw_or_filtered == "filtered",
      reactive = TRUE
    )

    validate(need(df, paste("data", dataname, "is empty")))

    validate(need(all(variables %in% names(df)), "not all selected variables exist"))

    dataframe_selected <- if (input$distinct) {
      dplyr::count_(df, variables)
    } else {
      df[variables]
    }

    # Return a DT data.frame
    DT::datatable(
      dataframe_selected,
      options = list(
        searching = FALSE,
        pageLength = 30,
        lengthMenu = c(5, 15, 30, 100),
        scrollX = TRUE
      )
    )
  })
}
