#' Cross table based on rtables
#'
#' @inheritDotParams teal.devel::standard_layout -output -encoding -forms
#' @param label (\code{character}) Label of the app in the teal menu
#' @param dataname (\code{character}) Name of the dataset used in the teal app. Just a single
#'   dataset is allowed!
#' @param xvar (\code{choices_selected}) object with all available
#'   choices with preselected option for variable X
#' @param yvar (\code{choices_selected}) object with all available
#'   choices with preselected option for variable Y
#' @param pre_output (\code{shiny.tag}) html tags appended below the output
#' @param post_output (\code{shiny.tag}) html tags appended after the output
#' @export
#'
#' @author wolfs25 waddella
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ASL <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     code = "ASL <- cadsl",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_percentage_cross_table_dummy_dummy(
#'       label = "Cross Table",
#'       dataname = "ASL",
#'       xvar = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           label = "Select X Variable",
#'           choices = "STRATA1",
#'           fixed = FALSE
#'         )
#'       ),
#'       yvar = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           label = "Select Y Variable",
#'           choices = "STRATA2",
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_percentage_cross_table_dummy_dummy <- function(label = "Cross Table",
                                        dataname,
                                        xvar,
                                        yvar,
                                        pre_output = NULL,
                                        post_output = NULL) {
  stopifnot(is.character.single(label))
  stopifnot(is.character.single(dataname))
  stopifnot(is.class.list("data_extract_spec")(xvar) || is(xvar, "data_extract_spec"))
  stopifnot(is.class.list("data_extract_spec")(yvar) || is(yvar, "data_extract_spec"))
  if (is.class.list("data_extract_spec")(xvar)) {
    stop_if_not(list(all(vapply(xvar, function(x) !isTRUE(x$columns$multiple), logical(1))),
                     "xvar variable should not allow multiple selection"))
  } else if (is(xvar, "data_extract_spec")) {
    stop_if_not(list(!isTRUE(xvar$columns$multiple),
                     "xvar variable should not allow multiple selection"))
  }
  if (is.class.list("data_extract_spec")(yvar)) {
    stop_if_not(list(all(vapply(yvar, function(x) !isTRUE(x$columns$multiple), logical(1))),
                     "yvar variable should not allow multiple selection"))
  } else if (is(yvar, "data_extract_spec")) {
    stop_if_not(list(!isTRUE(yvar$columns$multiple),
                     "yvar variable should not allow multiple selection"))
  }
  args <- as.list(environment())

  module(
    label = label,
    server = srv_percentage_cross_table_dummy,
    ui = ui_percentage_cross_table_dummy,
    ui_args = args,
    server_args = list(dataname = dataname, label = label, xvar = xvar, yvar = yvar),
    filters = dataname
  )
}

ui_percentage_cross_table_dummy <- function(id, ...) {
  a <- list(...)

  ns <- NS(id)

  standard_layout(
    output = white_small_well(uiOutput(ns("table"))),
    encoding = div(
      helpText("Dataset:", tags$code(a$dataname)),
      data_extract_input(ns("xvar"), label = "Row values", a$xvar),
      tags$hr(),
      data_extract_input(ns("yvar"), label = "Column values", a$yvar)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @importFrom rtables rrowl rtablel as_html
#' @importFrom stats addmargins
srv_percentage_cross_table_dummy <- function(input, output, session, datasets, dataname, label, xvar, yvar) {
  stopifnot(all(dataname %in% datasets$datanames()))

  # Data Extraction
  xvar_data <- callModule(data_extract_module,
                          id = "xvar",
                          datasets = datasets,
                          data_extract_spec = xvar
  )
  yvar_data <- callModule(data_extract_module,
                          id = "yvar",
                          datasets = datasets,
                          data_extract_spec = yvar
  )

  init_chunks()

  table_code <- reactive({
    xvar_name <- get_dataset_prefixed_col_names(xvar_data())
    yvar_name <- get_dataset_prefixed_col_names(yvar_data())

    validate(need(xvar_name != "", "Please define a column that is not empty."))
    validate(need(yvar_name != "", "Please define a column that is not empty."))

    dataset <- merge_datasets(
      list(
        xvar_data(),
        yvar_data()
      )
    )

  })

  output$table <- renderUI({
    table_code()

  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Cross Table",
      rcode = get_rcode(
        datasets = datasets,
        title = label
      )
    )
  })
}
