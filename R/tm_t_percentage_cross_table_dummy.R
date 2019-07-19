#' Cross table based on rtables
#'
#' @inheritDotParams teal.devel::standard_layout -output -encoding -forms
#' @param label (\code{character}) Label of the app in the teal menu
#' @param dataname (\code{character}) Name of the dataset used in the teal app. Just a single
#'   dataset is allowed!
#' @param x (\code{choices_selected}) object with all available
#'   choices with preselected option for variable X
#' @param y (\code{choices_selected}) object with all available
#'   choices with preselected option for variable Y
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#'
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
#'     teal.modules.general:::tm_t_percentage_cross_table_dummy(
#'       label = "Cross Table",
#'       dataname = "ASL",
#'       x = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           label = "Select X Variable",
#'           choices = c("STRATA1", "STRATA2"),
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           label = "Select Y Variable",
#'           choices = c("STRATA2", "STRATA1"),
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_percentage_cross_table_dummy <- function(label = "Cross Table",
                                        dataname,
                                        x,
                                        y,
                                        pre_output = NULL,
                                        post_output = NULL) {
  stopifnot(is.character.single(label))
  stopifnot(is.character.single(dataname))
  stopifnot(is.class.list("data_extract_spec")(x) || is(x, "data_extract_spec"))
  stopifnot(is.class.list("data_extract_spec")(y) || is(y, "data_extract_spec"))
  if (is.class.list("data_extract_spec")(x)) {
    stop_if_not(list(all(vapply(x, function(x) !isTRUE(x$columns$multiple), logical(1))),
                     "x variable should not allow multiple selection"))
  } else if (is(x, "data_extract_spec")) {
    stop_if_not(list(!isTRUE(x$columns$multiple),
                     "x variable should not allow multiple selection"))
  }
  if (is.class.list("data_extract_spec")(y)) {
    stop_if_not(list(all(vapply(y, function(x) !isTRUE(x$columns$multiple), logical(1))),
                     "y variable should not allow multiple selection"))
  } else if (is(y, "data_extract_spec")) {
    stop_if_not(list(!isTRUE(y$columns$multiple),
                     "y variable should not allow multiple selection"))
  }
  args <- as.list(environment())

  module(
    label = label,
    server = srv_percentage_cross_table_dummy,
    ui = ui_percentage_cross_table_dummy,
    ui_args = args,
    server_args = list(dataname = dataname, label = label, x = x, y = y),
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
      data_extract_input(ns("x"), label = "Row values", a$x),
      tags$hr(),
      data_extract_input(ns("y"), label = "Column values", a$y)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @importFrom rtables rrowl rtablel as_html
#' @importFrom stats addmargins
srv_percentage_cross_table_dummy <- function(input, output, session, datasets, dataname, label, x, y) {
  stopifnot(all(dataname %in% datasets$datanames()))

  # Data Extraction
  x_data <- callModule(data_extract_module,
                          id = "x",
                          datasets = datasets,
                          data_extract_spec = x
  )
  y_data <- callModule(data_extract_module,
                          id = "y",
                          datasets = datasets,
                          data_extract_spec = y
  )

  init_chunks()

  table_code <- reactive({
    x_name <- get_dataset_prefixed_col_names(x_data())
    y_name <- get_dataset_prefixed_col_names(y_data())

    validate(need(x_name != "", "Please define a column that is not empty."))
    validate(need(y_name != "", "Please define a column that is not empty."))
    dataset <- merge_datasets(
      list(
        x_data(),
        y_data()
      )
    )

  })

  output$table <- renderUI({
    table_code()
    NULL
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
