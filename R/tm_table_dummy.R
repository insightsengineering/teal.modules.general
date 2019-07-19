#' Create a simple cross-table
#'
#' Create a table with the \code{\link{table}[base]} function
#'
#' @param label (\code{chracter}) menu label
#' @param dataname (\code{chracter}) name of dataset used to generate table
#' @param x (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Specification how the user can select data to get encoded in the rows of the cross table.
#'   Please just use single selections inside the \code{columns_spec}.
#' @param y (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Specification how the user can select data to get encoded in the columns of the cross table.
#'   Please just use single selections inside the \code{columns_spec}.
#' @param useNA (\code{character}) optional pre-selected option indicating how to utilize NA in
#'   table display. One of \code{'ifany'}, \code{'always'}, \code{'no'}. If
#'   missing then \code{'ifany'} will be used. If vector then only the first
#'   one will be used.
#' @param pre_output (\code{shiny.tag}) html tags appended below the output
#' @param post_output (\code{shiny.tag}) html tags appended after the output
#'
#' @noRd
#'
#' @examples
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' keys(ASL) <- c("USUBJID", "STUDYID")
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     code = 'ASL <- cadsl; keys(ASL) <- c("USUBJID", "STUDYID")',
#'     check = FALSE
#'   ),
#'   root_modules(
#'     tm_table_dummy(
#'       "Table Choices",
#'       dataname =  "ASL",
#'       x = data_extract_spec(
#'         "ASL",
#'         columns = columns_spec(
#'           choices = base::setdiff(names(ASL), keys(ASL)),
#'           selected = names(ASL)[5],
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         "ASL",
#'         columns = columns_spec(
#'           choices = base::setdiff(names(ASL), keys(ASL)),
#'           selected = names(ASL)[6],
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_table_dummy <- function(label,
                     dataname,
                     x,
                     y,
                     useNA = c("ifany", "no", "always"), # nolint
                     pre_output = NULL,
                     post_output = NULL) {
  stopifnot(is.character.single(label))
  stopifnot(is.character.single(dataname))
  stopifnot(is.class.list("data_extract_spec")(x) || is(x, "data_extract_spec"))
  if (is.class.list("data_extract_spec")(x)) {
    stop_if_not(list(all(vapply(x, function(x) !(x$columns$multiple), logical(1))),
                     "'x' should not allow multiple selection"))
  } else if (is(x, "data_extract_spec")) {
    stop_if_not(list(!(x$columns$multiple),
                     "'x' should not allow multiple selection"))
  }
  stopifnot(is.class.list("data_extract_spec")(y) || is(y, "data_extract_spec"))
  if (is.class.list("data_extract_spec")(y)) {
    stop_if_not(list(all(vapply(y, function(x) !(x$columns$multiple), logical(1))),
                     "'y' should not allow multiple selection"))
  } else if (is(y, "data_extract_spec")) {
    stop_if_not(list(!(y$columns$multiple),
                     "'y' should not allow multiple selection"))
  }
  stopifnot(is.character.vector(useNA))
  stopifnot(all(useNA %in% c("ifany", "no", "always")))

  args <- as.list(environment())

  args$useNA <- match.arg(useNA) # nolint

  module(
    label = label,
    server = srv_table,
    ui = ui_table,
    server_args = list(dataname = dataname, x = x, y = y),
    ui_args = args,
    filters = dataname
  )
}


ui_table_dummy <- function(id,
                     label,
                     dataname,
                     x,
                     y,
                     useNA, # nolint
                     pre_output,
                     post_output) {
  ns <- NS(id)

  standard_layout(
    output = white_small_well(
      tableOutput(ns("table"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(dataname)),
      data_extract_input(ns("x"), label = "Row values", x),
      tags$hr(),
      data_extract_input(ns("y"), label = "Column values", y),
      tags$hr(),
      radioButtons(ns("useNA"),
                   label = "Display Missing Values",
                   choices = c("no", "ifany", "always"),
                   selected = useNA
      ),
      checkboxInput(ns("margins"), "Add margins", value = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @importFrom magrittr %>%
#' @importFrom methods substituteDirect
#' @importFrom stats addmargins
srv_table_dummy <- function(input, output, session, datasets, dataname, x, y) {
  stopifnot(all(dataname %in% datasets$datanames()))

  init_chunks()

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

  chunk_reactive <- reactive({

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
    validate_has_data(dataset, 10)
    useNA <- input$useNA # nolint
    use_margin <- input$margins

    chunks_reset()

    expression_to_use <- if (use_margin) {
      expr(stats::addmargins(
        table(dataset[[x_name]], dataset[[y_name]], useNA = useNA)
      )) %>%
        substituteDirect(list(useNA = useNA, x_name = x_name, y_name = y_name))
    } else {
      expr(table(dataset[[x_name]], dataset[[y_name]], useNA = useNA)) %>%
        substituteDirect(list(useNA = useNA, x_name = x_name, y_name = y_name))
    }

    chunks_push(expression = expression_to_use)
  })

  output$table <- renderTable({
    chunk_reactive()

    tbl <- chunks_eval()
    chunks_validate_is_ok()

    as.data.frame.matrix(tbl, row.names = rownames(tbl))
  }, rownames = TRUE, bordered = TRUE, html.table.attributes = 'style="background-color:white;"')

  observeEvent(input$show_rcode, {
    x_name <- get_dataset_prefixed_col_names(x_data())
    y_name <- get_dataset_prefixed_col_names(y_data())
    title <- paste("Cross-Table of", x_name, "vs.", y_name)

    show_rcode_modal(
      title = "R Code for the Current Table",
      rcode = get_rcode(
        datasets = datasets,
        merged_dataname = "dataset",
        merged_datasets = list(
          x_data(),
          y_data()
        ),
        title = title
      )
    )
  })
}
