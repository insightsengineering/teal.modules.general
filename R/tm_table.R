#' Create a simple cross-table
#'
#' Create a table with the \code{\link{table}[base]} function
#'
#' @param label (\code{chracter}) menu label
#' @param dataname (\code{chracter}) name of dataset used to generate table
#' @param x (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Specification how the user can select data to get encoded in the rows of the cross table.
#'   Please just use single selections inside the \code{select_spec}.
#' @param y (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Specification how the user can select data to get encoded in the columns of the cross table.
#'   Please just use single selections inside the \code{select_spec}.
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
#'
#' ADSL <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- cadsl",
#'     check = TRUE
#'   ),
#'   root_modules(
#'     tm_table(
#'       "Table Choices",
#'       dataname =  "ADSL",
#'       x = data_extract_spec(
#'         "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = names(ADSL),
#'           selected = names(ADSL)[5],
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = names(ADSL),
#'           selected = names(ADSL)[6],
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
#'
#' # datasets: different subsets of long dataset
#'
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADLB <- cadlb
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = "ADSL <- cadsl; ADLB <- cadlb",
#'     check = TRUE
#'   ),
#'   root_modules(
#'     tm_table(
#'       "Table Choices",
#'       dataname =  "ADLB",
#'       x = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = filter_spec(
#'           vars = "PARAMCD",
#'           choices = levels(ADLB$PARAMCD),
#'           selected = levels(ADLB$PARAMCD)[1],
#'           multiple = FALSE
#'         ),
#'         select = select_spec(
#'           choices = names(ADLB),
#'           selected = "AVISIT",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = filter_spec(
#'           vars = "PARAMCD",
#'           choices = levels(ADLB$PARAMCD),
#'           selected = levels(ADLB$PARAMCD)[1],
#'           multiple = FALSE
#'         ),
#'         select = select_spec(
#'           choices = names(ADLB),
#'           selected = "LOQFL",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_table <- function(label,
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
    stop_if_not(list(all(vapply(x, function(x) !(x$select$multiple), logical(1))),
                     "'x' should not allow multiple selection"))
  } else if (is(x, "data_extract_spec")) {
    stop_if_not(list(!(x$select$multiple),
                     "'x' should not allow multiple selection"))
  }
  stopifnot(is.class.list("data_extract_spec")(y) || is(y, "data_extract_spec"))
  if (is.class.list("data_extract_spec")(y)) {
    stop_if_not(list(all(vapply(y, function(x) !(x$select$multiple), logical(1))),
                     "'y' should not allow multiple selection"))
  } else if (is(y, "data_extract_spec")) {
    stop_if_not(list(!(y$select$multiple),
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


ui_table <- function(id,
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
                   label = "Display missing values:",
                   choices = c("no", "ifany", "always"),
                   selected = useNA
      ),
      checkboxInput(ns("margins"), "Add margins", value = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @importFrom magrittr %>%
#' @importFrom methods substituteDirect
#' @importFrom stats addmargins
srv_table <- function(input, output, session, datasets, dataname, x, y) {
  stopifnot(all(dataname %in% datasets$datanames()))

  init_chunks()

  dataname <- get_extract_datanames(list(x, y))

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(x, y),
    input_id = c("x", "y")
  )

  chunk_reactive <- reactive({
    ANL <- merged_data()$data()

    chunks_reset()

    x_name <- unname(merged_data()$columns_source$x)
    y_name <- unname(merged_data()$columns_source$y)

    validate(need(x_name != "", "Please define a column that is not empty."))
    validate(need(y_name != "", "Please define a column that is not empty."))

    validate_has_data(ANL, 10)

    useNA <- input$useNA # nolint
    use_margin <- input$margins

    expression_to_use <- if (use_margin) {
      expr(stats::addmargins(
        table(ANL[[x_name]], ANL[[y_name]], useNA = useNA)
      )) %>%
        substituteDirect(list(useNA = useNA, x_name = x_name, y_name = y_name))
    } else {
      expr(table(ANL[[x_name]], ANL[[y_name]], useNA = useNA)) %>%
        substituteDirect(list(useNA = useNA, x_name = x_name, y_name = y_name))
    }

    chunks_push(expression = expression_to_use)
  })

  output$table <- renderTable({
    chunk_reactive()

    tbl <- chunks_safe_eval()

    as.data.frame.matrix(tbl, row.names = rownames(tbl))
  }, rownames = TRUE, bordered = TRUE, html.table.attributes = 'style="background-color:white;"')

  observeEvent(input$show_rcode, {
    x_name <- merged_data()$columns_source$x
    y_name <- merged_data()$columns_source$y
    title <- paste("Cross-Table of", x_name, "vs.", y_name)

    show_rcode_modal(
      title = "R Code for the Current Table",
      rcode = get_rcode(
        datasets = datasets,
        merge_expression = merged_data()$expr,
        title = title
      )
    )
  })
}
