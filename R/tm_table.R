#' Create a simple cross-table
#'
#' Create a table with the \code{\link{table}[base]} function
#'
#' @param label (\code{chracter}) menu label
#' @param dataname (\code{chracter}) name of dataset used to generate table
#' @param xvar (\code{list} of \code{data_extract_spec}) Specification how the
#'  user can select data to get encoded in the rows of the cross table. Please just use
#'  single selections inside the \code{columns_spec}.
#' @param yvar (\code{list} of \code{data_extract_spec}) Specification how the
#'  user can select data to get encoded in the columns of the cross table. Please just use
#'  single selections inside the \code{columns_spec}.
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
#' asl_extract_xvar <- data_extract_spec(
#'   "ASL",
#'   columns = columns_spec(
#'     choices = base::setdiff(names(ASL), keys(ASL)),
#'     selected = names(ASL)[5],
#'     multiple = FALSE,
#'     fixed = FALSE
#'   )
#' )
#'
#' asl_extract_yvar <- data_extract_spec(
#'   "ASL",
#'   columns = columns_spec(
#'     choices = base::setdiff(names(ASL), keys(ASL)),
#'     selected = names(ASL)[6],
#'     multiple = FALSE,
#'     fixed = FALSE
#'   )
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     code = 'ASL <- cadsl; keys(ASL) <- c("USUBJID", "STUDYID")',
#'     check = FALSE
#'   ),
#'   root_modules(
#'     tm_table(
#'       "Table Choices",
#'       dataname =  "ASL",
#'       xvar = list(asl_extract_xvar),
#'       yvar = list(asl_extract_yvar)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_table <- function(label,
                     dataname,
                     xvar,
                     yvar,
                     useNA = c("ifany", "no", "always"), # nolint
                     pre_output = NULL,
                     post_output = NULL) {
  stopifnot(
    is.character.single(label),
    is.character.single(dataname),
    is.list(xvar),
    is.list(yvar),
    is.character.vector(useNA),
    all(useNA %in% c("ifany", "no", "always"))
  )

  lapply(xvar, function(ds_extract){
    stopifnot(!ds_extract$columns$multiple)
  })
  lapply(yvar, function(ds_extract){
    stopifnot(!ds_extract$columns$multiple)
  })

  args <- as.list(environment())

  args$useNA <- match.arg(useNA) # nolint

  module(
    label = label,
    server = srv_table,
    ui = ui_table,
    server_args = list(dataname = dataname, xvar = xvar, yvar = yvar),
    ui_args = args,
    filters = dataname
  )
}


ui_table <- function(id,
                     label,
                     dataname,
                     xvar,
                     yvar,
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
      data_extract_input(ns("xvar"), label = "Row values", xvar),
      tags$hr(),
      data_extract_input(ns("yvar"), label = "Column values", yvar),
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
srv_table <- function(input, output, session, datasets, dataname, xvar, yvar) {
  stopifnot(all(dataname %in% datasets$datanames()))

  use_chunks()

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

  chunk_reactive <- reactive({

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
    validate_has_data(dataset, 10)
    useNA <- input$useNA # nolint
    use_margin <- input$margins

    reset_chunks()

    expression_to_use <- if (use_margin) {
      expr(stats::addmargins(
        table(dataset[[xvar_name]], dataset[[yvar_name]], useNA = useNA)
      )) %>%
        substituteDirect(list(useNA = useNA, xvar_name = xvar_name, yvar_name = yvar_name))
    } else {
      expr(table(dataset[[xvar_name]], dataset[[yvar_name]], useNA = useNA)) %>%
        substituteDirect(list(useNA = useNA, xvar_name = xvar_name, yvar_name = yvar_name))
    }

    set_chunk(expression = expression_to_use)
  })

  output$table <- renderTable({
    chunk_reactive()
    tbl <- eval_chunks()

    as.data.frame.matrix(tbl, row.names = rownames(tbl))
  }, rownames = TRUE, bordered = TRUE, html.table.attributes = 'style="background-color:white;"')

  observeEvent(input$show_rcode, {
    xvar_name <- get_dataset_prefixed_col_names(xvar_data())
    yvar_name <- get_dataset_prefixed_col_names(yvar_data())
    title <- paste("Cross-Table of", xvar_name, "vs.", yvar_name)

    show_rcode_modal(
      title = "R Code for the Current Table",
      rcode = get_rcode(
        datasets = datasets,
        merged_dataname = "dataset",
        merged_datasets = list(
          xvar_data(),
          yvar_data()
        ),
        title = title
      )
    )
  })
}
