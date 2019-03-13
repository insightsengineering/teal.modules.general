#' Create a simple cross-table
#'
#' Create a table with the \code{\link{table}[base]} function
#'
#' @param label menu label
#' @param dataname name of dataset used to generate table
#' @param xvar (\code{choices_selected}) variable name of x variable. vector with variable names of possible x
#' variables. If
#'   missing or identincal to \code{xvar} then the table will be fixed to the
#'   \code{xvar}.
#' @param yvar  (\code{choices_selected}) variable name of y variable. vector with variable names of possible y
#' variables. If
#'   missing or identincal to \code{xvar} then the table will be fixed to the
#'   \code{yvar}.
#' @param useNA optional pre-selected option indicating how to utilize NA in
#'   table display. One of \code{'ifany'}, \code{'always'}, \code{'no'}. If
#'   missing then \code{'ifany'} will be used. If vector then only the first
#'   one will be used.
#' @param pre_output html tags appended below the output
#' @param post_output html tags appended after the output
#'
#' @export
#'
#' @importFrom xtable xtable
#' @importFrom xtable print.xtable
#'
#' @examples
#'
#' \dontrun{
#' library(random.cdisc.data)
#'
#' ASL <- radsl(seed = 1)
#'
#' attr(ASL, "source") <- "random.cdisc.data::radsl(seed = 1)"
#'
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   root_modules(
#'     tm_data_table(),
#'     tm_variable_browser(),
#'     tm_table("Table Choices", "ASL",
#'       xvar = choices_selected(c("SEX", "RACE", "STUDYID"), "SEX"),
#'       yvar = choices_selected(c("RACE", "SAFFL"), "RACE")
#'     ),
#'     tm_table("Table No Choices", "ASL",
#'       xvar = choices_selected("SEX"),
#'       yvar = choices_selected("RACE"),
#'       pre_output = helpText("Titles"),
#'       post_output = helpText("Footnotes")
#'     )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#' }
tm_table <- function(label,
                     dataname,
                     xvar,
                     yvar,
                     useNA = c("ifany", "no", "always"), # nolint
                     pre_output = NULL,
                     post_output = NULL) {

  args <- as.list(environment())

  args$useNA <- match.arg(useNA) # nolint


  teal::module(
    label = label,
    server = srv_table,
    ui = ui_table,
    server_args = list(dataname = dataname),
    ui_args = args,
    filters = dataname
  )
}


#' @import teal
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
    output = tableOutput(ns("table")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(dataname)),
      optionalSelectInput(ns("xvar"), "x variable (row)", xvar$choices, xvar$selected, multiple = FALSE),
      optionalSelectInput(ns("yvar"), "y variable (column)", yvar$choices, yvar$selected, multiple = FALSE),
      radioButtons(ns("useNA"),
        label = "Display Missing Values",
        choices = c("no", "ifany", "always"), selected = useNA
      ),
      checkboxInput(ns("margins"), "Add margins", value = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
}


#' @import stats
#' @importFrom teal.devel get_filter_txt
srv_table <- function(input, output, session, datasets, dataname) {
  use_chunks(session)

  output$table <- renderTable({
    dataset <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    xvar <- input$xvar
    yvar <- input$yvar
    useNA <- # nolint
      input$useNA # nolint
    use_margin <- input$margins

    validate(need(!is.null(dataset) && is.data.frame(dataset), "no data left"))
    validate(need(nrow(dataset) > 0, "no observations left"))
    validate(need(xvar, "no valid x variable selected"))
    validate(need(yvar, "no valid y variable selected"))
    validate(need(
      xvar %in% names(dataset),
      paste("variable", xvar, " is not available in data", dataname)
    ))
    validate(need(
      yvar %in% names(dataset),
      paste("variable", yvar, " is not available in data", dataname)
    ))

    if (use_margin) {
      expression_to_use <- expr(stats::addmargins(table(dataset[[xvar]], dataset[[yvar]], useNA = useNA)))
    } else {
      expression_to_use <- expr(table(dataset[[xvar]], dataset[[yvar]], useNA = useNA))
    }

    set_chunk(
      expression = expression_to_use,
      vars = list(dataset = dataset, dataname = dataname, xvar = xvar, yvar = yvar, useNA = useNA)
    )

    tbl <- eval_chunk()

    as.data.frame.matrix(tbl, row.names = rownames(tbl))
  }, rownames = TRUE, bordered = TRUE, html.table.attributes = 'style="background-color:white;"')

  observeEvent(input$show_rcode, {
    teal.devel::show_rcode_modal(
      title = "R Code for the Current Table",
      rcode = get_rcode(
        datasets = datasets,
        dataname = dataname,
        title = paste("Cross-Table of", input$xvar, "vs.", input$yvar),
        description = "",
        libraries = c(),
        git_pkgs = list(roche = c("NEST/teal", "NEST/teal.devel", "NEST/teal.modules.general"))
      )
    )
  })
}
