#' Create a simple cross-table
#'
#' Create a table with the \code{\link{table}[base]} function
#'
#' @param label menu label
#' @param dataname name of dataset used to generate table
#' @param xvar (\code{choices_selected}) variable name of x variable
#' @param yvar (\code{choices_selected}) variable name of y variable
#' @param pre_output html tags appended below the output
#' @param post_output html tags appended after the output
#'
#' @export
#'
#' @importFrom xtable xtable
#' @importFrom xtable print.xtable
#' @import teal.devel
#'
#' @examples
#'
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
#'     tm_table("Table Choices",
#'       "ASL",
#'       xvar = choices_selected(c("SEX", "RACE", "STUDYID"), "SEX"),
#'       yvar = choices_selected(c("RACE", "SAFFL"), "RACE")
#'     ),
#'     tm_table("Table No Choices",
#'       "ASL",
#'       xvar = choices_selected("SEX"),
#'       yvar = choices_selected("RACE"),
#'       pre_output = helpText("Titles"),
#'       post_output = helpText("Footnotes")
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(x$ui, x$server)
#' }
tm_table_with_chunks <- function(label,
                                 dataname,
                                 xvar,
                                 yvar,
                                 pre_output = NULL,
                                 post_output = NULL) {
  args <- as.list(environment())

  teal::module(
    label = label,
    server = srv_table_with_chunks,
    ui = ui_table_with_chunks,
    server_args = list(dataname = dataname),
    ui_args = args,
    filters = dataname
  )
}


#' @import teal
ui_table_with_chunks <- function(id,
                                 label,
                                 dataname,
                                 xvar,
                                 yvar,
                                 pre_output,
                                 post_output) {
  ns <- NS(id)

  standard_layout(
    output =
      teal.devel::white_small_well(
        tags$div(
          verbatimTextOutput(ns("text")),
          tableOutput(ns("table"))
        )
      ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      optionalSelectInput(ns("xvar"), "x variable (row)", xvar$choices, xvar$selected, multiple = FALSE),
      optionalSelectInput(ns("yvar"), "y variable (column)", yvar$choices, yvar$selected, multiple = FALSE),
      checkboxInput(ns("margins"), "Add margins", value = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
}


#' @import stats
#' @importFrom teal.devel get_filter_txt
#' @importFrom rlang expr
srv_table_with_chunks <- function(input,
                                  output,
                                  session,
                                  datasets,
                                  dataname) {
  use_chunks(session)

  output$table <- renderTable({
    dataset <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    xvar <- input$xvar
    yvar <- input$yvar
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
      expression_to_use <- rlang::expr(stats::addmargins(table(dataset[[xvar]], dataset[[yvar]])))
    } else {
      expression_to_use <- rlang::expr(table(dataset[[xvar]], dataset[[yvar]]))
    }
    set_chunk(
      expression = expression_to_use,
      vars = list(dataset = dataset, dataname = dataname, xvar = xvar, yvar = yvar)
    )
    tbl <- eval_chunk()
    as.data.frame.matrix(tbl, row.names = rownames(tbl))
  }, rownames = TRUE, bordered = TRUE, html.table.attributes = 'style="background-color:white;"')

  output$text <- renderText({
    set_chunk("textid",
      expression = rlang::expr(as.character(dim(dataset))),
      vars = list(dataset = datasets$get_data(dataname, reactive = TRUE, filtered = TRUE), dataname = dataname)
    )
    eval_chunk("textid")
  })

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
