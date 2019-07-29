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
#'     tm_t_percentage_cross_table(
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
#'
#' # datasets: multiple long datasets
#' library(random.cdisc.data)
#'
#' ASL <- cadsl
#' ADRS <- cadrs
#' ADTTE <- cadtte
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ADRS = ADRS,
#'     ADTTE = ADTTE,
#'     code = "ASL <- cadsl; ADRS <- cadrs; ADTTE <- cadtte",
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_t_percentage_cross_table(
#'       label = "Cross Table",
#'       dataname = c("ASL", "ADRS", "ADTTE"),
#'       x = data_extract_spec(
#'         dataname = "ADRS",
#'         columns = columns_spec(
#'           choices = names(ADRS),
#'           selected = "AVALC",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         ),
#'         filter = filter_spec(
#'           vars = c("PARAMCD", "AVISIT"),
#'           choices = apply(expand.grid(unique(ADRS$PARAMCD), unique(ADRS$AVISIT)),
#'                           1, paste, collapse = " - "),
#'           selected = "OVRINV - Screening",
#'           multiple = TRUE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADTTE",
#'         columns = columns_spec(
#'           choices = names(ADTTE),
#'           selected = "CNSR",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         ),
#'         filter = filter_spec(
#'           vars = c("PARAMCD"),
#'           choices = unique(ADTTE$PARAMCD),
#'           selected = "OS",
#'           multiple = TRUE
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: different subsets of long dataset
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ADLB <- cadlb
#'
#' keys(ASL) <- c("STUDYID", "USUBJID")
#' keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ADLB = ADLB,
#'     code = 'ASL <- cadsl
#'            ADLB <- cadlb
#'            keys(ASL) <- c("STUDYID", "USUBJID")
#'            keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_percentage_cross_table(
#'       label = "Cross Table",
#'       dataname = "ADLB",
#'       x = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Choose endpoint"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Choose visit"
#'           )
#'         ),
#'         columns = columns_spec(
#'           choices = c("STRATA1", "STRATA2"),
#'           selected = "STRATA1",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Variable"
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Choose endpoint"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Choose visit"
#'           )
#'         ),
#'         columns = columns_spec(
#'           choices = c("STRATA1", "STRATA2"),
#'           selected = "STRATA1",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Variable"
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_t_percentage_cross_table <- function(label = "Cross Table",
                                        dataname,
                                        x,
                                        y,
                                        pre_output = NULL,
                                        post_output = NULL) {
  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
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
    server = function(input, output, session, datasets, ...) return(NULL),
    ui = ui_percentage_cross_table,
    ui_args = args,
    server_args = list(dataname = dataname, label = label, x = x, y = y),
    filters = dataname
  )
}

ui_percentage_cross_table <- function(id, ...) {
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
srv_percentage_cross_table <- function(input, output, session, datasets, dataname, label) {
  stopifnot(all(dataname %in% datasets$datanames()))

  init_chunks()

  table_code <- reactive({
    anl_f <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)

    x <- input$x
    y <- input$y

    validate(need(anl_f, "data missing"))
    validate_has_data(anl_f, 10)

    validate(need(x, "selected x does not exist"))
    validate(need(y, "selected y does not exist"))

    data_name <- paste0(dataname, "_FILTERED")
    assign(data_name, anl_f)

    # Set chunks
    chunks_reset()

    chunks_push(expression = bquote(data_table <-
                                      stats::addmargins(table(.(as.name(data_name))[[.(x)]], .(as.name(data_name))[[.(y)]]))))

    chunks_push(expression = quote(perc_table <- data_table / data_table[nrow(data_table), ncol(data_table)]))

    chunks_push(
      expression =
        quote(add_row <- function(i, x, p) {
          rtables::rrowl(rownames(x)[i], Map(function(xii, pii) c(xii, pii), x[i, ], p[i, ]))
        })
    )
    chunks_push(expression = quote(rows <- lapply(1:nrow(data_table), add_row, x = data_table, p = perc_table)))
    chunks_push(expression = quote(rtables::rtablel(header = colnames(data_table), rows, format = "xx (xx.xx%)")))
  })

  output$table <- renderUI({
    table_code()
    t <- rtables::as_html(chunks_eval())
    chunks_validate_is_ok()
    t
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
