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
#' # Percentage cross table of variables from ADSL dataset
#' # datasets: single wide
#'
#' ADSL <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- cadsl",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_percentage_cross_table(
#'       label = "Cross Table",
#'       dataname = "ADSL",
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("COUNTRY", "STUDYID"),
#'           selected = "COUNTRY",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("SEX", "RACE"),
#'           selected = "SEX",
#'           multiple = FALSE,
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
#' # datasets: different wide
#' # Percentage cross table with AGE groups over RACE
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- cadsl
#' ADSL <- mutate_at(ADSL,
#'   .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'   .funs = list(~as.factor(.))
#' ) %>% select(
#'   "ARM", "ACTARM", "ACTARMCD",
#'   "SEX", "STRATA1", "AGE", "USUBJID", "STUDYID", "STRATA2"
#' )
#'
#' ADSL_2 <- mutate_at(cadsl,
#'   .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'   .funs = list(~as.factor(.))
#' ) %>% select("ACTARM", "AGE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     dataset("ADSL_2", ADSL_2),
#'     code = 'ADSL <- cadsl
#' ADSL_2 <- mutate_at(cadsl,
#' .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#' .funs = list(~as.factor(.))) %>%
#' select("ACTARM", "AGE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")',
#'     check = FALSE #TODO
#'   ),
#'   modules = root_modules(
#'     tm_t_percentage_cross_table("Cross Table",
#'      dataname = c("ADSL", "ADSL_2"),
#'      x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("AGE", "SEX", "STRATA1", "RACE"),
#'           selected = c("AGE"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )),
#'       y = data_extract_spec(
#'         dataname = "ADSL_2",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("COUNTRY", "AGE", "RACE"),
#'           selected = "RACE",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )),
#'      )
#'    )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'

#' # datasets: multiple long datasets
#' #' Percentage cross table of parameters from ADTTE and ADRS datasets.
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#' ADTTE <- cadtte
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     cdisc_dataset("ADTTE", ADTTE),
#'     code = "ADSL <- cadsl; ADRS <- cadrs; ADTTE <- cadtte",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_percentage_cross_table(
#'       label = "Cross Table",
#'       dataname = c("ADSL", "ADRS", "ADTTE"),
#'       x = data_extract_spec(
#'         dataname = "ADRS",
#'         filter = filter_spec(
#'           label = "Select endpoints:",
#'           vars = c("PARAMCD", "AVISIT"),
#'           choices = apply(expand.grid(levels(ADRS$PARAMCD), levels(ADRS$AVISIT)),
#'                           1, paste, collapse = " - "),
#'           selected = "OVRINV - Screening",
#'           multiple = TRUE
#'         ),
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = names(ADRS),
#'           selected = "AVALC",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADTTE",
#'         filter = filter_spec(
#'           label = "Select parameters:",
#'           vars = "PARAMCD",
#'           choices = unique(ADTTE$PARAMCD),
#'           selected = "OS",
#'           multiple = TRUE
#'         ),
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = names(ADTTE),
#'           selected = "CNSR",
#'           multiple = FALSE,
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
#' # datasets: wide, long
#' # Percentage cross table with AGE groups per STUDYID
#' #   filtered by response type and VISIT
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     code = "ADSL <- cadsl; ADRS <- cadrs",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_percentage_cross_table("Cross Table",
#'       dataname = c("ADSL", "ADRS"),
#'       x = data_extract_spec(
#'         dataname = "ADRS",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAM",
#'             choices = levels(ADRS$PARAM),
#'             selected = levels(ADRS$PARAM)[1],
#'             multiple = FALSE,
#'             label = "Select response:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADRS$AVISIT),
#'             selected = levels(ADRS$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = "STUDYID",
#'           selected = "STUDYID",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("SEX", "AGE", "RACE", "COUNTRY"),
#'           selected = "AGE",
#'           multiple = FALSE,
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
#' # datasets: same long
#' # Contingency table of variables from ADRS dataset
#'
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     code = "ADSL <- cadsl; ADRS <- cadrs",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_t_percentage_cross_table(
#'       "Scatterplot for same long dataset",
#'       dataname = "ADRS",
#'       x = data_extract_spec(
#'         dataname = "ADRS",
#'         select = select_spec(
#'           choices = c("PARAMCD", "AVISIT"),
#'           selected = c("PARAMCD"),
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADRS",
#'         select = select_spec(
#'           choices = c("AVISIT", "PARAMCD"),
#'           selected = c("AVISIT"),
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'



#'
#' # datasets: different subsets of long dataset
#' # Contingency table of variables from Lab dataset (ADLB)
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
#'   modules = root_modules(
#'     tm_t_percentage_cross_table(
#'       label = "Cross Table",
#'       dataname = "ADLB",
#'       x = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = filter_spec(
#'           vars = "PARAMCD",
#'           choices = levels(ADLB$PARAMCD),
#'           selected = levels(ADLB$PARAMCD)[1],
#'           multiple = TRUE,
#'           label = "Select lab:"
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
#'           selected = levels(ADLB$PARAMCD)[2],
#'           multiple = TRUE,
#'           label = "Select lab:"
#'         ),
#'         select = select_spec(
#'           choices = names(ADLB),
#'           selected = "ARMCD",
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
    stop_if_not(list(all(vapply(x, function(x) !isTRUE(x$select$multiple), logical(1))),
                     "x variable should not allow multiple selection"))
  } else if (is(x, "data_extract_spec")) {
    stop_if_not(list(!isTRUE(x$select$multiple),
                     "x variable should not allow multiple selection"))
  }
  if (is.class.list("data_extract_spec")(y)) {
    stop_if_not(list(all(vapply(y, function(x) !isTRUE(x$select$multiple), logical(1))),
                     "y variable should not allow multiple selection"))
  } else if (is(y, "data_extract_spec")) {
    stop_if_not(list(!isTRUE(y$select$multiple),
                     "y variable should not allow multiple selection"))
  }
  args <- as.list(environment())

  module(
    label = label,
    server = function(input, output, session, datasets, ...) return(NULL),
    ui = ui_percentage_cross_table,
    ui_args = args,
    server_args = list(dataname = dataname, label = label, x = x, y = y),
    filters = "all"
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
    forms = actionButton(ns("show_rcode"), "Show R code", width = "100%"),
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
