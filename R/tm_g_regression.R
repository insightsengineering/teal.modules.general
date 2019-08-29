#' Scatterplot and Regression Model
#'
#'
#' @param regressor (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'  regressor variable from an incoming dataset with filtering and selecting.
#' @param response (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'  response variable from an incoming dataset with filtering and selecting.
#' @param plot_height (\code{numeric}) a vector of length three with \code{c(value, min and max)} for a slider
#'  encoding the plot height.
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @export
#'
#' @examples
#' # datasets: same wide
#' # Regression graphs from selected response variable (BMRKR1) and
#' # selected regressors (AGE)
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
#'   modules = root_modules(
#'     tm_g_regression(
#'       label = "Regression",
#'       response = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("BMRKR1", "BMRKR2"),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       regressor = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = c("AGE", "SEX", "RACE"),
#'           selected = "AGE",
#'           multiple = TRUE,
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
#' # bug: RACE not found in ADSL_2
#' # Regression of BMRKR1 by AGE + RACE
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- cadsl
#' ADSL <- mutate_at(ADSL,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = list(~as.factor(.))) %>% select("ARM", "ACTARM", "ACTARMCD",
#'  "SEX", "AGE", "USUBJID", "STUDYID", "BMRKR1", "BMRKR2")
#' ADSL_2 <- mutate_at(cadsl,
#'                     .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                     .funs = list(~as.factor(.))) %>%
#'   select("ACTARM", "AGE", "RACE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     dataset("ADSL_2", ADSL_2, keys = list(primary = c("STUDYID", "USUBJID"),
#'                                           foreign = NULL,
#'                                           parent = NULL)),
#'     code = 'ADSL <- cadsl
#'             ADSL <- mutate_at(ADSL,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = list(~as.factor(.))) %>% select("ARM", "ACTARM", "ACTARMCD",
#'                      "SEX", "STRATA1", "AGE", "USUBJID", "STUDYID", "STRATA2", "BMRKR1", "BMRKR2")
#'             ADSL_2 <- mutate_at(cadsl,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = list(~as.factor(.))) %>% select("ACTARM", "AGE", "RACE", "STRATA2",
#'                  "COUNTRY", "USUBJID", "STUDYID")',
#'     check = FALSE #TODO
#'   ),
#'   modules = root_modules(
#'     tm_g_regression(
#'       label = "Regression",
#'       response = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("BMRKR1", "BMRKR2"),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       regressor = data_extract_spec(
#'         dataname = "ADSL_2",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = c("AGE", "RACE", "COUNTRY"),
#'           selected = c("AGE", "RACE"),
#'           multiple = TRUE,
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
#' # bug: AVISIT not renamed
#' # Examine linear relationship between responses of different parameters
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
#'     tm_g_regression(
#'       label = "Regression",
#'       response = data_extract_spec(
#'         dataname = "ADRS",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAM",
#'             choices = levels(ADRS$PARAM),
#'             selected = levels(ADRS$PARAM)[1],
#'             multiple = FALSE,
#'             label = "Select measurement:"
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
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       regressor = data_extract_spec(
#'         dataname = "ADRS",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAM",
#'             choices = levels(ADRS$PARAM),
#'             selected = levels(ADRS$PARAM)[1:2],
#'             multiple = TRUE,
#'             label = "Select measurements:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADRS$AVISIT),
#'             selected = levels(ADRS$AVISIT)[2],
#'             multiple = TRUE,
#'             label = "Select visits:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
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
#' # datasets: multiple long datasets
#' # Regression plots from model of different parameters from ADRS or ADTTE datasets
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
#'     tm_g_regression(
#'       label = "Regression Analysis on two long datasets",
#'       response = data_extract_spec(
#'         dataname = "ADTTE",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("AVAL", "CNSR"),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         ),
#'         filter = filter_spec(
#'           label = "Select parameter:",
#'           vars = c("PARAMCD"),
#'           choices = unique(ADTTE$PARAMCD),
#'           selected = "OS",
#'           multiple = FALSE
#'         )
#'       ),
#'       regressor = list(
#'         data_extract_spec(
#'           dataname = "ADRS",
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = names(ADRS),
#'             selected = "AVAL",
#'             multiple = TRUE,
#'             fixed = FALSE
#'           ),
#'           filter = filter_spec(
#'             label = "Select endpoints:",
#'             vars = c("PARAMCD", "AVISIT"),
#'             choices = apply(expand.grid(
#'             levels(ADRS$PARAMCD), levels(ADRS$AVISIT)), 1, paste, collapse = " - "),
#'             selected = "OVRINV - Screening",
#'             multiple = TRUE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = c("BMRKR1", "BMRKR2"),
#'             selected = "BMRKR1",
#'             multiple = TRUE,
#'             fixed = FALSE
#'           )
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: wide and long
#' # Regression of lab measurement (ADLB$AVAL) by
#' #   patient age (ADSL$AGE)
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
#'     tm_g_regression(
#'       label = "Regression",
#'       response = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAM",
#'             choices = levels(ADLB$PARAM),
#'             selected = levels(ADLB$PARAM)[1],
#'             multiple = FALSE,
#'             label = "Select measurement:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       ),
#'       regressor = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = c("BMRKR1", "BMRKR2", "AGE"),
#'           selected = "AGE",
#'           multiple = TRUE,
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
#' # datasets: wide, long, long
#'
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADLB <- cadlb
#' ADRS <- cadrs
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     cdisc_dataset("ADRS", ADRS),
#'     code = "ADSL <- cadsl; ADLB <- cadlb; ADRS <- cadrs",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_regression(
#'       label = "Regression",
#'       response = list(data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAM",
#'             choices = levels(ADLB$PARAM),
#'             selected = levels(ADLB$PARAM)[1],
#'             multiple = FALSE,
#'             label = "Select measurement:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       data_extract_spec(
#'         dataname = "ADRS",
#'         filter = list(
#'           filter_spec(
#'             vars = "ARMCD",
#'             choices = levels(ADRS$ARMCD),
#'             selected = levels(ADRS$ARMCD)[1],
#'             multiple = FALSE,
#'             label = "Select ARM:"
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
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )),
#'       regressor = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = c("BMRKR1", "BMRKR2"),
#'           selected = c("BMRKR1"),
#'           multiple = TRUE,
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
#' # datasets: different subsets of long dataset
#' # Examine linear relationship between different lab measurements
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
#'     tm_g_regression(
#'       label = "Regression",
#'       response = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Select lab:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       ),
#'       regressor = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[2:3],
#'             multiple = TRUE,
#'             label = "Select labs:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_regression <- function(label = "Regression Analysis",
                            regressor,
                            response,
                            plot_height = c(600, 200, 2000),
                            pre_output = NULL,
                            post_output = NULL) {
  if (!is.class.list("data_extract_spec")(regressor)) {
    regressor <- list(regressor)
  }
  if (!is.class.list("data_extract_spec")(response)) {
    response <- list(response)
  }

  stopifnot(is.character.single(label))
  stopifnot(is.class.list("data_extract_spec")(response))
  stop_if_not(list(all(vapply(response, function(x) !isTRUE(x$select$multiple), logical(1))),
                   "Response variable should not allow multiple selection"))
  stopifnot(is.class.list("data_extract_spec")(regressor))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])

  # No check necessary for regressor and response, as checked in data_extract_input

  # Send ui args
  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_regression,
    ui = ui_g_regression,
    ui_args = args,
    server_args = list(regressor = regressor, response = response),
    filters = "all"
  )
}


ui_g_regression <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)

  standard_layout(
    output = white_small_well(
      tags$div(verbatimTextOutput(ns("outtext"))),
      tags$div(verbatimTextOutput(ns("strtext"))),
      tags$div(DT::dataTableOutput(ns("outtable")))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("response", "regressor")]),
      data_extract_input(
        id = ns("response"),
        label = "Response variable",
        data_extract_spec = args$response
      ),
      data_extract_input(
        id = ns("regressor"),
        label = "Regressor variables",
        data_extract_spec = args$regressor
      ),
      radioButtons(
        ns("plot_type"),
        label = "Plot type:",
        choices = c(
          "Response vs Regressor",
          "Residuals vs Fitted",
          "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
          "Cook's dist vs Leverage h[ii]/(1 - h[ii])"
        ),
        selected = "Response vs Regressor"
      )
    ),
    pre_output = args$pre_output,
    post_output = args$post_output,
    forms = actionButton(ns("show_rcode"), "Show R code", width = "100%")
  )
}


#' @importFrom graphics plot abline
#' @importFrom magrittr %>%
#' @importFrom methods is substituteDirect
#' @importFrom stats as.formula
srv_g_regression <- function(input, output, session, datasets, response, regressor) {
  init_chunks(session)
  dataname <- get_extract_datanames(list(response, regressor))
  data_extract <- list(response, regressor)

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = data_extract,
    input_id = c("response", "regressor")
  )

  output$outtext <- renderText({
    chunks_reset()
    chunks_validate_is_ok()

    merged_data()$expr
  })

  output$strtext <- renderText({
    paste0(capture.output(str(merged_data())), collapse = "\n")
  })

  output$outtable <- DT::renderDataTable({
    merged_data()$data
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "R Code for a Scatterplotmatrix",
      rcode = get_rcode(
        datasets = datasets,
        merge_expression = merged_data()$expr,
        title = "",
        description = ""
      )
    )
  })
}
