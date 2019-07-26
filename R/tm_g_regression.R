#' Scatterplot and Regression Model
#'
#'
#' @param dataname name of datasets used to generate the regression plot (just used for labeling)
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
#' library(random.cdisc.data)
#' library(tern)
#'

#'
#' # datasets: same wide
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#'
#' keys(ASL) <- c("STUDYID", "USUBJID")
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     code = 'ASL <- cadsl
#'             keys(ASL) <- c("STUDYID", "USUBJID")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_regression(
#'       label = "Regression",
#'       dataname = c("ASL", "ALB"),
#'       response = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = c("AGE","SEX"),
#'           selected = "AGE",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "variable"
#'         )
#'       ),
#'       regressor = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
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
#' # datasets: different wide
#'
#' library(random.cdisc.data)
#' library(tern)
#' library(dplyr)
#'
#' ASL <- cadsl
#' ASL <- mutate_at(ASL,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = funs(as.factor(.))) %>% select("ARM", "ACTARM", "ACTARMCD",
#'  "SEX", "STRATA1", "AGE", "USUBJID", "STUDYID", "STRATA2")
#' keys(ASL) <- c("STUDYID", "USUBJID")
#'
#'
#' ADSL_2 <- mutate_at(cadsl,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = funs(as.factor(.))) %>% select("ACTARM", "AGE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
#' keys(ADSL_2) <- c("STUDYID", "USUBJID")
#'
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ADSL_2 = ADSL_2,
#'     code = 'ASL <- cadsl
#'             keys(ASL) <- c("STUDYID", "USUBJID")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_regression(
#'       label = "Regression",
#'       dataname = c("ASL", "ADSL_2"),
#'       response = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'          label = "Select variable",
#'           choices = c("AGE", "SEX", "STRATA1", "RACE"),
#'           selected = c("AGE"),
#'           multiple = FALSE
#'         )),
#'       regressor = data_extract_spec(
#'         dataname = "ADSL_2",
#'         columns = columns_spec(
#'           label = "Select variables",
#'           choices = c("COUNTRY", "AGE", "RACE"),
#'           selected = c("AGE", "COUNTRY", "RACE"),
#'           multiple = TRUE
#'         ))
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#' # datasets: same long
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ADLB <- cadlb
#'
#' keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ADLB = ADLB,
#'     code = 'ADLB <- cadlb
#'             keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_regression(
#'       label = "Regression",
#'       dataname = c("ASL", "ADLB"),
#'       response = data_extract_spec(
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
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "variable"
#'         )
#'       ),
#'       regressor = data_extract_spec(
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
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           label = "variable"
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
#'
#'# datasets: wide and long
#'library(random.cdisc.data)
#'library(tern)
#'
#'ASL <- cadsl
#'ADLB <- cadlb
#'
#'keys(ASL) <- c("STUDYID", "USUBJID")
#'keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#'app <- init(
#'  data = cdisc_data(
#'    ASL = ASL,
#'    ADLB = ADLB,
#'    code = 'ASL <- cadsl
#'            ADLB <- cadlb
#'            keys(ASL) <- c("STUDYID", "USUBJID")
#'            keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'    check = FALSE),
#'  modules = root_modules(
#'    tm_g_regression(
#'      label = "Regression",
#'      dataname = c("ASL", "ADLB"),
#'      response = data_extract_spec(
#'        dataname = "ADLB",
#'        filter = list(
#'          filter_spec(
#'            vars = "PARAMCD",
#'           choices = levels(ADLB$PARAMCD),
#'            selected = levels(ADLB$PARAMCD)[1],
#'            multiple = FALSE,
#'            label = "Choose endpoint"
#'          ),
#'          filter_spec(
#'            vars = "AVISIT",
#'            choices = levels(ADLB$AVISIT),
#'            selected = levels(ADLB$AVISIT)[1],
#'            multiple = FALSE,
#'            label = "Choose visit"
#'          )
#'        ),
#'        columns = columns_spec(
#'          choices = "AVAL",
#'          selected = "AVAL",
#'          multiple = FALSE,
#'          fixed = FALSE,
#'          label = "variable"
#'        )
#'     ),
#'      regressor = data_extract_spec(
#'        dataname = "ASL",
#'        columns = columns_spec(
#'          choices = c("BMRKR1", "BMRKR2"),
#'          selected = c("BMRKR1"),
#'          multiple = FALSE,
#'          fixed = FALSE
#'       )
#'      )
#'    )
#' )
#')
#'
#'\dontrun{
#'shinyApp(app$ui, app$server)
#'}
#'
#' # datasets: wide, long, long
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ADLB <- cadlb
#' ADRS <- cadrs
#'
#' keys(ASL) <- c("STUDYID", "USUBJID")
#' keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#' keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ADLB = ADLB,
#'     code = 'ASL <- cadsl
#'             ADLB <- cadlb
#'             keys(ASL) <- c("STUDYID", "USUBJID")
#'             keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'             keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_regression(
#'       label = "Regression",
#'       dataname = c("ASL", "ADLB", "ADRS"),
#'       response = list(data_extract_spec(
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
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "variable"
#'         )
#'       ),data_extract_spec(
#'         dataname = "ADRS",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADRS$ARMCD),
#'             selected = levels(ADRS$ARMCD)[1],
#'             multiple = FALSE,
#'             label = "Choose ARM"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADRS$AVISIT),
#'             selected = levels(ADRS$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Choose visit"
#'           )
#'         ),
#'         columns = columns_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )),
#'       regressor = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = c("BMRKR1", "BMRKR2"),
#'           selected = c("BMRKR1"),
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
tm_g_regression <- function(label = "Regression Analysis",
                            dataname,
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
  stopifnot(is.character.vector(dataname))
  stopifnot(is.class.list("data_extract_spec")(response))
  stop_if_not(list(all(vapply(response, function(x) !isTRUE(x$columns$multiple), logical(1))),
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
    server_args = list(regressor = regressor, response = response, dataname = dataname),
    filters = "all"
  )
}


ui_g_regression <- function(id, ...) {
  arguments <- list(...)

  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      tags$div(
        # This shall be wrapped in a teal::plot
        plot_height_output(id = ns("myplot")),
        tags$div(verbatimTextOutput(ns("text")))
      )
    ),
    encoding = div(
      helpText("Datasets: ", lapply(arguments$dataname, tags$code)),
      data_extract_input(
        id = ns("response"),
        label = "Response Variable",
        data_extract_spec = arguments$response
      ),
      data_extract_input(
        id = ns("regressor"),
        label = "Regressor Variable",
        data_extract_spec = arguments$regressor
      ),
      radioButtons(
        ns("plot_type"),
        label = "Plot Type",
        choices = c(
          "Response vs Regressor",
          "Residuals vs Fitted",
          "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
          "Cook's dist vs Leverage h[ii]/(1 - h[ii])"
        ),
        selected = "Response vs Regressor"
      ),
      # This shall be wrapped in a teal::plot
      plot_height_input(id = ns("myplot"), value = arguments$plot_height)
    ),
    pre_output = arguments$pre_output,
    post_output = arguments$post_output,
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%")
  )
}


#' @importFrom graphics plot abline
#' @importFrom magrittr %>%
#' @importFrom methods is substituteDirect
#' @importFrom stats as.formula
srv_g_regression <- function(input, output, session, datasets, dataname, response, regressor) {
  NULL
}
