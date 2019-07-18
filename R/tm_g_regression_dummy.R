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
#'
#' @noRd
#'
#' @examples
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ALB <- cadlb
#'
#' keys(ASL) <- c("STUDYID", "USUBJID")
#' keys(ALB) <- c("STUDYID", "USUBJID", "PARAMCD")
#'
#' asl_extracted <- data_extract_spec(
#'   dataname = "ASL",
#'   columns = columns_spec(
#'     choices = c("BMRKR1", "BMRKR2"),
#'     selected = c("BMRKR1"),
#'     multiple = FALSE,
#'     fixed = FALSE
#'   )
#' )
#' asl_extracted_regressor <- data_extract_spec(
#'   dataname = "ASL",
#'   columns = columns_spec(
#'     choices = c("SEX", "AGE", "BMRKR1", "BMRKR2"),
#'     selected = c("BMRKR1"),
#'     multiple = TRUE,
#'     fixed = FALSE
#'   )
#' )
#'
#' alb_filters <- filter_spec(
#'   vars = c("PARAMCD", "AVISIT"),
#'   sep = " - ",
#'   choices = paste0(unique(ALB$PARAMCD), " - WEEK 4 DAY 29"),
#'   selected = paste0(unique(ALB$PARAMCD), " - WEEK 4 DAY 29")[1],
#'   multiple = FALSE,
#'   label = "Choose endpoint"
#' )
#'
#' alb_extracted <- data_extract_spec(
#'   dataname = "ALB",
#'   filter = alb_filters,
#'   columns = columns_spec(
#'     choices = c("AVAL"),
#'     selected = "AVAL",
#'     multiple = FALSE,
#'     fixed = FALSE,
#'     label = "variable"
#'   )
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ALB = ALB,
#'     code = 'ASL <- cadsl
#'             ALB <- cadlb
#'             keys(ASL) <- c("STUDYID", "USUBJID")
#'             keys(ALB) <- c("STUDYID", "USUBJID", "PARAMCD")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_regression_dummy(
#'       label = "Regression",
#'       dataname = c("ASL", "ALB"),
#'       response = alb_extracted,
#'       regressor = asl_extracted
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_regression_dummy <- function(label = "Regression Analysis",
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
    server = function(input, output, session, datasets, ...) return(NULL),
    ui = ui_g_regression,
    ui_args = args,
    server_args = list(regressor = regressor, response = response, dataname = dataname),
    filters = "all"
  )
}
