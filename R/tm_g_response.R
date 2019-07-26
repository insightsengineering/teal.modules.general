#' Response Plots
#'
#'
#' @param dataname (\code{character}) Name of dataset used to generate the response plot
#' @param response (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Which variable to use as the response. You can define one fixed column by using the
#'   setting \code{fixed = TRUE} inside the \code{column_spec}.
#' @param x (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Which variable to use on the X-axis of the response plot. Allow the user to select multiple
#'   columns from the \code{data} allowed in teal. Just allow single columns by \code{multiple = FALSE}.
#' @param row_facet optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Which data columns to use for faceting rows.  Just allow single columns by \code{multiple = FALSE}.
#' @param col_facet optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Which data to use for faceting columns. Just allow single columns by \code{multiple = FALSE}.
#' @param coord_flip (\code{logical}) Whether to flip coordinates
#' @param freq (\code{logical}) Display frequency (\code{TRUE}) or density (\code{FALSE}).
#' @param plot_height (\code{numeric}) Vector of length three with \code{c(value, min and max)}.
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @export
#' @examples
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
#'     tm_g_response(
#'       label = "Response Plots",
#'       dataname = c("ASL"),
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
#'       x = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = c("BMRKR1", "BMRKR2"),
#'           selected = "BMRKR1",
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
#' # datasets: multiple long datasets
#' library(random.cdisc.data)
#' library(dplyr)
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
#'     tm_g_response(
#'       label = "Response Plot on two long datasets",
#'       dataname = c("ASL", "ADRS", "ADTTE"),
#'       response = data_extract_spec(
#'         dataname = "ADRS",
#'         columns = columns_spec(
#'           choices = "AVALC",
#'           selected = "AVALC",
#'           multiple = FALSE,
#'           fixed = TRUE,
#'           label = "variable"
#'         ),
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = unique(ADRS$PARAMCD),
#'             selected = unique(ADRS$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Choose endpoint"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADRS$AVISIT),
#'             selected = levels(ADRS$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Choose endpoint"
#'           )
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "ADTTE",
#'         columns = columns_spec(
#'           choices = c("CNSR", "EVENTDESC"),
#'           selected = "CNSR",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         ),
#'         filter = filter_spec(
#'           vars = c("PARAMCD"),
#'           choices = unique(ADTTE$PARAMCD),
#'           selected = "OS",
#'           multiple = FALSE,
#'           label = "ADTTE filter"
#'         )
#'       ),
#'       row_facet = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = c("SEX", "AGE"),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = c("SEX", "AGE"),
#'           selected = NULL,
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
#'             ASL <- mutate_at(ASL,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = funs(as.factor(.))) %>% select("ARM", "ACTARM", "ACTARMCD",
#'                      "SEX", "STRATA1", "AGE", "USUBJID", "STUDYID", "STRATA2")
#'             ADSL_2 <- mutate_at(cadsl,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = funs(as.factor(.))) %>% select("ACTARM", "AGE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
#'             keys(ASL) <- keys(ADSL_2) <- c("STUDYID", "USUBJID")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_response(
#'       label = "Response Plots",
#'       dataname = c("ASL", "ADSL_2"),
#'       response = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'          label = "Select variable",
#'           choices = c("AGE", "SEX", "STRATA1", "RACE"),
#'           selected = c("AGE"),
#'           multiple = FALSE
#'         )),
#'       x = data_extract_spec(
#'         dataname = "ADSL_2",
#'         columns = columns_spec(
#'           label = "Select variables",
#'           choices = c("COUNTRY", "AGE", "RACE"),
#'           selected = "COUNTRY",
#'           multiple = FALSE
#'         ))
#'     )
#'   )
#' )
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
#'    tm_g_response(
#'      label = "Response Plots",
#'      dataname = c("ASL", "ADLB"),
#'      response = data_extract_spec(
#'        dataname = "ADLB",
#'        filter = list(
#'          filter_spec(
#'            vars = "PARAM",
#'           choices = levels(ADLB$PARAM),
#'            selected = levels(ADLB$PARAM)[1],
#'            multiple = FALSE,
#'            label = "Choose measurement"
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
#'      x = data_extract_spec(
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
tm_g_response <- function(label = "Response Plot",
                          dataname,
                          response,
                          x,
                          row_facet = NULL,
                          col_facet = NULL,
                          coord_flip = TRUE,
                          freq = FALSE,
                          plot_height = c(600, 400, 5000),
                          pre_output = NULL,
                          post_output = NULL) {
  if (!is.class.list("data_extract_spec")(response)) {
    response <- list(response)
  }
  if (!is.class.list("data_extract_spec")(x)) {
    x <- list(x)
  }
  if (!is.class.list("data_extract_spec")(row_facet)) {
    row_facet <- list_or_null(row_facet)
  }
  if (!is.class.list("data_extract_spec")(col_facet)) {
    col_facet <- list_or_null(col_facet)
  }

  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  # No empty columns allowed for Response Var
  # No multiple Response variables allowed
  stopifnot(is.class.list("data_extract_spec")(response))
  # todo: this check should go into data_extract or at least create a function for reusability (add an argument allow_empty_values)
  # todo: also refactor this in tm_g_response.R to use this function
  stop_if_not(list(all(vapply(response, function(x) !("" %in% x$columns$choices), logical(1))),
                   "'response' should not allow empty values"))
  stop_if_not(list(all(vapply(response, function(x) !(x$columns$multiple), logical(1))),
                   "'response' should not allow multiple selection"))
  # No empty columns allowed for X-Var
  # No multiple X variables allowed
  stopifnot(is.class.list("data_extract_spec")(x))
  stop_if_not(list(all(vapply(x, function(x) !("" %in% x$columns$choices), logical(1))),
                   "'x' should not allow empty values"))
  stop_if_not(list(all(vapply(x, function(x) !(x$columns$multiple), logical(1))),
                   "'x' should not allow multiple selection"))
  stopifnot(is.null(row_facet) || is.class.list("data_extract_spec")(row_facet))
  stopifnot(is.null(col_facet) || is.class.list("data_extract_spec")(col_facet))
  stopifnot(is.logical.single(coord_flip))
  stopifnot(is.logical.single(freq))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])


  args <- as.list(environment())

  module(
    label = label,
    server = function(input, output, session, datasets, ...) return(NULL),
    ui = ui_g_response,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      response = response,
      x = x,
      row_facet = row_facet,
      col_facet = col_facet
    ),
    filters = "all"
  )
}

ui_g_response <- function(id, ...) {
  arguments <- list(...)

  ns <- NS(id)

  standard_layout(
    output = white_small_well(
      plot_height_output(id = ns("myplot"))
    ),
    encoding = div(
        helpText("Datasets: ", lapply(arguments$dataname, tags$code)),

      data_extract_input(
        id = ns("response"),
        label = "Response Variable",
        data_extract_spec = arguments$response
      ),
      data_extract_input(
        id = ns("x"),
        label = "X Variable",
        data_extract_spec = arguments$x
      ),
      if(!is.null( arguments$row_facet) ){
        data_extract_input(
          id = ns("row_facet"),
          label = "Row facetting Variables ",
          data_extract_spec = arguments$row_facet
        )
      },
      if(!is.null( arguments$col_facet) ){
        data_extract_input(
          id = ns("col_facet"),
          label = "col facetting Variables ",
          data_extract_spec = arguments$col_facet
        )
      },
      radioButtons(ns("freq"), NULL,
                   choices = c("frequency", "density"),
                   selected = ifelse(arguments$freq, "frequency", "density"), inline = TRUE
      ),
      checkboxInput(ns("coord_flip"), "swap axes", value = arguments$coord_flip),
      plot_height_input(id = ns("myplot"), value = arguments$plot_height)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = arguments$pre_output,
    post_output = arguments$post_output
  )
}

#' @importFrom forcats fct_rev
#' @importFrom magrittr %>%
#' @importFrom methods substituteDirect
srv_g_response <- function(input,
                           output,
                           session,
                           datasets,
                           dataname,
                           response,
                           x,
                           row_facet,
                           col_facet) {
  NULL
}
