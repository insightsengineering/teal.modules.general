#' Stack Plots of variables and show association with reference variable
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams teal::module
#' @param dataname (\code{character}) data set name to analyze
#' @param ref (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   reference variable, must set \code{multiple = FALSE}
#' @param vars (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   associated variables
#' @param show_association (\code{logical}) wheater show association of \code{vars} with refference variable
#' @param plot_height (\code{numeric}) vector with three elements defining selected, min and max plot height
#' @param with_show_r_code (\code{logical}) Whether show R Code button shall be enabled
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @export
#' @examples
#'
#' # datasets: single wide
#' library(random.cdisc.data)
#' ASL <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     code = "ASL <- cadsl",
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_association(
#'       dataname = "ASL",
#'       ref = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(label = "Reference variable",
#'                                choices = names(ASL),
#'                                selected = "AGE",
#'                                fixed = FALSE)
#'       ),
#'       vars = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(label = "Associated variables",
#'                                choices = names(ASL),
#'                                selected = "SEX",
#'                                multiple = TRUE,
#'                                fixed = FALSE)
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
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
#'     tm_g_association(
#'       label = "Association plots",
#'       dataname = c("ASL", "ADSL_2"),
#'       ref = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'          label = "Select variable",
#'           choices = c("AGE", "SEX", "STRATA1", "RACE"),
#'           selected = c("AGE"),
#'           multiple = FALSE
#'         )),
#'       vars = data_extract_spec(
#'         dataname = "ADSL_2",
#'         columns = columns_spec(
#'           label = "Select variables",
#'           choices = c("COUNTRY", "AGE", "RACE"),
#'           selected = c("AGE", "COUNTRY", "RACE"),
#'           multiple = TRUE
#'         ))
#'     ) #tm_g_regression
#'   )# root_modules
#' )# init
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#'# datasets: wide and long
#'
#'library(random.cdisc.data)
#'library(tern)
#'
#'ASL <- cadsl
#'ADRS <- cadrs
#'
#'keys(ASL) <- c("STUDYID", "USUBJID")
#'keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#'app <- init(
#'  data = cdisc_data(
#'    ASL = ASL,
#'    ADRS = ADRS,
#'    code = 'ASL <- cadsl
#'            ADRS <- cadrs
#'            keys(ASL) <- c("STUDYID", "USUBJID")
#'            keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'    check = FALSE),
#'  modules = root_modules(
#'    tm_g_association(
#'      label = "Association Plots",
#'      dataname = c("ASL", "ADRS"),
#'      ref = data_extract_spec(
#'        dataname = "ADRS",
#'        filter = list(
#'          filter_spec(
#'            vars = "PARAM",
#'           choices = unique(ADRS$PARAM),
#'            selected = unique(ADRS$PARAM)[1],
#'            multiple = FALSE,
#'            label = "Choose response"
#'          ),
#'          filter_spec(
#'            vars = "AVISIT",
#'            choices = levels(ADRS$AVISIT),
#'            selected = levels(ADRS$AVISIT)[1],
#'            multiple = FALSE,
#'            label = "Choose visit"
#'          )
#'        ),
#'        columns = columns_spec(
#'          choices = "AVAL",
#'          selected = "AVAL",
#'          multiple = FALSE,
#'          label = "variable"
#'        )
#'     ),
#'      vars = data_extract_spec(
#'        dataname = "ASL",
#'        columns = columns_spec(
#'          choices = c("SEX", "AGE", "RACE", "COUNTRY"),
#'          selected = c("SEX", "AGE"),
#'          multiple = TRUE,
#'          fixed = FALSE
#'       )
#'      )
#'     ) #tm_g_association
#'   )# root_modules
#' )# init
#'
#'\dontrun{
#'shinyApp(app$ui, app$server)
#'}
#'
tm_g_association <- function(label = "Association",
                             dataname,
                             ref,
                             vars,
                             show_association = TRUE,
                             plot_height = c(600, 400, 5000),
                             pre_output = NULL,
                             post_output = NULL,
                             with_show_r_code = TRUE) {
  if (!is.class.list("data_extract_spec")(ref)) {
    ref <- list(ref)
  }
  if (!is.class.list("data_extract_spec")(vars)) {
    vars <- list(vars)
  }

  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  stopifnot(is.class.list("data_extract_spec")(ref))
  stop_if_not(list(all(vapply(ref, function(x) !(x$columns$multiple), logical(1))),
                   "'ref' should not allow multiple selection"))
  stopifnot(is.class.list("data_extract_spec")(vars))
  stopifnot(is.logical.single(show_association))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])
  stopifnot(is.logical(with_show_r_code))

  args <- as.list(environment())

  module(
    label = label,
    server = function(input, output, session, datasets, ...) return(NULL),
    ui = ui_tm_g_association,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      ref = ref,
      vars = vars
    ),
    filters = "all"
  )
}


ui_tm_g_association <- function(id, ...) {
  ns <- NS(id)
  arguments <- list(...)

  browser()
  # standard_layout2(
  standard_layout(
    output = white_small_well(plot_height_output(id = ns("myplot"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(paste(arguments$dataname, collapse = ", "))),
      data_extract_input(
        id = ns("ref"),
        label = "Reference variable",
        data_extract_spec = arguments$ref
      ),
      data_extract_input(
        id = ns("vars"),
        label = "Associated variables",
        data_extract_spec = arguments$vars
      ),
      checkboxInput(ns("association"),
        "Association with First Variable",
        value = arguments$show_association
      ),
      checkboxInput(ns("show_dist"),
        "Distribution",
        value = FALSE
      ),
      checkboxInput(ns("log_transformation"),
        "Log transformed",
        value = FALSE
      ),
      plot_height_input(id = ns("myplot"), value = arguments$plot_height)
    ),
    forms = if (arguments$with_show_r_code) actionButton(ns("show_rcode"), "Show R Code", width = "100%") else NULL,
    pre_output = arguments$pre_output,
    post_output = arguments$post_output
  )
}


#' @importFrom grid grid.newpage grid.draw
#' @importFrom tern stack_grobs
srv_tm_g_association <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 dataname,
                                 ref,
                                 vars) {
  stopifnot(all(dataname %in% datasets$datanames()))

  NULL
}
