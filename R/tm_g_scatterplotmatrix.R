#' Create a scatterplot matrix
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param dataname Name of datasets used to generate the regression plot (just used for labeling).
#' @param selected (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'  Plotting variables from an incoming dataset with filtering and selecting.
#' @param plot_height (\code{numeric}) A vector of length three with \code{c(value, min and max)} for a slider
#'  encoding the plot height.
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @export
#' @examples
#'
#' # datasets: single wide dataset
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' keys(ASL) <- c("STUDYID", "USUBJID")
#'
#' app <- init(
#'   data = cdisc_data(
#'        ASL = ASL,
#'        code = 'ASL <- cadsl
#'                keys(ASL) <- c("STUDYID", "USUBJID")',
#'        check = FALSE),
#'   modules = root_modules(
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix",
#'       dataname = c("ASL"),
#'       selected = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           label = "Selected columns",
#'           choices = colnames(ASL),
#'           selected = c("AGE", "RACE", "SEX"),
#'           multiple = TRUE,
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
#'             ADSL_2 <- mutate_at(cadsl,
#'                .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                .funs = funs(as.factor(.))) %>% select("ACTARM", "AGE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
#'             keys(ASL) <- keys(ADSL_2) <- c("STUDYID", "USUBJID")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix",
#'       dataname = c("ASL", "ADSL_2"),
#'       selected = list(
#'                 data_extract_spec(
#'           dataname = "ASL",
#'           columns = columns_spec(
#'             label = "Selected columns",
#'             choices = colnames(ASL),
#'             selected = c("AGE", "ACTARM", "SEX"),
#'             multiple = TRUE,
#'             fixed = FALSE)
#'         ),
#'         data_extract_spec(
#'           dataname = "ADSL_2",
#'           columns = columns_spec(
#'             label = "Selected columns",
#'             choices = colnames(ADSL_2),
#'             selected = c("COUNTRY", "ACTARM", "STRATA2"),
#'             multiple = TRUE,
#'             fixed = FALSE)
#'          )
#'      )
#'     )
#'   )
#' )
#'
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
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix on two long datasets",
#'       dataname = c("ASL", "ADRS", "ADTTE"),
#'       selected = list(
#'         data_extract_spec(
#'           dataname = "ASL",
#'           columns = columns_spec(
#'             choices = names(ASL),
#'             selected = c("AGE", "SEX"),
#'             multiple = TRUE,
#'             fixed = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADRS",
#'           columns = columns_spec(
#'             choices = names(ADRS),
#'             selected = c("AVAL", "AVALC"),
#'             multiple = TRUE,
#'             fixed = FALSE
#'           ),
#'           filter = filter_spec(
#'             vars = c("PARAMCD", "AVISIT"),
#'           choices = apply(expand.grid(unique(ADRS$PARAMCD), unique(ADRS$AVISIT)), 1, paste, collapse = " - "),
#'             selected = "OVRINV - Screening",
#'             multiple = TRUE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADTTE",
#'           columns = columns_spec(
#'             choices = names(ADTTE),
#'             selected = c("AVAL", "CNSR"),
#'             multiple = TRUE,
#'             fixed = FALSE
#'           ),
#'           filter = filter_spec(
#'             vars = c("PARAMCD"),
#'             choices = unique(ADTTE$PARAMCD),
#'             selected = "OS",
#'             multiple = TRUE
#'           )
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_scatterplotmatrix <- function(label = "Scatterplot matrix",
                                   dataname,
                                   selected,
                                   plot_height = c(600, 200, 2000),
                                   pre_output = NULL,
                                   post_output = NULL) {
  if (!is.class.list("data_extract_spec")(selected)) {
    selected <- list(selected)
  }

  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  stopifnot(is.class.list("data_extract_spec")(selected))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])

  args <- as.list(environment())

  module(
    label = label,
    server = function(input, output, session, datasets, ...) return(NULL),
    ui = ui_g_scatterplotmatrix,
    ui_args = args,
    server_args = list(selected = selected, dataname = dataname),
    filters = "all"
  )
}


ui_g_scatterplotmatrix <- function(id, ...) {
  args <- list(...)

  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      tags$div(
        plot_height_output(ns("myplot")))
    ),
    encoding = div(
      helpText("Datasets: ", lapply(args$dataname, tags$code)),
      data_extract_input(
        id = ns("selected"),
        label = "Selected columns",
        data_extract_spec = args$selected
      ),
      sliderInput(ns("alpha"), "Opacity",
                  min = 0, max = 1, step = .05, value = .5, ticks = FALSE),
      sliderInput(ns("cex"), "Point Size",
                  min = 0.2, max = 3, step = .05, value = .65, ticks = FALSE),
      plot_height_input(id = ns("myplot"), value = args$plot_height)
    ),
    pre_output = args$pre_output,
    post_output = args$post_output,
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%")
  )
}


#' @importFrom dplyr mutate_if
#' @importFrom lattice splom
#' @importFrom methods substituteDirect
srv_g_scatterplotmatrix <- function(input,
                                    output,
                                    session,
                                    datasets,
                                    dataname,
                                    selected) {
  NULL
}
