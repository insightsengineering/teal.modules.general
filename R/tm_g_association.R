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
#'     tm_g_association(
#'       dataname = c("ASL", "ADRS", "ADTTE"),
#'       ref = list(
#'         data_extract_spec(
#'           dataname = "ADRS",
#'           columns = columns_spec(
#'             label = "Reference variable",
#'             choices = c("AVAL", "AVALC"),
#'             selected = "AVAL",
#'             fixed = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADTTE",
#'           columns = columns_spec(
#'             label = "Reference variable",
#'             choices = c("AVAL", "CNSR"),
#'             selected = "AVAL",
#'             fixed = FALSE
#'           )
#'         )
#'       ),
#'       vars = list(
#'         data_extract_spec(
#'           dataname = "ADRS",
#'           columns = columns_spec(
#'             label = "Associated variables",
#'             choices = names(ADRS),
#'             selected = c("AGE", "SEX"),
#'             fixed = FALSE
#'           ),
#'           filter = filter_spec(
#'             vars = c("PARAMCD"),
#'             choices = unique(ADTTE$PARAMCD),
#'             selected = "OS",
#'             multiple = FALSE,
#'             label = "ADTTE filter"
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADTTE",
#'           columns = columns_spec(
#'             label = "Associated variables",
#'             choices = names(ADTTE),
#'             selected = NULL,
#'             fixed = FALSE
#'           ),
#'           filter = filter_spec(
#'             vars = c("PARAMCD", "AVISIT"),
#'             choices = apply(expand.grid(unique(ADRS$PARAMCD), unique(ADRS$AVISIT)), 1, paste, collapse = " - "),
#'             selected = "OVRINV - Screening",
#'             multiple = TRUE,
#'             label = "ADRS filter"
#'           )
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
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
