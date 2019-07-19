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
#'
#' @noRd
#'
#'  @examples
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ADTE <- radtte(ASL, seed = 1, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))
#'
#' keys(ASL) <- c("STUDYID", "USUBJID")
#' keys(ADTE) <- c("STUDYID", "USUBJID", "PARAMCD")
#'
#'
#' app <- init(
#'   data = cdisc_data(
#'        ASL = ASL,
#'        ADTE = ADTE,
#'        code = 'ASL <- cadsl
#'                ADTE <- radtte(ASL, seed = 1, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))
#'                keys(ASL) <- c("STUDYID", "USUBJID")
#'                keys(ADTE) <- c("STUDYID", "USUBJID", "PARAMCD")
#'                ',
#'        check = FALSE),
#'   modules = root_modules(
#'     tm_g_scatterplotmatrix_dummy(
#'       label = "Scatterplot matrix",
#'       dataname = c("ASL","ADTE"),
#'       selected = data_extract_spec(
#'         dataname = "ADTE",
#'         filter = filter_spec(
#'           vars = c("PARAMCD"), #'  only key variables are allowed
#'           sep = " - ",
#'           choices = c("OS", "PFS", "EFS"),
#'           selected = "OS",
#'           multiple = TRUE, #'  if multiple, then a spread is needed
#'           label = "Choose endpoint"
#'         ),
#'         columns = columns_spec(
#'           choices = colnames(ADTE),
#'           selected = if (all(c('AGE', 'SEX') %in% colnames(ADTE))) {
#'             c('AGE', 'SEX')
#'           } else {
#'             colnames(ADTE)[1:2]
#'           },
#'           multiple = TRUE,
#'           fixed = FALSE, #'  Whether the user can select the item
#'           label = "" #'  Label the column select dropdown (optional)
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_scatterplotmatrix_dummy <- function(label = "Scatterplot matrix",
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
