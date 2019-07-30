#' Univariate and bivariate visualizations.
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param label (\code{character}) Label of the module
#' @param dataname (\code{character}) name of datasets used to generate the bivariate plot. You need
#'   to name all datasets used in the available \code{data_extract_spec}
#' @param x (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable name selected to plot along the x-axis by default. Variable can be numeric, factor or character.
#'   No empty selections are allowed!
#' @param y (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable name selected to plot along the y-axis by default. Variable can be numeric, factor or character.
#' @param use_density (\code{logical}) value for whether density (\code{TRUE}) is plotted or frequency (\code{FALSE})
#' @param row_facet (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable for row facetting
#' @param col_facet (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable for col facetting
#' @param expert_settings (\code{logical}) Whether coloring, filling and size should be chosen
#'   by the user
#' @param colour optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable selection for the colouring inside the expert settings
#' @param fill optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable selection for the filling inside the expert settings
#' @param size optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable selection for the size of \code{geom_point} plots inside the expert settings
#' @param free_x_scales (\code{logical}) If X scaling shall be changeable
#' @param free_y_scales (\code{logical}) If Y scaling shall be changeable
#' @param plot_height (\code{numeric}) \code{c(value, min and max)} of plot height slider
#' @param with_show_r_code (\code{logical}) Whether show R code button shall be shown
#' @param ggtheme (\code{character}) ggplot theme to be used by default. All themes can be chosen by the user.
#' @export
#' @details
#' This is a general module to visualize 1 & 2 dimensional data.
#'
#' @importFrom methods is
#'
#' @examples
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
#'     tm_g_bivariate(
#'       dataname = "ASL",
#'       x = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           label = "Reference variable",
#'           choices = names(ASL),
#'           selected = "AGE",
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           label = "Associated variables",
#'           choices = names(ASL),
#'           selected = "SEX",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       row_facet = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           label = "Associated variables",
#'           choices = names(ASL),
#'           selected = NULL,
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           label = "Associated variables",
#'           choices = names(ASL),
#'           selected = NULL,
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
#'
#' library(random.cdisc.data)
#' library(tern)
#' library(dplyr)
#'
#' ASL <- cadsl
#' ASL <- mutate_at(ASL,
#'  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'  .funs = funs(as.factor(.))) %>% select("ARM", "ACTARM", "ACTARMCD",
#'  "SEX", "STRATA1", "AGE", "USUBJID", "STUDYID", "STRATA2")
#' keys(ASL) <- c("STUDYID", "USUBJID")
#'
#'
#' ASL_2 <- mutate_at(cadsl,
#'   .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'   .funs = funs(as.factor(.))) %>% select("ACTARM", "AGE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
#' keys(ASL_2) <- c("STUDYID", "USUBJID")
#'
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ASL_2 = ASL_2,
#'     code = 'ASL <- cadsl
#'             ASL_2 <- mutate_at(cadsl,
#'                .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                .funs = funs(as.factor(.))) %>% select("ACTARM", "AGE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
#'             keys(ASL) <- keys(ASL_2) <- c("STUDYID", "USUBJID")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_bivariate(
#'       label = "Association plots",
#'       dataname = c("ASL", "ASL_2"),
#'       x = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           label = "Select variable",
#'           choices = c("AGE", "SEX", "STRATA1", "RACE"),
#'           selected = c("AGE"),
#'           multiple = FALSE
#'         )),
#'       y = data_extract_spec(
#'         dataname = "ASL_2",
#'         columns = columns_spec(
#'           label = "Select variables",
#'           choices = c("COUNTRY", "AGE", "RACE"),
#'           selected = "RACE",
#'           multiple = FALSE
#'         )),
#'       # Row facetting by first data set
#'       row_facet = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'          label = "Associated variables",
#'          choices = names(ASL),
#'          selected = NULL,
#'          multiple = FALSE,
#'          fixed = FALSE
#'         )
#'       ),
#'       # Col facetting by second data set
#'       col_facet = data_extract_spec(
#'         dataname = "ASL_2",
#'         columns = columns_spec(
#'           label = "Associated variables",
#'           choices = names(ASL),
#'           selected = NULL,
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
#'     tm_g_bivariate(
#'       label = "Bivariate Plots of two long datasets",
#'       dataname = c("ASL", "ADRS", "ADTTE"),
#'       x = data_extract_spec(
#'         dataname = "ADRS",
#'         filter = filter_spec(
#'           vars = c("PARAMCD", "AVISIT"),
#'           choices = apply(expand.grid(unique(ADRS$PARAMCD), unique(ADRS$AVISIT)),
#'                           1, paste, collapse = " - "),
#'           selected = "OVRINV - Screening",
#'           multiple = TRUE,
#'           label = "ADRS filter"
#'         ),
#'         columns = columns_spec(
#'           choices = c("AVAL", "AVALC"),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADTTE",
#'         columns = columns_spec(
#'           choices = c("AVAL", "CNSR"),
#'           selected = "AVAL",
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
#'         dataname = "ADRS",
#'         filter = filter_spec(
#'           vars = c("PARAMCD", "AVISIT"),
#'           choices = apply(expand.grid(unique(ADRS$PARAMCD), unique(ADRS$AVISIT)),
#'                           1, paste, collapse = " - "),
#'           selected = "OVRINV - Screening",
#'           multiple = TRUE,
#'           label = "ADRS filter"
#'         ),
#'         columns = columns_spec(
#'           choices = c("AVAL", "AVISIT"),
#'           selected = "AVISIT",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = c("SEX", "RACE"),
#'           selected = NULL,
#'           multiple = TRUE,
#'           fixed = FALSE,
#'           label = "variable"
#'         )
#'       ),
#'       expert_settings = TRUE,
#'       plot_height = c(600, 200, 2000),
#'       ggtheme = "grey"
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: wide, long
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ADRS <- cadrs
#'
#' keys(ASL) <- c("STUDYID", "USUBJID")
#' keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#' app <- init(
#'  data = cdisc_data(
#'    ASL = ASL,
#'    ADRS = ADRS,
#'    code = 'ASL <- cadsl
#'            ADRS <- cadrs
#'            keys(ASL) <- c("STUDYID", "USUBJID")
#'            keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'    check = FALSE),
#'  modules = root_modules(
#'    tm_g_bivariate(
#'      label = "Association Plots",
#'      dataname = c("ASL", "ADRS"),
#'      x = data_extract_spec(
#'        dataname = "ADRS",
#'        filter = list(
#'          filter_spec(
#'            vars = "PARAM",
#'            choices = unique(ADRS$PARAM),
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
#'      y = data_extract_spec(
#'        dataname = "ASL",
#'        columns = columns_spec(
#'          choices = c("BMRKR1", "SEX", "AGE", "RACE", "COUNTRY"),
#'          selected = "BMRKR1",
#'          multiple = FALSE,
#'          fixed = FALSE
#'       )
#'      ),
#'      row_facet = data_extract_spec(
#'        dataname = "ADRS",
#'        columns = columns_spec(
#'           choices = c("__NONE__", "PARAMCD"),
#'           selected = "PARAMCD",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "variable"
#'         )
#'      ),
#'      col_facet = data_extract_spec(
#'        dataname = "ADRS",
#'        columns = columns_spec(
#'           choices = c("__NONE__", "AVISIT"),
#'           selected = "AVISIT",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "variable"
#'         )
#'      )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: wide, long, long
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ADRS <- cadrs
#' ADLB <- cadlb
#'
#' keys(ASL) <- c("STUDYID", "USUBJID")
#' keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#' keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#' app <- init(
#'  data = cdisc_data(
#'    ASL = ASL,
#'    ADRS = ADRS,
#'    ADLB = ADLB,
#'    code = 'ASL <- cadsl
#'            ADRS <- cadrs
#'            ADLB <- cadlb
#'            keys(ASL) <- c("STUDYID", "USUBJID")
#'            keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'            keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'    check = FALSE),
#'  modules = root_modules(
#'    tm_g_bivariate(
#'      label = "Association Plots",
#'      dataname = c("ASL", "ADRS", "ADLB"),
#'      x = data_extract_spec(
#'        dataname = "ADRS",
#'        filter = list(
#'          filter_spec(
#'            vars = "PARAMCD",
#'            choices = unique(ADRS$PARAMCD),
#'            selected = unique(ADRS$PARAMCD)[1],
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
#'      y = data_extract_spec(
#'        dataname = "ASL",
#'        columns = columns_spec(
#'          choices = c("BMRKR1", "SEX", "AGE", "RACE", "COUNTRY"),
#'          selected = "BMRKR1",
#'          multiple = FALSE,
#'          fixed = FALSE
#'       )
#'      ),
#'      row_facet = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAM",
#'             choices = levels(ADLB$PARAM),
#'             selected = levels(ADLB$PARAM)[1],
#'             multiple = FALSE,
#'             label = "Choose measurement"
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
#'           choices = c("__NONE__", "AVAL"),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "variable"
#'         )
#'       ),
#'      col_facet = data_extract_spec(
#'        dataname = "ASL",
#'        columns = columns_spec(
#'          choices = c("SEX", "AGE", "RACE", "COUNTRY"),
#'          selected = NULL,
#'          multiple = FALSE,
#'          fixed = FALSE
#'       )
#'      ),
#'        expert_settings = TRUE,
#'       plot_height = c(600, 200, 2000),
#'       ggtheme = "grey"
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#'#' # datasets: different subsets of long dataset
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
#' app <- teal::init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ADLB = ADLB,
#'     code = 'ASL <- cadsl
#'             ADLB <- cadlb
#'             keys(ASL) <- c("STUDYID", "USUBJID")
#'             keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_bivariate(
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
#'             vars = "STRATA1",
#'             choices = levels(ADLB$STRATA1),
#'             selected = levels(ADLB$STRATA1)[1],
#'             multiple = FALSE,
#'             label = "Choose category"
#'           )
#'         ),
#'         columns = columns_spec(
#'           choices = c("AVISIT", "AVISITN"),
#'           selected = c("AVISIT"),
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
#'             vars = "STRATA1",
#'             choices = levels(ADLB$STRATA1),
#'             selected = levels(ADLB$STRATA1)[1],
#'             multiple = FALSE,
#'             label = "Choose category"
#'           )
#'         ),
#'         columns = columns_spec(
#'           choices = c("AVAL", "CHG"),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Variable"
#'         )
#'       ),
#'       use_density = FALSE,
#'       row_facet = data_extract_spec(
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
#'             vars = "STRATA1",
#'             choices = levels(ADLB$STRATA1),
#'             selected = levels(ADLB$STRATA1)[1],
#'             multiple = FALSE,
#'             label = "Choose category"
#'           )
#'         ),
#'         columns = columns_spec(
#'           choices = c("","RACE", "SEX", "ARMCD", "ACTARMCD"),
#'           selected = "",
#'           multiple = TRUE,
#'           fixed = FALSE,
#'           label = "Facetting variable"
#'         )
#'       ),
#'       col_facet = data_extract_spec(
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
#'             vars = "STRATA1",
#'             choices = levels(ADLB$STRATA1),
#'             selected = levels(ADLB$STRATA1)[1],
#'             multiple = FALSE,
#'             label = "Choose category"
#'           )
#'         ),
#'         columns = columns_spec(
#'           choices = c("","RACE", "SEX", "ARMCD", "ACTARMCD"),
#'           selected = "",
#'           multiple = TRUE,
#'           fixed = FALSE,
#'           label = "Facetting variable"
#'         )
#'       ),
#'       expert_settings = TRUE,
#'       plot_height = c(600, 200, 2000),
#'       ggtheme = "grey"
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_bivariate <- function(label = "Bivariate Plots",
                           dataname,
                           x,
                           y,
                           row_facet,
                           col_facet,
                           colour = NULL,
                           fill = NULL,
                           size = NULL,
                           use_density = FALSE,
                           expert_settings = TRUE,
                           free_x_scales = FALSE,
                           free_y_scales = FALSE,
                           plot_height = c(600, 200, 2000),
                           ggtheme = "minimal",
                           with_show_r_code = TRUE,
                           pre_output = NULL,
                           post_output = NULL) {
  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  stopifnot(is.class.list("data_extract_spec")(x) || is(x, "data_extract_spec"))
  stopifnot(is.class.list("data_extract_spec")(y) || is(y, "data_extract_spec"))
  stopifnot(is.class.list("data_extract_spec")(row_facet) || is(row_facet, "data_extract_spec"))
  stopifnot(is.class.list("data_extract_spec")(col_facet) || is(col_facet, "data_extract_spec"))
  stopifnot(is.null(colour) || is.class.list("data_extract_spec")(colour) || is(colour, "data_extract_spec"))
  stopifnot(is.null(fill) || is.class.list("data_extract_spec")(fill) || is(fill, "data_extract_spec"))
  stopifnot(is.null(size) || is.class.list("data_extract_spec")(size) || is(size, "data_extract_spec"))
  if (is.class.list("data_extract_spec")(x)) {
    stop_if_not(list(all(vapply(x, function(xx) !isTRUE(xx$columns$multiple), logical(1))),
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
  stopifnot(is.logical.single(use_density))
  stopifnot(is.logical.single(expert_settings))
  stopifnot(is.logical.single(free_x_scales))
  stopifnot(is.logical.single(free_y_scales))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])
  stopifnot(is.character.single(ggtheme))
  stopifnot(is.logical.single(with_show_r_code))

  if (expert_settings) {
    if (is.null(colour)) {
      colour <- `if`(inherits(x, "list"), x, list(x))
      colour[[1]]$columns$selected <- ""
      colour[[1]]$columns$choices <- c("", colour[[1]]$columns$choices)
    }
    if (is.null(fill)) {
      fill <- `if`(inherits(x, "list"), x, list(x))
      fill[[1]]$columns$selected <- ""
      fill[[1]]$columns$choices <- c("", fill[[1]]$columns$choices)
    }
    if (is.null(size)) {
      size <- `if`(inherits(x, "list"), x, list(x))
      size[[1]]$columns$selected <- ""
      size[[1]]$columns$selected <- ""
      size[[1]]$columns$choices <- c("", size[[1]]$columns$choices)
    }
  }

  args <- as.list(environment())

  module(
    label = label,
    server = function(input, output, session, datasets, ...) return(NULL),
    ui = ui_g_bivariate,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      x = x,
      y = y,
      row_facet = row_facet,
      col_facet = col_facet,
      expert_settings = expert_settings,
      colour = colour,
      fill = fill,
      size = size
    ),
    filters = "all"
  )
}


#' @importFrom shinyWidgets switchInput
ui_g_bivariate <- function(id, ...) {
  arguments <- list(...)

  # Set default values for expert settings in case those were not given
  if (is.null(arguments$colour) || length(arguments$colour) == 0) {
    a[["colour"]] <- arguments$x
  }
  if (is.null(arguments$fill) || length(arguments$fill) == 0) {
    a[["fill"]] <- arguments$x
  }
  if (is.null(arguments$size) || length(arguments$size) == 0) {
    a[["size"]] <- arguments$x
  }

  ns <- NS(id)

  standard_layout(
    output = white_small_well(plot_height_output(id = ns("myplot"))),
    encoding = div(
      helpText("Analysis data:", tags$code(paste(arguments$dataname, collapse = ", "))),
      data_extract_input(
        id = ns("x"),
        label = "X Variable",
        data_extract_spec = arguments$x
      ),
      data_extract_input(
        id = ns("y"),
        label = "Y Variable",
        data_extract_spec = arguments$y
      ),
      radioButtons(
        inputId = ns("use_density"),
        label = NULL,
        choices = c("frequency", "density"),
        selected = ifelse(arguments$use_density, "density", "frequency"),
        inline = TRUE
      ),
      div(
        style = "border: 1px solid #e3e3e3; border-radius: 5px; padding: 0.6em; margin-left: -0.6em",
        tags$label("Facetting:"),
        switchInput(inputId = ns("facetting"), value = TRUE, size = "small"),
        conditionalPanel(
          condition = paste0("input['", ns("facetting"), "']"),
          ui_facetting(ns, arguments$row_facet, arguments$col_facet, arguments$free_x_scales, arguments$free_y_scales)
        )
      ),
      if (arguments$expert_settings) {
        # Put a grey border around the expert settings
        div(
          style = "border: 1px solid #e3e3e3; border-radius: 5px; padding: 0.6em; margin-left: -0.6em",
          tags$label("Expert settings:"),
          switchInput(inputId = ns("expert"), value = FALSE, size = "small"),
          conditionalPanel(
            condition = paste0("input['", ns("expert"), "']"),
            ui_expert(ns, arguments$colour, arguments$fill, arguments$size)
          )
        )
      },
      plot_height_input(id = ns("myplot"), value = arguments$plot_height),
      optionalSelectInput(
        inputId = ns("ggtheme"),
        label = "Theme (by ggplot)",
        choices = c("grey", "gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test"),
        selected = arguments$ggtheme,
        multiple = FALSE
      )
    ),
    forms = if (arguments$with_show_r_code) {
      actionButton(ns("show_rcode"), "Show R Code", width = "100%")
    } else {
      NULL
    },
    pre_output = arguments$pre_output,
    post_output = arguments$post_output
  )
}

ui_facetting <- function(ns,
                         row_facet_spec,
                         col_facet_spec,
                         free_x_scales,
                         free_y_scales) {
  div(
    data_extract_input(
      id = ns("row_facet"),
      label = "Row facetting Variable",
      data_extract_spec = row_facet_spec
    ),
    data_extract_input(
      id = ns("col_facet"),
      label = "Column facetting Variable",
      data_extract_spec = col_facet_spec
    ),
    checkboxInput(ns("free_x_scales"), "free x scales", value = free_x_scales),
    checkboxInput(ns("free_y_scales"), "free y scales", value = free_y_scales)
  )
}

ui_expert <- function(ns,
                      colour_spec,
                      fill_spec,
                      size_spec) {
  div(
    data_extract_input(
      id = ns("colour"),
      label = "Colour by variable",
      data_extract_spec = colour_spec
    ),
    data_extract_input(
      id = ns("fill"),
      label = "Fill colour by variable",
      data_extract_spec = fill_spec
    ),
    data_extract_input(
      id = ns("size"),
      label = "Size of points by variable (only if x and y are numeric)",
      data_extract_spec = size_spec
    )
  )
}


#' @importFrom magrittr %>%
#' @importFrom methods is
#' @importFrom tern keys
srv_g_bivariate <- function(input,
                            output,
                            session,
                            datasets,
                            dataname,
                            x,
                            y,
                            row_facet,
                            col_facet,
                            expert_settings = FALSE,
                            colour,
                            fill,
                            size) {
  stopifnot(all(dataname %in% datasets$datanames()))

  init_chunks()

  # Data Extraction
  x_data <- callModule(data_extract_module,
                          id = "x",
                          datasets = datasets,
                          data_extract_spec = x
  )
  y_data <- callModule(data_extract_module,
                          id = "y",
                          datasets = datasets,
                          data_extract_spec = y
  )
  row_facet_data <- callModule(data_extract_module,
                                   id = "row_facet",
                                   datasets = datasets,
                                   data_extract_spec = row_facet
  )
  col_facet_data <- callModule(data_extract_module,
                                   id = "col_facet",
                                   datasets = datasets,
                                   data_extract_spec = col_facet
  )

  if (expert_settings) {
    colour_data <- callModule(data_extract_module,
                                  id = "colour",
                                  datasets = datasets,
                                  data_extract_spec = colour
    )
    fill_data <- callModule(data_extract_module,
                                id = "fill",
                                datasets = datasets,
                                data_extract_spec = fill
    )
    size_data <- callModule(data_extract_module,
                                id = "size",
                                datasets = datasets,
                                data_extract_spec = size
    )
  }

  # Merging data ::: Preparation
  data_to_merge <- function(do_expert) {
    standard_data <- list(
      x_data(),
      y_data(),
      row_facet_data(),
      col_facet_data()
    )
    expert_data <- list()
    if (do_expert) {
      expert_data <- list(
        colour_data(),
        fill_data(),
        size_data()
      )
    }
    all_data <- append(standard_data, expert_data)
    return(all_data)
  }

  # Merging data ::: Execution
  data_reactive <- reactive({
    merge_datasets(
      data_to_merge(expert_settings && input$expert)
    )
  })

  # Access variables ::: Pre-checks
  variable_reactive <- reactive({
    anl <- data_reactive()
    x_name <- get_dataset_prefixed_col_names(x_data())
    y_name <- get_dataset_prefixed_col_names(y_data())

    validate(need(!(!is.null(y_name) && y_name %in% keys(y_data())),
                  "Please do not select key variables inside data"))
    validate(need(!(!is.null(x_name) && x_name %in% keys(y_data())),
                  "Please do not select key variables inside data"))

    if (input$facetting) {
      row_facet_name <- get_dataset_prefixed_col_names(row_facet_data())
      col_facet_name <- get_dataset_prefixed_col_names(col_facet_data())

      validate(need(!(!is.null(col_facet_name) &&
                        col_facet_name %in% keys(col_facet_data())),
                    "Please do not select key variables inside data"))
      validate(need(!(!is.null(row_facet_name) &&
                        row_facet_name %in% keys(row_facet_data())),
                    "Please do not select key variables inside data"))

      if (!is.null(col_facet_name) && !is.null(row_facet_name)) {
        validate(need(
          length(intersect(row_facet_name, col_facet_name)) == 0,
          "x and y facet variables cannot overlap"
        ))
      }
    }

    if (expert_settings) {
      if (input$expert) {
        colour_name <- get_dataset_prefixed_col_names(colour_data())
        fill_name <- get_dataset_prefixed_col_names(fill_data())
        size_name <- get_dataset_prefixed_col_names(size_data())
        validate(need(!(!is.null(colour_name) && colour_name %in% keys(colour_data())),
                      "Please do not select key variables inside data"))
        validate(need(!(!is.null(fill_name) && fill_name %in% keys(fill_data())),
                      "Please do not select key variables inside data"))
        validate(need(!(!is.null(size_name) && size_name %in% keys(size_data())),
                      "Please do not select key variables inside data"))
      }
    }
    use_density <- input$use_density == "density"
    free_x_scales <- input$free_x_scales
    free_y_scales <- input$free_y_scales

    validate_has_data(anl, 10)
    validate(need(!is.null(x), "Please define a valid column for the X-variable"))

    return(environment())
  })

  # Insert the plot into a plot_height module from teal.devel
  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  output$plot <- renderPlot({

    validate(need(is.environment(variable_reactive()), "Error in your variable selection"))

    # Copy all variables over from variable_reactive
    for (n in ls(variable_reactive(), all.names = TRUE)) {
      assign(n, get(n, variable_reactive()), environment())
    }

    cl <- bivariate_plot_call(
      data_name = "anl",
      x = x_name,
      y = y_name,
      x_class = class(anl[[x_name]]),
      y_class = if (!is.null(y_name)) class(anl[[y_name]]) else NULL,
      freq = !use_density
    )

    if (input$facetting) {
      facet_cl <- facet_ggplot_call(row_facet_name, col_facet_name, free_x_scales, free_y_scales)

      if (!is.null(facet_cl)) {
        cl <- call("+", cl, facet_cl)
      }
    }

    expert_cl <- NULL
    if (expert_settings) {
      if (input$expert) {
        expert_cl <- expert_ggplot_call(
          colour = colour_name, fill = fill_name, size = size_name,
          is_point = any(grepl("geom_point", cl %>% deparse()))
        )
      }
      if (!is.null(expert_cl)) {
        cl <- call("+", cl, expert_cl)
      }
    }

    ggtheme <- input$ggtheme
    if (!is.null(ggtheme)) {
      cl <- call("+", cl, as.call(parse(text = paste0("theme_", ggtheme))))
    }

    chunks_reset()

    chunks_push(expression = cl, id = "plotCall")

    p <- chunks_eval()

    chunks_validate_is_ok()

    p
  })


  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Bivariate Plot",
      rcode = get_rcode(
        datasets = datasets,
        merge_expression = "",
        title = "Bivariate Plot"
      )
    )
  })
}


#' Get Substituted ggplot call
#'
#' @noRd
#'
#' @examples
#'
#' bivariate_plot_call("ANL", "BAGE", "RACE", "numeric", "factor")
#' bivariate_plot_call("ANL", "BAGE", NULL, "numeric", "NULL")
bivariate_plot_call <- function(data_name,
                                x,
                                y = NULL,
                                x_class,
                                y_class,
                                freq = TRUE,
                                col_var = NULL,
                                x_facet = NULL,
                                y_facet = NULL) {
  cl <- bivariate_ggplot_call(x_class = x_class, y_class = y_class, freq = freq)

  if (is.null(x)) {
    x <- "-"
  }
  if (is.null(y)) {
    y <- "-"
  }

  cl_plot <- substitute_q(cl, list(
    .ggplotcall = bquote(ggplot(.(as.name(data_name)))),
    .x = if (is.call(x)) x else as.name(x),
    .y = if (is.call(y)) y else as.name(y)
  ))

  cl_plot
}

substitute_q <- function(x, env) {
  stopifnot(is.language(x))
  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}


#' Create ggplot part of plot call
#'
#' Due to the type of the x and y variable the plot type is chosen
#'
#' @noRd
#'
#' @examples
#' bivariate_ggplot_call("numeric", "NULL")
#' bivariate_ggplot_call("numeric", "NULL", freq = FALSE)
#'
#' bivariate_ggplot_call("NULL", "numeric")
#' bivariate_ggplot_call("NULL", "numeric", freq = FALSE)
#'
#' bivariate_ggplot_call("NULL", "factor")
#' bivariate_ggplot_call("NULL", "factor", freq = FALSE)
#'
#' bivariate_ggplot_call("factor", "NULL")
#' bivariate_ggplot_call("factor", "NULL", freq = FALSE)
#'
#' bivariate_ggplot_call("numeric", "numeric")
#' bivariate_ggplot_call("numeric", "factor")
#' bivariate_ggplot_call("factor", "numeric")
#' bivariate_ggplot_call("factor", "factor")
bivariate_ggplot_call <- function(x_class = c("NULL", "numeric", "integer", "factor", "character", "logical"),
                                  y_class = c("NULL", "numeric", "integer", "factor", "character", "logical"),
                                  freq = TRUE) {
  x_class <- match.arg(x_class)
  y_class <- match.arg(y_class)

  if (x_class %in% c("character", "logical")) {
    x_class <- "factor"
  }
  if (x_class %in% c("integer")) {
    x_class <- "numeric"
  }
  if (y_class %in% c("character", "logical")) {
    y_class <- "factor"
  }
  if (y_class %in% c("integer")) {
    y_class <- "numeric"
  }

  if (all(c(x_class, y_class) == "NULL")) {
    stop("either x or y is required")
  }

  # Single data plots
  if (x_class == "numeric" && y_class == "NULL") {
    if (freq) {
      quote(.ggplotcall + aes(x = .x) + geom_histogram() + ylab("Frequency"))
    } else {
      quote(.ggplotcall + aes(x = .x) + geom_histogram(aes(y = ..density..)) + ylab("Density")) # nolint
    }
  } else if (x_class == "NULL" && y_class == "numeric") {
    if (freq) {
      quote(.ggplotcall + aes(x = .y) + geom_histogram() + ylab("Frequency") + coord_flip())
    } else {
      quote(.ggplotcall + aes(x = .y) + geom_histogram(aes(y = ..density..)) + ylab("Density") + coord_flip()) # nolint
    }
  } else if (x_class == "factor" && y_class == "NULL") {
    if (freq) {
      quote(.ggplotcall + aes(x = .x) + geom_bar() + ylab("Frequency"))
    } else {
      quote(.ggplotcall + aes(x = .x) + geom_bar(aes(y = ..prop.., group = 1)) + ylab("Proportion")) # nolint
    }
  } else if (x_class == "NULL" && y_class == "factor") {
    if (freq) {
      quote(.ggplotcall + aes(x = .y, fill = factor(.fill)) + geom_bar() + ylab("Frequency") + coord_flip()) # nolint
    } else {
      quote(.ggplotcall + aes(x = .y) + geom_bar(aes(y = ..prop.., group = 1)) + # nolint
              ylab("Proportion") + coord_flip())
    }

    # Numeric Plots
  } else if (x_class == "numeric" && y_class == "numeric") {
    quote(.ggplotcall + aes(x = .x, y = .y) + geom_point())
  } else if (x_class == "numeric" && y_class == "factor") {
    quote(.ggplotcall + aes(x = .y, y = .x) + geom_boxplot() + coord_flip())
  } else if (x_class == "factor" && y_class == "numeric") {
    quote(.ggplotcall + aes(x = .x, y = .y) + geom_boxplot())

    # Factor and character plots
  } else if (x_class == "factor" && y_class == "factor") {
    quote(.ggplotcall + geom_mosaic(aes(x = product(.x), fill = .y), na.rm = TRUE))
  } else {
    stop("x y type combination not allowed")
  }
}


#' Create facet call
#'
#' @noRd
#'
#' @examples
#'
#' facet_ggplot_call(LETTERS[1:3])
#' facet_ggplot_call(NULL, LETTERS[23:26])
#' facet_ggplot_call(LETTERS[1:3], LETTERS[23:26])
facet_ggplot_call <- function(row_facet = NULL,
                              col_facet = NULL,
                              free_x_scales = FALSE,
                              free_y_scales = FALSE) {
  scales <- if (free_x_scales && free_y_scales) {
    "free"
  } else if (free_x_scales) {
    "free_x"
  } else if (free_y_scales) {
    "free_y"
  } else {
    "fixed"
  }

  if (is.null(row_facet) && is.null(col_facet)) {
    NULL
  } else if (!is.null(row_facet) && is.null(col_facet)) {
    call("facet_grid", rows = call_fun_dots("vars", row_facet), scales = scales)
  } else if (is.null(row_facet) && !is.null(col_facet)) {
    call("facet_grid", cols = call_fun_dots("vars", col_facet), scales = scales)
  } else {
    call("facet_grid",
         rows = call_fun_dots("vars", row_facet),
         cols = call_fun_dots("vars", col_facet),
         scales = scales
    )
  }
}

expert_ggplot_call <- function(colour,
                               fill,
                               size,
                               is_point = FALSE) {
  if (!is.null(colour) && !is.null(fill) && is_point && !is.null(size)) {
    bquote(aes(
      colour = .(as.name(colour)),
      fill = .(as.name(fill)),
      size = .(as.name(size))
    ))
  } else if (!is.null(colour) && !is.null(fill) && (!is_point || is.null(size))) {
    bquote(aes(
      colour = .(as.name(colour)),
      fill = .(as.name(fill))
    ))
  } else if (!is.null(colour) && is.null(fill) && (!is_point || is.null(size))) {
    bquote(aes(
      colour = .(as.name(colour))
    ))
  } else if (is.null(colour) && !is.null(fill) && (!is_point || is.null(size))) {
    bquote(aes(
      fill = .(as.name(fill))
    ))
  } else if (is.null(colour) && is.null(fill) && is_point && !is.null(size)) {
    bquote(aes(
      size = .(as.name(size))
    ))
  } else if (!is.null(colour) && is.null(fill) && is_point && !is.null(size)) {
    bquote(aes(
      colour = .(as.name(colour)),
      size = .(as.name(size))
    ))
  } else if (is.null(colour) && !is.null(fill) && is_point && !is.null(size)) {
    bquote(aes(
      fill = .(as.name(fill)),
      size = .(as.name(size))
    ))
  } else {
    NULL
  }
}
