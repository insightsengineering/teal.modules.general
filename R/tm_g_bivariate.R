#' Univariate and bivariate visualizations.
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param label (\code{character}) Label of the module
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
#'
#'
#'
#' @details
#' This is a general module to visualize 1 & 2 dimensional data.
#'
#' @importFrom methods is
#' @export
#'
#'
#' @examples
#' # datasets: single wide
#' # Bivariate plot of selected variable (AGE) against selected (SEX)
#' library(random.cdisc.data)
#' ADSL <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- cadsl",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_bivariate(
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = names(ADSL),
#'           selected = "AGE",
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = names(ADSL),
#'           selected = "SEX",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       row_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = names(ADSL),
#'           selected = NULL,
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = names(ADSL),
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
#' # Bivariate plot with RACE (factor) plotted over AGE (numeric
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- cadsl
#' ADSL <- mutate_at(ADSL,
#'  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'  .funs = list(~as.factor(.)))
#'
#' ADSL_2 <- mutate_at(cadsl,
#'   .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'   .funs = list(~as.factor(.)))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     dataset("ADSL_2", ADSL_2, keys = list(primary = c("STUDYID", "USUBJID"),
#'                                           foreign = NULL,
#'                                           parent = NULL)),
#'     code = 'ADSL <- cadsl
#'             ADSL_2 <- mutate_at(cadsl,
#'                .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                .funs = list(~as.factor(.)))',
#'     check = FALSE #TODO
#'   ),
#'   modules = root_modules(
#'     tm_g_bivariate(
#'       label = "Association plots",
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("AGE", "SEX", "STRATA1", "RACE"),
#'           selected = c("AGE"),
#'           multiple = FALSE
#'         )),
#'       y = data_extract_spec(
#'         dataname = "ADSL_2",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = c("COUNTRY", "AGE", "RACE"),
#'           selected = "RACE",
#'           multiple = FALSE
#'         )),
#'       # Row facetting by first data set
#'       row_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'          label = "Select variable:",
#'          choices = names(ADSL),
#'          selected = NULL,
#'          multiple = FALSE,
#'          fixed = FALSE
#'         )
#'       ),
#'       # Col facetting by second data set
#'       col_facet = data_extract_spec(
#'         dataname = "ADSL_2",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = names(ADSL),
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
#' # Bivariate plot of different parameters from ADRS or ADTTE datasets
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
#'     tm_g_bivariate(
#'       label = "Bivariate Plots of two long datasets",
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
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADTTE",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("AVAL", "CNSR"),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         ),
#'         filter = filter_spec(
#'           label = "Select endpoint:",
#'           vars = c("PARAMCD"),
#'           choices = unique(ADTTE$PARAMCD),
#'           selected = "OS",
#'           multiple = FALSE
#'         )
#'       ),
#'       row_facet = data_extract_spec(
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
#'           choices = c("AVAL", "AVISIT"),
#'           selected = "AVISIT",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = c("SEX", "RACE"),
#'           selected = NULL,
#'           multiple = TRUE,
#'           fixed = FALSE
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
#' # Bivariate plot with biomarker measurement (ADSL$BMRKR1) plotted over
#' # date of response (ADRS$AVAL)
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#'
#' app <- init(
#'  data = cdisc_data(
#'    cdisc_dataset("ADSL", ADSL),
#'    cdisc_dataset("ADRS", ADRS),
#'    code = "ADSL <- cadsl; ADRS <- cadrs",
#'    check = TRUE
#'  ),
#'  modules = root_modules(
#'    tm_g_bivariate(
#'      label = "Association Plots",
#'      x = data_extract_spec(
#'        dataname = "ADRS",
#'        filter = list(
#'          filter_spec(
#'            vars = "PARAM",
#'            choices = levels(ADRS$PARAM),
#'            selected = levels(ADRS$PARAM)[1],
#'            multiple = FALSE,
#'            label = "Select response:"
#'          ),
#'          filter_spec(
#'            vars = "AVISIT",
#'            choices = levels(ADRS$AVISIT),
#'            selected = levels(ADRS$AVISIT)[1],
#'            multiple = FALSE,
#'            label = "Select visit:"
#'          )
#'        ),
#'        select = select_spec(
#'          choices = "AVAL",
#'          selected = "AVAL",
#'          multiple = FALSE,
#'          label = "Select variable:",
#'          fixed = TRUE
#'        )
#'     ),
#'      y = data_extract_spec(
#'        dataname = "ADSL",
#'        select = select_spec(
#'          choices = c("BMRKR1", "SEX", "AGE", "RACE", "COUNTRY"),
#'          selected = "BMRKR1",
#'          multiple = FALSE,
#'          label = "Select variable:",
#'          fixed = FALSE
#'        )
#'      ),
#'      row_facet = data_extract_spec(
#'        dataname = "ADRS",
#'        select = select_spec(
#'           choices = c(NULL, "PARAMCD"),
#'           selected = "PARAMCD",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'      ),
#'      col_facet = data_extract_spec(
#'        dataname = "ADRS",
#'        select = select_spec(
#'           choices = c(NULL, "AVISIT"),
#'           selected = "AVISIT",
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
#' # datasets: wide, long, long
#'
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#' ADLB <- cadlb
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = "ADSL <- cadsl; ADRS <- cadrs; ADLB <- cadlb",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_bivariate(
#'       label = "Association Plots",
#'       x = data_extract_spec(
#'         dataname = "ADRS",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADRS$PARAMCD),
#'             selected = levels(ADRS$PARAMCD)[1],
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
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = c("BMRKR1", "SEX", "AGE", "RACE", "COUNTRY"),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       row_facet = data_extract_spec(
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
#'           choices = c(NULL, "ARMCD"),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = c("SEX", "AGE", "RACE", "COUNTRY"),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
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
#' # datasets: same dataset long
#' # Bivariate plot of AVAL vs biomarker value split by PARAMCD and AVISIT
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
#'     tm_g_bivariate(
#'       x = data_extract_spec(
#'         dataname = "ADRS",
#'         select = select_spec(
#'           choices = c("AVAL"),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADRS",
#'         select = select_spec(
#'           choices = c("BMRKR1", "BMRKR2"),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       row_facet = data_extract_spec(
#'         dataname = "ADRS",
#'         select = select_spec(
#'           choices = "PARAMCD",
#'           selected = "PARAMCD",
#'           multiple = TRUE,
#'           fixed = FALSE,
#'           label = "Select variables:"
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ADRS",
#'         select = select_spec(
#'           choices = "AVISIT",
#'           selected = "AVISIT",
#'           multiple = TRUE,
#'           fixed = FALSE,
#'           label = "Select variables:"
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#'   shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: different subsets of long dataset
#' # Bivariate plot of one lab measurement versus another, optionally split by additional variables
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
#'     tm_g_bivariate(
#'       x = data_extract_spec(
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
#'             vars = "STRATA1",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[2],
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
#'       y = data_extract_spec(
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
#'             vars = "STRATA1",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[2],
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
#'       use_density = FALSE,
#'       row_facet = data_extract_spec(
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
#'             vars = "STRATA1",
#'             choices = levels(ADLB$STRATA1),
#'             selected = levels(ADLB$STRATA1)[1],
#'             multiple = FALSE,
#'             label = "Select category:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = c("RACE", "SEX", "ARMCD", "ACTARMCD"),
#'           selected = NULL,
#'           multiple = TRUE,
#'           fixed = FALSE,
#'           label = "Select variable:"
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
#'             label = "Select lab:"
#'           ),
#'           filter_spec(
#'             vars = "STRATA1",
#'             choices = levels(ADLB$STRATA1),
#'             selected = levels(ADLB$STRATA1)[1],
#'             multiple = FALSE,
#'             label = "Select category:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = c("RACE", "SEX", "ARMCD", "ACTARMCD"),
#'           selected = NULL,
#'           multiple = TRUE,
#'           fixed = FALSE,
#'           label = "Select variables:"
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
tm_g_bivariate <- function(label = "Bivariate Plots",
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
  stopifnot(is.class.list("data_extract_spec")(x) || is(x, "data_extract_spec"))
  stopifnot(is.class.list("data_extract_spec")(y) || is(y, "data_extract_spec"))
  stopifnot(is.class.list("data_extract_spec")(row_facet) || is(row_facet, "data_extract_spec"))
  stopifnot(is.class.list("data_extract_spec")(col_facet) || is(col_facet, "data_extract_spec"))
  stopifnot(is.null(colour) || is.class.list("data_extract_spec")(colour) || is(colour, "data_extract_spec"))
  stopifnot(is.null(fill) || is.class.list("data_extract_spec")(fill) || is(fill, "data_extract_spec"))
  stopifnot(is.null(size) || is.class.list("data_extract_spec")(size) || is(size, "data_extract_spec"))
  if (is.class.list("data_extract_spec")(x)) {
    stop_if_not(list(all(vapply(x, function(xx) !isTRUE(xx$select$multiple), logical(1))),
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
      colour[[1]]$select["selected"] <- list(NULL)
    }
    if (is.null(fill)) {
      fill <- `if`(inherits(x, "list"), x, list(x))
      fill[[1]]$select["selected"] <- list(NULL)
    }
    if (is.null(size)) {
      size <- `if`(inherits(x, "list"), x, list(x))
      size[[1]]$select["selected"] <- list(NULL)
    }
  }

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_bivariate,
    ui = ui_g_bivariate,
    ui_args = args,
    server_args = list(
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


#' @importFrom shinyWidgets radioGroupButtons switchInput
ui_g_bivariate <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      tags$div(verbatimTextOutput(ns("outtext"))),
      tags$div(verbatimTextOutput(ns("strtext"))),
      tags$div(DT::dataTableOutput(ns("outtable")))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("x", "y", "row_facet", "col_facet", "colour", "fill", "size")]),
      data_extract_input(
        id = ns("xyz_1"),
        label = "X variable",
        data_extract_spec = args$x
      ),
      data_extract_input(
        id = ns("xyz_2"),
        label = "Y variable",
        data_extract_spec = args$y
      ),
      radioGroupButtons(
        inputId = ns("use_density"),
        label = NULL,
        choices = c("frequency", "density"),
        selected = ifelse(args$use_density, "density", "frequency"),
        justified = TRUE
      ),
      div(
        class = "data-extract-box",
        tags$label("Facetting"),
        switchInput(inputId = ns("facetting"), value = TRUE, size = "mini"),
        conditionalPanel(
          condition = paste0("input['", ns("facetting"), "']"),
          div(
            data_extract_input(
              id = ns("xyz_3"),
              label = "Row facetting variable",
              data_extract_spec = args$row_facet
            ),
            data_extract_input(
              id = ns("xyz_4"),
              label = "Column facetting variable",
              data_extract_spec = args$col_facet
            ),
            checkboxInput(ns("free_x_scales"), "free x scales", value = args$free_x_scales),
            checkboxInput(ns("free_y_scales"), "free y scales", value = args$free_y_scales)
          )
        )
      ),
      if (args$expert_settings) {
        # Put a grey border around the expert settings
        div(
          class = "data-extract-box",
          tags$label("Expert settings"),
          switchInput(inputId = ns("expert"), value = FALSE, size = "mini"),
          conditionalPanel(
            condition = paste0("input['", ns("expert"), "']"),
            div(
              data_extract_input(
                id = ns("xyz_5"),
                label = "Colour by variable",
                data_extract_spec = args$colour
              ),
              data_extract_input(
                id = ns("xyz_6"),
                label = "Fill colour by variable",
                data_extract_spec = args$fill
              ),
              data_extract_input(
                id = ns("xyz_7"),
                label = "Size of points by variable (only if x and y are numeric)",
                data_extract_spec = args$size
              )
            )
          )
        )
      }
    ),
    forms = if (args$with_show_r_code) {
      actionButton(ns("show_rcode"), "Show R code", width = "100%")
    } else {
      NULL
    },
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}


#' @importFrom magrittr %>%
#' @importFrom methods is
srv_g_bivariate <- function(input,
                            output,
                            session,
                            datasets,
                            x,
                            y,
                            row_facet,
                            col_facet,
                            expert_settings = FALSE,
                            colour,
                            fill,
                            size) {
  init_chunks(session)
  dataname <- get_extract_datanames(list(x, y, row_facet, col_facet, colour, fill, size))
  data_extract <- if (expert_settings) {
    list(x, y, row_facet, col_facet, colour, fill, size)
  } else {
    list(x, y, row_facet, col_facet)
  }

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = data_extract,
    input_id = paste0("xyz_", seq_along(data_extract))
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
