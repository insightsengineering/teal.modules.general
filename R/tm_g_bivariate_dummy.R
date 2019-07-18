#' Univariate and bivariate visualizations.
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param label (\code{character}) Label of the module
#' @param dataname (\code{character}) name of datasets used to generate the bivariate plot. You need
#'   to name all datasets used in the available \code{data_extract_spec}
#' @param xvar (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable name selected to plot along the x-axis by default. Variable can be numeric, factor or character.
#'   No empty selections are allowed!
#' @param yvar (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable name selected to plot along the y-axis by default. Variable can be numeric, factor or character.
#' @param use_density (\code{logical}) value for whether density (\code{TRUE}) is plotted or frequency (\code{FALSE})
#' @param row_facet_var (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable for row facetting
#' @param col_facet_var (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable for col facetting
#' @param expert_settings (\code{logical}) Whether coloring, filling and size should be chosen
#'   by the user
#' @param colour_var optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable selection for the colouring inside the expert settings
#' @param fill_var optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable selection for the filling inside the expert settings
#' @param size_var optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable selection for the size of \code{geom_point} plots inside the expert settings
#' @param free_x_scales (\code{logical}) If X scaling shall be changeable
#' @param free_y_scales (\code{logical}) If Y scaling shall be changeable
#' @param plot_height (\code{numeric}) \code{c(value, min and max)} of plot height slider
#' @param with_show_r_code (\code{logical}) Whether show R code button shall be shown
#' @param ggtheme (\code{character}) ggplot theme to be used by default. All themes can be chosen by the user.
#'
#' @details
#' This is a general module to visualize 1 & 2 dimensional data.
#'
#' @importFrom methods is
#'
#' @noRd
#'
#' @examples
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ARS <- cadrs
#'
#' keys(ASL) <- c("STUDYID", "USUBJID")
#' keys(ARS) <- c("STUDYID", "USUBJID", "PARAMCD")
#'
#' ars_filters <- filter_spec(
#'     vars = c("PARAMCD"),
#'     sep = " - ",
#'     choices = c("BESRSPI", "INVET"),
#'     selected = "BESRSPI",
#'     multiple = FALSE,
#'     label = "Choose endpoint"
#' )
#' ars_extracted_response <- data_extract_spec(
#'     dataname = "ARS",
#'     filter = ars_filters,
#'     columns = columns_spec(
#'         choices = c("","AVAL", "AVALC"),
#'         selected = "AVALC",
#'         multiple = FALSE,
#'         fixed = FALSE,
#'         label = "variable"
#'     )
#' )
#' asl_extracted <- data_extract_spec(
#'     dataname = "ASL",
#'     columns = columns_spec(
#'         choices = c(base::setdiff(names(ASL), keys(ASL))), # strict call of setdiff
#'         selected = c("AGE"),
#'         multiple = FALSE,
#'         fixed = FALSE,
#'         label = "variable"
#'     )
#' )
#' asl_extracted_row <- data_extract_spec(
#'     dataname = "ASL",
#'     columns = columns_spec(
#'         choices = c("","SEX", "RACE"),
#'         selected = "",
#'         multiple = TRUE,
#'         fixed = FALSE,
#'         label = "variable"
#'     )
#' )
#'
#' app <- init(
#'  data = cdisc_data(
#'    ASL = ASL,
#'    ARS = ARS,
#'    code = 'ASL <- cadsl
#'           ARS <- cadrs
#'           keys(ASL) <- c("STUDYID", "USUBJID")
#'           keys(ARS) <- c("STUDYID", "USUBJID", "PARAMCD")',
#'    check = FALSE),
#'  modules = root_modules(
#'    tm_g_bivariate_dummy(
#'      dataname = c("ASL","ARS"),
#'      xvar = asl_extracted,
#'      yvar = ars_extracted_response,
#'      use_density = FALSE,
#'      row_facet_var = asl_extracted_row,
#'      col_facet_var = asl_extracted_row,
#'      expert_settings = TRUE,
#'      plot_height = c(600, 200, 2000),
#'      ggtheme = "grey"
#'    )
#'  )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_bivariate_dummy <- function(label = "Bivariate Plots",
                           dataname,
                           xvar,
                           yvar,
                           row_facet_var,
                           col_facet_var,
                           colour_var = NULL,
                           fill_var = NULL,
                           size_var = NULL,
                           use_density = FALSE,
                           expert_settings = TRUE,
                           free_x_scales = FALSE,
                           free_y_scales = FALSE,
                           plot_height = c(600, 200, 2000),
                           ggtheme = "minimal",
                           with_show_r_code = TRUE,
                           pre_output = NULL,
                           post_output = NULL) {
  if (!is.class.list("data_extract_spec")(xvar)) {
    xvar <- list(xvar)
  }
  if (!is.class.list("data_extract_spec")(yvar)) {
    yvar <- list(yvar)
  }
  if (!is.class.list("data_extract_spec")(row_facet_var)) {
    row_facet_var <- list(row_facet_var)
  }
  if (!is.class.list("data_extract_spec")(col_facet_var)) {
    col_facet_var <- list_or_null(col_facet_var)
  }
  if (!is.class.list("data_extract_spec")(colour_var)) {
    colour_var <- list(colour_var)
  }
  if (!is.class.list("data_extract_spec")(fill_var)) {
    fill_var <- list_or_null(fill_var)
  }
  if (!is.class.list("data_extract_spec")(size_var)) {
    size_var <- list_or_null(size_var)
  }

  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  stopifnot(is.class.list("data_extract_spec")(xvar))
  stop_if_not(list(all(vapply(xvar, function(x) !("" %in% x$columns$choices), logical(1))),
                   "'xvar' should not allow empty values")) # todo: move to data_extract
  stop_if_not(list(all(vapply(xvar, function(x) !(x$columns$multiple), logical(1))),
                   "'xvar' should not allow multiple selection"))
  stopifnot(is.class.list("data_extract_spec")(yvar))
  stopifnot(is.logical.single(use_density))
  stopifnot(is.class.list("data_extract_spec")(row_facet_var))
  stopifnot(is.class.list("data_extract_spec")(col_facet_var))
  stopifnot(is.logical.single(expert_settings))
  stopifnot(is.null(colour_var) || is.class.list("data_extract_spec")(colour_var))
  stopifnot(is.null(fill_var) || is.class.list("data_extract_spec")(fill_var))
  stopifnot(is.null(size_var) || is.class.list("data_extract_spec")(size_var))
  stopifnot(is.logical.single(free_x_scales))
  stopifnot(is.logical.single(free_y_scales))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])
  stopifnot(is.character.single(ggtheme))
  stopifnot(is.logical.single(with_show_r_code))

  if (expert_settings) {
    if (is.null(colour_var)) {
      colour_var <- `if`(inherits(xvar, "list"), xvar, list(xvar))
      colour_var[[1]]$columns$selected <- ""
      colour_var[[1]]$columns$choices <- c("", colour_var[[1]]$columns$choices)
    }
    if (is.null(fill_var)) {
      fill_var <- `if`(inherits(xvar, "list"), xvar, list(xvar))
      fill_var[[1]]$columns$selected <- ""
      fill_var[[1]]$columns$choices <- c("", fill_var[[1]]$columns$choices)
    }
    if (is.null(size_var)) {
      size_var <- `if`(inherits(xvar, "list"), xvar, list(xvar))
      size_var[[1]]$columns$selected <- ""
      size_var[[1]]$columns$selected <- ""
      size_var[[1]]$columns$choices <- c("", size_var[[1]]$columns$choices)
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
      xvar = xvar,
      yvar = yvar,
      row_facet_var = row_facet_var,
      col_facet_var = col_facet_var,
      expert_settings = expert_settings,
      colour_var = colour_var,
      fill_var = fill_var,
      size_var = size_var
    ),
    filters = "all"
  )
}
