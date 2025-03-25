# nocov start
roxygen_ggplot2_args_param <- function(...) {
  paste(
    sep = " ",
    "(`ggplot2_args`) optional, object created by [`teal.widgets::ggplot2_args()`]",
    "with settings for all the plots or named list of `ggplot2_args` objects for plot-specific settings.",
    "The argument is merged with options variable `teal.ggplot2_args` and default module setup.\n\n",
    sprintf(
      "List names should match the following: `c(\"default\", %s)`.\n\n",
      paste("\"", unlist(list(...)), "\"", collapse = ", ", sep = "")
    ),
    "For more details see the vignette: `vignette(\"custom-ggplot2-arguments\", package = \"teal.widgets\")`."
  )
}

# nocov end

#' Shared parameters documentation
#'
#' Defines common arguments shared across multiple functions in the package
#' to avoid repetition by using `inheritParams`.
#'
#' @param plot_height (`numeric`) optional, specifies the plot height as a three-element vector of
#' `value`, `min`, and `max` intended for use with a slider UI element.
#' @param plot_width (`numeric`) optional, specifies the plot width as a three-element vector of
#' `value`, `min`, and `max` for a slider encoding the plot width.
#' @param rotate_xaxis_labels (`logical`) optional, whether to rotate plot X axis labels. Does not
#' rotate by default (`FALSE`).
#' @param ggtheme (`character`) optional, `ggplot2` theme to be used by default. Defaults to `"gray"`.
#' @param ggplot2_args (`ggplot2_args`) object created by [teal.widgets::ggplot2_args()]
#' with settings for the module plot.
#' The argument is merged with options variable `teal.ggplot2_args` and default module setup.
#'
#' For more details see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`
#' @param basic_table_args (`basic_table_args`) object created by [teal.widgets::basic_table_args()]
#' with settings for the module table.
#' The argument is merged with options variable `teal.basic_table_args` and default module setup.
#'
#' For more details see the vignette: `vignette("custom-basic-table-arguments", package = "teal.widgets")`
#' @param pre_output (`shiny.tag`) optional, text or UI element to be displayed before the module's output,
#' providing context or a title.
#'  with text placed before the output to put the output into context. For example a title.
#' @param post_output (`shiny.tag`) optional, text or UI element to be displayed after the module's output,
#' adding context or further instructions. Elements like `shiny::helpText()` are useful.
#' @param alpha (`integer(1)` or `integer(3)`) optional, specifies point opacity.
#' - When the length of `alpha` is one: the plot points will have a fixed opacity.
#' - When the length of `alpha` is three: the plot points opacity are dynamically adjusted based on
#' vector of `value`, `min`, and `max`.
#' @param size (`integer(1)` or `integer(3)`) optional, specifies point size.
#' - When the length of `size` is one: the plot point sizes will have a fixed size.
#' - When the length of `size` is three: the plot points size are dynamically adjusted based on
#' vector of `value`, `min`, and `max`.
#' @param decorators `r lifecycle::badge("experimental")`
#' (named `list` of lists of `teal_transform_module`) optional,
#' decorator for tables or plots included in the module output reported.
#' The decorators are applied to the respective output objects.
#'
#' @param table_datanames (`character`) names of the datasets which should be listed below the plot 
#'  when some data points are selected. Objects named after `table_datanames` will be pulled from
#'  `data` so it is important that data actually contains these datasets. Please be aware that
#'  table datasets must be linked with `plot_dataname` by the relevant [join_keys()].  
#' See section "Decorating Module" below for more details.
#'
#' @return Object of class `teal_module` to be used in `teal` applications.
#'
#' @name shared_params
#' @keywords internal
NULL