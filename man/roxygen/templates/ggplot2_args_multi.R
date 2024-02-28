#' @param ggplot2_args (`ggplot2_args` or `named list` of `ggplot2_args`s) optional.
#' Object created by [`teal.widgets::ggplot2_args()`] and contains settings for all the plots,
#' or is a named `list` of `ggplot2_args` objects for plot-specific settings.
#' The argument is merged with options variable `teal.ggplot2_args` and default module setup.
#'
#' List names should match the following: `c("default", <%=ggnames%>)`.
#'
#' For more details see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`.
