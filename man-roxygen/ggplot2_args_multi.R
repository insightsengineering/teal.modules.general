#' @param ggplot2_args optional, (`ggplot2_args`) object created by [`teal.devel::ggplot2_args()`]
#'  with settings for all the plots or named list of `ggplot2_args` objects for plot-specific settings.
#'  For more details see the help vignette:
#'  `vignette("Custom ggplot2 arguments module", package = "teal.devel")`.
#'  List names should match the following:\cr
#'  `c("default", <%=ggnames%>)`.
#'  The argument is merged with options variable `teal.ggplot2_args` and default module setup.
