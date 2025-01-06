# nocov start
roxygen_ggplot2_args_param <- function(...) {
  paste(
    sep = " ",
    "(`ggplot2_args`) optional, object created by [`teal.widgets::ggplot2_args()`]",
    "with settings for all the plots or named list of `ggplot2_args` objects for plot-specific settings.",
    "The argument is merged with options variable `teal.ggplot2_args` and default module setup.\n\n",
    sprintf(
      "List names should match the following: `c(\"default\", %s)`.\n\n",
      paste("\"", unlist(rlang::list2(...)), "\"", collapse = ", ", sep = "")
    ),
    "For more details see the vignette: `vignette(\"custom-ggplot2-arguments\", package = \"teal.widgets\")`."
  )
}

# nocov end
