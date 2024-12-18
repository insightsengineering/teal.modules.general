# nocov start
roxygen_decorators_param <- function(module_name) {
  paste(
    sep = " ",
    lifecycle::badge("experimental"),
    " (`list` of `teal_transform_module`, named `list` of `teal_transform_module` or",
    "`NULL`) optional, if not `NULL`, decorator for tables or plots included in the module.",
    "When a named list of `teal_transform_module`, the decorators are applied to the",
    "respective output objects.\n\n",
    "Otherwise, the decorators are applied to all objects, which is equivalent as using the name `default`.\n\n",
    sprintf("See section \"Decorating `%s`\"", module_name),
    "below for more details."
  )
}

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
