.onLoad <- function(libname, pkgname) {
  teal.logger::register_logger(namespace = "teal.modules.general")
  teal.logger::register_handlers("teal.modules.general")
}

### global variables
ggplot_themes <- c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void")

#' @importFrom lifecycle deprecated
#' @importFrom rlang :=
#' @importFrom forcats fct_na_value_to_level
#' @importFrom forcats fct_rev
#' @importFrom forcats fct_infreq
#' @import tidyselect
interactive <- NULL
