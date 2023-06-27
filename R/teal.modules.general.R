#' teal.modules.general: General modules to add to a teal application
#'
#' The modules in this package are generic modules that should work with any data set
#' (not necessarily for clinical trials data).
#'
#' @import ggplot2
#' @import ggmosaic
#' @import shiny
#' @import shinyTree
#' @import teal
#' @importFrom magrittr %>%
#'
#'
#' @docType package
#' @name teal.modules.general
#' @keywords internal
NULL

# nolint start
# Note ggmosaic (version <= 0.3.3) needs to be in DEPENDS as the following does not work if it is imported
# df <- data.frame(x = c("A", "B", "C", "A"), y = c("Z", "Z", "W", "W"))
# ggplot(df) +  ggmosaic::geom_mosaic(aes(x = ggmosaic::product(x), fill = y))
# nolint end

# Needed to avoid R CMD note on no visible binding
utils::globalVariables(c(
  "count", ".", "ADT", "ADY", "AENDT", "AENDY", "ANRIND", "ASTDT", "ASTDY", "AVAL", "AVALC", "AVALU", "AVISITN",
  "BASETYPE", "DTHDT", "LBSEQ", "LBTESTCD", "LSTALVDT", "PARAM", "PARAMCD", "PARCAT1", "PARCAT2",
  "STUDYID", "USUBJID", "conmed_level", "conmed_var", "conmed_var_name", "line_col", "line_col_legend", "line_col_opt"
))
