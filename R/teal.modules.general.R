#' `teal.modules.general`: General modules to add to a `teal` application
#'
#' The modules in this package are generic modules that should work with any data set
#' (not necessarily for clinical trials data).
#'
#' @import ggplot2
#' @import shiny
#' @import teal
#' @import teal.transform
#' @importFrom dplyr %>%
#'
#'
#' @name teal.modules.general
#' @keywords internal
"_PACKAGE"

# nolint start.
# @import ggmosaic
# Note ggmosaic (version <= 0.3.3) needs to be in DEPENDS as the following does not work if it is imported
# df <- data.frame(x = c("A", "B", "C", "A"), y = c("Z", "Z", "W", "W"))
# ggplot(df) + ggmosaic::geom_mosaic(aes(x = ggmosaic::product(x), fill = y))
# nolint end

# Needed to avoid R CMD note on no visible binding
utils::globalVariables("count")
