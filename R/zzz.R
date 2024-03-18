.onLoad <- function(libname, pkgname) {
  teal.logger::register_logger(namespace = "teal.modules.general")
}

### global variables
ggplot_themes <- c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void")
