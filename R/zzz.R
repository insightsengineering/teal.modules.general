.onLoad <- function(libname, pkgname) { # nolint
  teal.logger::register_logger(namespace = "teal.modules.general")
  teal.logger::register_handlers("teal.modules.general")
}
