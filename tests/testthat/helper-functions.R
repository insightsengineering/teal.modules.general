local_logger_threshold <- function(threshold, envir = parent.frame()) {
  old <- logger::log_threshold(namespace = "teal.modules.general")
  if (!requireNamespace("withr", quietly = FALSE)) {
    return(invisible(old))
  }
  withr::defer(logger::log_threshold(old, namespace = "teal.modules.general"), envir = envir)
  logger::log_threshold(threshold, namespace = "teal.modules.general")
  invisible(old)
}
