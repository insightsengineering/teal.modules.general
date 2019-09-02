call_fun_dots <- function(fun, str_args) {
  do.call("call", c(list(fun), lapply(str_args, as.name)), quote = TRUE)
}

list_or_null <- function(obj) {
  if (is.null(obj)) {
    NULL
  } else {
    list(obj)
  }
}
