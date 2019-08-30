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

#' Suppresses warning that chunk was already evaluated and validates that chunks are okay
#'
#' @return value of \code{chunks_eval}
safe_chunks_eval <- function() {
  res <- chunks_eval()
  chunks_validate_is_ok()
  return(res)
}
