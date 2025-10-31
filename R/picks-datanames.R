.picks_datanames <- function(x) {
  checkmate::assert_list(x, c("picks", "NULL"))
  datanames_list <- lapply(x, function(x) {
    if (is.character(x$datasets$choices)) {
      x$datasets$choices
    } else {
      NULL
    }
  })

  if (any(vapply(datanames_list, is.null, logical(1)))) {
    "all"
  } else {
    unique(unlist(datanames_list))
  }
}
