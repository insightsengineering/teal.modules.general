local_logger_threshold <- function(threshold, envir = parent.frame()) {
  old <- logger::log_threshold(namespace = "teal.modules.general")
  if (!requireNamespace("withr", quietly = FALSE)) {
    return(invisible(old))
  }
  withr::defer(logger::log_threshold(old, namespace = "teal.modules.general"), envir = envir)
  logger::log_threshold(threshold, namespace = "teal.modules.general")
  invisible(old)
}

# Create a mock data extact spec for tests
mock_data_extract_spec <- function(dataname = "MOCK_DATASET",
                                   select_choices = sample(LETTERS, sample(2:10, 1)),
                                   select_multiple = FALSE) {
  teal.transform::data_extract_spec(
    dataname = dataname,
    select = teal.transform::select_spec(
      choices = select_choices,
      multiple = select_multiple
    )
  )
}
