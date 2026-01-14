pkg_name <- "teal.modules.general"
if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  is_on_ci <- isTRUE(as.logical(Sys.getenv("CI")))

  if (requireNamespace("shinytest2", quietly = TRUE)) {
    options(shinytest2.load_timeout = 120 * 1000)
    options(shinytest2.timeout = 60 * 1000)

    chromote::set_chrome_args(c(chromote::default_chrome_args(), "--no-sandbox"))

    tmpt <- Sys.time()
    while (!chromote::has_default_chromote_object() && Sys.time() - tmpt < 1) {
      try(chromote::set_default_chromote_object(chromote::Chromote$new()), silent = TRUE)
    }
  }

  if (is_on_ci) {
    reporter <- MultiReporter$new(list(
      CheckReporter$new()
    ))
    test_results <- test_check(pkg_name, reporter = reporter)
    saveRDS(test_results, "unit_testing_results.rds")
  } else {
    reporter <- ParallelProgressReporter$new()
    test_check(pkg_name, reporter = reporter)
  }
}
