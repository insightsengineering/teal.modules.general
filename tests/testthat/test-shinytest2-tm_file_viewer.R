test_that("e2e: tm_file_viewer initializes without errors and shows encodings", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_file_viewer()

  app_driver$expect_no_shiny_error()

  # encoding are visible
  testthat::expect_true(app_driver$is_visible(selector = app_driver$active_module_element("tree")))

  app_driver$stop()
})
