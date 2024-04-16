testthat::test_that("e2e - tm_outliers: data parameter and module label is passed properly", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_outlier()
  app_driver$expect_no_shiny_error()

  testthat::expect_equal(
    app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
    "Outliers Module"
  )

  encoding_dataset <- app_driver$get_text("#teal-main_ui-root-outliers_module .help-block")
  testthat::expect_match(encoding_dataset, "Dataset:[\n ]*CO2", all = FALSE)
  testthat::expect_match(encoding_dataset, "Outlier data points", all = FALSE)

  app_driver$stop()
})

testthat::test_that("e2e - tm_outliers:
  data extract spec elements are initialized with the default values
  specified by outlier_var and categorical_var argument", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_outlier()
  app_driver$expect_no_shiny_error()

  app_driver$stop()
})

testthat::test_that("e2e - tm_outliers:", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_outlier()
  app_driver$expect_no_shiny_error()

  app_driver$stop()
})

testthat::test_that("e2e - tm_outliers:", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_outlier()
  app_driver$expect_no_shiny_error()

  app_driver$stop()
})

testthat::test_that("e2e - tm_outliers:", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_outlier()
  app_driver$expect_no_shiny_error()

  app_driver$stop()
})
