testthat::test_that("e2e - tm_outliers: data parameter and module label is passed properly", {
  skip_if_too_deep(5)

  app <- app_driver_tm_outlier()
  app$expect_no_shiny_error()

  testthat::expect_equal(
    app$get_text("#teal-main_ui-root-active_tab > li.active > a"),
    "Outliers Module"
  )

  encoding_dataset <- app$get_text("#teal-main_ui-root-outliers_module .help-block")
  testthat::expect_match(encoding_dataset, "Dataset:[\n ]*CO2", all = FALSE)
  testthat::expect_match(encoding_dataset, "Outlier data points", all = FALSE)

  app$stop()
})

testthat::test_that("e2e - tm_outliers:
  data extract spec elements are initialized with the default values
  specified by outlier_var and categorical_var argument", {
  skip_if_too_deep(5)

  app <- app_driver_tm_outlier()
  app$expect_no_shiny_error()

  app$stop()
})

testthat::test_that("e2e - tm_outliers:", {
  skip_if_too_deep(5)

  app <- app_driver_tm_outlier()
  app$expect_no_shiny_error()

  app$stop()
})

testthat::test_that("e2e - tm_outliers:", {
  skip_if_too_deep(5)

  app <- app_driver_tm_outlier()
  app$expect_no_shiny_error()

  app$stop()
})

testthat::test_that("e2e - tm_outliers:", {
  skip_if_too_deep(5)

  app <- app_driver_tm_outlier()
  app$expect_no_shiny_error()

  app$stop()
})
