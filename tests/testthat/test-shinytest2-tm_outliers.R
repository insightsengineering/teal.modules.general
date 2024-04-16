testthat::test_that("e2e - tm_outliers: skeleton", {
  skip_if_too_deep(5)

  app <- app_driver_tm_outlier()
  app$expect_no_shiny_error()
  app$stop()
})
