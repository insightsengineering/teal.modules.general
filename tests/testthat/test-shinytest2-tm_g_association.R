testthat::test_that("e2e - tm_g_association: ", {
  skip_if_too_deep(5)
  app <- app_driver_tm_g_association()

  app$expect_no_shiny_error()

  app$stop()
})
