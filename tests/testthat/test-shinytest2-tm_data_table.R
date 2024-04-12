test_that("e2e: tm_front_page initializes without errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_data_table()

  app_driver$expect_no_shiny_error()

  # tables
  testthat::expect_match(app_driver$get_active_module_output("iris-data_table"), "IRIS")
  testthat::expect_true(app_driver$is_visible(selector = app_driver$active_module_element("iris-data_table")))

  # variable selected
  testthat::expect_equal(
    app_driver$get_active_module_input("mtcars-variables"),
    c("mpg", "cyl", "disp", "hp", "drat", "wt")
  )
  app_driver$set_module_input("mtcars-variables", c("vs", "am"))
  testthat::expect_equal(
    app_driver$get_active_module_input("mtcars-variables"),
    c("vs", "am")
  )

  app_driver$stop()
})
