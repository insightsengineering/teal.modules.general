test_that("e2e: tm_front_page initializes without errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_data_table()

  app_driver$expect_no_shiny_error()
  app_driver$stop()
})

test_that("e2e: tm_front_page displays data table", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_data_table()

  # table
  testthat::expect_match(app_driver$get_active_module_output("iris-data_table"), "Table Caption")
  testthat::expect_true(app_driver$is_visible(selector = app_driver$active_module_element("iris-data_table")))

  app_driver$stop()
})

test_that("e2e: tm_front_page variable selection", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_data_table()

  # default variable selection
  testthat::expect_equal(
    app_driver$get_active_module_input("iris-variables"),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species" )
  )

  # new variable selection
  app_driver$set_module_input("iris-variables", c("Petal.Width", "Species"))
  app_driver$expect_no_validation_error()
  testthat::expect_equal(
    app_driver$get_active_module_input("iris-variables"),
    c("Petal.Width", "Species")
  )

  app_driver$stop()
})
