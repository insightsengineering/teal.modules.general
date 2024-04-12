test_that("e2e: tm_missing_data initializes without errors and shows encodings", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_missing_data()

  app_driver$expect_no_shiny_error()

  testthat::expect_setequal(app_driver$get_active_module_input("iris-variables_select"),
                            c("Petal.Length", "Sepal.Length", "Petal.Width", "Species", "Sepal.Width"))

  app_driver$click(selector = app_driver$active_module_element("iris-filter_na"))
  app_driver$expect_no_validation_error()

  app_driver$click(selector = app_driver$active_module_element("iris-any_na"))
  app_driver$expect_no_validation_error()

  app_driver$set_module_input("iris-ggtheme", "classic", timeout_ = 10000)

  app_driver$set_module_input(input_id = "iris-summary_type", "Summary")
  app_driver$expect_no_validation_error()

  app_driver$set_module_input(input_id = "iris-summary_type", "By Variable Levels")
  app_driver$expect_no_validation_error()


  app_driver$stop()
})
