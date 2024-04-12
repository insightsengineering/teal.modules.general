testthat::test_that("e2e - tm_a_regerssion: verify encoding values and widgets", {
  skip_if_too_deep(5)

  app <- app_driver_tm_a_regression()
  app$expect_no_shiny_error()

  testthat::expect_equal(
    app$get_text("#teal-main_ui-root-active_tab > li.active > a"),
    "Regression"
  )

  # Check MAIN encoding panel
  # DO WE NEED any function for checking top of the encoding panel part?
  encoding_dataset <- app$get_text("#teal-main_ui-root-regression .help-block")
  testthat::expect_match(encoding_dataset, "Dataset:[\n ]*CO2", all = FALSE)


  # Check plot settings
  testthat::expect_identical(
    app$get_active_module_input("plot_type"),
    "Normal Q-Q"
  )

  testthat::expect_identical(
    app$get_active_module_input("regressor-dataset_CO2_singleextract-select"),
    "conc"
  )
  app$set_module_input("regressor-dataset_CO2_singleextract-select", "Treatment")
  app$expect_no_validation_error()
  testthat::expect_identical(
    app$get_active_module_input("regressor-dataset_CO2_singleextract-select"),
    "Treatment"
  )

  plot_types <- app$active_module_element_text("plot_type > div")
  testthat::expect_match(plot_types, "Response vs Regressor", fixed = TRUE)
  testthat::expect_match(plot_types, "Scale-Location", fixed = TRUE)
  testthat::expect_match(plot_types, "Residuals vs Leverage", fixed = TRUE)

  app$set_module_input("plot_type", "Residuals vs Fitted")
  app$expect_no_validation_error()
  app$set_module_input("plot_type", "Normal Q-Q")
  app$expect_no_validation_error()

  # No outlier definition.
  app$set_module_input("show_outlier", FALSE)
  testthat::expect_false(app$is_visible(app$active_module_element("outlier-label")))
  app$expect_no_validation_error()

  # Bring back outlier definition.
  app$set_module_input("show_outlier", TRUE)
  testthat::expect_true(app$is_visible(app$active_module_element("outlier-label")))
  app$expect_no_validation_error()

  # Plot settings are not visible.
  testthat::expect_false(app$is_visible(app$active_module_element("size-label")))
  # After click they are visible.
  app$click("#_div > div.panel-heading.collapsed")
  testthat::expect_true(app$is_visible(app$active_module_element("outlier-label")))
  app$set_module_input("size-label", 3)
  app$expect_no_validation_error()

  app$stop()
})
