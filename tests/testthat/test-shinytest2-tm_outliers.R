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

  testthat::expect_identical(
    app_driver$get_active_module_input("outlier_var-dataset_CO2_singleextract-select"),
    "uptake"
  )
  app_driver$set_active_module_input("outlier_var-dataset_CO2_singleextract-select", "conc")
  app_driver$expect_no_shiny_error()

  testthat::expect_identical(
    app_driver$get_active_module_input("categorical_var-dataset_CO2_singleextract-filter1-col"),
    "Plant"
  )
  app_driver$set_active_module_input("categorical_var-dataset_CO2_singleextract-filter1-col", "Type")
  app_driver$expect_no_shiny_error()

  # testthat::expect_identical(
  #   app_driver$get_active_module_input("categorical_var-dataset_CO2_singleextract-filter1-vals"),
  #   "TODO: THIS DOESNT WORK"
  # )
  # app_driver$set_active_module_input("categorical_var-dataset_CO2_singleextract-filter1-col", "TODO")
  # app_driver$expect_no_shiny_error()

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
