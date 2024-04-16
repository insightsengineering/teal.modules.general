testthat::test_that("e2e - tm_g_association: data parameter and module label is passed properly", {
  skip_if_too_deep(5)

  app <- app_driver_tm_g_association()
  app$expect_no_shiny_error()

  testthat::expect_equal(
    app$get_text("#teal-main_ui-root-active_tab > li.active > a"),
    "Association"
  )

  encoding_dataset <- app$get_text("#teal-main_ui-root-association .help-block")
  testthat::expect_match(encoding_dataset, "Dataset:[\n ]*CO2", all = FALSE)

  app$stop()
})

testthat::test_that("e2e - tm_g_association:
  data extract spec elements are initialized with the default values specified by ref and vars arguments", {
  skip_if_too_deep(5)
  app <- app_driver_tm_g_association()

  app$expect_no_shiny_error()

  testthat::expect_identical(
    app$get_active_module_input("ref-dataset_CO2_singleextract-select"),
    "Plant"
  )
  app$set_active_module_input("ref-dataset_CO2_singleextract-select", "Type")
  app$wait_for_idle()
  app$expect_no_validation_error()

  testthat::expect_identical(
    app$get_active_module_input("vars-dataset_CO2_singleextract-select"),
    "Treatment"
  )
  app$set_active_module_input("vars-dataset_CO2_singleextract-select", "Plant")
  app$wait_for_idle()
  app$expect_no_validation_error()

  app$stop()
})


testthat::test_that("e2e - tm_g_association: test if default radio buttons are checked", {
  skip_if_too_deep(5)
  app <- app_driver_tm_g_association()

  app$expect_no_shiny_error()

  testthat::expect_true(app$get_active_module_input("association"))
  testthat::expect_false(app$get_active_module_input("show_dist"))
  testthat::expect_false(app$get_active_module_input("log_transformation"))

  app$set_active_module_input("association", FALSE)
  app$set_active_module_input("show_dist", TRUE)
  app$set_active_module_input("log_transformation", TRUE)
  app$wait_for_idle()
  app$expect_no_shiny_error()

  app$stop()
})
