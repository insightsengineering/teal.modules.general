testthat::test_that("e2e - tm_g_association: ", {
  skip_if_too_deep(5)
  app <- app_driver_tm_g_association()

  app$expect_no_shiny_error()

  testthat::expect_identical(
    app$get_active_module_input("ref-dataset_CO2_singleextract-select"),
    "Plant"
  )
  app$set_module_input("ref-dataset_CO2_singleextract-select", "Type")
  app$expect_no_validation_error()
  testthat::expect_identical(
    app$get_active_module_input("ref-dataset_CO2_singleextract-select"),
    "Type"
  )

  testthat::expect_identical(
    app$get_active_module_input("vars-dataset_CO2_singleextract-select"),
    "Treatment"
  )
  app$set_module_input("vars-dataset_CO2_singleextract-select", "Plant")
  app$expect_no_validation_error()
  testthat::expect_identical(
    app$get_active_module_input("vars-dataset_CO2_singleextract-select"),
    "Plant"
  )

  testthat::expect_true(app$get_active_module_input("association"))
  testthat::expect_false(app$get_active_module_input("show_dist"))
  testthat::expect_false(app$get_active_module_input("log_transformation"))

  app$set_module_input("association", FALSE)
  app$set_module_input("show_dist", TRUE)
  app$set_module_input("log_transformation", TRUE)
  app$expect_no_shiny_error()

  # Plot settings are not visible.
  testthat::expect_false(app$is_visible(app$active_module_element("swap_axes")))
  # After click they are visible.
  app$click(selector = "#_div > div.panel-heading")
  testthat::expect_true(app$is_visible(app$active_module_element("swap_axes")))
  app$set_module_input("swap_axes", TRUE)
  app$set_module_input("rotate_xaxis_labels", TRUE)
  app$expect_no_validation_error()

  app$stop()
})
