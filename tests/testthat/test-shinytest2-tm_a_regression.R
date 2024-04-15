testthat::test_that("e2e - tm_a_regerssion:
  data extract spec elements are initialized with the default values specified by response and regressor arg", {
  skip_if_too_deep(5)

  app <- app_driver_tm_a_regression()
  app$expect_no_shiny_error()

  testthat::expect_equal(
    app$get_text("#teal-main_ui-root-active_tab > li.active > a"),
    "Regression"
  )

  encoding_dataset <- app$get_text("#teal-main_ui-root-regression .help-block")
  testthat::expect_match(encoding_dataset, "Dataset:[\n ]*CO2", all = FALSE)

  testthat::expect_identical(
    app$get_active_module_input("plot_type"),
    "Normal Q-Q"
  )

  testthat::expect_identical(
    app$get_active_module_input("regressor-dataset_CO2_singleextract-select"),
    "conc"
  )
  app$set_module_input("regressor-dataset_CO2_singleextract-select", "Treatment")
  app$stop()
})

testthat::test_that("e2e - tm_a_regerssion:
  plot type has 7 specific choices & changing choices does not throw errors", {
  skip_if_too_deep(5)

  app <- app_driver_tm_a_regression()
  app$expect_no_shiny_error()

  plot_types <- app$active_module_element_text("plot_type > div")

  possible_choices <-
    c("Response vs Regressor", "Scale-Location", "Residuals vs Leverage",
      "Residuals vs Fitted", "Normal Q-Q", "Cook's distance", "Cook's dist vs Leverage")

  invisible(
    lapply(
      possible_choices,
      function(choice) {
        expect_match(plot_types, choice, fixed = TRUE)
        app$set_module_input("plot_type", choice)
        app$expect_no_validation_error()
      }
    )
  )

  app$stop()
})

testthat::test_that("e2e - tm_a_regerssion: outlier definition and label are visible by default", {
  skip_if_too_deep(5)

  app <- app_driver_tm_a_regression()
  app$expect_no_shiny_error()

  testthat::expect_true(app$get_active_module_input("show_outlier"))
  testthat::expect_true(app$is_visible(app$active_module_element("label_var_input")))

  app$stop()
})

testthat::test_that("e2e - tm_a_regerssion: unchecking display outlier hides outlier label and definition", {
  skip_if_too_deep(5)

  app <- app_driver_tm_a_regression()
  app$expect_no_shiny_error()

  app$set_module_input("show_outlier", FALSE)
  testthat::expect_false(app$is_visible(app$active_module_element("outlier-label")))
  testthat::expect_false(app$is_visible(app$active_module_element("label_var_input")))

  app$expect_no_validation_error()

  app$stop()
})

testthat::test_that("e2e - tm_a_regerssion: clicking plot settings accordion panel shows plot setting", {
  skip_if_too_deep(5)

  app <- app_driver_tm_a_regression()
  app$expect_no_shiny_error()

  testthat::expect_false(app$is_visible(app$active_module_element("size-label")))
  testthat::expect_false(app$is_visible(app$active_module_element("alpha-label")))
  testthat::expect_false(app$is_visible(app$active_module_element("label_min_segment-label")))
  testthat::expect_false(app$is_visible(app$active_module_element("module-ggtheme-label")))

  # After click they are visible.
  app$click(selector = "#_div > div.panel-heading.collapsed")
  testthat::expect_true(app$is_visible(app$active_module_element("size-label")))
  app$set_module_input("size-label", 3)
  app$expect_no_validation_error()

  testthat::expect_true(app$is_visible(app$active_module_element("alpha-label")))
  testthat::expect_true(app$is_visible(app$active_module_element("label_min_segment-label")))
  testthat::expect_true(app$is_visible(app$active_module_element("module-ggtheme-label")))

  app$stop()
})
