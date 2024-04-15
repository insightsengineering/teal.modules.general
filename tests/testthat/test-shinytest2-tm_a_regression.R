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
    c(
      "Response vs Regressor", "Scale-Location", "Residuals vs Leverage",
      "Residuals vs Fitted", "Normal Q-Q", "Cook's distance", "Cook's dist vs Leverage"
    )

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
  testthat::expect_true(app$is_visible(app$active_module_element("outlier-label")))
  testthat::expect_true(app$is_visible(app$active_module_element("label_var_input")))

  app$stop()
})

testthat::test_that("e2e - tm_a_regerssion: outlier definition and label have default values and label text", {
  skip_if_too_deep(5)

  app <- app_driver_tm_a_regression()
  app$expect_no_shiny_error()

  testthat::expect_match(
    app$active_module_element_text("label_var_input"),
    "Outlier label",
    fixed = TRUE
  )
  outlier_label <- app$active_module_element_text("outlier-label")
  testthat::expect_match(
    outlier_label,
    "Outlier definition:",
    fixed = TRUE
  )
  testthat::expect_match(
    outlier_label,
    "distance greater than the value on the slider times the mean of the Cook",
    fixed = TRUE
  )

  testthat::expect_identical(app$get_active_module_input("label_var"), "uptake")
  testthat::expect_identical(app$get_active_module_input("outlier"), 9)

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

  testthat::expect_false(app$is_visible(app$active_module_element("size")))
  testthat::expect_false(app$is_visible(app$active_module_element("alpha")))
  testthat::expect_false(app$is_visible(app$active_module_element("label_min_segment")))
  testthat::expect_false(app$is_visible(app$active_module_element("module-ggtheme")))

  # After click they are visible.
  app$click(selector = "#_div > div.panel-heading.collapsed")
  testthat::expect_true(app$is_visible(app$active_module_element("size")))
  testthat::expect_true(app$is_visible(app$active_module_element("alpha")))
  testthat::expect_true(app$is_visible(app$active_module_element("label_min_segment")))
  testthat::expect_true(app$is_visible(app$active_module_element("ggtheme")))

  app$stop()
})

testthat::test_that("e2e - tm_a_regerssion: plot settings have default values", {
  skip_if_too_deep(5)

  app <- app_driver_tm_a_regression()
  app$expect_no_shiny_error()

  app$click(selector = "#_div > div.panel-heading.collapsed")

  testthat::expect_identical(app$get_active_module_input("size"), 2)
  testthat::expect_identical(app$get_active_module_input("alpha"), 1)
  testthat::expect_identical(app$get_active_module_input("label_min_segment"), 0.5)
  testthat::expect_identical(app$get_active_module_input("ggtheme"), "gray")

  app$set_module_input("size", 3)
  app$set_module_input("alpha", 0.5)
  app$set_module_input("label_min_segment", 0.6)
  app$set_module_input("ggtheme", "bw")
  app$expect_no_validation_error()

  app$stop()
})