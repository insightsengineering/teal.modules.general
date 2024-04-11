test_that("e2e: tm_front_page initializes without errors", {
  skip_if_too_deep(5)
  require(shinytest2)

  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = teal::modules(
      tm_data_table(
        variables_selected = list(
          iris = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
        ),
        datasets_selected = "mtcars",
        dt_args = list(caption = "Table Caption")
      )
    ),
    timeout = 3000
  )

  app$expect_no_shiny_error()

  # tables
  app$expect_screenshot(selector = app$active_module_element("mtcars-data_table"))

  # variable selected
  testthat::expect_equal(
    app$get_active_module_input("mtcars-variables"),
    c("mpg", "cyl", "disp", "hp", "drat", "wt")
  )
  app$set_module_input("mtcars-variables", c("vs", "am"))
  testthat::expect_equal(
    app$get_active_module_input("mtcars-variables"),
    c("vs", "am")
  )

  # distinct selection
  app$click(selector = app$active_module_element("if_distinct"))
  testthat::expect_true(app$is_visible(app$active_module_element("mtcars-data_table")))
  app$expect_screenshot(selector = app$active_module_element("mtcars-data_table"))

  app$stop()
})
