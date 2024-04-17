app_driver_tm_data_table <- function() {
  TealAppDriver$new(
    data = simple_teal_data(),
    modules = tm_data_table(
      label = "Data Table",
      variables_selected = list(
        iris = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
      ),
      datasets_selected = c("iris", "mtcars"),
      dt_args = list(caption = "Table Caption"),
      dt_options = list(
        searching = FALSE, pageLength = 30, lengthMenu = c(5, 15, 30, 100),
        scrollX = TRUE
      ),
      server_rendering = FALSE,
      pre_output = NULL,
      post_output = NULL
    ),
    timeout = 3000
  )
}

test_that("e2e - tm_data_table: Initializes without errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_data_table()

  app_driver$expect_no_shiny_error()

  testthat::expect_equal(
    app_driver$get_text("#teal-main_ui-root-active_tab > li.active > a"),
    "Data Table"
  )

  app_driver$stop()
})

test_that("e2e - tm_data_table: Verify module displays data table", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_data_table()

  # table
  testthat::expect_match(app_driver$get_active_module_output("iris-data_table"), "Table Caption")
  testthat::expect_true(app_driver$is_visible(selector = app_driver$active_module_element("iris-data_table")))

  app_driver$stop()
})

test_that("e2e - tm_data_table: Verify default variable selection and set new selection", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_data_table()

  # default variable selection
  testthat::expect_equal(
    app_driver$get_active_module_input("iris-variables"),
    c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
  )

  # new variable selection
  app_driver$set_active_module_input("iris-variables", c("Petal.Width", "Species"))
  app_driver$expect_no_validation_error()
  testthat::expect_equal(
    app_driver$get_active_module_input("iris-variables"),
    c("Petal.Width", "Species")
  )

  app_driver$stop()
})
