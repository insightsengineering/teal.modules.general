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

  app$wait_for_idle()
  app$expect_no_shiny_error()

  # variable selected

  # tab selection

  # tables

  # table caption


  app$stop()
})
