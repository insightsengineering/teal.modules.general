testthat::describe("tests for module creation", {
  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_variable_browser(
      ),
      "teal_module"
    )
  })

  it("uses non default datanames", {
    testthat::expect_s3_class(
      tm_variable_browser(
        datanames = "my-dataset"
      ),
      "teal_module"
    )
  })

  it("uses non parent_datanames", {
    testthat::expect_s3_class(
      tm_variable_browser(
        parent_dataname = "my-parent_dataset"
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with pre_output", {
    pre_output <- shiny::actionButton("pre_output", "My pre output")
    default_mod <- tm_variable_browser()
    testthat::expect_null(default_mod$ui_args$pre_output)

    pre_output_mod <- tm_variable_browser(
      pre_output = pre_output
    )
    testthat::expect_equal(pre_output_mod$ui_args$pre_output, pre_output)
  })

  it("creates a teal_module object with post_output", {
    post_output <- shiny::actionButton("post_output", "My post output")
    default_mod <- tm_missing_data()
    testthat::expect_null(default_mod$ui_args$post_output)

    post_output_mod <- tm_variable_browser(
      post_output = post_output
    )
    testthat::expect_equal(post_output_mod$ui_args$post_output, post_output)
  })

    it("accepts a transformator", {
      transformator_iris <- teal_transform_module(
        label = "Custom transformator for iris",
        ui = function(id) {
          ns <- NS(id)
          tags$div(
            numericInput(ns("n_rows"), "Number of rows to display", value = 6, min = 1, max = 150, step = 1)
          )
        },
        server = function(id, data) {
          moduleServer(id, function(input, output, session) {
            reactive({
              within(
                data(),
                iris <- head(iris, num_rows),
                num_rows = input$n_rows
              )
            })
          })
        }
      )

      testthat::expect_s3_class(
        tm_variable_browser(
          transformators = list(
            teal::teal_transform_module()
          )
        ),
        "teal_module"
      )
  })

  it("accepts valid ggplot2_args", {
    testthat::expect_s3_class(
      tm_variable_browser(
        ggplot2_args = teal.widgets::ggplot2_args(
            labs = list(title = "User default title"),
            theme = list(legend.position = "right", legend.direction = "vertical")
          )
      ),
      "teal_module"
    )
  })
})

testthat::test_that("remove_outliers_from does not remove outliers when outlier_definition is zero", {
  var <- c(-100, 5, 10, 10, 20, 30, 40, 40, 40.5, 10000)
  testthat::expect_equal(remove_outliers_from(var, outlier_definition = 0), var)
})

testthat::test_that("remove_outliers_from removes outliers when outlier definition is non-zero", {
  var <- c(-100, 5, 10, 10, 20, 30, 40, 40, 40, 40.5, 10000)
  testthat::expect_equal(remove_outliers_from(var, outlier_definition = 1), c(5, 10, 10, 20, 30, 40, 40, 40, 40.5))
})

testthat::test_that("remove_outliers_from keeps missing values unchanged", {
  var <- c(-100, 5, NA, 10, 10, 20, 30, 40, 40, 40, 40.5, 10000)
  testthat::expect_equal(remove_outliers_from(var, outlier_definition = 1), c(5, NA, 10, 10, 20, 30, 40, 40, 40, 40.5))
})
