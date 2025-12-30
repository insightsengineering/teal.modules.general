testthat::describe("tm_g_scatterplotmatrix module creation", {
  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_g_scatterplotmatrix(
        label = "Scatterplot Matrix",
        variables = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with default options", {
    testthat::expect_s3_class(
      tm_g_scatterplotmatrix(
        variables = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with list of data extract specs", {
    testthat::expect_s3_class(
      tm_g_scatterplotmatrix(
        variables = list(
          mock_data_extract_spec(dataname = "A", select_multiple = TRUE),
          mock_data_extract_spec(dataname = "B", select_multiple = TRUE)
        )
      ),
      "teal_module"
    )
  })

  it("creates a module with datanames taken from data extracts", {
    mod <- tm_g_scatterplotmatrix(
      variables = list(
        mock_data_extract_spec(dataname = "A", select_multiple = TRUE),
        mock_data_extract_spec(dataname = "B", select_multiple = TRUE)
      )
    )

    testthat::expect_setequal(
      mod$datanames,
      c("A", "B")
    )
  })

  it("creates a module that is bookmarkable", {
    mod <- tm_g_scatterplotmatrix(
      variables = mock_data_extract_spec(select_multiple = TRUE)
    )

    testthat::expect_true(attr(mod, "teal_bookmarkable"))
  })
})

testthat::describe("tm_g_scatterplotmatrix input validation", {
  it("fails when label is not a string", {
    testthat::expect_error(
      tm_g_scatterplotmatrix(
        label = 123,
        variables = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "Assertion on 'label' failed"
    )
  })

  it("fails when variables is not a data_extract_spec or list", {
    testthat::expect_error(
      tm_g_scatterplotmatrix(
        variables = "not a spec"
      ),
      "Assertion on 'variables' failed"
    )
  })

  it("fails when plot_height is not valid", {
    testthat::expect_error(
      tm_g_scatterplotmatrix(
        variables = mock_data_extract_spec(select_multiple = TRUE),
        plot_height = c(100, 200, 300) # min > max
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot_height has wrong length", {
    testthat::expect_error(
      tm_g_scatterplotmatrix(
        variables = mock_data_extract_spec(select_multiple = TRUE),
        plot_height = c(600, 200)
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot_width is not valid", {
    testthat::expect_error(
      tm_g_scatterplotmatrix(
        variables = mock_data_extract_spec(select_multiple = TRUE),
        plot_width = c(100, 200, 300) # min > max
      ),
      "Assertion on 'plot_width' failed"
    )
  })

  it("accepts NULL plot_width", {
    testthat::expect_s3_class(
      tm_g_scatterplotmatrix(
        variables = mock_data_extract_spec(select_multiple = TRUE),
        plot_width = NULL
      ),
      "teal_module"
    )
  })

  it("fails when decorators has invalid object types", {
    testthat::expect_error(
      tm_g_scatterplotmatrix(
        variables = mock_data_extract_spec(select_multiple = TRUE),
        decorators = list(
          plot = "not a teal_transform_module"
        )
      ),
      "Assertion on 'decorators' failed|Make sure that the named list contains"
    )
  })

  it("accepts valid decorators", {
    testthat::expect_s3_class(
      tm_g_scatterplotmatrix(
        variables = mock_data_extract_spec(select_multiple = TRUE),
        decorators = list(
          plot = teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
  })
})


create_scatterplotmatrix_module <- function(data, vars, vars_selected, ...) {
  tm_g_scatterplotmatrix(
    variables = list(
      teal.transform::data_extract_spec(
        dataname = "test_data",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices(
            data = isolate(data())[["test_data"]],
            vars
          ),
          selected = vars_selected,
          multiple = TRUE
        )
      )
    ),
    ...
  )
}

testthat::describe("tm_g_scatterplotmatrix module server behavior", {
  it("server function executes successfully through module interface", {
    test_data_df <- data.frame(
      var1 = rnorm(30),
      var2 = rnorm(30),
      var3 = rnorm(30)
    )
    data <- create_test_data(test_data_df)

    mod <- create_scatterplotmatrix_module(
      data,
      vars = c("var1", "var2", "var3"),
      vars_selected = c("var1", "var2", "var3")
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "variables-dataset_test_data_singleextract-select" = c("var1", "var2", "var3"),
          "cor" = FALSE,
          "alpha" = 0.5,
          "cex" = 1
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "trellis"))
      }
    )
  })

  it("server function generates scatterplot matrix with two variables", {
    test_data_df <- data.frame(
      var1 = rnorm(30),
      var2 = rnorm(30),
      var3 = rnorm(30)
    )
    data <- create_test_data(test_data_df)

    mod <- create_scatterplotmatrix_module(
      data,
      vars = c("var1", "var2", "var3"),
      vars_selected = c("var1", "var2")
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "variables-dataset_test_data_singleextract-select" = c("var1", "var2"),
          "cor" = FALSE,
          "alpha" = 0.5,
          "cex" = 1
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "trellis"))
      }
    )
  })
})

