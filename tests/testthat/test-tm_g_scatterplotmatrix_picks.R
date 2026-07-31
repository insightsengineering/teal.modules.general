testthat::describe("tm_g_scatterplotmatrix module creation", {
  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_g_scatterplotmatrix(
        label = "Scatterplot Matrix",
        variables = list(variables = mock_teal_picks(select_multiple = TRUE))
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with default options", {
    testthat::expect_s3_class(
      tm_g_scatterplotmatrix(
        variables = list(variables = mock_teal_picks(select_multiple = TRUE))
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with list of picks", {
    testthat::expect_s3_class(
      tm_g_scatterplotmatrix(
        variables = list(
          a = mock_teal_picks(dataname = "A", select_multiple = TRUE),
          b = mock_teal_picks(dataname = "B", select_multiple = TRUE)
        )
      ),
      "teal_module"
    )
  })

  it("creates a module with datanames taken from picks", {
    mod <- tm_g_scatterplotmatrix(
      variables = list(
        a = mock_teal_picks(dataname = "A", select_multiple = TRUE),
        b = mock_teal_picks(dataname = "B", select_multiple = TRUE)
      )
    )

    testthat::expect_setequal(
      mod$datanames,
      c("A", "B")
    )
  })

  it("creates a module that is bookmarkable", {
    mod <- tm_g_scatterplotmatrix(
      variables = list(variables = mock_teal_picks(select_multiple = TRUE))
    )

    testthat::expect_true(attr(mod, "teal_bookmarkable"))
  })
})

testthat::describe("tm_g_scatterplotmatrix input validation", {
  it("fails when label is not a string", {
    testthat::expect_error(
      tm_g_scatterplotmatrix(
        label = 123,
        variables = list(variables = mock_teal_picks(select_multiple = TRUE))
      ),
      "Assertion on 'label' failed"
    )
  })

  it("fails when variables is not a picks or list", {
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
        variables = list(variables = mock_teal_picks(select_multiple = TRUE)),
        plot_height = c(100, 200, 300) # testing when min > max
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot_height has wrong length", {
    testthat::expect_error(
      tm_g_scatterplotmatrix(
        variables = list(variables = mock_teal_picks(select_multiple = TRUE)),
        plot_height = c(600, 200)
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot_width is not valid", {
    testthat::expect_error(
      tm_g_scatterplotmatrix(
        variables = list(variables = mock_teal_picks(select_multiple = TRUE)),
        plot_width = c(100, 200, 300) # testing when min > max
      ),
      "Assertion on 'plot_width' failed"
    )
  })

  it("accepts NULL plot_width", {
    testthat::expect_s3_class(
      tm_g_scatterplotmatrix(
        variables = list(variables = mock_teal_picks(select_multiple = TRUE)),
        plot_width = NULL
      ),
      "teal_module"
    )
  })

  it("fails when decorators has invalid object types", {
    testthat::expect_error(
      tm_g_scatterplotmatrix(
        variables = list(variables = mock_teal_picks(select_multiple = TRUE)),
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
        variables = list(variables = mock_teal_picks(select_multiple = TRUE)),
        decorators = list(
          plot = teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
  })
})

testthat::describe("tm_g_scatterplotmatrix module server behavior", {
  create_scatterplotmatrix_mod <- function(data, vars, vars_selected, ...) {
    tm_g_scatterplotmatrix(
      variables = list(
        variables = teal.picks::picks(
          teal.picks::datasets("test_data", "test_data"),
          teal.picks::variables(vars, vars_selected, multiple = TRUE)
        )
      ),
      ...
    )
  }

  it("server function executes successfully through module interface", {
    test_data_df <- data.frame(
      var1 = rnorm(30),
      var2 = rnorm(30),
      var3 = rnorm(30)
    )
    data <- create_test_data(test_data_df)

    mod <- create_scatterplotmatrix_mod(
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
        .change_selectors(selectors, variables = c("var1", "var2", "var3"))
        session$setInputs(
          "cor" = FALSE,
          "alpha" = 0.5,
          "cex" = 1.5
        )
        testthat::expect_s4_class(validated_q(), "teal_data")
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_s3_class(plot_result, "ggplot")
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

    mod <- create_scatterplotmatrix_mod(
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
        .change_selectors(selectors, variables = c("var1", "var2"))
        session$setInputs(
          "cor" = FALSE,
          "alpha" = 0.5,
          "cex" = 1.5
        )
        testthat::expect_s4_class(validated_q(), "teal_data")
        testthat::expect_true(inherits(output_q(), "teal_data"))
        testthat::expect_s3_class(plot_r(), "ggplot")
      }
    )
  })
})
