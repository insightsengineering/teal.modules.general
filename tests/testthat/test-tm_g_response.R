testthat::describe("tm_g_response module creation", {
  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_g_response(
        label = "Response Plot",
        response = mock_data_extract_spec(select_multiple = FALSE),
        x = mock_data_extract_spec(select_multiple = FALSE)
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with default options", {
    testthat::expect_s3_class(
      tm_g_response(
        response = mock_data_extract_spec(select_multiple = FALSE),
        x = mock_data_extract_spec(select_multiple = FALSE)
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with list of data extract specs", {
    testthat::expect_s3_class(
      tm_g_response(
        response = list(
          mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
          mock_data_extract_spec(dataname = "B", select_multiple = FALSE)
        ),
        x = list(
          mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
          mock_data_extract_spec(dataname = "B", select_multiple = FALSE)
        )
      ),
      "teal_module"
    )
  })

  it("creates a module with datanames taken from data extracts", {
    mod <- tm_g_response(
      response = list(
        mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
        mock_data_extract_spec(dataname = "B", select_multiple = FALSE)
      ),
      x = list(
        mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
        mock_data_extract_spec(dataname = "B", select_multiple = FALSE)
      )
    )

    testthat::expect_setequal(
      mod$datanames,
      c("A", "B")
    )
  })

  it("creates a module that is bookmarkable", {
    mod <- tm_g_response(
      response = mock_data_extract_spec(select_multiple = FALSE),
      x = mock_data_extract_spec(select_multiple = FALSE)
    )

    testthat::expect_true(attr(mod, "teal_bookmarkable"))
  })
})

testthat::describe("tm_g_response input validation", {
  it("fails when label is not a string", {
    testthat::expect_error(
      tm_g_response(
        label = 123,
        response = mock_data_extract_spec(select_multiple = FALSE),
        x = mock_data_extract_spec(select_multiple = FALSE)
      ),
      "Assertion on 'label' failed"
    )
  })

  it("fails when response is not a data_extract_spec or list", {
    testthat::expect_error(
      tm_g_response(
        response = "not a spec",
        x = mock_data_extract_spec(select_multiple = FALSE)
      ),
      "Assertion on 'response' failed"
    )
  })

  it("fails when response allows multiple selection", {
    testthat::expect_error(
      tm_g_response(
        response = mock_data_extract_spec(select_multiple = TRUE),
        x = mock_data_extract_spec(select_multiple = FALSE)
      ),
      "'response' should not allow multiple selection"
    )
  })

  it("fails when x is not a data_extract_spec or list", {
    testthat::expect_error(
      tm_g_response(
        response = mock_data_extract_spec(select_multiple = FALSE),
        x = "not a spec"
      ),
      "Assertion on 'x' failed"
    )
  })

  it("fails when x allows multiple selection", {
    testthat::expect_error(
      tm_g_response(
        response = mock_data_extract_spec(select_multiple = FALSE),
        x = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "'x' should not allow multiple selection"
    )
  })

  it("fails when plot_height is not valid", {
    testthat::expect_error(
      tm_g_response(
        response = mock_data_extract_spec(select_multiple = FALSE),
        x = mock_data_extract_spec(select_multiple = FALSE),
        plot_height = c(100, 200, 300) # testing when min > max
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot_height has wrong length", {
    testthat::expect_error(
      tm_g_response(
        response = mock_data_extract_spec(select_multiple = FALSE),
        x = mock_data_extract_spec(select_multiple = FALSE),
        plot_height = c(600, 200)
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot_width is not valid", {
    testthat::expect_error(
      tm_g_response(
        response = mock_data_extract_spec(select_multiple = FALSE),
        x = mock_data_extract_spec(select_multiple = FALSE),
        plot_width = c(100, 200, 300) # testing when min > max
      ),
      "Assertion on 'plot_width' failed"
    )
  })

  it("accepts NULL plot_width", {
    testthat::expect_s3_class(
      tm_g_response(
        response = mock_data_extract_spec(select_multiple = FALSE),
        x = mock_data_extract_spec(select_multiple = FALSE),
        plot_width = NULL
      ),
      "teal_module"
    )
  })

  it("fails when ggtheme is invalid", {
    testthat::expect_error(
      tm_g_response(
        response = mock_data_extract_spec(select_multiple = FALSE),
        x = mock_data_extract_spec(select_multiple = FALSE),
        ggtheme = "invalid_theme"
      ),
      "should be one of"
    )
  })

  it("accepts valid ggtheme options", {
    for (theme in c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void")) {
      testthat::expect_s3_class(
        tm_g_response(
          response = mock_data_extract_spec(select_multiple = FALSE),
          x = mock_data_extract_spec(select_multiple = FALSE),
          ggtheme = theme
        ),
        "teal_module"
      )
    }
  })

  it("fails when ggplot2_args is not a ggplot2_args object", {
    testthat::expect_error(
      tm_g_response(
        response = mock_data_extract_spec(select_multiple = FALSE),
        x = mock_data_extract_spec(select_multiple = FALSE),
        ggplot2_args = list(
          invalid_name = teal.widgets::ggplot2_args()
        )
      ),
      "Assertion on 'ggplot2_args' failed"
    )
  })

  it("accepts valid ggplot2_args", {
    testthat::expect_s3_class(
      tm_g_response(
        response = mock_data_extract_spec(select_multiple = FALSE),
        x = mock_data_extract_spec(select_multiple = FALSE),
        ggplot2_args = teal.widgets::ggplot2_args()
      ),
      "teal_module"
    )
  })

  it("fails when decorators has invalid object types", {
    testthat::expect_error(
      tm_g_response(
        response = mock_data_extract_spec(select_multiple = FALSE),
        x = mock_data_extract_spec(select_multiple = FALSE),
        decorators = list(
          plot = "not a teal_transform_module"
        )
      ),
      "Assertion on 'decorators' failed|Make sure that the named list contains"
    )
  })

  it("accepts valid decorators", {
    testthat::expect_s3_class(
      tm_g_response(
        response = mock_data_extract_spec(select_multiple = FALSE),
        x = mock_data_extract_spec(select_multiple = FALSE),
        decorators = list(
          plot = teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
  })
})


# Server and UI function tests removed - srv_g_response and ui_g_response are not exported

testthat::describe("tm_g_response module server behavior", {
  it("server function executes successfully through module interface", {
    data <- shiny::reactive(
      within(teal.data::teal_data(), {
        require(nestcolor)
        test_data <- data.frame(
          response = factor(rep(c("A", "B", "C"), 10)),
          x_var = factor(rep(c("X", "Y"), 15))
        )
      })
    )

    mod <- tm_g_response(
      response = list(
        teal.transform::data_extract_spec(
          dataname = "test_data",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              data = isolate(data())[["test_data"]],
              c("response", "x_var")
            ),
            selected = "response",
            multiple = FALSE
          )
        )
      ),
      x = list(
        teal.transform::data_extract_spec(
          dataname = "test_data",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              data = isolate(data())[["test_data"]],
              c("response", "x_var")
            ),
            selected = "x_var",
            multiple = FALSE
          )
        )
      )
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "response-dataset_test_data_singleextract-select" = "response",
          "x-dataset_test_data_singleextract-select" = "x_var",
          "freq" = TRUE,
          "coord_flip" = FALSE,
          "count_labels" = TRUE,
          "rotate_xaxis_labels" = FALSE,
          "ggtheme" = "gray"
        )
        # Wait for validation to pass
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })

  it("server function generates frequency plot through module interface", {
    data <- shiny::reactive(
      within(teal.data::teal_data(), {
        require(nestcolor)
        test_data <- data.frame(
          response = factor(rep(c("A", "B", "C"), 10)),
          x_var = factor(rep(c("X", "Y"), 15))
        )
      })
    )

    mod <- tm_g_response(
      response = list(
        teal.transform::data_extract_spec(
          dataname = "test_data",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              data = isolate(data())[["test_data"]],
              c("response", "x_var")
            ),
            selected = "response",
            multiple = FALSE
          )
        )
      ),
      x = list(
        teal.transform::data_extract_spec(
          dataname = "test_data",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              data = isolate(data())[["test_data"]],
              c("response", "x_var")
            ),
            selected = "x_var",
            multiple = FALSE
          )
        )
      )
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "response-dataset_test_data_singleextract-select" = "response",
          "x-dataset_test_data_singleextract-select" = "x_var",
          "freq" = TRUE,
          "coord_flip" = FALSE,
          "count_labels" = TRUE,
          "rotate_xaxis_labels" = FALSE,
          "ggtheme" = "gray"
        )
        # Wait for validation to pass
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })

  it("server function generates density plot through module interface", {
    data <- shiny::reactive(
      within(teal.data::teal_data(), {
        require(nestcolor)
        test_data <- data.frame(
          response = factor(rep(c("A", "B", "C"), 10)),
          x_var = factor(rep(c("X", "Y"), 15))
        )
      })
    )

    mod <- tm_g_response(
      response = list(
        teal.transform::data_extract_spec(
          dataname = "test_data",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              data = isolate(data())[["test_data"]],
              c("response", "x_var")
            ),
            selected = "response",
            multiple = FALSE
          )
        )
      ),
      x = list(
        teal.transform::data_extract_spec(
          dataname = "test_data",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              data = isolate(data())[["test_data"]],
              c("response", "x_var")
            ),
            selected = "x_var",
            multiple = FALSE
          )
        )
      )
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "response-dataset_test_data_singleextract-select" = "response",
          "x-dataset_test_data_singleextract-select" = "x_var",
          "freq" = FALSE,
          "coord_flip" = FALSE,
          "count_labels" = TRUE,
          "rotate_xaxis_labels" = FALSE,
          "ggtheme" = "gray"
        )
        # Wait for validation to pass
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })
})
