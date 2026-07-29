testthat::describe("tm_g_response module creation", {
  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_g_response(
        label = "Response Plot",
        response = mock_teal_picks(select_multiple = FALSE),
        x = mock_teal_picks(select_multiple = FALSE)
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with default options", {
    testthat::expect_s3_class(
      tm_g_response(
        response = mock_teal_picks(select_multiple = FALSE),
        x = mock_teal_picks(select_multiple = FALSE)
      ),
      "teal_module"
    )
  })

  it("creates a module with datanames taken from data extracts", {
    mod <- tm_g_response(
      response = mock_teal_picks(dataname = "A", select_multiple = FALSE),
      x = mock_teal_picks(dataname = "B", select_multiple = FALSE),
      row_facet = mock_teal_picks(dataname = "C", select_multiple = FALSE),
      col_facet = mock_teal_picks(dataname = "D", select_multiple = FALSE)
    )

    testthat::expect_setequal(
      mod$datanames,
      c("A", "B", "C", "D")
    )
  })

  it("creates a module that is bookmarkable", {
    mod <- tm_g_response(
      response = mock_teal_picks(select_multiple = FALSE),
      x = mock_teal_picks(select_multiple = FALSE)
    )

    testthat::expect_true(attr(mod, "teal_bookmarkable"))
  })
})

testthat::describe("tm_g_response input validation", {
  it("fails when label is not a string", {
    testthat::expect_error(
      tm_g_response(
        label = 123,
        response = mock_teal_picks(select_multiple = FALSE),
        x = mock_teal_picks(select_multiple = FALSE)
      ),
      "Assertion on 'label' failed"
    )
  })

  it("fails when response is not a data_extract_spec or list", {
    testthat::expect_error(
      tm_g_response(
        response = "not a spec",
        x = mock_teal_picks(select_multiple = FALSE)
      ),
      "Assertion on 'response' failed"
    )
  })

  it("throws warning when response has multiple selection enabled", {
    local_spec <- mock_teal_picks(select_multiple = TRUE)
    testthat::expect_warning(
      tm_g_response(
        response = local_spec,
        x = mock_teal_picks(select_multiple = FALSE)
      ),
      "multiple"
    )
  })

  it("fails when x is not a data_extract_spec or list", {
    testthat::expect_error(
      tm_g_response(
        response = mock_teal_picks(select_multiple = FALSE),
        x = "not a spec"
      ),
      "Assertion on 'x' failed"
    )
  })

  it("throws warning when x has multiple selection enabled", {
    local_spec <- mock_teal_picks(select_multiple = TRUE)
    testthat::expect_warning(
      tm_g_response(
        response = mock_teal_picks(select_multiple = FALSE),
        x = local_spec
      ),
      "multiple"
    )
  })

  it("fails when plot_height is not valid", {
    testthat::expect_error(
      tm_g_response(
        response = mock_teal_picks(select_multiple = FALSE),
        x = mock_teal_picks(select_multiple = FALSE),
        plot_height = c(100, 200, 300) # testing when min > max
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot_height has wrong length", {
    testthat::expect_error(
      tm_g_response(
        response = mock_teal_picks(select_multiple = FALSE),
        x = mock_teal_picks(select_multiple = FALSE),
        plot_height = c(600, 200)
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot_width is not valid", {
    testthat::expect_error(
      tm_g_response(
        response = mock_teal_picks(select_multiple = FALSE),
        x = mock_teal_picks(select_multiple = FALSE),
        plot_width = c(100, 200, 300) # testing when min > max
      ),
      "Assertion on 'plot_width' failed"
    )
  })

  it("accepts NULL plot_width", {
    testthat::expect_s3_class(
      tm_g_response(
        response = mock_teal_picks(select_multiple = FALSE),
        x = mock_teal_picks(select_multiple = FALSE),
        plot_width = NULL
      ),
      "teal_module"
    )
  })

  it("fails when ggtheme is invalid", {
    testthat::expect_error(
      tm_g_response(
        response = mock_teal_picks(select_multiple = FALSE),
        x = mock_teal_picks(select_multiple = FALSE),
        ggtheme = "invalid_theme"
      ),
      "should be one of"
    )
  })

  it("accepts valid ggtheme options", {
    for (theme in c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void")) {
      testthat::expect_s3_class(
        tm_g_response(
          response = mock_teal_picks(select_multiple = FALSE),
          x = mock_teal_picks(select_multiple = FALSE),
          ggtheme = theme
        ),
        "teal_module"
      )
    }
  })

  it("fails when ggplot2_args object does not inherit from 'ggplot2_args'", {
    testthat::expect_error(
      tm_g_response(
        response = mock_teal_picks(select_multiple = FALSE),
        x = mock_teal_picks(select_multiple = FALSE),
        ggplot2_args = list(teal.widgets::ggplot2_args())
      ),
      "Assertion on 'ggplot2_args' failed"
    )
  })

  it("accepts valid ggplot2_args", {
    testthat::expect_s3_class(
      tm_g_response(
        response = mock_teal_picks(select_multiple = FALSE),
        x = mock_teal_picks(select_multiple = FALSE),
        ggplot2_args = teal.widgets::ggplot2_args()
      ),
      "teal_module"
    )
  })

  it("fails when decorators has invalid object types", {
    testthat::expect_error(
      tm_g_response(
        response = mock_teal_picks(select_multiple = FALSE),
        x = mock_teal_picks(select_multiple = FALSE),
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
        response = mock_teal_picks(select_multiple = FALSE),
        x = mock_teal_picks(select_multiple = FALSE),
        decorators = list(
          plot = teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
  })
})


testthat::describe("tm_g_response module server behavior", {
  create_response_module <- function(response_vars, response_selected, x_vars, x_selected, ...) {
    tm_g_response(
      response = teal.picks::picks(
        teal.picks::datasets("test_data", "test_data"),
        teal.picks::variables(response_vars, response_selected)
      ),
      x = teal.picks::picks(
        teal.picks::datasets("test_data", "test_data"),
        teal.picks::variables(x_vars, x_selected)
      ),
      ...
    )
  }

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

    mod <- create_response_module(
      response_vars = c("response", "x_var"),
      response_selected = "response",
      x_vars = c("response", "x_var"),
      x_selected = "x_var"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        .change_selectors(selectors, response = "response", x = "x_var")
        session$setInputs(
          "freq" = TRUE,
          "coord_flip" = FALSE,
          "count_labels" = TRUE,
          "rotate_xaxis_labels" = FALSE,
          "ggtheme" = "gray"
        )
        # Wait for validation to pass
        testthat::expect_s4_class(validated_q(), "teal_data")
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

    mod <- create_response_module(
      response_vars = c("response", "x_var"),
      response_selected = "response",
      x_vars = c("response", "x_var"),
      x_selected = "x_var"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        .change_selectors(selectors, response = "response", x = "x_var")
        session$setInputs(
          "freq" = TRUE,
          "coord_flip" = FALSE,
          "count_labels" = TRUE,
          "rotate_xaxis_labels" = FALSE,
          "ggtheme" = "gray"
        )
        # Wait for validation to pass
        testthat::expect_s4_class(validated_q(), "teal_data")
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

    mod <- create_response_module(
      response_vars = c("response", "x_var"),
      response_selected = "response",
      x_vars = c("response", "x_var"),
      x_selected = "x_var"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        .change_selectors(selectors, response = "response", x = "x_var")
        session$setInputs(
          "freq" = FALSE,
          "coord_flip" = FALSE,
          "count_labels" = TRUE,
          "rotate_xaxis_labels" = FALSE,
          "ggtheme" = "gray"
        )
        # Wait for validation to pass
        testthat::expect_s4_class(validated_q(), "teal_data")
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })
})
