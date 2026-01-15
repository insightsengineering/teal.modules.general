testthat::describe("tm_g_association module creation", {
  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_g_association(
        label = "Association",
        ref = mock_data_extract_spec(select_multiple = FALSE),
        vars = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with list of data extract specs", {
    testthat::expect_s3_class(
      tm_g_association(
        ref = list(
          mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
          mock_data_extract_spec(dataname = "B", select_multiple = FALSE)
        ),
        vars = list(
          mock_data_extract_spec(dataname = "A", select_multiple = TRUE),
          mock_data_extract_spec(dataname = "B", select_multiple = TRUE)
        )
      ),
      "teal_module"
    )
  })

  it("creates a module with datanames taken from data extracts", {
    mod <- tm_g_association(
      ref = list(
        mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
        mock_data_extract_spec(dataname = "B", select_multiple = FALSE)
      ),
      vars = list(
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
    mod <- tm_g_association(
      ref = mock_data_extract_spec(select_multiple = FALSE),
      vars = mock_data_extract_spec(select_multiple = TRUE)
    )

    testthat::expect_true(attr(mod, "teal_bookmarkable"))
  })
})

testthat::describe("tm_g_association input validation", {
  it("fails when label is not a string", {
    testthat::expect_error(
      tm_g_association(
        label = 123,
        ref = mock_data_extract_spec(select_multiple = FALSE),
        vars = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "Assertion on 'label' failed"
    )
  })

  it("fails when ref is not a data_extract_spec or list", {
    testthat::expect_error(
      tm_g_association(
        ref = "not a spec",
        vars = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "Assertion on 'ref' failed"
    )
  })

  it("fails when ref allows multiple selection", {
    testthat::expect_error(
      tm_g_association(
        ref = mock_data_extract_spec(select_multiple = TRUE),
        vars = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "'ref' should not allow multiple selection"
    )
  })

  it("fails when vars is not a data_extract_spec or list", {
    testthat::expect_error(
      tm_g_association(
        ref = mock_data_extract_spec(select_multiple = FALSE),
        vars = "not a spec"
      ),
      "Assertion on 'vars' failed"
    )
  })

  it("fails when plot_height is not valid", {
    testthat::expect_error(
      tm_g_association(
        ref = mock_data_extract_spec(select_multiple = FALSE),
        vars = mock_data_extract_spec(select_multiple = TRUE),
        plot_height = c(100, 200, 300) # testing when min > max
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot_height has wrong length", {
    testthat::expect_error(
      tm_g_association(
        ref = mock_data_extract_spec(select_multiple = FALSE),
        vars = mock_data_extract_spec(select_multiple = TRUE),
        plot_height = c(600, 200)
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot_width is not valid", {
    testthat::expect_error(
      tm_g_association(
        ref = mock_data_extract_spec(select_multiple = FALSE),
        vars = mock_data_extract_spec(select_multiple = TRUE),
        plot_width = c(100, 200, 300) # testing when min > max
      ),
      "Assertion on 'plot_width' failed"
    )
  })

  it("accepts NULL plot_width", {
    testthat::expect_s3_class(
      tm_g_association(
        ref = mock_data_extract_spec(select_multiple = FALSE),
        vars = mock_data_extract_spec(select_multiple = TRUE),
        plot_width = NULL
      ),
      "teal_module"
    )
  })

  it("fails when distribution_theme is invalid", {
    testthat::expect_error(
      tm_g_association(
        ref = mock_data_extract_spec(select_multiple = FALSE),
        vars = mock_data_extract_spec(select_multiple = TRUE),
        distribution_theme = "invalid_theme"
      ),
      "should be one of"
    )
  })

  it("accepts valid distribution_theme options", {
    for (theme in c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void")) {
      testthat::expect_s3_class(
        tm_g_association(
          ref = mock_data_extract_spec(select_multiple = FALSE),
          vars = mock_data_extract_spec(select_multiple = TRUE),
          distribution_theme = theme
        ),
        "teal_module"
      )
    }
  })

  it("fails when association_theme is invalid", {
    testthat::expect_error(
      tm_g_association(
        ref = mock_data_extract_spec(select_multiple = FALSE),
        vars = mock_data_extract_spec(select_multiple = TRUE),
        association_theme = "invalid_theme"
      ),
      "should be one of"
    )
  })

  it("accepts valid association_theme options", {
    for (theme in c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void")) {
      testthat::expect_s3_class(
        tm_g_association(
          ref = mock_data_extract_spec(select_multiple = FALSE),
          vars = mock_data_extract_spec(select_multiple = TRUE),
          association_theme = theme
        ),
        "teal_module"
      )
    }
  })

  it("fails when ggplot2_args object uses a named object that is not supported", {
    testthat::expect_error(
      tm_g_association(
        ref = mock_data_extract_spec(select_multiple = FALSE),
        vars = mock_data_extract_spec(select_multiple = TRUE),
        ggplot2_args = list(
          invalid_named_object = teal.widgets::ggplot2_args()
        )
      ),
      "Assertion on 'names\\(ggplot2_args\\)' failed"
    )
  })

  it("accepts valid ggplot2_args", {
    testthat::expect_s3_class(
      tm_g_association(
        ref = mock_data_extract_spec(select_multiple = FALSE),
        vars = mock_data_extract_spec(select_multiple = TRUE),
        ggplot2_args = list(
          default = teal.widgets::ggplot2_args(),
          Bivariate1 = teal.widgets::ggplot2_args(),
          Bivariate2 = teal.widgets::ggplot2_args()
        )
      ),
      "teal_module"
    )
  })

  it("fails when decorators has invalid object types", {
    testthat::expect_error(
      tm_g_association(
        ref = mock_data_extract_spec(select_multiple = FALSE),
        vars = mock_data_extract_spec(select_multiple = TRUE),
        decorators = list(
          plot = "not a teal_transform_module"
        )
      ),
      "Assertion on 'decorators' failed|Make sure that the named list contains"
    )
  })

  it("accepts valid decorators", {
    testthat::expect_s3_class(
      tm_g_association(
        ref = mock_data_extract_spec(select_multiple = FALSE),
        vars = mock_data_extract_spec(select_multiple = TRUE),
        decorators = list(
          plot = teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
  })
})

# Helper functions for server behavior tests
create_association_module <- function(data, ref_vars, vars_vars, ref_selected, vars_selected, ...) {
  tm_g_association(
    ref = list(
      teal.transform::data_extract_spec(
        dataname = "test_data",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices(
            data = isolate(data())[["test_data"]],
            ref_vars
          ),
          selected = ref_selected,
          multiple = FALSE
        )
      )
    ),
    vars = list(
      teal.transform::data_extract_spec(
        dataname = "test_data",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices(
            data = isolate(data())[["test_data"]],
            vars_vars
          ),
          selected = vars_selected,
          multiple = TRUE
        )
      )
    ),
    ...
  )
}

testthat::describe("tm_g_association module server behavior", {
  it("server function executes successfully through module interface with factor variables", {
    data <- create_test_data(data.frame(
      ref = factor(rep(c("A", "B", "C"), 10)),
      var1 = factor(rep(c("X", "Y"), 15)),
      var2 = factor(rep(c("M", "N"), 15))
    ))

    mod <- create_association_module(
      data = data,
      ref_vars = c("ref", "var1", "var2"),
      vars_vars = c("ref", "var1", "var2"),
      ref_selected = "ref",
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
          "ref-dataset_test_data_singleextract-select" = "ref",
          "vars-dataset_test_data_singleextract-select" = c("var1", "var2"),
          "show_dist" = FALSE,
          "association" = TRUE,
          "rotate_xaxis_labels" = FALSE,
          "swap_axes" = FALSE,
          "log_transformation" = FALSE,
          "distribution_theme" = "gray",
          "association_theme" = "gray"
        )
        # Wait for validation to pass
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "grob"))
      }
    )
  })

  it("server function handles show_dist option through module interface", {
    data <- create_test_data(data.frame(
      ref = factor(rep(c("A", "B", "C"), 10)),
      var1 = factor(rep(c("X", "Y"), 15))
    ))

    mod <- create_association_module(
      data = data,
      ref_vars = c("ref", "var1"),
      vars_vars = c("ref", "var1"),
      ref_selected = "ref",
      vars_selected = "var1"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "ref-dataset_test_data_singleextract-select" = "ref",
          "vars-dataset_test_data_singleextract-select" = "var1",
          "show_dist" = TRUE,
          "association" = TRUE,
          "rotate_xaxis_labels" = FALSE,
          "swap_axes" = FALSE,
          "log_transformation" = FALSE,
          "distribution_theme" = "gray",
          "association_theme" = "gray"
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "grob"))
      }
    )
  })

  it("server function handles association toggle through module interface", {
    data <- create_test_data(data.frame(
      ref = factor(rep(c("A", "B", "C"), 10)),
      var1 = factor(rep(c("X", "Y"), 15))
    ))

    mod <- create_association_module(
      data = data,
      ref_vars = c("ref", "var1"),
      vars_vars = c("ref", "var1"),
      ref_selected = "ref",
      vars_selected = "var1"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "ref-dataset_test_data_singleextract-select" = "ref",
          "vars-dataset_test_data_singleextract-select" = "var1",
          "show_dist" = FALSE,
          "association" = FALSE,
          "rotate_xaxis_labels" = FALSE,
          "swap_axes" = FALSE,
          "log_transformation" = FALSE,
          "distribution_theme" = "gray",
          "association_theme" = "gray"
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "grob"))
      }
    )
  })

  it("server function handles different themes through module interface", {
    data <- create_test_data(data.frame(
      ref = factor(rep(c("A", "B", "C"), 10)),
      var1 = factor(rep(c("X", "Y"), 15))
    ))

    mod <- create_association_module(
      data = data,
      ref_vars = c("ref", "var1"),
      vars_vars = c("ref", "var1"),
      ref_selected = "ref",
      vars_selected = "var1"
    )

    # Test with different themes
    for (theme in c("bw", "minimal", "dark")) {
      shiny::testServer(
        mod$server,
        args = c(
          list(id = "test", data = data),
          mod$server_args
        ),
        expr = {
          session$setInputs(
            "ref-dataset_test_data_singleextract-select" = "ref",
            "vars-dataset_test_data_singleextract-select" = "var1",
            "show_dist" = FALSE,
            "association" = TRUE,
            "rotate_xaxis_labels" = FALSE,
            "swap_axes" = FALSE,
            "log_transformation" = FALSE,
            "distribution_theme" = theme,
            "association_theme" = theme
          )
          testthat::expect_true(iv_r()$is_valid())
          output_result <- output_q()
          testthat::expect_true(inherits(output_result, "teal_data"))
        }
      )
    }
  })

  it("server function handles rotate_xaxis_labels option through module interface", {
    data <- create_test_data(data.frame(
      ref = factor(rep(c("A", "B", "C"), 10)),
      var1 = factor(rep(c("X", "Y"), 15))
    ))

    mod <- create_association_module(
      data = data,
      ref_vars = c("ref", "var1"),
      vars_vars = c("ref", "var1"),
      ref_selected = "ref",
      vars_selected = "var1"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "ref-dataset_test_data_singleextract-select" = "ref",
          "vars-dataset_test_data_singleextract-select" = "var1",
          "show_dist" = FALSE,
          "association" = TRUE,
          "rotate_xaxis_labels" = TRUE,
          "swap_axes" = FALSE,
          "log_transformation" = FALSE,
          "distribution_theme" = "gray",
          "association_theme" = "gray"
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "grob"))
      }
    )
  })

  it("server function handles swap_axes option through module interface", {
    data <- create_test_data(data.frame(
      ref = factor(rep(c("A", "B", "C"), 10)),
      var1 = factor(rep(c("X", "Y"), 15))
    ))

    mod <- create_association_module(
      data = data,
      ref_vars = c("ref", "var1"),
      vars_vars = c("ref", "var1"),
      ref_selected = "ref",
      vars_selected = "var1"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "ref-dataset_test_data_singleextract-select" = "ref",
          "vars-dataset_test_data_singleextract-select" = "var1",
          "show_dist" = FALSE,
          "association" = TRUE,
          "rotate_xaxis_labels" = FALSE,
          "swap_axes" = TRUE,
          "log_transformation" = FALSE,
          "distribution_theme" = "gray",
          "association_theme" = "gray"
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "grob"))
      }
    )
  })

  it("server function handles numeric variables through module interface", {
    data <- create_test_data(data.frame(
      ref = 1:30,
      var1 = 1:30 + rnorm(30),
      var2 = 1:30 + rnorm(30)
    ))

    mod <- create_association_module(
      data = data,
      ref_vars = c("ref", "var1", "var2"),
      vars_vars = c("ref", "var1", "var2"),
      ref_selected = "ref",
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
          "ref-dataset_test_data_singleextract-select" = "ref",
          "vars-dataset_test_data_singleextract-select" = c("var1", "var2"),
          "show_dist" = FALSE,
          "association" = TRUE,
          "rotate_xaxis_labels" = FALSE,
          "swap_axes" = FALSE,
          "log_transformation" = FALSE,
          "distribution_theme" = "gray",
          "association_theme" = "gray",
          "alpha" = 0.7,
          "size" = 3
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "grob"))
      }
    )
  })

  it("server function handles log transformation for numeric variables through module interface", {
    data <- create_test_data(data.frame(
      ref = abs(rnorm(30)) + 1,
      var1 = abs(rnorm(30)) + 1,
      var2 = abs(rnorm(30)) + 1
    ))

    mod <- create_association_module(
      data = data,
      ref_vars = c("ref", "var1", "var2"),
      vars_vars = c("ref", "var1", "var2"),
      ref_selected = "ref",
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
          "ref-dataset_test_data_singleextract-select" = "ref",
          "vars-dataset_test_data_singleextract-select" = c("var1", "var2"),
          "show_dist" = FALSE,
          "association" = TRUE,
          "rotate_xaxis_labels" = FALSE,
          "swap_axes" = FALSE,
          "log_transformation" = TRUE,
          "distribution_theme" = "gray",
          "association_theme" = "gray",
          "alpha" = 0.7,
          "size" = 3
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "grob"))
      }
    )
  })

  it("server function handles mixed factor and numeric variables through module interface", {
    data <- create_test_data(data.frame(
      ref = factor(rep(c("A", "B", "C"), 10)),
      var1 = 1:30 + rnorm(30),
      var2 = factor(rep(c("X", "Y"), 15))
    ))

    mod <- create_association_module(
      data = data,
      ref_vars = c("ref", "var1", "var2"),
      vars_vars = c("ref", "var1", "var2"),
      ref_selected = "ref",
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
          "ref-dataset_test_data_singleextract-select" = "ref",
          "vars-dataset_test_data_singleextract-select" = c("var1", "var2"),
          "show_dist" = FALSE,
          "association" = TRUE,
          "rotate_xaxis_labels" = FALSE,
          "swap_axes" = FALSE,
          "log_transformation" = FALSE,
          "distribution_theme" = "gray",
          "association_theme" = "gray"
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "grob"))
      }
    )
  })
})
