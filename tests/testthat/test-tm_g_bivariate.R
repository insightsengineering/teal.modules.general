testthat::describe("tm_g_bivariate module creation", {
  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        row_facet = mock_data_extract_spec(select_multiple = FALSE),
        col_facet = mock_data_extract_spec(select_multiple = FALSE),
        facet = TRUE,
        color_settings = TRUE,
        use_density = TRUE,
        free_x_scales = TRUE,
        free_y_scales = TRUE,
        plot_height = c(400, 100, 600),
        plot_width = c(600, 100, 600),
        rotate_xaxis_labels = TRUE,
        swap_axes = TRUE
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with default options", {
    testthat::expect_s3_class(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE)
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with multiple data extract specs", {
    testthat::expect_s3_class(
      tm_g_bivariate(
        "a label",
        list(mock_data_extract_spec(select_multiple = FALSE), mock_data_extract_spec(select_multiple = FALSE)),
        list(mock_data_extract_spec(select_multiple = FALSE), mock_data_extract_spec(select_multiple = FALSE)),
        plot_height = c(400, 100, 600),
        plot_width = c(600, 100, 600)
      ),
      "teal_module"
    )
  })

  it("creates a module with datanames taken from data extracts", {
    mod <- tm_g_bivariate(
      "a label",
      list(
        mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
        mock_data_extract_spec(dataname = "B", select_multiple = FALSE)
      ),
      mock_data_extract_spec(dataname = "C", select_multiple = FALSE)
    )

    testthat::expect_setequal(
      mod$datanames,
      c("A", "B", "C")
    )
  })

  it("creates a module that is bookmarkable", {
    mod <- tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE)
    )

    testthat::expect_true(attr(mod, "teal_bookmarkable"))
  })
})

testthat::describe("tm_g_bivariate data extract spec validation", {
  it("fails when x contains a spec with multiple selection", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        list(
          mock_data_extract_spec(select_multiple = TRUE)
        ),
        list()
      ),
      "'x' should not allow multiple selection"
    )
  })

  it("fails when x contains multiple spec with at least one multiple selection", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        list(
          mock_data_extract_spec(select_multiple = FALSE),
          mock_data_extract_spec(select_multiple = TRUE)
        ),
        list(mock_data_extract_spec(select_multiple = FALSE))
      ),
      "'x' should not allow multiple selection"
    )
  })

  it("fails when y contains a spec with multiple selection", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        list(
          mock_data_extract_spec(select_multiple = FALSE),
          mock_data_extract_spec(select_multiple = TRUE)
        )
      ),
      "'y' should not allow multiple selection"
    )
  })

  it("fails when row_facet contains a spec with multiple selection", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        row_facet = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "'row_facet' should not allow multiple selection"
    )
  })

  it("fails when col_facet contains a spec with multiple selection", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        col_facet = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "'col_facet' should not allow multiple selection"
    )
  })

  it("accepts NULL for row_facet and col_facet", {
    testthat::expect_s3_class(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        row_facet = NULL,
        col_facet = NULL
      ),
      "teal_module"
    )
  })

  it("fails when color contains a spec with multiple selection", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        color_settings = TRUE,
        color = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "'color' should not allow multiple selection"
    )
  })

  it("fails when fill contains a spec with multiple selection", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        color_settings = TRUE,
        fill = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "'fill' should not allow multiple selection"
    )
  })

  it("fails when size contains a spec with multiple selection", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        color_settings = TRUE,
        size = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "'size' should not allow multiple selection"
    )
  })
})

testthat::describe("tm_g_bivariate plot dimensions validation", {
  it("fails when plot_height value is greater than maximum", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        plot_height = c(100, 10, 20)
      ),
      "Assertion on 'plot_height' failed: Element 1 is not <= 20"
    )
  })

  it("fails when plot_height value is less than minimum", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        plot_height = c(1, 10, 20)
      ),
      "Assertion on 'plot_height' failed: Element 1 is not >= 10"
    )
  })

  it("fails when plot_height has wrong length", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        plot_height = 100
      ),
      "Assertion on 'plot_height' failed: Must have length 3, but has length 1"
    )
  })

  it("fails when plot_width value is greater than maximum", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        plot_width = c(100, 10, 20)
      ),
      "Assertion on 'plot_width' failed: Element 1 is not <= 20"
    )
  })

  it("fails when plot_width value is less than minimum", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        plot_width = c(1, 10, 20)
      ),
      "Assertion on 'plot_width' failed: Element 1 is not >= 10"
    )
  })

  it("fails when plot_width has wrong length", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        plot_width = 100
      ),
      "Assertion on 'plot_width' failed: Must have length 3, but has length 1"
    )
  })
})

testthat::describe("tm_g_bivariate color settings validation", {
  it("fails when color_settings is FALSE and color is supplied", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        color_settings = FALSE,
        color = mock_data_extract_spec()
      ),
      "'color_settings' argument needs to be set to TRUE if 'color', 'fill', and/or 'size' is/are supplied."
    )
  })

  it("fails when color_settings is FALSE and size is supplied", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        color_settings = FALSE,
        size = mock_data_extract_spec()
      ),
      "'color_settings' argument needs to be set to TRUE if 'color', 'fill', and/or 'size' is/are supplied."
    )
  })

  it("fails when color_settings is FALSE and fill is supplied", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        color_settings = FALSE,
        fill = mock_data_extract_spec()
      ),
      "'color_settings' argument needs to be set to TRUE if 'color', 'fill', and/or 'size' is/are supplied."
    )
  })

  it("determines color, size and fill when color_settings is TRUE", {
    mod <- tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE),
      color_settings = TRUE
    )

    testthat::expect_contains(
      vapply(
        unlist(mod$ui_args[c("color", "size", "fill")], recursive = FALSE),
        class,
        character(1)
      ),
      "data_extract_spec"
    )
  })
})

testthat::describe("tm_g_bivariate argument validation", {
  it("fails when label is not a string", {
    testthat::expect_error(
      tm_g_bivariate(
        label = 123,
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE)
      ),
      "Assertion on 'label' failed"
    )
  })

  it("fails when facet is not a logical", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        row_facet = mock_data_extract_spec(select_multiple = FALSE),
        facet = "TRUE"
      ),
      "Assertion on 'facet' failed"
    )
  })

  it("fails when use_density is not a logical", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        use_density = "TRUE"
      ),
      "Assertion on 'use_density' failed"
    )
  })

  it("fails when free_x_scales is not a logical", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        free_x_scales = "TRUE"
      ),
      "Assertion on 'free_x_scales' failed"
    )
  })

  it("fails when free_y_scales is not a logical", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        free_y_scales = "TRUE"
      ),
      "Assertion on 'free_y_scales' failed"
    )
  })

  it("fails when rotate_xaxis_labels is not a logical", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        rotate_xaxis_labels = "TRUE"
      ),
      "Assertion on 'rotate_xaxis_labels' failed"
    )
  })

  it("fails when swap_axes is not a logical", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        swap_axes = "TRUE"
      ),
      "Assertion on 'swap_axes' failed"
    )
  })

  it("fails when ggtheme is invalid", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        ggtheme = "invalid_theme"
      ),
      "should be one of"
    )
  })

  it("accepts valid ggtheme options", {
    for (theme in c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void")) {
      testthat::expect_s3_class(
        tm_g_bivariate(
          "a label",
          mock_data_extract_spec(select_multiple = FALSE),
          mock_data_extract_spec(select_multiple = FALSE),
          ggtheme = theme
        ),
        "teal_module"
      )
    }
  })

  it("fails when ggplot2_args is not a ggplot2_args object", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        ggplot2_args = "not a ggplot2_args"
      ),
      "Assertion on 'ggplot2_args' failed"
    )
  })

  it("accepts valid ggplot2_args", {
    testthat::expect_s3_class(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        ggplot2_args = teal.widgets::ggplot2_args()
      ),
      "teal_module"
    )
  })

  it("fails when pre_output is not a valid HTML object", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        pre_output = 123
      ),
      "Assertion on 'pre_output' failed"
    )
  })

  it("accepts valid pre_output", {
    testthat::expect_s3_class(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        pre_output = tags$div("Pre output")
      ),
      "teal_module"
    )
  })

  it("fails when post_output is not a valid HTML object", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        post_output = 123
      ),
      "Assertion on 'post_output' failed"
    )
  })

  it("accepts valid post_output", {
    testthat::expect_s3_class(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        post_output = tags$div("Post output")
      ),
      "teal_module"
    )
  })

  it("fails when decorators has invalid object types", {
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        decorators = list(
          plot = "not a teal_transform_module"
        )
      ),
      "Assertion on 'decorators' failed|Make sure that the named list contains"
    )
  })

  it("accepts valid decorators", {
    testthat::expect_s3_class(
      tm_g_bivariate(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        decorators = list(
          plot = teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
  })
})

create_bivariate_module <- function(data, x_vars, y_vars, x_selected, y_selected, ...) {
  tm_g_bivariate(
    x = list(
      teal.transform::data_extract_spec(
        dataname = "test_data",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices(
            data = isolate(data())[["test_data"]],
            x_vars
          ),
          selected = x_selected,
          multiple = FALSE
        )
      )
    ),
    y = list(
      teal.transform::data_extract_spec(
        dataname = "test_data",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices(
            data = isolate(data())[["test_data"]],
            y_vars
          ),
          selected = if (is.null(y_selected)) character(0) else y_selected,
          multiple = FALSE
        )
      )
    ),
    ...
  )
}

testthat::describe("tm_g_bivariate module server behavior", {
  it("server function executes successfully with numeric variables", {
    data <- create_test_data(data.frame(
      x_var = 1:30,
      y_var = 1:30 + rnorm(30)
    ))

    mod <- create_bivariate_module(
      data = data,
      x_vars = c("x_var", "y_var"),
      y_vars = c("x_var", "y_var"),
      x_selected = "x_var",
      y_selected = "y_var"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "x-dataset_test_data_singleextract-select" = "x_var",
          "y-dataset_test_data_singleextract-select" = "y_var",
          "use_density" = "frequency",
          "rotate_xaxis_labels" = FALSE,
          "swap_axes" = FALSE,
          "ggtheme" = "gray",
          "alpha" = 0.5,
          "fixed_size" = 2,
          "add_lines" = FALSE,
          "facetting" = FALSE
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })

  it("server function executes successfully with factor variables", {
    data <- create_test_data(data.frame(
      x_var = factor(rep(c("A", "B", "C"), 10)),
      y_var = factor(rep(c("X", "Y"), 15))
    ))

    mod <- create_bivariate_module(
      data = data,
      x_vars = c("x_var", "y_var"),
      y_vars = c("x_var", "y_var"),
      x_selected = "x_var",
      y_selected = "y_var"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "x-dataset_test_data_singleextract-select" = "x_var",
          "y-dataset_test_data_singleextract-select" = "y_var",
          "use_density" = "frequency",
          "rotate_xaxis_labels" = FALSE,
          "swap_axes" = FALSE,
          "ggtheme" = "gray",
          "add_lines" = FALSE,
          "facetting" = FALSE
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })

  it("server function executes successfully with mixed numeric and factor variables", {
    data <- create_test_data(data.frame(
      x_var = 1:30,
      y_var = factor(rep(c("A", "B"), 15))
    ))

    mod <- create_bivariate_module(
      data = data,
      x_vars = c("x_var", "y_var"),
      y_vars = c("x_var", "y_var"),
      x_selected = "x_var",
      y_selected = "y_var"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "x-dataset_test_data_singleextract-select" = "x_var",
          "y-dataset_test_data_singleextract-select" = "y_var",
          "use_density" = "frequency",
          "rotate_xaxis_labels" = FALSE,
          "swap_axes" = FALSE,
          "ggtheme" = "gray",
          "add_lines" = FALSE,
          "facetting" = FALSE
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })

  it("server function handles use_density option", {
    data <- create_test_data(data.frame(
      x_var = 1:30
    ))

    mod <- create_bivariate_module(
      data = data,
      x_vars = c("x_var"),
      y_vars = c("x_var"),
      x_selected = "x_var",
      y_selected = character(0)
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "x-dataset_test_data_singleextract-select" = "x_var",
          "y-dataset_test_data_singleextract-select" = character(0),
          "use_density" = "density",
          "rotate_xaxis_labels" = FALSE,
          "swap_axes" = FALSE,
          "ggtheme" = "gray",
          "add_lines" = FALSE,
          "facetting" = FALSE
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })

  it("server function handles rotate_xaxis_labels option", {
    data <- create_test_data(data.frame(
      x_var = factor(rep(c("A", "B", "C"), 10)),
      y_var = factor(rep(c("X", "Y"), 15))
    ))

    mod <- create_bivariate_module(
      data = data,
      x_vars = c("x_var", "y_var"),
      y_vars = c("x_var", "y_var"),
      x_selected = "x_var",
      y_selected = "y_var"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "x-dataset_test_data_singleextract-select" = "x_var",
          "y-dataset_test_data_singleextract-select" = "y_var",
          "use_density" = "frequency",
          "rotate_xaxis_labels" = TRUE,
          "swap_axes" = FALSE,
          "ggtheme" = "gray",
          "add_lines" = FALSE,
          "facetting" = FALSE
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })

  it("server function handles swap_axes option", {
    data <- create_test_data(data.frame(
      x_var = 1:30,
      y_var = 1:30 + rnorm(30)
    ))

    mod <- create_bivariate_module(
      data = data,
      x_vars = c("x_var", "y_var"),
      y_vars = c("x_var", "y_var"),
      x_selected = "x_var",
      y_selected = "y_var"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "x-dataset_test_data_singleextract-select" = "x_var",
          "y-dataset_test_data_singleextract-select" = "y_var",
          "use_density" = "frequency",
          "rotate_xaxis_labels" = FALSE,
          "swap_axes" = TRUE,
          "ggtheme" = "gray",
          "alpha" = 0.5,
          "fixed_size" = 2,
          "add_lines" = FALSE,
          "facetting" = FALSE
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })

  it("server function handles add_lines option for scatterplots", {
    data <- create_test_data(data.frame(
      x_var = 1:30,
      y_var = 1:30 + rnorm(30)
    ))

    mod <- create_bivariate_module(
      data = data,
      x_vars = c("x_var", "y_var"),
      y_vars = c("x_var", "y_var"),
      x_selected = "x_var",
      y_selected = "y_var"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "x-dataset_test_data_singleextract-select" = "x_var",
          "y-dataset_test_data_singleextract-select" = "y_var",
          "use_density" = "frequency",
          "rotate_xaxis_labels" = FALSE,
          "swap_axes" = FALSE,
          "ggtheme" = "gray",
          "alpha" = 0.5,
          "fixed_size" = 2,
          "add_lines" = TRUE,
          "facetting" = FALSE
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })

  it("server function handles different themes", {
    data <- create_test_data(data.frame(
      x_var = 1:30,
      y_var = 1:30 + rnorm(30)
    ))

    mod <- create_bivariate_module(
      data = data,
      x_vars = c("x_var", "y_var"),
      y_vars = c("x_var", "y_var"),
      x_selected = "x_var",
      y_selected = "y_var"
    )

    for (theme in c("bw", "minimal", "dark")) {
      shiny::testServer(
        mod$server,
        args = c(
          list(id = "test", data = data),
          mod$server_args
        ),
        expr = {
          session$setInputs(
            "x-dataset_test_data_singleextract-select" = "x_var",
            "y-dataset_test_data_singleextract-select" = "y_var",
            "use_density" = "frequency",
            "rotate_xaxis_labels" = FALSE,
            "swap_axes" = FALSE,
            "ggtheme" = theme,
            "alpha" = 0.5,
            "fixed_size" = 2,
            "add_lines" = FALSE,
            "facetting" = FALSE
          )
          testthat::expect_true(iv_r()$is_valid())
          output_result <- output_q()
          testthat::expect_true(inherits(output_result, "teal_data"))
        }
      )
    }
  })
})
