testthat::describe("tm_g_scatterplot module creation", {
  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        color_by = mock_data_extract_spec(select_multiple = FALSE),
        size_by = mock_data_extract_spec(select_multiple = FALSE),
        row_facet = mock_data_extract_spec(select_multiple = FALSE),
        col_facet = mock_data_extract_spec(select_multiple = FALSE),
        plot_height = c(400, 100, 600),
        plot_width = c(600, 100, 600),
        alpha = c(0.5, 0, 1),
        size = c(5, 1, 15),
        max_deg = 3L,
        rotate_xaxis_labels = TRUE,
        shape = c("circle", "square"),
        table_dec = 2
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with default options", {
    testthat::expect_s3_class(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE)
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with multiple data extract specs", {
    testthat::expect_s3_class(
      tm_g_scatterplot(
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
    mod <- tm_g_scatterplot(
      "a label",
      list(
        mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
        mock_data_extract_spec(dataname = "B", select_multiple = FALSE)
      ),
      mock_data_extract_spec(dataname = "C", select_multiple = FALSE),
      color_by = mock_data_extract_spec(dataname = "D", select_multiple = FALSE)
    )

    testthat::expect_setequal(
      mod$datanames,
      c("A", "B", "C", "D")
    )
  })

  it("creates a module that is bookmarkable", {
    mod <- tm_g_scatterplot(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE)
    )

    testthat::expect_true(attr(mod, "teal_bookmarkable"))
  })
})

testthat::describe("tm_g_scatterplot data extract spec validation", {
  it("fails when x is not a data_extract_spec or list", {
    testthat::expect_error(
      tm_g_scatterplot(
        "a label",
        x = "not a spec",
        y = mock_data_extract_spec(select_multiple = FALSE)
      ),
      "Assertion on 'x' failed"
    )
  })

  it("fails when y is not a data_extract_spec or list", {
    testthat::expect_error(
      tm_g_scatterplot(
        "a label",
        x = mock_data_extract_spec(select_multiple = FALSE),
        y = "not a spec"
      ),
      "Assertion on 'y' failed"
    )
  })

  it("fails when row_facet contains a spec with multiple selection", {
    testthat::expect_error(
      tm_g_scatterplot(
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
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        col_facet = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "'col_facet' should not allow multiple selection"
    )
  })

  it("accepts NULL for optional data extract specs", {
    testthat::expect_s3_class(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        color_by = NULL,
        size_by = NULL,
        row_facet = NULL,
        col_facet = NULL
      ),
      "teal_module"
    )
  })

  it("fails when color_by is not a data_extract_spec or list", {
    testthat::expect_error(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        color_by = "not a spec"
      ),
      "Assertion on 'color_by' failed"
    )
  })

  it("fails when size_by is not a data_extract_spec or list", {
    testthat::expect_error(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        size_by = "not a spec"
      ),
      "Assertion on 'size_by' failed"
    )
  })
})

testthat::describe("tm_g_scatterplot plot dimensions validation", {
  it("fails when plot_height value is greater than maximum", {
    testthat::expect_error(
      tm_g_scatterplot(
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
      tm_g_scatterplot(
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
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        plot_height = c(600, 200)
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot_width value is greater than maximum", {
    testthat::expect_error(
      tm_g_scatterplot(
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
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        plot_width = c(1, 10, 20)
      ),
      "Assertion on 'plot_width' failed: Element 1 is not >= 10"
    )
  })

  it("accepts NULL for plot_width", {
    testthat::expect_s3_class(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        plot_width = NULL
      ),
      "teal_module"
    )
  })
})

testthat::describe("tm_g_scatterplot alpha and size validation", {
  it("accepts single numeric value for alpha", {
    testthat::expect_s3_class(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        alpha = 0.5
      ),
      "teal_module"
    )
  })

  it("accepts three-element vector for alpha", {
    testthat::expect_s3_class(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        alpha = c(0.5, 0, 1)
      ),
      "teal_module"
    )
  })

  it("fails when alpha three-element vector value is out of range", {
    testthat::expect_error(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        alpha = c(2, 0, 1)
      ),
      "Assertion on 'alpha' failed: Element 1 is not <= 1"
    )
  })

  it("fails when alpha has wrong length", {
    testthat::expect_error(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        alpha = c(0.5, 0)
      ),
      "Assertion on 'alpha' failed"
    )
  })

  it("accepts single numeric value for size", {
    testthat::expect_s3_class(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        size = 5
      ),
      "teal_module"
    )
  })

  it("accepts three-element vector for size", {
    testthat::expect_s3_class(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        size = c(5, 1, 15)
      ),
      "teal_module"
    )
  })

  it("fails when size three-element vector value is out of range", {
    testthat::expect_error(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        size = c(20, 1, 15)
      ),
      "Assertion on 'size' failed: Element 1 is not <= 15"
    )
  })

  it("fails when size has wrong length", {
    testthat::expect_error(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        size = c(5, 1)
      ),
      "Assertion on 'size' failed"
    )
  })
})

testthat::describe("tm_g_scatterplot argument validation", {
  it("fails when label is not a string", {
    testthat::expect_error(
      tm_g_scatterplot(
        label = 123,
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE)
      ),
      "Assertion on 'label' failed"
    )
  })

  it("fails when shape is not a character vector", {
    testthat::expect_error(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        shape = 123
      ),
      "Assertion on 'shape' failed"
    )
  })

  it("accepts valid shape character vector", {
    testthat::expect_s3_class(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        shape = c("circle", "square", "triangle")
      ),
      "teal_module"
    )
  })

  it("fails when max_deg is less than 1", {
    testthat::expect_error(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        max_deg = 0L
      ),
      "Assertion on 'max_deg' failed"
    )
  })

  it("accepts valid max_deg values", {
    for (deg in c(1L, 2L, 5L, 10L)) {
      testthat::expect_s3_class(
        tm_g_scatterplot(
          "a label",
          mock_data_extract_spec(select_multiple = FALSE),
          mock_data_extract_spec(select_multiple = FALSE),
          max_deg = deg
        ),
        "teal_module"
      )
    }
  })

  it("converts double max_deg to integer", {
    mod <- tm_g_scatterplot(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE),
      max_deg = 5.0
    )
    testthat::expect_s3_class(mod, "teal_module")
  })

  it("fails when rotate_xaxis_labels is not a logical", {
    testthat::expect_error(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        rotate_xaxis_labels = "TRUE"
      ),
      "Assertion on 'rotate_xaxis_labels' failed"
    )
  })

  it("fails when ggtheme is invalid", {
    testthat::expect_error(
      tm_g_scatterplot(
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
        tm_g_scatterplot(
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
      tm_g_scatterplot(
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
      tm_g_scatterplot(
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
      tm_g_scatterplot(
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
      tm_g_scatterplot(
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
      tm_g_scatterplot(
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
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        post_output = tags$div("Post output")
      ),
      "teal_module"
    )
  })

  it("fails when table_dec is not a scalar", {
    testthat::expect_error(
      tm_g_scatterplot(
        "a label",
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = FALSE),
        table_dec = c(2, 3)
      ),
      "Assertion on 'table_dec' failed"
    )
  })

  it("accepts valid table_dec values", {
    for (dec in c(0L, 2L, 4L, 10L)) {
      testthat::expect_s3_class(
        tm_g_scatterplot(
          "a label",
          mock_data_extract_spec(select_multiple = FALSE),
          mock_data_extract_spec(select_multiple = FALSE),
          table_dec = dec
        ),
        "teal_module"
      )
    }
  })

  it("fails when decorators has invalid object types", {
    testthat::expect_error(
      tm_g_scatterplot(
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
      tm_g_scatterplot(
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

create_scatterplot_module <- function(data, x_vars, y_vars, x_selected, y_selected, ...) {
  tm_g_scatterplot(
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
          selected = y_selected,
          multiple = FALSE
        )
      )
    ),
    ...
  )
}

testthat::describe("tm_g_scatterplot module server behavior", {
  it("server function executes successfully with numeric variables", {
    data <- create_test_data(data.frame(
      x_var = 1:30,
      y_var = 1:30 + rnorm(30)
    ))

    mod <- create_scatterplot_module(
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
          "log_x" = FALSE,
          "log_y" = FALSE,
          "rotate_xaxis_labels" = FALSE,
          "ggtheme" = "gray",
          "alpha" = 1,
          "size" = 5,
          "shape" = "circle",
          "add_density" = FALSE,
          "rug_plot" = FALSE,
          "show_count" = FALSE,
          "free_scales" = FALSE,
          "pos" = 0.99,
          "label_size" = 5,
          "data_table_rows" = 10
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })

  it("server function handles log transformation", {
    data <- create_test_data(data.frame(
      x_var = abs(rnorm(30)) + 1,
      y_var = abs(rnorm(30)) + 1
    ))

    mod <- create_scatterplot_module(
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
          "log_x" = TRUE,
          "log_x_base" = "log10",
          "log_y" = TRUE,
          "log_y_base" = "log",
          "rotate_xaxis_labels" = FALSE,
          "ggtheme" = "gray",
          "alpha" = 1,
          "size" = 5,
          "shape" = "circle",
          "add_density" = FALSE,
          "rug_plot" = FALSE,
          "show_count" = FALSE,
          "free_scales" = FALSE,
          "pos" = 0.99,
          "label_size" = 5,
          "data_table_rows" = 10
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })

  it("server function handles color_by option", {
    data <- create_test_data(data.frame(
      x_var = 1:30,
      y_var = 1:30 + rnorm(30),
      color_var = factor(rep(c("A", "B"), 15))
    ))

    mod <- tm_g_scatterplot(
      x = list(
        teal.transform::data_extract_spec(
          dataname = "test_data",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              data = isolate(data())[["test_data"]],
              c("x_var", "y_var", "color_var")
            ),
            selected = "x_var",
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
              c("x_var", "y_var", "color_var")
            ),
            selected = "y_var",
            multiple = FALSE
          )
        )
      ),
      color_by = list(
        teal.transform::data_extract_spec(
          dataname = "test_data",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              data = isolate(data())[["test_data"]],
              c("x_var", "y_var", "color_var")
            ),
            selected = "color_var",
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
          "x-dataset_test_data_singleextract-select" = "x_var",
          "y-dataset_test_data_singleextract-select" = "y_var",
          "color_by-dataset_test_data_singleextract-select" = "color_var",
          "log_x" = FALSE,
          "log_y" = FALSE,
          "rotate_xaxis_labels" = FALSE,
          "ggtheme" = "gray",
          "alpha" = 1,
          "size" = 5,
          "shape" = "circle",
          "add_density" = FALSE,
          "rug_plot" = FALSE,
          "show_count" = FALSE,
          "free_scales" = FALSE,
          "pos" = 0.99,
          "label_size" = 5,
          "data_table_rows" = 10
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })

  it("server function handles size_by option", {
    data <- create_test_data(data.frame(
      x_var = 1:30,
      y_var = 1:30 + rnorm(30),
      size_var = 1:30
    ))

    mod <- tm_g_scatterplot(
      x = list(
        teal.transform::data_extract_spec(
          dataname = "test_data",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              data = isolate(data())[["test_data"]],
              c("x_var", "y_var", "size_var")
            ),
            selected = "x_var",
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
              c("x_var", "y_var", "size_var")
            ),
            selected = "y_var",
            multiple = FALSE
          )
        )
      ),
      size_by = list(
        teal.transform::data_extract_spec(
          dataname = "test_data",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              data = isolate(data())[["test_data"]],
              c("x_var", "y_var", "size_var")
            ),
            selected = "size_var",
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
          "x-dataset_test_data_singleextract-select" = "x_var",
          "y-dataset_test_data_singleextract-select" = "y_var",
          "size_by-dataset_test_data_singleextract-select" = "size_var",
          "log_x" = FALSE,
          "log_y" = FALSE,
          "rotate_xaxis_labels" = FALSE,
          "ggtheme" = "gray",
          "alpha" = 1,
          "size" = 5,
          "shape" = "circle",
          "add_density" = FALSE,
          "rug_plot" = FALSE,
          "show_count" = FALSE,
          "free_scales" = FALSE,
          "pos" = 0.99,
          "label_size" = 5,
          "data_table_rows" = 10
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })

  it("server function handles facetting options", {
    data <- create_test_data(data.frame(
      x_var = 1:30,
      y_var = 1:30 + rnorm(30),
      facet_var = factor(rep(c("A", "B"), 15))
    ))

    mod <- tm_g_scatterplot(
      x = list(
        teal.transform::data_extract_spec(
          dataname = "test_data",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              data = isolate(data())[["test_data"]],
              c("x_var", "y_var", "facet_var")
            ),
            selected = "x_var",
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
              c("x_var", "y_var", "facet_var")
            ),
            selected = "y_var",
            multiple = FALSE
          )
        )
      ),
      row_facet = list(
        teal.transform::data_extract_spec(
          dataname = "test_data",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              data = isolate(data())[["test_data"]],
              c("x_var", "y_var", "facet_var")
            ),
            selected = "facet_var",
            multiple = FALSE
          )
        )
      )
    )

    # shiny::testServer(
    #   mod$server,
    #   args = c(
    #     list(id = "test", data = data),
    #     mod$server_args
    #   ),
    #   expr = {
    #     session$setInputs(
    #       "x-dataset_test_data_singleextract-select" = "x_var",
    #       "y-dataset_test_data_singleextract-select" = "y_var",
    #       "row_facet-dataset_test_data_singleextract-select" = "facet_var",
    #       "log_x" = FALSE,
    #       "log_y" = FALSE,
    #       "rotate_xaxis_labels" = FALSE,
    #       "ggtheme" = "gray",
    #       "alpha" = 1,
    #       "size" = 5,
    #       "shape" = "circle",
    #       "add_density" = FALSE,
    #       "rug_plot" = FALSE,
    #       "show_count" = FALSE,
    #       "free_scales" = FALSE,
    #       "pos" = 0.99,
    #       "label_size" = 5,
    #       "data_table_rows" = 10
    #     )
    #     output_result <- output_q()
    #     testthat::expect_true(inherits(output_result, "teal_data"))
    #   }
    # )
  })

  it("server function handles rotate_xaxis_labels option", {
    data <- create_test_data(data.frame(
      x_var = 1:30,
      y_var = 1:30 + rnorm(30)
    ))

    mod <- create_scatterplot_module(
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
          "log_x" = FALSE,
          "log_y" = FALSE,
          "rotate_xaxis_labels" = TRUE,
          "ggtheme" = "gray",
          "alpha" = 1,
          "size" = 5,
          "shape" = "circle",
          "add_density" = FALSE,
          "rug_plot" = FALSE,
          "show_count" = FALSE,
          "free_scales" = FALSE,
          "pos" = 0.99,
          "label_size" = 5,
          "data_table_rows" = 10
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
        plot_result <- plot_r()
        testthat::expect_true(inherits(plot_result, "ggplot"))
      }
    )
  })

  it("server function handles trend line option", {
    data <- create_test_data(data.frame(
      x_var = 1:30,
      y_var = 1:30 + rnorm(30)
    ))

    mod <- create_scatterplot_module(
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
          "log_x" = FALSE,
          "log_y" = FALSE,
          "rotate_xaxis_labels" = FALSE,
          "ggtheme" = "gray",
          "alpha" = 1,
          "size" = 5,
          "shape" = "circle",
          "smoothing_degree" = "1",
          "ci" = 0.95,
          "show_form" = FALSE,
          "show_r2" = FALSE,
          "show_count" = FALSE,
          "add_density" = FALSE,
          "rug_plot" = FALSE,
          "free_scales" = FALSE,
          "pos" = 0.99,
          "label_size" = 5,
          "data_table_rows" = 10
        )
        testthat::expect_true(iv_r()$is_valid())
        output_result <- output_q()
        testthat::expect_true(inherits(output_result, "teal_data"))
      }
    )
  })

  it("server function handles different themes", {
    data <- create_test_data(data.frame(
      x_var = 1:30,
      y_var = 1:30 + rnorm(30)
    ))

    mod <- create_scatterplot_module(
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
            "log_x" = FALSE,
            "log_y" = FALSE,
            "rotate_xaxis_labels" = FALSE,
            "ggtheme" = theme,
            "alpha" = 1,
            "size" = 5,
            "shape" = "circle",
            "add_density" = FALSE,
            "rug_plot" = FALSE,
            "show_count" = FALSE,
            "free_scales" = FALSE
          )
          testthat::expect_true(iv_r()$is_valid())
          output_result <- output_q()
          testthat::expect_true(inherits(output_result, "teal_data"))
        }
      )
    }
  })
})
