testthat::describe("add_facet_labels", {
  it("returns ggplotGrob when both labels are NULL", {
    p <- ggplot2::ggplot(mtcars) +
      ggplot2::aes(x = mpg, y = disp) +
      ggplot2::geom_point()

    result <- add_facet_labels(p, xfacet_label = NULL, yfacet_label = NULL)
    testthat::expect_s3_class(result, "gtable")
  })

  it("adds x facet label when provided", {
    p <- ggplot2::ggplot(mtcars) +
      ggplot2::aes(x = mpg, y = disp) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(gear ~ cyl)

    result <- add_facet_labels(p, xfacet_label = "cylinders", yfacet_label = NULL)
    testthat::expect_s3_class(result, "gTree")
  })

  it("adds y facet label when provided", {
    p <- ggplot2::ggplot(mtcars) +
      ggplot2::aes(x = mpg, y = disp) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(gear ~ cyl)

    result <- add_facet_labels(p, xfacet_label = NULL, yfacet_label = "gear")
    testthat::expect_s3_class(result, "gTree")
  })

  it("adds both x and y facet labels when both provided", {
    p <- ggplot2::ggplot(mtcars) +
      ggplot2::aes(x = mpg, y = disp) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(gear ~ cyl)

    result <- add_facet_labels(p, xfacet_label = "cylinders", yfacet_label = "gear")
    testthat::expect_s3_class(result, "gTree")
  })

  it("joins multiple x facet labels with &", {
    p <- ggplot2::ggplot(mtcars) +
      ggplot2::aes(x = mpg, y = disp) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(gear ~ cyl)

    result <- add_facet_labels(p, xfacet_label = c("cylinders", "type"), yfacet_label = NULL)
    testthat::expect_s3_class(result, "gTree")
  })

  it("fails when p is not a ggplot object", {
    testthat::expect_error(
      add_facet_labels("not a ggplot", xfacet_label = "test"),
      "Assertion on 'p' failed"
    )
  })

  it("fails when xfacet_label is not character", {
    p <- ggplot2::ggplot(mtcars) +
      ggplot2::aes(x = mpg, y = disp) +
      ggplot2::geom_point()

    testthat::expect_error(
      add_facet_labels(p, xfacet_label = 123),
      "Assertion on 'xfacet_label' failed"
    )
  })

  it("fails when yfacet_label is not character", {
    p <- ggplot2::ggplot(mtcars) +
      ggplot2::aes(x = mpg, y = disp) +
      ggplot2::geom_point()

    testthat::expect_error(
      add_facet_labels(p, yfacet_label = 123),
      "Assertion on 'yfacet_label' failed"
    )
  })
})

testthat::describe("Module with decorators:", {
  # We test it with tm_g_scatterplot as it requires decorators to be useful to users
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

  it("one default decorator executes successfully", {
    data <- create_test_data(mtcars)
    mod <- create_scatterplot_module(
      data,
      x_vars = c("mpg", "disp"),
      y_vars = c("mpg", "disp"),
      x_selected = "mpg",
      y_selected = "disp",
      decorators = list(
        all = teal_transform_module()
      )
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test_data", data = data),
        mod$server_args
      ),
      {
        session$setInputs(
          "x-dataset_test_data_singleextract-select" = "mpg",
          "y-dataset_test_data_singleextract-select" = "disp",
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
          "data_table_rows" = 10,
          color = "#000000"
        )
        session$flushReact()
        testthat::expect_true(endsWith(get_code(session$returned()), "plot"))
      }
    )
  })


  it("one decorator failure is handled", {
    data <- create_test_data(mtcars)
    mod <- create_scatterplot_module(
      data,
      x_vars = c("mpg", "disp"),
      y_vars = c("mpg", "disp"),
      x_selected = "mpg",
      y_selected = "disp",
      decorators = list(
        all = teal_transform_module(server = function(id, data) {
          reactive({
            stop()
          })
        })
      )
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test_data", data = data),
        mod$server_args
      ),
      {
        session$setInputs(
          "x-dataset_test_data_singleextract-select" = "am",
          "y-dataset_test_data_singleextract-select" = "gear",
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
          "data_table_rows" = 10,
          color = "#000000"
        )
        session$flushReact()
        testthat::expect_is(tryCatch(session$returned(), error = function(e) e), "shiny.silent.error")
      }
    )
  })

  it("Multiple decorators execute successfully", {
    data <- create_test_data(mtcars)
    mod <- create_scatterplot_module(
      data,
      x_vars = c("am", "gear"),
      y_vars = c("carb", "cyl"),
      x_selected = "am",
      y_selected = "carb",
      decorators = list(plot = list(teal_transform_module(), teal_transform_module()))
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test_data", data = data),
        mod$server_args
      ),
      {
        session$setInputs(
          "x-dataset_test_data_singleextract-select" = "am",
          "y-dataset_test_data_singleextract-select" = "gear",
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
          "data_table_rows" = 10,
          color = "#000000"
        )
        session$flushReact()
        testthat::expect_true(endsWith(get_code(session$returned()), "plot"))
      }
    )
  })

  it("Default and multiple decorators to one object execute successfully", {
    data <- create_test_data(mtcars)
    mod <- create_scatterplot_module(
      data,
      x_vars = c("am", "gear"),
      y_vars = c("carb", "cyl"),
      x_selected = "am",
      y_selected = "carb",
      decorators = list(
        all = teal_transform_module(),
        plot = list(teal_transform_module(), teal_transform_module())
      )
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test_data", data = data),
        mod$server_args
      ),
      {
        session$setInputs(
          "x-dataset_test_data_singleextract-select" = "am",
          "y-dataset_test_data_singleextract-select" = "gear",
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
          "data_table_rows" = 10,
          color = "#000000"
        )
        session$flushReact()
        testthat::expect_true(endsWith(get_code(session$returned()), "plot"))
      }
    )
  })

  it("Default and one decorator to one object executes successfully", {
    data <- create_test_data(mtcars)
    mod <- create_scatterplot_module(
      data,
      x_vars = c("am", "gear"),
      y_vars = c("carb", "cyl"),
      x_selected = "am",
      y_selected = "carb",
      decorators = list(
        all = teal_transform_module(),
        plot = teal_transform_module()
      )
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test_data", data = data),
        mod$server_args
      ),
      {
        session$setInputs(
          "x-dataset_test_data_singleextract-select" = "am",
          "y-dataset_test_data_singleextract-select" = "gear",
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
          "data_table_rows" = 10,
          color = "#000000"
        )
        session$flushReact()
        testthat::expect_true(endsWith(get_code(session$returned()), "plot"))
      }
    )
  })
})
