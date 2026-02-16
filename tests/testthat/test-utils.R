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
  # We test it with tm_gtsummary as it requires decorators to be useful to users
  create_gtsummary_module <- function(data, by_vars, include_vars, by_selected, include_selected, ...) {
    tm_gtsummary(
      by = teal.transform::data_extract_spec(
        dataname = "test_data",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices(
            data = isolate(data())[["test_data"]],
            by_vars
          ),
          selected = by_selected,
          multiple = FALSE
        )
      ),
      include = list(
        teal.transform::data_extract_spec(
          dataname = "test_data",
          select = teal.transform::select_spec(
            choices = teal.transform::variable_choices(
              data = isolate(data())[["test_data"]],
              include_vars
            ),
            selected = include_selected,
            multiple = TRUE
          )
        )
      ),
      ...
    )
  }

  it("one default decorator executes successfully", {
    data <- create_test_data(mtcars)
    mod <- create_gtsummary_module(
      data,
      by_vars = c("am", "gear"),
      include_vars = c("carb", "cyl"),
      by_selected = c("am"),
      include_selected = c("carb", "cyl"),
      decorators = list(
        default = teal_transform_module()
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
          "by-dataset_test_data_singleextract-select" = "am",
          "include-dataset_test_data_singleextract-select" = c("carb", "cyl")
        )
        testthat::expect_true(endsWith(get_code(print_output_decorated()), "table"))
      }
    )
  })


  it("one decorator failure is handled", {
    data <- create_test_data(mtcars)
    mod <- create_gtsummary_module(
      data,
      by_vars = c("am", "gear"),
      include_vars = c("carb", "cyl"),
      by_selected = c("am"),
      include_selected = c("carb", "cyl"),
      decorators = list(
        default = teal_transform_module(server = function(id, data) {
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
          "by-dataset_test_data_singleextract-select" = "am",
          "include-dataset_test_data_singleextract-select" = c("carb", "cyl")
        )
        testthat::expect_is(tryCatch(print_output_decorated(), error = function(e) e), "shiny.silent.error")
      }
    )
  })

  it("Multiple decorators execute successfully", {
    data <- create_test_data(mtcars)
    mod <- create_gtsummary_module(
      data,
      by_vars = c("am", "gear"),
      include_vars = c("carb", "cyl"),
      by_selected = c("am"),
      include_selected = c("carb", "cyl"),
      decorators = list(table = list(teal_transform_module(), teal_transform_module()))
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test_data", data = data),
        mod$server_args
      ),
      {
        session$setInputs(
          "by-dataset_test_data_singleextract-select" = "am",
          "include-dataset_test_data_singleextract-select" = c("carb", "cyl")
        )
        testthat::expect_true(endsWith(get_code(print_output_decorated()), "table"))
      }
    )
  })

  it("Default and multiple decorators to one object execute successfully", {
    data <- create_test_data(mtcars)
    mod <- create_gtsummary_module(
      data,
      by_vars = c("am", "gear"),
      include_vars = c("carb", "cyl"),
      by_selected = c("am"),
      include_selected = c("carb", "cyl"),
      decorators = list(
        default = teal_transform_module(),
        table = list(teal_transform_module(), teal_transform_module())
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
          "by-dataset_test_data_singleextract-select" = "am",
          "include-dataset_test_data_singleextract-select" = c("carb", "cyl")
        )
        testthat::expect_true(endsWith(get_code(print_output_decorated()), "table"))
      }
    )
  })
  it("Default and one decorator to one object executes successfully", {
    data <- create_test_data(mtcars)
    mod <- create_gtsummary_module(
      data,
      by_vars = c("am", "gear"),
      include_vars = c("carb", "cyl"),
      by_selected = c("am"),
      include_selected = c("carb", "cyl"),
      decorators = list(
        default = teal_transform_module(),
        table = teal_transform_module()
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
          "by-dataset_test_data_singleextract-select" = "am",
          "include-dataset_test_data_singleextract-select" = c("carb", "cyl")
        )
        testthat::expect_true(endsWith(get_code(print_output_decorated()), "table"))
      }
    )
  })
})
