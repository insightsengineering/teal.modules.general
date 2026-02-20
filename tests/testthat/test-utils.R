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
        testthat::expect_true(endsWith(get_code(session$returned()), "table"))
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
        table = teal_transform_module(server = function(id, data) {
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
        testthat::expect_is(tryCatch(session$returned(), error = function(e) e), "shiny.silent.error")
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
      args = c(list(id = "test_data", data = data), mod$server_args),
      {
        session$setInputs(
          "by-dataset_test_data_singleextract-select" = "am",
          "include-dataset_test_data_singleextract-select" = c("carb", "cyl")
        )
        testthat::expect_match(get_code(session$returned()), "\ntable$")
      }
    )
  })

  it("2 decorated objects execute successfully and modify returned reactive", {
    data <- create_test_data(mtcars)
    mod <- tm_g_distribution(
      dist_var = data_extract_spec(
        dataname = "test_data",
        select = select_spec(variable_choices("test_data"), "mpg")
      ),
      decorators = list(
        test_table = list(
          teal_transform_module(
            server = function(id, data) {
              reactive(within(data(), test_ncols <- ncol(test_table)))
            }
          ),
          teal_transform_module(
            server = function(id, data) {
              reactive(within(data(), test_ncols <- test_ncols + 1))
            }
          )
        ),
        summary_table = teal_transform_module(
          server = function(id, data) {
            reactive(within(data(), summary_ncols <- ncol(summary_table)))
          }
        )
      )
    )

    shiny::testServer(
      mod$server,
      args = c(list(id = "test_data", data = data), mod$server_args),
      {
        session$setInputs(
          "dist_i-dataset_test_data_singleextract-select" = "mpg",
          dist_tests = "Shapiro-Wilk",
          bins = 10,
          main_type = "Density",
          add_dens = TRUE,
          tabs = "Histogram",
          roundn = 2
        )
        testthat::expect_equal(session$returned()$test_ncols, 3 + 1)
        testthat::expect_equal(session$returned()$summary_ncols, 6)
      }
    )
  })
})
