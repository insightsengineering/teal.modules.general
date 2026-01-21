testthat::describe("tm_g_distribtuion module creation", {
  it("creates a teal_module object with default options", {
    testthat::expect_s3_class(
      tm_g_distribution(dist_var = mock_data_extract_spec("iris", "Petal.Length")),
      "teal_module"
    )
  })

  it("creates a teal_module object with all data_extract_specs", {
    testthat::expect_s3_class(
      tm_g_distribution(
        dist_var = mock_data_extract_spec("iris", "Petal.Length"),
        strata_var = mock_data_extract_spec("iris", "Species"),
        group_var = mock_data_extract_spec("iris", "Species")
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with list of data extract specs", {
    testthat::expect_s3_class(
      tm_g_distribution(
        dist_var = list(
          mock_data_extract_spec("iris", "Petal.Length"),
          mock_data_extract_spec("iris", "Sepal.Length")
        )
      ),
      "teal_module"
    )
  })

  it("creates a module with datanames taken from data extracts", {
    mod <- tm_g_distribution(dist_var = list(
      mock_data_extract_spec("iris", "Petal.Length"),
      mock_data_extract_spec("mtcars", "cyl")
    ))
    testthat::expect_setequal(mod$datanames, c("iris", "mtcars"))
  })

  it("creates a module that is bookmarkable", {
    testthat::expect_true(
      attr(
        tm_g_distribution(dist_var = mock_data_extract_spec("iris", "Petal.Length")),
        "teal_bookmarkable",
        exact = TRUE
      )
    )
  })
})

testthat::describe("tm_g_distribution input validation", {
  spec <- mock_data_extract_spec("iris", "Petal.Length")
  it("fails when label is not a string", {
    testthat::expect_error(
      tm_g_distribution(label = 123, dist_var = mock_data_extract_spec("iris", "Petal.Length")),
      "label"
    )
  })

  it("fails when dist_var is not a list of data_extract_spec", {
    testthat::expect_error(tm_g_distribution(dist_var = "not_a_spec"), "dist_var")
    testthat::expect_error(tm_g_distribution(dist_var = list("not_a_spec")), "dist_var")
  })

  it("fails when dist_var has multiple selection enabled", {
    local_spec <- mock_data_extract_spec("iris", "Petal.Length", TRUE)
    testthat::expect_error(tm_g_distribution(dist_var = local_spec), "multiple")
  })

  it("fails when strata_var is not a list of data_extract_spec or NULL", {
    testthat::expect_error(
      tm_g_distribution(dist_var = spec, strata_var = "not_a_spec"),
      "strata_var"
    )
  })

  it("fails when group_var is not a list of data_extract_spec or NULL", {
    testthat::expect_error(
      tm_g_distribution(dist_var = spec, group_var = "not_a_spec"),
      "group_var"
    )
  })

  it("fails when freq is not a flag", {
    testthat::expect_error(
      tm_g_distribution(dist_var = spec, freq = "not_logical"),
      "freq"
    )
  })

  it("fails when ggtheme is not a valid choice", {
    testthat::expect_error(
      tm_g_distribution(dist_var = spec, ggtheme = "not_a_theme"),
      "should be one of"
    )
  })

  it("fails when ggplot2_args is not a list of ggplot2_args", {
    testthat::expect_error(
      tm_g_distribution(dist_var = spec, ggplot2_args = list("not_ggplot2_args")),
      "ggplot2_args"
    )
  })

  it("fails when bins is not numeric or has wrong length", {
    testthat::expect_error(
      tm_g_distribution(dist_var = spec, bins = "not_numeric"),
      "bins"
    )
    testthat::expect_error(
      tm_g_distribution(dist_var = spec, bins = c(1, 2)),
      "bins"
    )
  })

  it("fails when plot_height is not numeric of length 3", {
    testthat::expect_error(
      tm_g_distribution(dist_var = spec, plot_height = 100),
      "plot_height"
    )
  })

  it("fails when plot_width is not numeric of length 3 or NULL", {
    testthat::expect_error(
      tm_g_distribution(dist_var = spec, plot_width = 100),
      "plot_width"
    )
  })

  it("fails when pre_output is not a shiny.tag, shiny.tag.list, html or NULL", {
    testthat::expect_error(
      tm_g_distribution(dist_var = spec, pre_output = 123),
      "pre_output"
    )
  })

  it("fails when post_output is not a shiny.tag, shiny.tag.list, html or NULL", {
    testthat::expect_error(
      tm_g_distribution(dist_var = spec, post_output = 123),
      "post_output"
    )
  })

  it("fails when decorators is not a valid decorators list", {
    testthat::expect_error(
      tm_g_distribution(
        dist_var = mock_data_extract_spec("iris", "Petal.Length"),
        decorators = list(foo = "bar")
      ),
      "decorators"
    )
  })

  it("fails when transformators has invalid object types", {
    testthat::expect_error(
      tm_g_distribution(dist_var = spec, transformators = list("not a teal_transform_module")),
      "May only contain the following types: \\{teal_transform_module\\}"
    )
  })

  it("accepts valid transformators", {
    testthat::expect_s3_class(
      tm_g_distribution(dist_var = spec, transformators = list(teal::teal_transform_module())),
      "teal_module"
    )
  })
})

testthat::describe("tm_g_response module server behavior", {
  default_distribution_mod <- function() {
    tm_g_distribution(
      dist_var = mock_data_extract_spec("iris", "Petal.Length")
    )
  }
  data <- within(teal.data::teal_data(), iris <- tibble::rowid_to_column(iris, var = "row_number"))
  teal.data::join_keys(data) <- teal.data::join_keys(teal.data::join_key("iris", "iris", "row_number"))

  set_shared_inputs <- function(session) {
    session$setInputs(
      "dist_i-dataset" = "iris",
      "dist_i-dataset_iris_singleextract-select" = "Petal.Length",
      "tabs" = "Histogram",
      "bins" = 30,
      "main_type" = "Density",
      "add_dens" = TRUE,
      "roundn" = 2,
      "ggtheme" = "gray",
      "scales_types_ui-scales_type" = "Fixed",
      "t_dist" = "normal",
      "dist_param1" = 1.18,
      "dist_param2" = 0.59,
      "dist_tests" = "Shapiro-Wilk",
    )
  }

  it("server function fails to execute with missing test", {
    mod <- default_distribution_mod()
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(data))
      ),
      expr = {
        testthat::expect_error(session$returned())
      }
    )
  })

  it("server function returns teal_report object with valid inputs", {
    mod <- default_distribution_mod()
    testthat::expect_warning(
      shiny::testServer(
        mod$server,
        args = c(
          mod$server_args,
          list(id = "test", data = reactive(data))
        ),
        expr = {
          set_shared_inputs(session)
          session$flushReact()
          testthat::expect_s4_class(session$returned(), "teal_report")
        }
      ),
      "Appended `teal_card` doesn't remove some of the elements from previous",
      fixed = TRUE
    )
  })

  it("server function resets the parameters", {
    testthat::local_mocked_bindings(
      updateNumericInput = function(session = getDefaultReactiveDomain(), inputId, value, ...) {
        args <- stats::setNames(list(value), inputId)
        session$setInputs(!!!args)
      }
    )

    mod <- default_distribution_mod()
    testthat::expect_warning(
      shiny::testServer(
        mod$server,
        args = c(
          mod$server_args,
          list(id = "test", data = reactive(data))
        ),
        expr = {
          set_shared_inputs(session)
          session$setInputs("dist_param1" = 9)
          session$setInputs("dist_param2" = 10)
          session$flushReact()
          testthat::expect_equal(input$dist_param1, 9)
          testthat::expect_equal(input$dist_param2, 10)
          session$setInputs("params_reset" = 1)
          session$flushReact()
          testthat::expect_failure(testthat::expect_equal(input$dist_param1, 9))
          testthat::expect_failure(testthat::expect_equal(input$dist_param2, 10))
        }
      ),
      "Appended `teal_card` doesn't remove some of the elements from previous",
      fixed = TRUE
    )
  })

  it("server function returns teal_report object with valid inputs (with strata)", {
    mod <- tm_g_distribution(
      dist_var = mock_data_extract_spec("iris", "Petal.Length"),
      strata_var = mock_data_extract_spec("iris", "Species")
    )

    testthat::expect_warning(
      shiny::testServer(
        mod$server,
        args = c(mod$server_args, list(id = "test", data = reactive(data))),
        expr = {
          set_shared_inputs(session)
          session$setInputs("strata_i-dataset_iris_singleextract-select" = "Species")
          session$flushReact()
          testthat::expect_match(
            teal.code::get_code(session$returned()),
            "strata_vars <- \"Species\"",
            all = FALSE,
            fixed = TRUE
          )
        }
      ),
      "Appended `teal_card` doesn't remove some of the elements from previous",
      fixed = TRUE
    )
  })

  it("server function returns teal_report object with valid inputs (with group)", {
    mod <- tm_g_distribution(
      dist_var = data_extract_spec(
        dataname = "iris",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices("iris", c("Petal.Length", "Sepal.Length")),
          selected = "Petal.Length",
        )
      ),
      group_var = teal.transform::data_extract_spec(
        dataname = "iris",
        filter = teal.transform::filter_spec(
          vars = teal.transform::choices_selected(
            teal.transform::variable_choices("iris", c("Species")),
            selected = "Species",
          ),
          multiple = TRUE
        )
      )
    )
    testthat::expect_warning(
      shiny::testServer(
        mod$server,
        args = c(mod$server_args, list(id = "test", data = reactive(data))),
        expr = {
          set_shared_inputs(session)
          session$setInputs(
            "group_i-dataset_iris_singleextract-select" = "Species",
            "group_i-dataset_iris_singleextract-filter1-vals" = c("setosa", "versicolor", "virginica"),
            "scales_type" = "Fixed"
          )
          session$flushReact()
          testthat::expect_match(
            teal.code::get_code(session$returned()),
            "strata_vars <- \"Species\"",
            all = FALSE,
            fixed = TRUE
          )
        }
      ),
      "Appended `teal_card` doesn't remove some of the elements from previous",
      fixed = TRUE
    )
  })

  it("server function returns teal_report object with valid inputs (with group and strata)", {
    mod <- tm_g_distribution(
      dist_var = data_extract_spec(
        dataname = "iris",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices("iris", c("Petal.Length", "Sepal.Length")),
          selected = "Petal.Length",
        )
      ),
      strata_var = teal.transform::data_extract_spec(
        dataname = "iris",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices("iris", c("Species")),
          selected = "Species",
        )
      ),
      group_var = teal.transform::data_extract_spec(
        dataname = "iris",
        filter = teal.transform::filter_spec(
          vars = teal.transform::choices_selected(
            teal.transform::variable_choices("iris", c("Species")),
            selected = "Species",
          ),
          multiple = TRUE
        )
      )
    )
    testthat::expect_warning(
      shiny::testServer(
        mod$server,
        args = c(mod$server_args, list(id = "test", data = reactive(data))),
        expr = {
          set_shared_inputs(session)
          session$setInputs(
            "strata_i-dataset_iris_singleextract-select" = "Species",
            "group_i-dataset_iris_singleextract-select" = "Species",
            "group_i-dataset_iris_singleextract-filter1-vals" = c("setosa", "versicolor", "virginica"),
            "scales_type" = "Fixed"
          )
          session$flushReact()
          testthat::expect_match(
            teal.code::get_code(session$returned()),
            "strata_vars <- c(\"group_i.Species\", \"dist_i.Species\")",
            all = FALSE,
            fixed = TRUE
          )
        }
      ),
      "Appended `teal_card` doesn't remove some of the elements from previous",
      fixed = TRUE
    )
  })
})

testthat::describe("tm_g_distribution module ui behavior returns a htmltools tag or taglist", {
  it("with minimal arguments", {
    mod <- tm_g_distribution(dist_var = mock_data_extract_spec("iris", "Petal.Length"))
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    checkmate::expect_multi_class(ui, c("shiny.tag", "shiny.tag.list"))
  })

  it("with minimal arguments and strata", {
    mod <- tm_g_distribution(
      dist_var = mock_data_extract_spec("iris", "Petal.Length"),
      strata_var = mock_data_extract_spec("iris", "Species")
    )
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    checkmate::expect_multi_class(ui, c("shiny.tag", "shiny.tag.list"))
  })

  it("with default arguments and group", {
    mod <- tm_g_distribution(
      dist_var = mock_data_extract_spec("iris", "Petal.Length"),
      group_var = mock_data_extract_spec("iris", "Species")
    )
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    checkmate::expect_multi_class(ui, c("shiny.tag", "shiny.tag.list"))
  })
})
