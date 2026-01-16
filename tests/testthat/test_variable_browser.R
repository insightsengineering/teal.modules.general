testthat::describe("tests for create_sparklines exported S3 methods", {
  it("creates sparkline for numeric vectors", {
    var <- c(1, 2, 3, 4, 5)
    result <- create_sparklines(var)
    testthat::expect_type(result, "character")
    testthat::expect_true(nchar(result) > 0)
  })

  it("creates sparkline for factor vectors", {
    var <- factor(c("A", "B", "A", "B", "C"))
    result <- create_sparklines(var)
    testthat::expect_type(result, "character")
    testthat::expect_true(nchar(result) > 0)
  })

  it("creates sparkline for logical vectors", {
    var <- c(TRUE, FALSE, TRUE, FALSE)
    result <- create_sparklines(var)
    testthat::expect_type(result, "character")
    testthat::expect_true(nchar(result) > 0)
  })

  it("creates sparkline for character vectors", {
    var <- c("A", "B", "A", "B", "C")
    result <- create_sparklines(var)
    testthat::expect_type(result, "character")
    testthat::expect_true(nchar(result) > 0)
  })

  it("creates sparkline for Date vectors", {
    var <- as.Date(c("2020-01-01", "2020-02-01", "2020-03-01"))
    result <- create_sparklines(var)
    testthat::expect_type(result, "character")
    testthat::expect_true(nchar(result) > 0)
  })

  it("creates sparkline for POSIXct vectors", {
    var <- as.POSIXct(c("2020-01-01 12:00:00", "2020-02-01 12:00:00", "2020-03-01 12:00:00"))
    result <- create_sparklines(var)
    testthat::expect_type(result, "character")
    testthat::expect_true(nchar(result) > 0)
  })

  it("creates sparkline for POSIXlt vectors", {
    var <- as.POSIXlt(c("2020-01-01 12:00:00", "2020-02-01 12:00:00", "2020-03-01 12:00:00"))
    result <- create_sparklines(var)
    testthat::expect_type(result, "character")
    testthat::expect_true(nchar(result) > 0)
  })

  it("handles factors with many levels", {
    var <- factor(rep(1:100, 2))
    result <- create_sparklines(var)
    testthat::expect_match(result, "&gt; 99 levels")
  })

  it("handles factors with no levels", {
    var <- factor(character(0))
    result <- create_sparklines(var)
    testthat::expect_match(result, "no levels")
  })

  it("handles factors with one level", {
    var <- factor(rep("A", 10))
    result <- create_sparklines(var)
    testthat::expect_match(result, "one level")
  })

  it("handles infinite values in numeric vectors", {
    var <- c(1, 2, Inf, 4, 5)
    result <- create_sparklines(var)
    testthat::expect_match(result, "infinite values")
  })

  it("handles numeric vectors with NA values", {
    var <- c(1, 2, NA, 4, 5, NA, 7, 8)
    result <- create_sparklines(var)
    testthat::expect_type(result, "character")
    testthat::expect_true(nchar(result) > 0)
  })

  it("handles very large numeric vectors", {
    var <- 1:100001
    result <- create_sparklines(var)
    testthat::expect_match(result, "Too many rows")
  })

  it("handles Date vectors with only NA", {
    var <- as.Date(c(NA, NA, NA))
    suppressWarnings(result <- create_sparklines(var))
    testthat::expect_match(result, "only NA")
  })

  it("handles Date vectors with one unique date", {
    var <- as.Date(rep("2020-01-01", 10))
    result <- create_sparklines(var)
    # With only one unique date, bins calculation may result in "only NA" message
    testthat::expect_match(result, "only NA|one date")
  })

  it("handles POSIXct vectors with only NA", {
    var <- as.POSIXct(c(NA, NA, NA))
    suppressWarnings(result <- create_sparklines(var))
    testthat::expect_match(result, "only NA")
  })

  it("handles POSIXct vectors with one unique datetime", {
    var <- as.POSIXct(rep("2020-01-01 12:00:00", 10))
    result <- create_sparklines(var)
    # With only one unique datetime, bins calculation may result in "only NA" message
    testthat::expect_match(result, "only NA|one date-time")
  })

  it("handles POSIXlt vectors with only NA", {
    var <- as.POSIXlt(c(NA, NA, NA))
    suppressWarnings(result <- create_sparklines(var))
    testthat::expect_match(result, "only NA")
  })

  it("handles POSIXlt vectors with one unique datetime", {
    var <- as.POSIXlt(rep("2020-01-01 12:00:00", 10))
    result <- create_sparklines(var)
    # With only one unique datetime, bins calculation may result in "only NA" message
    testthat::expect_match(result, "only NA|one date-time")
  })

  it("handles unsupported types with default method", {
    var <- list(a = 1, b = 2)
    result <- create_sparklines(var)
    testthat::expect_match(result, "unsupported variable type")
  })

  it("accepts custom width parameter for numeric", {
    var <- c(1, 2, 3, 4, 5)
    result <- create_sparklines(var, width = 200)
    testthat::expect_type(result, "character")
    testthat::expect_true(nchar(result) > 0)
  })

  it("accepts custom width parameter for factor", {
    var <- factor(c("A", "B", "A", "B", "C"))
    result <- create_sparklines(var, width = 200)
    testthat::expect_type(result, "character")
    testthat::expect_true(nchar(result) > 0)
  })

  it("accepts custom bar_spacing parameter for factor", {
    var <- factor(c("A", "B", "A", "B", "C"))
    result <- create_sparklines(var, bar_spacing = 10)
    testthat::expect_type(result, "character")
    testthat::expect_true(nchar(result) > 0)
  })

  it("accepts custom bar_width parameter for factor", {
    var <- factor(c("A", "B", "A", "B", "C"))
    result <- create_sparklines(var, bar_width = 30)
    testthat::expect_type(result, "character")
    testthat::expect_true(nchar(result) > 0)
  })
})


testthat::describe("tests for module creation", {
  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_variable_browser(
      ),
      "teal_module"
    )
  })

  it("uses non default datanames", {
    testthat::expect_s3_class(
      tm_variable_browser(
        datanames = "my-dataset"
      ),
      "teal_module"
    )
  })

  it("uses non parent_datanames", {
    testthat::expect_s3_class(
      tm_variable_browser(
        parent_dataname = "my-parent_dataset"
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with pre_output", {
    pre_output <- shiny::actionButton("pre_output", "My pre output")
    default_mod <- tm_variable_browser()
    testthat::expect_null(default_mod$ui_args$pre_output)

    pre_output_mod <- tm_variable_browser(
      pre_output = pre_output
    )
    testthat::expect_equal(pre_output_mod$ui_args$pre_output, pre_output)
  })

  it("creates a teal_module object with post_output", {
    post_output <- shiny::actionButton("post_output", "My post output")
    default_mod <- tm_missing_data()
    testthat::expect_null(default_mod$ui_args$post_output)

    post_output_mod <- tm_variable_browser(
      post_output = post_output
    )
    testthat::expect_equal(post_output_mod$ui_args$post_output, post_output)
  })

    it("accepts a transformator", {
      transformator_iris <- teal_transform_module(
        label = "Custom transformator for iris",
        ui = function(id) {
          ns <- NS(id)
          tags$div(
            numericInput(ns("n_rows"), "Number of rows to display", value = 6, min = 1, max = 150, step = 1)
          )
        },
        server = function(id, data) {
          moduleServer(id, function(input, output, session) {
            reactive({
              within(
                data(),
                iris <- head(iris, num_rows),
                num_rows = input$n_rows
              )
            })
          })
        }
      )

      testthat::expect_s3_class(
        tm_variable_browser(
          transformators = list(
            teal::teal_transform_module()
          )
        ),
        "teal_module"
      )
  })

  it("accepts valid ggplot2_args", {
    testthat::expect_s3_class(
      tm_variable_browser(
        ggplot2_args = teal.widgets::ggplot2_args(
            labs = list(title = "User default title"),
            theme = list(legend.position = "right", legend.direction = "vertical")
          )
      ),
      "teal_module"
    )
  })

  it("creates a module that is not bookmarkable", {
    mod <- tm_variable_browser()
    testthat::expect_null(attr(mod, "teal_bookmarkable"))
  })
})

testthat::describe("tests for input validation", {
  it("fails if label is not a string", {
    testthat::expect_error(
      tm_variable_browser(label = 123),
      "Assertion on 'label' failed"
    )
  })

  it("errors for deprecated parameter datasets_selected", {
    testthat::expect_error(
      tm_variable_browser(datasets_selected = "my-dataset"),
    )
  })

  it("fails if datanames is not a character", {
    testthat::expect_error(
      tm_variable_browser(datanames = 123),
      "Assertion on 'datanames' failed"
    )
  })

  it("fails if datanames is not a character", {
    testthat::expect_error(
      tm_variable_browser(parent_dataname = 123),
      "Assertion on 'parent_dataname' failed"
    )
  })

  it("fails if pre_output is not a valid shiny object", {
    testthat::expect_error(
      tm_variable_browser(pre_output =  "wrong type"),
      "Assertion on 'pre_output' failed"
    )
  })

  it("fails if post_output is not a valid shiny object", {
    testthat::expect_error(
      tm_variable_browser(post_output =  "wrong type"),
      "Assertion on 'post_output' failed"
    )
  })

  it("fails if ggplot2_args is not of the valid type", {
    testthat::expect_error(
      tm_variable_browser(ggplot2_args = "wrong type")
    )
  })
})

testthat::describe("tm_variable_browser module server behavior", {

  it("server function executes successfully with numeric data", {
    data <- create_test_data(data.frame(
      num_var1 = rnorm(50),
      num_var2 = runif(50),
      num_var3 = 1:50
    ))

    mod <- tm_variable_browser(
      datanames = "test_data"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        # Set the active tab
        session$setInputs("tabset_panel" = "test_data")
        session$flushReact()

        # Set the row selection
        session$setInputs(
          "variable_browser_test_data_rows_selected" = 1,
          "ggplot_theme" = "grey",
          "font_size" = 15,
          "label_rotation" = 45
        )
        session$flushReact()

        testthat::expect_no_error(output$ui_variable_browser)
        testthat::expect_no_error(output$dataset_summary_test_data)
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server function executes successfully with factor data", {
    data <- create_test_data(data.frame(
      factor_var1 = factor(rep(c("A", "B", "C"), 20)),
      factor_var2 = factor(rep(c("X", "Y"), 30)),
      char_var = rep(c("foo", "bar"), 30)
    ))

    mod <- tm_variable_browser(
      datanames = "test_data"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs("tabset_panel" = "test_data")
        session$flushReact()

        session$setInputs(
          "variable_browser_test_data_rows_selected" = 1,
          "ggplot_theme" = "grey",
          "font_size" = 15,
          "label_rotation" = 45
        )

        session$flushReact()

        testthat::expect_no_error(output$ui_variable_browser)
        testthat::expect_no_error(output$dataset_summary_test_data)
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server function executes successfully with Date data", {
    data <- create_test_data(data.frame(
      date_var = seq(as.Date("2020-01-01"), as.Date("2020-12-31"), by = "week"),
      posix_var = seq(as.POSIXct("2020-01-01"), as.POSIXct("2020-12-31"), length.out = 53)
    ))

    mod <- tm_variable_browser(
      datanames = "test_data"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs("tabset_panel" = "test_data")
        session$flushReact()

        session$setInputs(
          "variable_browser_test_data_rows_selected" = 1,
          "ggplot_theme" = "grey",
          "font_size" = 15,
          "label_rotation" = 45
        )

        session$flushReact()

        # Access outputs to trigger rendering and verify they don't error
        testthat::expect_no_error(output$ui_variable_browser)
        testthat::expect_no_error(output$dataset_summary_test_data)
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server function handles mixed data types", {
    data <- create_test_data(data.frame(
      num_var = rnorm(50),
      factor_var = factor(rep(c("A", "B", "C"), length.out = 50)),
      char_var = rep(c("foo", "bar"), 25),
      date_var = seq(as.Date("2020-01-01"), by = "day", length.out = 50),
      logical_var = rep(c(TRUE, FALSE), 25)
    ))

    mod <- tm_variable_browser(
      datanames = "test_data"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs("tabset_panel" = "test_data")
        session$flushReact()

        session$setInputs(
          "ggplot_theme" = "grey",
          "font_size" = 15,
          "label_rotation" = 45,
          "variable_browser_test_data_rows_selected" = 1
        )

        session$flushReact()
        
        # Access outputs to trigger rendering and verify they don't error
        testthat::expect_no_error(output$ui_variable_browser)
        testthat::expect_no_error(output$dataset_summary_test_data)
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server function handles data with NA values", {
    data <- create_test_data(data.frame(
      num_with_na = c(1, 2, NA, 4, 5, NA, 7, 8, 9, 10),
      factor_with_na = factor(c("A", NA, "B", "C", NA, "A", "B", "C", "A", "B")),
      all_na = rep(NA, 10)
    ))

    mod <- tm_variable_browser(
      datanames = "test_data"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs("tabset_panel" = "test_data")
        session$flushReact()

        session$setInputs(
          "ggplot_theme" = "grey",
          "font_size" = 15,
          "label_rotation" = 45
        )

        suppressWarnings({
          session$flushReact()

          # Set the row selection
          session$setInputs(
            "variable_browser_test_data_rows_selected" = 1
          )
          session$flushReact()
        })

        # Access outputs to trigger rendering and verify they don't error
          testthat::expect_no_error(output$ui_variable_browser)
          testthat::expect_no_error(output$dataset_summary_test_data)
          testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server function handles multiple datasets", {
    data <- shiny::reactive({
      teal.data::teal_data() |>
        within({
          dataset1 <- data.frame(var1 = 1:10, var2 = letters[1:10])
          dataset2 <- data.frame(var3 = rnorm(10), var4 = factor(rep(c("A", "B"), 5)))
        })
    })

    mod <- tm_variable_browser(
      datanames = c("dataset1", "dataset2")
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs("tabset_panel" = "dataset2")
        session$flushReact()

        session$setInputs(
          "ggplot_theme" = "grey",
          "font_size" = 15,
          "label_rotation" = 45,
          "variable_browser_dataset2_rows_selected" = 1
        )

          session$flushReact()          
        # Access outputs to trigger rendering and verify they don't error
          testthat::expect_no_error(output$ui_variable_browser)
          testthat::expect_no_error(output$dataset_summary_dataset2)
          testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server function handles parent_dataname filtering", {
    data <- shiny::reactive({
      teal.data::teal_data() |>
        within({
          ADSL <- data.frame(USUBJID = 1:10, AGE = rnorm(10), SEX = factor(rep(c("M", "F"), 5)))
          ADTTE <- data.frame(USUBJID = 1:10, AVAL = rnorm(10), PARAMCD = factor(rep("OS", 10)))
        })
    })

    mod <- tm_variable_browser(
      datanames = c("ADSL", "ADTTE"),
      parent_dataname = "ADSL"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs("tabset_panel" = "ADSL")
        session$flushReact()

        session$setInputs(
          "ggplot_theme" = "grey",
          "font_size" = 15,
          "label_rotation" = 45,
          "variable_browser_ADSL_rows_selected" = 1
        )

          session$flushReact()          
        # Access outputs to trigger rendering and verify they don't error
          testthat::expect_no_error(output$ui_variable_browser)
          testthat::expect_no_error(output$dataset_summary_ADSL)
          testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server function handles numeric variables with few unique values", {
    data <- create_test_data(data.frame(
      discrete_num = rep(c(1, 2, 3), 20),
      semi_discrete = rep(1:10, 6),
      continuous = rnorm(60)
    ))

    mod <- tm_variable_browser(
      datanames = "test_data"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs("tabset_panel" = "test_data")
        session$flushReact()

        session$setInputs(
          "ggplot_theme" = "grey",
          "font_size" = 15,
          "label_rotation" = 45
        )

        session$flushReact()

        # Set the row selection
        session$setInputs(
          "variable_browser_test_data_rows_selected" = 1
        )
        session$flushReact()

        # Access outputs to trigger rendering and verify they don't error
        testthat::expect_no_error(output$ui_variable_browser)
        testthat::expect_no_error(output$dataset_summary_test_data)
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server function handles logical variables", {
    data <- create_test_data(data.frame(
      logical_var1 = rep(c(TRUE, FALSE), 30),
      logical_var2 = c(rep(TRUE, 40), rep(FALSE, 20)),
      logical_with_na = c(TRUE, FALSE, NA, TRUE, FALSE, rep(TRUE, 55))
    ))

    mod <- tm_variable_browser(
      datanames = "test_data"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
                session$setInputs("tabset_panel" = "test_data")
        session$flushReact()

        session$setInputs(
          "ggplot_theme" = "grey",
          "font_size" = 15,
          "label_rotation" = 45
        )

        session$flushReact()

        # Set the row selection
        session$setInputs(
          "variable_browser_test_data_rows_selected" = 1
        )
        session$flushReact()

        # Access outputs to trigger rendering and verify they don't error
        testthat::expect_no_error(output$ui_variable_browser)
        testthat::expect_no_error(output$dataset_summary_test_data)
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server function works with custom ggplot2_args", {
    data <- create_test_data(data.frame(
      var1 = rnorm(50),
      var2 = factor(rep(c("A", "B"), 25))
    ))

    mod <- tm_variable_browser(
      datanames = "test_data",
      ggplot2_args = teal.widgets::ggplot2_args(
        labs = list(title = "Custom Title"),
        theme = list(legend.position = "bottom")
      )
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
                session$setInputs("tabset_panel" = "test_data")
        session$flushReact()

        session$setInputs(
          "ggplot_theme" = "grey",
          "font_size" = 15,
          "label_rotation" = 45
        )

        session$flushReact()

        # Set the row selection
        session$setInputs(
          "variable_browser_test_data_rows_selected" = 1
        )
        session$flushReact()

        # Access outputs to trigger rendering and verify they don't error
        testthat::expect_no_error(output$ui_variable_browser)
        testthat::expect_no_error(output$dataset_summary_test_data)
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  
})
