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

testthat::describe("testServer for data exceptions", {
  it("server function handles empty dataframes", {
    data <- create_test_data(data.frame())

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

        testthat::expect_no_error(output$ui_variable_browser)
        testthat::expect_no_error(output$dataset_summary_test_data)
      }
    )
  })

  it("server function handles single row dataframes", {
    data <- create_test_data(data.frame(
      var1 = 1,
      var2 = "A",
      var3 = TRUE
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
        suppressWarnings({
          session$setInputs("tabset_panel" = "test_data")
          session$flushReact()

          session$setInputs(
            "ggplot_theme" = "grey",
            "font_size" = 15,
            "label_rotation" = 45,
            "variable_browser_test_data_rows_selected" = 1
          )

          session$flushReact()
        })


        testthat::expect_no_error(output$ui_variable_browser)
        testthat::expect_no_error(output$dataset_summary_test_data)
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server function handles single column dataframes", {
    data <- create_test_data(data.frame(
      single_var = rnorm(100)
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

        testthat::expect_no_error(output$ui_variable_browser)
        testthat::expect_no_error(output$dataset_summary_test_data)
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server function handles different ggplot themes", {
    data <- create_test_data(data.frame(
      var1 = rnorm(50),
      var2 = factor(rep(c("A", "B"), 25))
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

        # Test different themes
        for (theme in c("grey", "bw", "minimal", "classic", "dark")) {
          session$setInputs(
            "ggplot_theme" = theme,
            "font_size" = 15,
            "label_rotation" = 45,
            "variable_browser_test_data_rows_selected" = 1
          )
          session$flushReact()

          testthat::expect_no_error(output$variable_summary_table)
        }
      }
    )
  })

  it("server function handles different font sizes", {
    data <- create_test_data(data.frame(
      var1 = rnorm(50)
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

        # Test different font sizes
        for (font_size in c(10, 12, 15, 18, 20)) {
          session$setInputs(
            "ggplot_theme" = "grey",
            "font_size" = font_size,
            "label_rotation" = 45,
            "variable_browser_test_data_rows_selected" = 1
          )
          session$flushReact()

          testthat::expect_no_error(output$variable_summary_table)
        }
      }
    )
  })

  it("server function handles different label rotations", {
    data <- create_test_data(data.frame(
      var1 = rnorm(50)
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

        # Test different label rotations
        for (rotation in c(0, 45, 90)) {
          session$setInputs(
            "ggplot_theme" = "grey",
            "font_size" = 15,
            "label_rotation" = rotation,
            "variable_browser_test_data_rows_selected" = 1
          )
          session$flushReact()

          testthat::expect_no_error(output$variable_summary_table)
        }
      }
    )
  })

  it("server function handles switching between different variables", {
    data <- create_test_data(data.frame(
      numeric_var = rnorm(50),
      factor_var = factor(rep(c("A", "B", "C"), length.out = 50)),
      logical_var = rep(c(TRUE, FALSE), 25),
      date_var = seq(as.Date("2020-01-01"), by = "day", length.out = 50)
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

        # Test switching between different variables
        for (row_num in 1:4) {
          session$setInputs(
            "variable_browser_test_data_rows_selected" = row_num
          )
          session$flushReact()

          testthat::expect_no_error(output$variable_summary_table)
        }
      }
    )
  })

  it("server function handles datasets with special characters in names", {
    data <- shiny::reactive({
      teal.data::teal_data() |>
        within({
          `data-with-dashes` <- data.frame(var1 = 1:10, var2 = letters[1:10])
        })
    })

    mod <- tm_variable_browser(
      datanames = "data-with-dashes"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs("tabset_panel" = "data-with-dashes")
        session$flushReact()

        session$setInputs(
          "ggplot_theme" = "grey",
          "font_size" = 15,
          "label_rotation" = 45,
          "variable_browser_data-with-dashes_rows_selected" = 1
        )

        session$flushReact()

        testthat::expect_no_error(output$ui_variable_browser)
      }
    )
  })

  it("server function handles integer variables", {
    data <- create_test_data(data.frame(
      int_var = as.integer(1:100),
      int_with_na = as.integer(c(1:50, rep(NA, 50)))
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

        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server function handles factors with ordered levels", {
    data <- create_test_data(data.frame(
      ordered_factor = ordered(rep(c("Low", "Medium", "High"), length.out = 60), 
                               levels = c("Low", "Medium", "High"))
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

        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

})

testthat::describe("UI switches and controls", {
  it("server handles toggling display_density switch", {
    data <- create_test_data(data.frame(
      numeric_var = rnorm(100)
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

        # Toggle density display OFF
        session$setInputs("display_density" = FALSE)
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)

        # Toggle density display ON
        session$setInputs("display_density" = TRUE)
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server handles toggling remove_NA_hist for categorical variables", {
    data <- create_test_data(data.frame(
      factor_with_na = factor(c("A", "B", NA, "C", "A", NA, "B", "C", "A", "B",
                                  rep(c("A", "B", "C"), 30)))
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

        # Toggle remove NA OFF
        session$setInputs("remove_NA_hist" = FALSE)
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)

        # Toggle remove NA ON
        session$setInputs("remove_NA_hist" = TRUE)
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server handles different outlier definition thresholds", {
    data <- create_test_data(data.frame(
      numeric_var = c(rnorm(95), 50, 60, 70, -50, -60)  # with outliers
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

        # Enable outlier removal
        session$setInputs("remove_outliers" = TRUE)
        session$flushReact()

        # Test different outlier thresholds
        for (threshold in c(1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5)) {
          session$setInputs("outlier_definition_slider" = threshold)
          session$flushReact()
          testthat::expect_no_error(output$variable_summary_table)
        }

        # Disable outlier removal
        session$setInputs("remove_outliers" = FALSE)
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server handles numeric_as_factor checkbox toggle", {
    data <- create_test_data(data.frame(
      # Numeric with few unique values (< 30)
      discrete_numeric = rep(c(1, 2, 3, 4, 5), 20)
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

        # Treat as continuous (numeric_as_factor = FALSE)
        session$setInputs("numeric_as_factor" = FALSE)
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)

        # Treat as factor (numeric_as_factor = TRUE)
        session$setInputs("numeric_as_factor" = TRUE)
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)

        # Toggle display_density when treated as numeric
        session$setInputs("numeric_as_factor" = FALSE)
        session$flushReact()
        session$setInputs("display_density" = TRUE)
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)

        session$setInputs("display_density" = FALSE)
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server handles variables with different unique value counts", {
    data <- create_test_data(data.frame(
      # Very few unique values (< 6 - default as factor)
      very_discrete = rep(c(1, 2, 3), 40),
      # Some unique values (6-29 - user can choose)
      somewhat_discrete = rep(1:15, 8),
      # Many unique values (>= 30 - always continuous)
      continuous = 1:120
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

        # Test very discrete (should default to factor)
        session$setInputs("variable_browser_test_data_rows_selected" = 1)
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)

        # Test somewhat discrete (user can toggle)
        session$setInputs("variable_browser_test_data_rows_selected" = 2)
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)

        # Test continuous (no factor option)
        session$setInputs("variable_browser_test_data_rows_selected" = 3)
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server handles categorical variables with many levels", {
    data <- create_test_data(data.frame(
      # Factor with >= 30 levels (many categories)
      many_categories = factor(rep(paste0("Cat", 1:40), 5))
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

        # Should not show remove_NA_hist option for many levels
        testthat::expect_no_error(output$variable_summary_table)
        testthat::expect_no_error(output$ui_histogram_display)
      }
    )
  })

  it("server handles data without join_keys", {
    data <- shiny::reactive({
      # Create teal_data without join_keys
      td <- teal.data::teal_data()
      td <- within(td, {
        dataset1 <- data.frame(var1 = 1:10, var2 = rnorm(10))
      })
      td
    })

    mod <- tm_variable_browser(
      datanames = "dataset1"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs("tabset_panel" = "dataset1")
        session$flushReact()

        session$setInputs(
          "ggplot_theme" = "grey",
          "font_size" = 15,
          "label_rotation" = 45,
          "variable_browser_dataset1_rows_selected" = 1
        )
        session$flushReact()

        testthat::expect_no_error(output$dataset_summary_dataset1)
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server handles variables with all NA values", {
    data <- create_test_data(data.frame(
      all_na_numeric = rep(NA_real_, 50),
      all_na_character = rep(NA_character_, 50),
      all_na_factor = factor(rep(NA, 50))
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

        # Test all NA numeric
        suppressWarnings({
          session$setInputs("variable_browser_test_data_rows_selected" = 1)
          session$flushReact()
        })
        testthat::expect_no_error(output$variable_summary_table)

        # Test all NA character
        suppressWarnings({
          session$setInputs("variable_browser_test_data_rows_selected" = 2)
          session$flushReact()
        })
        testthat::expect_no_error(output$variable_summary_table)

        # Test all NA factor
        suppressWarnings({
          session$setInputs("variable_browser_test_data_rows_selected" = 3)
          session$flushReact()
        })
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server handles wrap_character parameter for long labels", {
    data <- create_test_data(data.frame(
      long_labels = factor(c(
        "Very Long Category Name That Should Be Wrapped",
        "Another Very Long Category Name",
        "Yet Another Extremely Long Category Name",
        "Short"
      ) |> rep(25))
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

        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })

  it("server handles combined outlier removal and density display", {
    data <- create_test_data(data.frame(
      numeric_var = c(rnorm(90), rep(c(100, -100), 5))
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

        # Enable both outlier removal and density display
        session$setInputs(
          "remove_outliers" = TRUE,
          "outlier_definition_slider" = 3,
          "display_density" = TRUE
        )
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)

        # Outliers ON, density OFF
        session$setInputs("display_density" = FALSE)
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)

        # Outliers OFF, density ON
        session$setInputs(
          "remove_outliers" = FALSE,
          "display_density" = TRUE
        )
        session$flushReact()
        testthat::expect_no_error(output$variable_summary_table)
      }
    )
  })
})
