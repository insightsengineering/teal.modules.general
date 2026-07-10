create_dummy_module_data <- function() {
  teal_data()
  data <- within(data, {
    require(nestcolor)

    add_nas <- function(x) {
      x[sample(seq_along(x), floor(length(x) * runif(1, .05, .17)))] <- NA
      x
    }

    iris <- iris
    mtcars <- mtcars

    iris[] <- lapply(iris, add_nas)
    mtcars[] <- lapply(mtcars, add_nas)
    mtcars[["cyl"]] <- as.factor(mtcars[["cyl"]])
    mtcars[["gear"]] <- as.factor(mtcars[["gear"]])
  })
}

testthat::describe("tm_missing_data module creation", {
  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_missing_data(
        label = "Missing Data",
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with pre_output", {
    pre_output <- shiny::actionButton("pre_output", "My pre output")
    default_mod <- tm_missing_data(
      label = "Missing Data"
    )
    testthat::expect_null(default_mod$ui_args$pre_output)

    pre_output_mod <- tm_missing_data(
      label = "Missing Data",
      pre_output = pre_output
    )
    testthat::expect_equal(pre_output_mod$ui_args$pre_output, pre_output)
  })

  it("creates a teal_module object with post_output", {
    post_output <- shiny::actionButton("post_output", "My post output")
    default_mod <- tm_missing_data(
      label = "Missing Data"
    )
    testthat::expect_null(default_mod$ui_args$post_output)

    post_output_mod <- tm_missing_data(
      label = "Missing Data",
      post_output = post_output
    )
    testthat::expect_equal(post_output_mod$ui_args$post_output, post_output)
  })

  it("accepts valid ggtheme_theme options", {
    for (theme in c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void")) {
      testthat::expect_s3_class(
        tm_missing_data(
          label = "Missing Data",
          ggtheme = theme
        ),
        "teal_module"
      )
    }
  })

  it("accepts a decorator", {
    testthat::expect_s3_class(
      tm_missing_data(
        decorators = list(
          summary_plot = teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
  })

  it("accepts multiple decorators", {
    ggplot_caption_decorator <- function(default_caption = "I am a dummy decorator") {
      teal_transform_module(
        label = "Caption",
        ui = function(id) {
          shiny::textInput(shiny::NS(id, "footnote"), "Footnote", value = default_caption)
        },
        server = function(id, data) {
          moduleServer(id, function(input, output, session) {
            reactive({
              data() |>
                within(
                  {
                    plot <- plot + ggplot2::labs(caption = footnote)
                  },
                  footnote = input$footnote
                )
            })
          })
        }
      )
    }

    testthat::expect_s3_class(
      tm_missing_data(
        decorators = list(
          summary_plot = ggplot_caption_decorator(),
          combination_plot = teal::teal_transform_module(),
          by_subject_plot = teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
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
      tm_missing_data(
        transformators = list(
          teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
  })

  it("creates a module with specified datanames", {
    mod <- tm_missing_data(
      label = "Missing Data",
      datanames = c("iris", "mtcars")
    )
    testthat::expect_setequal(
      mod$datanames,
      c("iris", "mtcars", "ADSL")
    )
  })

  it("creates a module with datanames='all'", {
    mod <- tm_missing_data(
      label = "Missing Data",
      datanames = "all"
    )
    testthat::expect_equal(
      mod$datanames,
      "all"
    )
  })

  it("creates a module that is bookmarkable", {
    mod <- tm_missing_data(
      label = "Missing Data"
    )
    testthat::expect_true(attr(mod, "teal_bookmarkable"))
  })

  it("accepts NULL plot_width", {
    testthat::expect_s3_class(
      tm_missing_data(
        label = "Missing Data",
        plot_width = NULL
      ),
      "teal_module"
    )
  })

  it("accepts valid ggplot2_args", {
    testthat::expect_s3_class(
      tm_missing_data(
        label = "Missing Data",
        ggplot2_args = list(
          default = teal.widgets::ggplot2_args(
            labs = list(title = "User default title"),
            theme = list(legend.position = "right", legend.direction = "vertical")
          ),
          "Summary Obs" = teal.widgets::ggplot2_args(),
          "Summary Patients" = teal.widgets::ggplot2_args(),
          "Combinations Main" = teal.widgets::ggplot2_args(),
          "Combinations Hist" = teal.widgets::ggplot2_args(),
          "By Subject" = teal.widgets::ggplot2_args()
        )
      ),
      "teal_module"
    )
  })
})

testthat::describe("tm_missing_data input_validation", {
  it("fails when plot height is not valid", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        plot_height = c(100, 200, 300) # min > max nolint: commented_code_linter
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot_height has wrong length", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        plot_height = c(600, 200)
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot width is not valid", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        plot_width = c(100, 200, 300) # min > max nolint: commented_code_linter
      ),
      "Assertion on 'plot_width' failed"
    )
  })

  it("fails when datanames is wrong type", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        datanames = FALSE
      ),
      "Assertion on 'datanames' failed"
    )
  })

  it("fails when ggtheme is invalid name", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        ggtheme = "invalid"
      ),
      "should be one of"
    )
  })

  it("fails when ggplot_args has not valid type", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        ggplot2_args = list("wrong ggplot2_args", "wrong_ggplot2_args"),
      ),
      "Assertion on 'ggplot2_args' failed"
    )
  })

  it("fails if decorators has not valid type", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        decorators = list(
          summary_plot = "wrong teal_transform_module"
        )
      ),
      "Assertion on 'decorators' failed"
    )
  })

  it("accepts valid decorators with all allowed names", {
    testthat::expect_s3_class(
      tm_missing_data(
        label = "Missing Data",
        decorators = list(
          summary_plot = teal::teal_transform_module(),
          combination_plot = teal::teal_transform_module(),
          by_subject_plot = teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
  })

  it("fails if transformators has not valid type", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        transformators = list(
          "wrong teal_transform_module"
        )
      ),
      "Assertion on 'transformators' failed"
    )
  })

  it("fails when label is not a string", {
    testthat::expect_error(
      tm_missing_data(
        label = 123
      ),
      "Assertion on 'label' failed"
    )
  })

  it("fails when ggplot2_args has invalid names", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        ggplot2_args = list(
          invalid_name = teal.widgets::ggplot2_args()
        )
      ),
      "Assertion on 'names\\(ggplot2_args\\)' failed"
    )
  })
})

testthat::describe("tm_missing_data using single or all datasets ui functions", {
  it("uses diferent functions in single or all datasets", {
    mod_single <- tm_missing_data(datanames = "ADSL")
    mod_all <- tm_missing_data()

    expect_false(as.character(mod_single$ui("test")) == as.character(mod_all$ui("test")))
  })
})


testthat::describe("tm_missing_data module server behavior", {
  it("server function executes successfully through module interface with Summary tab", {
    data <- create_test_data(data.frame(
      var1 = c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10),
      var2 = c(NA, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      var3 = factor(c("A", "B", "A", NA, "B", "A", "B", "A", "B", "A"))
    ))

    mod <- tm_missing_data(
      label = "Missing Data",
      datanames = "test_data"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "test_data-variables_select" = c("var1", "var2", "var3"),
          "test_data-summary_type" = "Summary",
          "test_data-any_na" = FALSE,
          "test_data-ggtheme" = "gray"
        )
        session$flushReact()
        testthat::expect_true(inherits(session$returned(), "teal_data"))
      }
    )
  })

  it("server function handles Combinations tab", {
    data <- create_test_data(data.frame(
      var1 = c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10),
      var2 = c(NA, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      var3 = c(1, NA, 3, 4, 5, 6, 7, 8, 9, 10)
    ))

    mod <- tm_missing_data(
      label = "Missing Data",
      datanames = "test_data"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "test_data-variables_select" = c("var1", "var2", "var3"),
          "test_data-summary_type" = "Combinations",
          "test_data-ggtheme" = "gray"
        )
        session$flushReact()
        # Need to wait for combination_cutoff to be available
        session$setInputs("test_data-combination_cutoff" = 1)
        session$flushReact()
        testthat::expect_true(inherits(session$returned(), "teal_data"))
      }
    )
  })

  it("server function handles By Variable Levels tab", {
    data <- create_test_data(data.frame(
      group = factor(c("A", "A", "B", "B", "C", "C", "A", "B", "C", "A")),
      var1 = c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10),
      var2 = c(NA, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    ))

    mod <- tm_missing_data(
      label = "Missing Data",
      datanames = "test_data"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "test_data-variables_select" = c("group", "var1", "var2"),
          "test_data-summary_type" = "By Variable Levels",
          "test_data-group_by_var" = "group",
          "test_data-group_by_vals" = c("A", "B", "C"),
          "test_data-count_type" = "counts",
          "test_data-ggtheme" = "gray"
        )
        session$flushReact()
        testthat::expect_true(inherits(session$returned(), "teal_data"))
      }
    )
  })

  it("server function handles By Variable Levels tab with proportions", {
    data <- create_test_data(data.frame(
      group = factor(c("A", "A", "B", "B", "C", "C", "A", "B", "C", "A")),
      var1 = c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10),
      var2 = c(NA, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    ))

    mod <- tm_missing_data(
      label = "Missing Data",
      datanames = "test_data"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "test_data-variables_select" = c("group", "var1", "var2"),
          "test_data-summary_type" = "By Variable Levels",
          "test_data-group_by_var" = "group",
          "test_data-group_by_vals" = c("A", "B", "C"),
          "test_data-count_type" = "proportions",
          "test_data-ggtheme" = "gray"
        )
        session$flushReact()
        testthat::expect_true(inherits(session$returned(), "teal_data"))
      }
    )
  })

  it("server function handles Grouped by Subject tab", {
    data <- create_test_data(data.frame(
      USUBJID = c("S1", "S1", "S2", "S2", "S3", "S3", "S4", "S4", "S5", "S5"),
      var1 = c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10),
      var2 = c(NA, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    ))

    mod <- tm_missing_data(
      label = "Missing Data",
      datanames = "test_data",
      parent_dataname = "test_data"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "test_data-variables_select" = c("USUBJID", "var1", "var2"),
          "test_data-summary_type" = "Grouped by Subject",
          "test_data-ggtheme" = "gray"
        )
        session$flushReact()
        testthat::expect_true(inherits(session$returned(), "teal_data"))
      }
    )
  })

  it("server function handles By Variable Levels tab with labeled columns", {
    test_df <- data.frame(
      group = factor(c("A", "A", "B", "B", "C", "C", "A", "B", "C", "A")),
      var1 = c(1, 2, NA, 4, 5, 6, 7, 8, 9, 10),
      var2 = c(NA, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    )
    attr(test_df$group, "label") <- "Group Category"
    attr(test_df$var1, "label") <- "Variable 1"
    attr(test_df$var2, "label") <- "Variable 2"

    data <- create_test_data(test_df)

    mod <- tm_missing_data(
      label = "Missing Data",
      datanames = "test_data"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "dataname_tab" = "test_data",
          "test_data-variables_select" = c("group", "var1", "var2"),
          "test_data-summary_type" = "By Variable Levels",
          "test_data-group_by_var" = "group",
          "test_data-group_by_vals" = c("A", "B", "C"),
          "test_data-count_type" = "counts",
          "test_data-ggtheme" = "gray"
        )
        session$flushReact()

        result <- session$returned()
        testthat::expect_true(inherits(result, "teal_data"))
        testthat::expect_true(inherits(result[["by_variable_plot"]], "ggplot"))
        testthat::expect_true("label" %in% names(result[["ANL"]]))
      }
    )
  })
})
