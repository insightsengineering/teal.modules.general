testthat::describe("tm_outliers module creation", {
  data <- teal_data()
  data <- within(data, {
    CO2 <- CO2
    CO2[["primary_key"]] <- seq_len(nrow(CO2))
  })
  join_keys(data) <- join_keys(join_key("CO2", "CO2", "primary_key"))
  vars <- choices_selected(variable_choices(data[["CO2"]], c("Plant", "Type", "Treatment")))
  outlier_var <- list(
          var1 = data_extract_spec(
              dataname = "CO2",
              select = select_spec(
                label = "Select variable:",
                choices = variable_choices(data[["CO2"]], c("conc", "uptake")),
                selected = "uptake",
                multiple = FALSE,
                fixed = FALSE
              )
            )
        )

  it("module is created with default and mandatory arguments", {
    testthat::expect_s3_class(tm_outliers(outlier_var = outlier_var), "teal_module")
  })

  it("module is created with specific ggtheme set", {
    testthat::expect_s3_class(tm_outliers(outlier_var = outlier_var, ggtheme = "minimal"), "teal_module")
  })

  it("module is created with plot width defined", {
    testthat::expect_s3_class(tm_outliers(outlier_var = outlier_var, plot_width = c(200, 100, 500)), "teal_module")
  })

  it("module is created with plot width defined", {
    testthat::expect_s3_class(tm_outliers(outlier_var = outlier_var, plot_width = c(200, 100, 500)), "teal_module")
  })

  it("it creates a module with pre_output", {
    pre_output <- shiny::actionButton("pre_output", "My pre output")
    default_mod <- tm_outliers(outlier_var = outlier_var)
    testthat::expect_null(default_mod$ui_args$pre_output)

    pre_output_mod <- tm_outliers(outlier_var = outlier_var, pre_output = pre_output)
    testthat::expect_equal(pre_output_mod$ui_args$pre_output, pre_output)
  })

  it("it creates a module with post_output", {
    post_output <- shiny::actionButton("pre_output", "My pre output")
    default_mod <- tm_outliers(outlier_var = outlier_var)
    testthat::expect_null(default_mod$ui_args$post_output)

    post_output_mod <- tm_outliers(outlier_var = outlier_var, post_output = post_output)
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
      tm_outliers(outlier_var = outlier_var, transformators = list(teal::teal_transform_module())),
      "teal_module"
    )
  })

  it("accepts a decorator", {
    ggplot_caption_decorator <- function(default_caption = "I am a good decorator") {
      teal::teal_transform_module(
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
      })
    }

    testthat::expect_s3_class(
      tm_outliers(
        outlier_var = outlier_var,
        decorators = list(box_plot = ggplot_caption_decorator())
      ),
      "teal_module"
    )
  })

  it("accepts valid ggplot2_args", {
    testthat::expect_s3_class(
      tm_outliers(
        outlier_var = outlier_var,
        ggplot2_args = teal.widgets::ggplot2_args(
          labs = list(title = "User default title"),
          theme = list(legend.position = "right", legend.direction = "vertical")
        )
      ),
      "teal_module"
    )
  })

  it("uses a categorical var", {
    categorical_var = list(
      data_extract_spec(
        dataname = "CO2",
        filter = filter_spec(
          vars = vars,
          choices = value_choices(data[["CO2"]], vars$selected),
          selected = value_choices(data[["CO2"]], vars$selected),
          multiple = TRUE
        )
      )
    )
    testthat::expect_s3_class(
      tm_outliers(outlier_var = outlier_var, categorical_var = categorical_var),
      "teal_module"
    )
  })

  it("creates an ui of the expected type", {
    mod <- tm_outliers(outlier_var = outlier_var)
    testthat::expect_s3_class(
      mod$ui("test", outlier_var = outlier_var),
      "shiny.tag.list"
    )
  })

  it("creates a module that is bookmarkable", {
    mod <- tm_outliers(outlier_var = outlier_var)
    testthat::expect_true(attr(mod, "teal_bookmarkable"))
  })
})

testthat::describe("test for input validation", {
  data <- teal_data()
  data <- within(data, {
    CO2 <- CO2
    CO2[["primary_key"]] <- seq_len(nrow(CO2))
  })
  join_keys(data) <- join_keys(join_key("CO2", "CO2", "primary_key"))
  vars <- choices_selected(variable_choices(data[["CO2"]], c("Plant", "Type", "Treatment")))
  outlier_var <- list(
          var1 = data_extract_spec(
              dataname = "CO2",
              select = select_spec(
                label = "Select variable:",
                choices = variable_choices(data[["CO2"]], c("conc", "uptake")),
                selected = "uptake",
                multiple = FALSE,
                fixed = FALSE
              )
            )
        )

  it("fails if no outlier_var defined", {
    testthat::expect_error(tm_outliers(), "argument \"outlier_var\" is missing, with no default")
  })

  it("fails if outlier_var is not of the expected type", {
    testthat::expect_error(tm_outliers(outlier_var = list(wrong_type = "wrong_type")),
    "Assertion on 'outlier_var' failed")
  })

  it("fails if categorical_var is not of the expected type", {
    testthat::expect_error(tm_outliers(outlier_var = outlier_var, categorical_var = list(wrong_type = "wrong_type")),
    "Assertion on 'categorical_var' failed")
  })

  it("fails if ggtheme is not within expected values", {
    testthat::expect_error(tm_outliers(outlier_var = outlier_var, ggtheme = "wrong_type"),
    "'arg' should be one of")
  })
})

# Helper function for creating test data for outliers tests
create_outliers_test_data <- function(test_data_df) {
  # Add row_id to the dataframe before creating teal_data
  test_data_df$.row_id <- seq_len(nrow(test_data_df))
  force(test_data_df)
  
  data_reactive <- shiny::reactive({
    data_obj <- eval(substitute(
      within(teal.data::teal_data(), {
        test_data <- df
      }),
      list(df = test_data_df)
    ))
    datanames(data_obj) <- "test_data"
    teal.data::join_keys(data_obj) <- teal.data::join_keys(
      teal.data::join_key("test_data", "test_data", ".row_id")
    )
    data_obj
  })
  
  data_reactive
}

# Helper function for creating outliers module with test data
create_outliers_module <- function(data, outlier_vars, categorical_vars = NULL, 
                                   outlier_selected, categorical_selected = NULL, ...) {
  tm_outliers(
    outlier_var = list(
      teal.transform::data_extract_spec(
        dataname = "test_data",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices(
            data = isolate(data())[["test_data"]],
            outlier_vars
          ),
          selected = outlier_selected,
          multiple = FALSE
        )
      )
    ),
    categorical_var = if (!is.null(categorical_vars)) {
      list(
        teal.transform::data_extract_spec(
          dataname = "test_data",
          filter = teal.transform::filter_spec(
            vars = teal.transform::variable_choices(
              data = isolate(data())[["test_data"]],
              categorical_vars
            ),
            choices = teal.transform::value_choices(
              data = isolate(data())[["test_data"]],
              categorical_selected
            ),
            selected = teal.transform::value_choices(
              data = isolate(data())[["test_data"]],
              categorical_selected
            ),
            multiple = TRUE
          )
        )
      )
    } else {
      NULL
    },
    ...
  )
}

testthat::describe("tm_outliers module server behavior", {
  it("server function executes successfully with numeric variables and IQR method", {
    data <- create_outliers_test_data(data.frame(
      var1 = c(1:28, 100, 200),  # outliers at end
      var2 = rnorm(30)
    ))

    mod <- create_outliers_module(
      data = data,
      outlier_vars = c("var1", "var2"),
      outlier_selected = "var1"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "outlier_var-dataset_test_data_singleextract-select" = "var1",
          "method" = "IQR",
          "iqr_slider" = 1.5,
          "boxplot_alts" = "Box plot",
          "tabs" = "Boxplot"
        )
        testthat::expect_true(iv_r()$is_valid())
        result <- common_code_q()
        testthat::expect_true(inherits(result, "teal_data"))
        testthat::expect_true("ANL_OUTLIER" %in% names(result))
      }
    )
  })

  it("server function handles Z-score method through module interface", {
    data <- create_outliers_test_data(data.frame(
      var1 = c(rnorm(28), 10, -10),  # outliers at end
      var2 = rnorm(30)
    ))

    mod <- create_outliers_module(
      data = data,
      outlier_vars = c("var1", "var2"),
      outlier_selected = "var1"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "outlier_var-dataset_test_data_singleextract-select" = "var1",
          "method" = "Z-score",
          "zscore_slider" = 3,
          "boxplot_alts" = "Box plot",
          "tabs" = "Boxplot"
        )
        testthat::expect_true(iv_r()$is_valid())
        result <- common_code_q()
        testthat::expect_true(inherits(result, "teal_data"))
        testthat::expect_true("ANL_OUTLIER" %in% names(result))
      }
    )
  })

  it("server function handles Percentile method through module interface", {
    data <- create_outliers_test_data(data.frame(
      var1 = c(1:28, 100, 200),
      var2 = rnorm(30)
    ))

    mod <- create_outliers_module(
      data = data,
      outlier_vars = c("var1", "var2"),
      outlier_selected = "var1"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "outlier_var-dataset_test_data_singleextract-select" = "var1",
          "method" = "Percentile",
          "percentile_slider" = 0.05,
          "boxplot_alts" = "Box plot",
          "tabs" = "Boxplot"
        )
        testthat::expect_true(iv_r()$is_valid())
        result <- common_code_q()
        testthat::expect_true(inherits(result, "teal_data"))
        testthat::expect_true("ANL_OUTLIER" %in% names(result))
      }
    )
  })

  it("server function handles categorical variables through module interface", {
    data <- create_outliers_test_data(data.frame(
      var1 = c(1:28, 100, 200),
      cat1 = factor(rep(c("A", "B", "C"), length.out = 30)),
      cat2 = factor(rep(c("X", "Y"), length.out = 30))
    ))

    mod <- create_outliers_module(
      data = data,
      outlier_vars = c("var1"),
      categorical_vars = c("cat1", "cat2"),
      outlier_selected = "var1",
      categorical_selected = c("cat1", "cat2")
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "outlier_var-dataset_test_data_singleextract-select" = "var1",
          "categorical_var-dataset_test_data_singleextract-filter1-vals" = c("A", "B", "C"),
          "method" = "IQR",
          "iqr_slider" = 1.5,
          "boxplot_alts" = "Box plot",
          "tabs" = "Boxplot",
          "split_outliers" = FALSE,
          "order_by_outlier" = FALSE
        )
        testthat::expect_true(iv_r()$is_valid())
        result <- common_code_q()
        testthat::expect_true(inherits(result, "teal_data"))
      }
    )
  })

  it("server function handles Violin plot type through module interface", {
    data <- create_outliers_test_data(data.frame(
      var1 = c(1:28, 100, 200),
      var2 = rnorm(30)
    ))

    mod <- create_outliers_module(
      data = data,
      outlier_vars = c("var1", "var2"),
      outlier_selected = "var1"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "outlier_var-dataset_test_data_singleextract-select" = "var1",
          "method" = "IQR",
          "iqr_slider" = 1.5,
          "boxplot_alts" = "Violin plot",
          "tabs" = "Boxplot"
        )
        testthat::expect_true(iv_r()$is_valid())
        box_result <- box_plot_q()
        testthat::expect_true(inherits(box_result, "teal_data"))
      }
    )
  })

  it("server function handles Density Plot tab through module interface", {
    data <- create_outliers_test_data(data.frame(
      var1 = c(1:28, 100, 200),
      var2 = rnorm(30)
    ))

    mod <- create_outliers_module(
      data = data,
      outlier_vars = c("var1", "var2"),
      outlier_selected = "var1"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "outlier_var-dataset_test_data_singleextract-select" = "var1",
          "method" = "IQR",
          "iqr_slider" = 1.5,
          "boxplot_alts" = "Box plot",
          "tabs" = "Density Plot"
        )
        testthat::expect_true(iv_r()$is_valid())
        density_result <- density_plot_q()
        testthat::expect_true(inherits(density_result, "teal_data"))
        testthat::expect_true("density_plot" %in% names(density_result))
      }
    )
  })

  it("server function handles Cumulative Distribution Plot tab through module interface", {
    data <- create_outliers_test_data(data.frame(
      var1 = c(1:28, 100, 200),
      var2 = rnorm(30)
    ))

    mod <- create_outliers_module(
      data = data,
      outlier_vars = c("var1", "var2"),
      outlier_selected = "var1"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "outlier_var-dataset_test_data_singleextract-select" = "var1",
          "method" = "IQR",
          "iqr_slider" = 1.5,
          "boxplot_alts" = "Box plot",
          "tabs" = "Cumulative Distribution Plot"
        )
        testthat::expect_true(iv_r()$is_valid())
        cumulative_result <- cumulative_plot_q()
        testthat::expect_true(inherits(cumulative_result, "teal_data"))
        testthat::expect_true("cumulative_plot" %in% names(cumulative_result))
      }
    )
  })

  it("server function handles different ggtheme options through module interface", {
    data <- create_outliers_test_data(data.frame(
      var1 = c(1:28, 100, 200),
      var2 = rnorm(30)
    ))

    for (theme in c("bw", "minimal", "dark")) {
      mod <- create_outliers_module(
        data = data,
        outlier_vars = c("var1", "var2"),
        outlier_selected = "var1",
        ggtheme = theme
      )

      shiny::testServer(
        mod$server,
        args = c(
          list(id = "test", data = data),
          mod$server_args
        ),
        expr = {
          session$setInputs(
            "outlier_var-dataset_test_data_singleextract-select" = "var1",
            "method" = "IQR",
            "iqr_slider" = 1.5,
            "boxplot_alts" = "Box plot",
            "tabs" = "Boxplot"
          )
          testthat::expect_true(iv_r()$is_valid())
          result <- common_code_q()
          testthat::expect_true(inherits(result, "teal_data"))
        }
      )
    }
  })

  it("server function produces box plot output through module interface", {
    data <- create_outliers_test_data(data.frame(
      var1 = c(1:28, 100, 200),
      var2 = rnorm(30)
    ))

    mod <- create_outliers_module(
      data = data,
      outlier_vars = c("var1", "var2"),
      outlier_selected = "var1"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "outlier_var-dataset_test_data_singleextract-select" = "var1",
          "method" = "IQR",
          "iqr_slider" = 1.5,
          "boxplot_alts" = "Box plot",
          "tabs" = "Boxplot"
        )
        testthat::expect_true(iv_r()$is_valid())
        plot_result <- box_plot_r()
        testthat::expect_true(inherits(plot_result, "gg"))
      }
    )
  })

  it("server function produces density plot output through module interface", {
    data <- create_outliers_test_data(data.frame(
      var1 = c(1:28, 100, 200),
      var2 = rnorm(30)
    ))

    mod <- create_outliers_module(
      data = data,
      outlier_vars = c("var1", "var2"),
      outlier_selected = "var1"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "outlier_var-dataset_test_data_singleextract-select" = "var1",
          "method" = "IQR",
          "iqr_slider" = 1.5,
          "boxplot_alts" = "Box plot",
          "tabs" = "Density Plot"
        )
        testthat::expect_true(iv_r()$is_valid())
        plot_result <- density_plot_r()
        testthat::expect_true(inherits(plot_result, "gg"))
      }
    )
  })

  it("server function produces cumulative plot output through module interface", {
    data <- create_outliers_test_data(data.frame(
      var1 = c(1:28, 100, 200),
      var2 = rnorm(30)
    ))

    mod <- create_outliers_module(
      data = data,
      outlier_vars = c("var1", "var2"),
      outlier_selected = "var1"
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        session$setInputs(
          "outlier_var-dataset_test_data_singleextract-select" = "var1",
          "method" = "IQR",
          "iqr_slider" = 1.5,
          "boxplot_alts" = "Box plot",
          "tabs" = "Cumulative Distribution Plot"
        )
        testthat::expect_true(iv_r()$is_valid())
        plot_result <- cumulative_plot_r()
        testthat::expect_true(inherits(plot_result, "gg"))
      }
    )
  })

  
})

