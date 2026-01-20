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
