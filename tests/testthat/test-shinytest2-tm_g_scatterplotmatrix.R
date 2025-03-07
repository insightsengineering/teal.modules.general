app_driver_tm_g_scatterplotmatrix <- function() { # nolint: object_length_linter.
  data <- simple_cdisc_data()
  init_teal_app_driver(
    data = data,
    modules = tm_g_scatterplotmatrix(
      label = "Scatterplot matrix",
      variables = list(
        teal.transform::data_extract_spec(
          dataname = "ADSL",
          select = teal.transform::select_spec(
            label = "Select variables:",
            choices = teal.transform::variable_choices(data[["ADSL"]]),
            selected = c("AGE", "RACE", "SEX"),
            multiple = TRUE,
            ordered = TRUE,
            fixed = FALSE
          )
        ),
        teal.transform::data_extract_spec(
          dataname = "ADRS",
          filter = teal.transform::filter_spec(
            label = "Select endpoints:",
            vars = c("PARAMCD", "AVISIT"),
            choices = teal.transform::value_choices(data[["ADRS"]], c("PARAMCD", "AVISIT"), c("PARAM", "AVISIT")),
            selected = "INVET - END OF INDUCTION",
            multiple = TRUE
          ),
          select = teal.transform::select_spec(
            label = "Select variables:",
            choices = teal.transform::variable_choices(data[["ADRS"]]),
            selected = c("AGE", "AVAL", "ADY"),
            multiple = TRUE,
            ordered = TRUE,
            fixed = FALSE
          )
        )
      ),
      plot_height = c(600, 200, 2000),
      plot_width = NULL,
      pre_output = NULL,
      post_output = NULL
    ),
    timeout = 3000
  )
}

test_that("e2e - tm_g_scatterplotmatrix: Initializes without errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_scatterplotmatrix()

  app_driver$expect_no_shiny_error()

  testthat::expect_equal(
    app_driver$get_text("#teal-teal_modules-active_tab .active"),
    "Scatterplot matrix"
  )

  encoding_dataset <- app_driver$get_text("#teal-teal_modules-scatterplot_matrix .help-block")
  testthat::expect_match(encoding_dataset, "Datasets.*ADSL.*ADRS", all = FALSE)

  app_driver$stop()
})

test_that("e2e - tm_g_scatterplotmatrix: Verify module displays data table", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_scatterplotmatrix()

  # table
  testthat::expect_true(
    app_driver$is_visible(sprintf("%s .shiny-plot-output", app_driver$active_module_element("myplot-plot_out_main")))
  )

  app_driver$stop()
})

test_that("e2e - tm_g_scatterplotmatrix: Verify default values and settings (data_extracts) for data selection", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_scatterplotmatrix()

  # default variable selection
  dataset <- app_driver$get_active_module_input("variables-dataset")
  variables <- app_driver$get_active_module_input(sprintf(
    "variables-dataset_%s_singleextract-select",
    dataset
  ))
  testthat::expect_equal(
    dataset,
    "ADSL"
  )
  testthat::expect_equal(
    variables,
    c("AGE", "SEX", "RACE")
  )

  # new variable selection
  app_driver$set_active_module_input("variables-dataset", "ADRS")
  app_driver$set_active_module_input("dataset_ADRS_singleextract-select", c("SEX", "RACE", "ETHNIC"))
  app_driver$expect_no_validation_error()

  app_driver$stop()
})

test_that("e2e - tm_g_scatterplotmatrix: Change plot settings", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_scatterplotmatrix()

  app_driver$set_active_module_input("alpha", 0.7)
  app_driver$set_active_module_input("cex", 2)

  app_driver$expect_no_validation_error()

  app_driver$click(selector = app_driver$active_module_element("cor"))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})
