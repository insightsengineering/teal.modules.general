app_driver_tm_g_scatterplotmatrix <- function() {
  TealAppDriver$new(
    data = simple_cdisc_data(),
    modules = tm_g_scatterplotmatrixmatrix(
      label = "Scatterplot matrix",
      variables = list(
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            label = "Select variables:",
            choices = variable_choices(data[["ADSL"]]),
            selected = c("AGE", "RACE", "SEX"),
            multiple = TRUE,
            ordered = TRUE,
            fixed = FALSE
          )
        ),
        data_extract_spec(
          dataname = "ADRS",
          filter = filter_spec(
            label = "Select endpoints:",
            vars = c("PARAMCD", "AVISIT"),
            choices = value_choices(data[["ADRS"]], c("PARAMCD", "AVISIT"), c("PARAM", "AVISIT")),
            selected = "INVET - END OF INDUCTION",
            multiple = TRUE
          ),
          select = select_spec(
            label = "Select variables:",
            choices = variable_choices(data[["ADRS"]]),
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

test_that("e2e: tm_g_scatterplotmatrix initializes without errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_scatterplotmatrix()

  app_driver$expect_no_shiny_error()
  app_driver$stop()
})

test_that("e2e: tm_g_scatterplotmatrix displays data table", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_scatterplotmatrix()

  # table
  testthat::expect_true(app_driver$is_visible(selector = app_driver$active_module_element("myplot-plot_out_main")))

  app_driver$stop()
})

test_that("e2e: tm_g_scatterplotmatrix data selection (data_extracts) default value and setting", {
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

test_that("e2e: tm_g_scatterplotmatrix changes plot settings", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_scatterplotmatrix()

  app_driver$set_module_input("alpha", 0.7)
  app_driver$set_module_input("cex", 2)

  app_driver$expect_no_validation_error()

  app_driver$click(selector = app_driver$active_module_element("cor"))
  app_driver$expect_no_validation_error()


  app_driver$stop()
})
