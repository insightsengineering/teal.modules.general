app_driver_tm_g_association <- function() {
  data <- within(teal.data::teal_data(), {
    require(nestcolor)
    require(ggplot2)
    CO2 <- CO2 # nolint: object_name.
    .factors <- names(Filter(isTRUE, vapply(CO2, is.factor, logical(1L))))
    CO2[.factors] <- lapply(CO2[.factors], as.character) # nolint: object_name.
  })

  init_teal_app_driver(
    data = data,
    modules = tm_g_association(
      label = "Association",
      ref = teal.transform::data_extract_spec(
        dataname = "CO2",
        select = teal.transform::select_spec(
          label = "Select variable:",
          choices = teal.transform::variable_choices(data[["CO2"]], c("Plant", "Type", "Treatment")),
          selected = "Plant",
          fixed = FALSE
        )
      ),
      vars = teal.transform::data_extract_spec(
        dataname = "CO2",
        select = teal.transform::select_spec(
          label = "Select variables:",
          choices = teal.transform::variable_choices(data[["CO2"]], c("Plant", "Type", "Treatment")),
          selected = "Treatment",
          multiple = TRUE,
          fixed = FALSE
        )
      ),
      show_association = TRUE,
      plot_height = c(600, 400, 5000),
      plot_width = NULL,
      distribution_theme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
      association_theme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
      pre_output = NULL,
      post_output = NULL,
      ggplot2_args = teal.widgets::ggplot2_args(
        labs = list(subtitle = "Plot generated by Association Module")
      )
    )
  )
}

testthat::test_that("e2e - tm_g_association: Data parameter and module label is passed properly.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_g_association()
  app_driver$expect_no_shiny_error()

  testthat::expect_equal(
    app_driver$get_text("#teal-teal_modules-active_tab li a.active"),
    "Association"
  )

  encoding_dataset <- app_driver$get_text("#teal-teal_modules-association .help-block")
  testthat::expect_match(encoding_dataset, "Dataset:[\n ]*CO2", all = FALSE)

  app_driver$stop()
})

testthat::test_that("e2e - tm_g_association:
  Data extract spec elements are initialized with the default values specified by ref and vars arguments.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_association()

  testthat::expect_identical(
    app_driver$get_active_module_input("ref-dataset_CO2_singleextract-select"),
    "Plant"
  )
  app_driver$set_active_module_input("ref-dataset_CO2_singleextract-select", "Type")
  app_driver$expect_no_validation_error()

  testthat::expect_identical(
    app_driver$get_active_module_input("vars-dataset_CO2_singleextract-select"),
    "Treatment"
  )
  app_driver$set_active_module_input("vars-dataset_CO2_singleextract-select", "Plant")
  app_driver$expect_no_validation_error()

  app_driver$stop()
})

testthat::test_that("e2e - tm_g_association: Module plot is visible.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_association()

  testthat::expect_true(app_driver$is_visible(app_driver$active_module_element("myplot-plot_main > img")))

  app_driver$stop()
})

testthat::test_that("e2e - tm_g_association: Check and set default values for radio buttons.", {
  testthat::skip("chromium")
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_association()

  testthat::expect_true(app_driver$get_active_module_input("association"))
  testthat::expect_false(app_driver$get_active_module_input("show_dist"))
  testthat::expect_false(app_driver$get_active_module_input("log_transformation"))

  app_driver$set_active_module_input("association", FALSE)
  app_driver$set_active_module_input("show_dist", TRUE)
  app_driver$set_active_module_input("log_transformation", TRUE)
  app_driver$expect_no_validation_error()
  app_driver$expect_no_shiny_error()

  app_driver$stop()
})
