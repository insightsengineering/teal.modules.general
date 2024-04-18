app_driver_tm_a_pca <- function() {
  # Dataset only used once
  data <- within(teal.data::teal_data(), {
    require(nestcolor)

    USArrests <- USArrests # nolint: object_name.
  })
  teal.data::datanames(data) <- "USArrests"


  init_teal_app_driver(
    data = data,
    modules = tm_a_pca(
      dat = teal.transform::data_extract_spec(
        dataname = "USArrests",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices(
            data = data[["USArrests"]],
            c("Murder", "Assault", "UrbanPop", "Rape")
          ),
          selected = c("Murder", "Assault"),
          multiple = TRUE
        )
      )
    )
  )
}

testthat::test_that("e2e - tm_a_pca: Data selection (data_extract) changes eigenvector table", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_pca()

  # Data selection (adds rows to tables)
  app_driver$set_active_module_input("dat-dataset_USArrests_singleextract-select", c("Murder", "Assault"), wait = FALSE)
  app_driver$expect_no_validation_error()

  testthat::expect_match(app_driver$get_active_module_output("tbl_eigenvector"), "Assault")

  testthat::expect_no_match(app_driver$get_active_module_output("tbl_eigenvector"), "UrbanPop")
})

testthat::test_that("e2e - tm_a_pca: Original coordinates (data_extract) changes output of plot", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_pca()

  app_driver$set_active_module_input("plot_type", "Circle plot")

  app_driver$set_active_module_input("dat-dataset_USArrests_singleextract-select", c("Murder", "UrbanPop"))
  app_driver$expect_no_validation_error()

  testthat::expect_match(app_driver$get_active_module_output("tbl_eigenvector"), "UrbanPop")
})

testthat::test_that("e2e - tm_a_pca: Color by columns (data_extract) must be from non-selected variable set", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_pca()

  app_driver$set_active_module_input("plot_type", "Biplot")

  # Change colors of data points
  app_driver$set_active_module_input("response-dataset_USArrests_singleextract-select", c("UrbanPop"))
  app_driver$expect_no_validation_error()

  app_driver$set_active_module_input("response-dataset_USArrests_singleextract-select", c("Murder"))
  app_driver$expect_validation_error()

  app_driver$stop()
})

testthat::test_that("e2e - tm_a_pca: Changing output encodings does not generate errors", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_pca()

  # Display section (hides tables)

  app_driver$set_active_module_input("tables_display", c())
  app_driver$expect_no_validation_error()

  testthat::expect_type(app_driver$get_active_module_output("tbl_importance"), "list")
  testthat::expect_setequal(names(app_driver$get_active_module_output("tbl_importance")), c("message", "call", "type"))

  testthat::expect_type(app_driver$get_active_module_output("tbl_eigenvector"), "list")
  testthat::expect_setequal(names(app_driver$get_active_module_output("tbl_eigenvector")), c("message", "call", "type"))

  # Plot type (select each)

  # Changing input will trigger an output change
  app_driver$set_active_module_input("plot_type", "Circle plot")
  app_driver$expect_no_validation_error()

  app_driver$set_active_module_input("plot_type", "Biplot")
  app_driver$expect_no_validation_error()

  app_driver$set_active_module_input("plot_type", "Eigenvector plot")
  app_driver$expect_no_validation_error()

  app_driver$set_active_module_input("plot_type", "Elbow plot") # Initial value
  app_driver$expect_no_validation_error()

  # Pre-processing

  app_driver$set_active_module_input("standardization", "center")
  app_driver$expect_no_validation_error
  app_driver$set_active_module_input("standardization", "center_scale")
  app_driver$expect_no_validation_error
  app_driver$set_active_module_input("standardization", "none") # Initial value
  app_driver$expect_no_validation_error

  # NA Action

  app_driver$set_active_module_input("na_action", "drop")
  app_driver$set_active_module_input("na_action", "none")

  # Selected plot's specific settings is not visible
  no_plot_settings_selector <- sprintf("#%s-%s %s", app_driver$active_module_ns(), "plot_settings", "span.help-block")
  x_axis_selector <- sprintf("#%s-%s", app_driver$active_module_ns(), "x_axis")
  color_by_selector <- sprintf(
    "#%s-%s",
    app_driver$active_module_ns(),
    "response-dataset_USArrests_singleextract-select_input"
  )

  app_driver$set_active_module_input("plot_type", "Elbow plot", wait = FALSE)
  testthat::expect_true(app_driver$is_visible(no_plot_settings_selector))
  testthat::expect_false(app_driver$is_visible(x_axis_selector))
  testthat::expect_false(app_driver$is_visible(color_by_selector))

  app_driver$set_active_module_input("plot_type", "Circle plot", wait = FALSE)
  testthat::expect_false(app_driver$is_visible(no_plot_settings_selector))
  testthat::expect_true(app_driver$is_visible(x_axis_selector))

  app_driver$set_active_module_input("plot_type", "Biplot", wait = FALSE)
  testthat::expect_false(app_driver$is_visible(no_plot_settings_selector))
  testthat::expect_true(app_driver$is_visible(x_axis_selector))
  testthat::expect_true(app_driver$is_visible(color_by_selector))

  # Theme

  app_driver$set_active_module_input("ggtheme-selectized", "bw")
  app_driver$expect_no_validation_error()
  app_driver$set_active_module_input("ggtheme-selectized", "light")
  app_driver$expect_no_validation_error()
  app_driver$set_active_module_input("ggtheme-selectized", "dark")
  app_driver$expect_no_validation_error()

  # Font size

  app_driver$set_active_module_input("font_size", "8")
  app_driver$expect_no_validation_error()

  app_driver$set_active_module_input("font_size", "20")
  app_driver$expect_no_validation_error()

  app_driver$set_active_module_input("font_size", "15")
  app_driver$expect_no_validation_error()

  app_driver$stop()
})
