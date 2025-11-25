app_driver_tm_a_pca <- function() {
  # Dataset only used once
  data <- within(teal.data::teal_data(), {
    require(nestcolor)

    USArrests <- USArrests # nolint: object_name.
  })

  init_teal_app_driver(
    teal::init(
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
        ),
        size = c(3, 1, 5),
        alpha = c(.5, 0, 1),
        font_size = c(10, 8, 15),
        ggtheme = "light",
        rotate_xaxis_labels = TRUE,
        pre_output = shiny::tags$div(id = "unique_id_pre", "A pre output"),
        post_output = shiny::tags$div(id = "unique_id_post", "A post output")
      )
    ),
    timeout = 10000
  )
}

testthat::test_that("e2e - tm_a_pca: Module is initialised with the specified defaults in function call.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_pca()

  app_driver$expect_no_shiny_error()

  testthat::expect_setequal(
    app_driver$get_active_module_input("dat-dataset_USArrests_singleextract-select"),
    c("Murder", "Assault")
  )

  testthat::expect_equal(
    app_driver$get_text("#unique_id_pre"),
    "A pre output"
  )
  testthat::expect_equal(
    app_driver$get_text("#unique_id_post"),
    "A post output"
  )

  # Plot options that can be changed in call
  testthat::expect_true(app_driver$get_active_module_input("rotate_xaxis_labels"))
  testthat::expect_equal(app_driver$get_active_module_input("ggtheme"), "light")
  testthat::expect_equal(app_driver$get_active_module_input("font_size"), 10)

  app_driver$stop()
})

testthat::test_that("e2e - tm_a_pca: Eigenvector table should have data extract selection Murder/Assault on header.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_pca()

  # Data selection (adds rows to tables)
  app_driver$set_active_module_input("dat-dataset_USArrests_singleextract-select", c("Murder", "Assault"), wait = FALSE)
  app_driver$expect_no_validation_error()

  testthat::expect_match(app_driver$get_active_module_output("tbl_eigenvector"), "Assault")
  testthat::expect_match(app_driver$get_active_module_output("tbl_eigenvector"), "Murder")

  testthat::expect_no_match(app_driver$get_active_module_output("tbl_eigenvector"), "UrbanPop")

  app_driver$stop()
})

testthat::test_that("e2e - tm_a_pca: Eigenvector table should have data extract selection Murder/UrbanPop on header.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_pca()

  app_driver$set_active_module_input("plot_type", "Circle plot")

  app_driver$set_active_module_input("dat-dataset_USArrests_singleextract-select", c("Murder", "UrbanPop"))
  app_driver$expect_no_validation_error()

  testthat::expect_match(app_driver$get_active_module_output("tbl_eigenvector"), "UrbanPop")
  testthat::expect_match(app_driver$get_active_module_output("tbl_eigenvector"), "Murder")
  testthat::expect_no_match(app_driver$get_active_module_output("tbl_eigenvector"), "Assault")

  app_driver$stop()
})

testthat::test_that("e2e - tm_a_pca: Color by columns (data_extract) must be from non-selected variable set.", {
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

testthat::test_that("e2e - tm_a_pca: Changing output encodings of tables_display does not generate errors.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_pca()
  app_driver$expect_no_validation_error()

  # Display section (hides tables)

  app_driver$set_active_module_input("tables_display", c())
  app_driver$expect_no_validation_error()

  # Tables are removed from DOM (output should generate a silent error empty message)
  testthat::expect_type(app_driver$get_active_module_output("tbl_importance"), "list")
  testthat::expect_setequal(names(app_driver$get_active_module_output("tbl_importance")), c("message", "call", "type"))

  testthat::expect_type(app_driver$get_active_module_output("tbl_eigenvector"), "list")
  testthat::expect_setequal(names(app_driver$get_active_module_output("tbl_eigenvector")), c("message", "call", "type"))

  app_driver$stop()
})

testthat::test_that("e2e - tm_a_pca: Changing output encodings for 'plot type' does not generate errors.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_pca()

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

  app_driver$stop()
})

testthat::test_that("e2e - tm_a_pca: Changing output encodings of 'standardization' does not generate errors.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_pca()
  app_driver$expect_no_validation_error()

  # Pre-processing

  app_driver$set_active_module_input("standardization", "center")
  app_driver$expect_no_validation_error()
  app_driver$set_active_module_input("standardization", "center_scale")
  app_driver$expect_no_validation_error()
  app_driver$set_active_module_input("standardization", "none") # Initial value
  app_driver$expect_no_validation_error()

  app_driver$stop()
})

testthat::test_that("e2e - tm_a_pca: Changing output encodings of 'NA action' does not generate errors.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_pca()
  app_driver$expect_no_validation_error()

  # NA Action

  app_driver$set_active_module_input("na_action", "drop")
  app_driver$set_active_module_input("na_action", "none")

  app_driver$stop()
})

testthat::test_that("e2e - tm_a_pca: Changing output encodings of 'plot_type' hides and shows options.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_pca()
  app_driver$expect_no_validation_error()

  # Selected plot's specific settings is not visible
  no_plot_settings_selector <- app_driver$namespaces(TRUE)$module("plot_settings span.help-block")
  x_axis_selector <- app_driver$namespaces(TRUE)$module("x_axis")
  color_by_selector <- app_driver$namespaces(TRUE)$module("response-dataset_USArrests_singleextract-select_input")

  app_driver$set_active_module_input("plot_type", "Elbow plot", wait_ = FALSE)
  testthat::expect_true(app_driver$is_visible(no_plot_settings_selector))
  testthat::expect_false(app_driver$is_visible(x_axis_selector))
  testthat::expect_false(app_driver$is_visible(color_by_selector))

  app_driver$set_active_module_input("plot_type", "Circle plot", wait_ = FALSE)
  testthat::expect_false(app_driver$is_visible(no_plot_settings_selector))
  testthat::expect_true(app_driver$is_visible(x_axis_selector))

  app_driver$set_active_module_input("plot_type", "Biplot", wait_ = FALSE)
  testthat::expect_false(app_driver$is_visible(no_plot_settings_selector))
  testthat::expect_true(app_driver$is_visible(x_axis_selector))
  testthat::expect_true(app_driver$is_visible(color_by_selector))

  app_driver$stop()
})

testthat::test_that("e2e - tm_a_pca: Changing output encodings of 'theme' does not generate errors.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_pca()
  app_driver$expect_no_validation_error()

  # Theme

  app_driver$set_active_module_input("ggtheme-selectized", "bw")
  app_driver$expect_no_validation_error()
  app_driver$set_active_module_input("ggtheme-selectized", "light")
  app_driver$expect_no_validation_error()
  app_driver$set_active_module_input("ggtheme-selectized", "dark")
  app_driver$expect_no_validation_error()

  app_driver$stop()
})

testthat::test_that("e2e - tm_a_pca: Changing output encodings of 'font size' does not generate errors.", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_a_pca()
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
