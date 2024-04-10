testthat::test_that("e2e - tm_a_pca: data extract changes output", {
  skip_if_too_deep(5)

  require(shinytest2)

  data <- within(teal_data(), {
    require(nestcolor)
    require(ggplot2)

    USArrests <- USArrests
  })
  datanames(data) <- "USArrests"


  app <- TealAppDriver$new(
    data = data,
    modules = tm_a_pca(
      dat = data_extract_spec(
        dataname = "USArrests",
        select = select_spec(
          choices = variable_choices(
            data = data[["USArrests"]],
            c("Murder", "Assault", "UrbanPop", "Rape")
          ),
          selected = c("Murder", "Assault"),
          multiple = TRUE
        )
      )
    )
  )

  # Data selection (adds rows to tables)
  app$set_module_input("dat-dataset_USArrests_singleextract-select", c("Murder", "Assault"))
  app$expect_no_validation_error()

  testthat::expect_match(app$get_active_module_output("tbl_eigenvector"), "Assault")

  testthat::expect_failure(
    testthat::expect_match(app$get_active_module_output("tbl_eigenvector"), "UrbanPop")
  )

  app$set_module_input("dat-dataset_USArrests_singleextract-select", c("Murder", "UrbanPop"))
  app$expect_no_validation_error()

  testthat::expect_match(app$get_active_module_output("tbl_eigenvector"), "UrbanPop")

  # Original Coordinate
  app$set_module_input("plot_type", "Biplot")
  app$set_module_input("variables", c("Murder"))
  app$expect_no_validation_error()

  # Color by
  app$set_module_input("response-dataset_USArrests_singleextract-select", c("Assault"))
  app$expect_no_validation_error()

  app$set_module_input("response-dataset_USArrests_singleextract-select", c("Murder"))
  app$expect_validation_error()

  app$stop()
})

testthat::test_that("e2e - tm_a_pca: encodings update main panel", {
  skip_if_too_deep(5)

  require(shinytest2)

  data <- within(teal_data(), {
    require(nestcolor)

    USArrests <- USArrests
  })
  datanames(data) <- "USArrests"

  app <- TealAppDriver$new(
    data = data,
    modules = tm_a_pca(
      dat = data_extract_spec(
        dataname = "USArrests",
        select = select_spec(
          choices = variable_choices(
            data = data[["USArrests"]],
            c("Murder", "Assault", "UrbanPop", "Rape")
          ),
          selected = c("Murder", "Assault"),
          multiple = TRUE
        )
      )
    )
  )

  # Display section (hides tables)

  app$set_module_input("tables_display", c())
  app$expect_no_validation_error()

  testthat::expect_type(app$get_active_module_output("tbl_importance"), "list")
  testthat::expect_setequal(names(app$get_active_module_output("tbl_importance")), c("message", "call", "type"))

  testthat::expect_type(app$get_active_module_output("tbl_eigenvector"), "list")
  testthat::expect_setequal(names(app$get_active_module_output("tbl_eigenvector")), c("message", "call", "type"))

  # Plot type (select each)

  # Changing input will trigger an output change
  app$set_module_input("plot_type", "Circle plot")
  app$expect_no_validation_error()

  app$set_module_input("plot_type", "Biplot")
  app$expect_no_validation_error()

  app$set_module_input("plot_type", "Eigenvector plot")
  app$expect_no_validation_error()

  app$set_module_input("plot_type", "Elbow plot") # Initial value
  app$expect_no_validation_error()

  # Pre-processing

  app$set_module_input("standardization", "center")
  app$set_module_input("standardization", "center_scale")
  app$set_module_input("standardization", "none") # Initial value

  # NA Action

  app$set_module_input("na_action", "drop")
  app$set_module_input("na_action", "none")

  # Selected plot specific settings is not visible for Elbow plot
  no_plot_settings_selector <- sprintf("#%s-%s %s", app$active_module_ns(), "plot_settings", "span.help-block")
  x_axis_selector <- sprintf("#%s-%s", app$active_module_ns(), "x_axis")
  color_by_selector <- sprintf("#%s-%s", app$active_module_ns(), "response-dataset_USArrests_singleextract-select_input")

  app$set_module_input("plot_type", "Elbow plot", wait = FALSE)
  testthat::expect_true(app$is_visible(no_plot_settings_selector))
  testthat::expect_false(app$is_visible(x_axis_selector))
  testthat::expect_false(app$is_visible(color_by_selector))

  app$set_module_input("plot_type", "Circle plot", wait = FALSE)
  testthat::expect_true(app$is_visible(x_axis_selector))

  app$set_module_input("plot_type", "Biplot", wait = FALSE)
  testthat::expect_true(app$is_visible(color_by_selector))

  # Theme

  app$set_module_input("ggtheme-selectized", "bw")
  app$expect_no_validation_error()
  app$set_module_input("ggtheme-selectized", "light")
  app$expect_no_validation_error()
  app$set_module_input("ggtheme-selectized", "dark")
  app$expect_no_validation_error()

  # Font size

  app$set_module_input("font_size", "8")
  app$expect_no_validation_error()

  app$set_module_input("font_size", "20")
  app$expect_no_validation_error()

  app$set_module_input("font_size", "15")
  app$expect_no_validation_error()

  app$stop()
})
