testthat::test_that("e2e - tm_variable_browser: content is displayed correctly", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_variable_browser()

  app_driver$expect_no_shiny_error()

  # Test tab name
  testthat::expect_equal(
    trimws(app_driver$get_text("#teal-main_ui-root-active_tab > li.active")),
    "Variable browser (e2e)"
  )

  # All datanames are available on the left sidebar
  testthat::expect_setequal(
    trimws(app_driver$get_text(
      sprintf("%s .nav li", app_driver$active_module_element("ui_variable_browser"))
    )),
    c("iris", "mtcars", "women", "faithful", "CO2")
  )

  # Numeric types have statistics table in main output
  testthat::expect_contains(
    trimws(app_driver$active_module_element_text("variable_summary_table table th")),
    "Statistic"
  )

  # Categorical type have levels table in main output
  current_var <- trimws(app_driver$get_text(
    sprintf("%s .nav li.active", app_driver$active_module_element("ui_variable_browser"))
  ))

  categorical_selector <- app_driver$active_module_element(
    sprintf(
      "variable_browser_%s table tr td:nth-child(1) i",
      current_var
    )
  )

  ## Using AppDriver to click does not trigger DT callback.
  ## Find a categorical variable to mock a click
  categorical_index <- min(
    c(
      which(
        grepl("fa-chart-bar", app_driver$get_attr(categorical_selector, "class")),
        arr.ind = TRUE
      ),
      Inf
    ),
    na.rm = TRUE
  )

  if (!is.infinite(categorical_index)) {
    app_driver$set_active_module_input(
      sprintf("variable_browser_%s_rows_selected", current_var),
      categorical_index,
      allow_no_input_binding_ = TRUE
    )
    app_driver$set_active_module_input(
      sprintf("variable_browser_%s_last_clicked", current_var),
      categorical_index,
      allow_no_input_binding_ = TRUE
    )
  }

  ## Test will fail if Level column is not found
  testthat::expect_contains(
    trimws(app_driver$active_module_element_text("variable_summary_table table th")),
    "Level"
  )

  app_driver$stop()
})


testthat::test_that("e2e - tm_variable_browser: main output interactivity doesn't show errors", {
  skip_if_too_deep(5)

  app_driver <- app_driver_tm_variable_browser()

  app_driver$expect_no_shiny_error()

  selector_ns <- app_driver$active_module_element

  # Show density button being clicked on and off
  app_driver$click(selector = selector_ns("display_density"))
  app_driver$expect_no_shiny_error()
  app_driver$click(selector = selector_ns("display_density"))
  app_driver$expect_no_shiny_error()

  # Test Enable Remove outliers button
  testthat::expect_null(
    app_driver$active_module_element_text("outlier_definition_slider")
  ) ## Does not exist initially

  app_driver$click(selector = selector_ns("remove_outliers"))
  app_driver$expect_no_shiny_error()
  testthat::expect_length(
    app_driver$active_module_element_text("outlier_definition_slider"),
    1
  ) ## Added to UI

  app_driver$set_active_module_input("outlier_definition_slider", 2)
  app_driver$expect_no_shiny_error()

  app_driver$click(selector = selector_ns("remove_outliers"))
  app_driver$expect_no_shiny_error()

  # Test changing plot settings
  testthat::expect_false(app_driver$is_visible(selector_ns("ggplot_theme-selectized")))

  app_driver$set_active_module_input("ggplot_theme", "bw")
  app_driver$expect_no_validation_error()
  app_driver$set_active_module_input("ggplot_theme", "light")
  app_driver$expect_no_validation_error()
  app_driver$set_active_module_input("ggplot_theme", "dark")
  app_driver$expect_no_validation_error()

  app_driver$set_active_module_input("font_size", "8")
  app_driver$expect_no_validation_error()
  app_driver$set_active_module_input("font_size", "20")
  app_driver$expect_no_validation_error()
  app_driver$set_active_module_input("font_size", "15")
  app_driver$expect_no_validation_error()

  app_driver$set_active_module_input("label_rotation", "25")
  app_driver$expect_no_validation_error()
  app_driver$set_active_module_input("label_rotation", "0")
  app_driver$expect_no_validation_error()
  app_driver$set_active_module_input("label_rotation", "90")
  app_driver$expect_no_validation_error()

  app_driver$stop()
})
