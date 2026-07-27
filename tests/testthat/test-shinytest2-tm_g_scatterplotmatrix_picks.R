app_driver_tm_g_scatterplotmatrix <- function() { # nolint: object_length_linter.
  data <- simple_cdisc_data()
  init_teal_app_driver(
    teal::init(
      data = data,
      modules = tm_g_scatterplotmatrix(
        label = "Scatterplot matrix",
        variables = list(
          teal.picks::picks(
            teal.picks::datasets("ADSL", "ADSL"),
            suppressWarnings(
              teal.picks::variables(selected = c("AGE", "RACE", "SEX"), multiple = TRUE),
              class = "picks_delayed"
            )
          ),
          teal.picks::picks(
            teal.picks::datasets("ADRS", "ADRS"),
            suppressWarnings(
              teal.picks::variables(selected = NULL, multiple = TRUE),
              class = "picks_delayed"
            )
          )
        ),
        plot_height = c(600, 200, 2000),
        plot_width = NULL,
        pre_output = NULL,
        post_output = NULL
      )
    )
  )
}

test_that("e2e - tm_g_scatterplotmatrix: Initializes without errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_scatterplotmatrix()

  app_driver$expect_no_shiny_error()

  testthat::expect_equal(app_driver$get_text(".teal-modules-tree .active"), "Scatterplot matrix")

  encoding_dataset <- trimws(app_driver$get_text(".badge-dropdown-label"))
  testthat::expect_match(encoding_dataset, "^(ADSL|ADRS)[[:space:]]")

  app_driver$stop()
})

test_that("e2e - tm_g_scatterplotmatrix: Verify module displays data table", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_scatterplotmatrix()

  # table
  app_driver$expect_visible(
    app_driver$namespaces(TRUE)$module("myplot-plot_out_main .shiny-plot-output")
  )

  app_driver$stop()
})

test_that("e2e - tm_g_scatterplotmatrix: Verify default values and settings (data_extracts) for data selection", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_scatterplotmatrix()

  # default variable selection
  testthat::expect_equal(
    app_driver$get_values()$export[[app_driver$namespaces()$module("pick_1-picks_resolved")]]$dataset$selected,
    "ADSL"
  )
  testthat::expect_equal(
    app_driver$get_active_module_input("pick_1-variables-selected"),
    c("AGE", "SEX", "RACE")
  )

  # new variable selection
  set_picks_slot_selected(app_driver, "pick_1", NULL)
  set_picks_slot_selected(app_driver, "pick_2", c("SEX", "RACE", "ETHNIC"))
  app_driver$expect_no_validation_error()

  app_driver$stop()
})

test_that("e2e - tm_g_scatterplotmatrix: Change plot settings", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_g_scatterplotmatrix()

  app_driver$set_active_module_input("alpha", 0.7)
  app_driver$set_active_module_input("size", 2)

  app_driver$expect_no_validation_error()

  app_driver$click(selector = app_driver$namespaces(TRUE)$module("cor"))
  app_driver$expect_no_validation_error()
  app_driver$stop()
})
