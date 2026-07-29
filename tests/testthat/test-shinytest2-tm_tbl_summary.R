app_driver_tm_tbl_summary <- function() {
  init_teal_app_driver(
    teal::init(
      data = data <- within(teal.data::teal_data(), CO2 <- CO2), # nolint object_name_linter.
      modules = tm_tbl_summary(
        by = teal.picks::picks(
          teal.picks::datasets("CO2", "CO2"),
          suppressWarnings(teal.picks::variables(selected = "Plant"), classes = "picks_delayed")
        ),
        include = teal.picks::picks(
          teal.picks::datasets("CO2", "CO2"),
          suppressWarnings(
            teal.picks::variables(selected = c("Type", "Treatment"), multiple = TRUE),
            classes = "picks_delayed"
          )
        )
      )
    )
  )
}

test_that("e2e - tm_tbl_summary: Initializes without errors", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_tbl_summary()

  app_driver$expect_no_shiny_error()

  testthat::expect_equal(app_driver$get_text(".teal-modules-tree .active"), "Summary table")

  encoding_dataset <- trimws(app_driver$get_text(".badge-dropdown-label"))
  testthat::expect_match(encoding_dataset, "^(CO2)[[:space:]]")

  app_driver$stop()
})

test_that("e2e - tm_tbl_summary: Verify module displays data table", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_tbl_summary()

  # table
  app_driver$expect_visible(selector = app_driver$namespaces(TRUE)$module("table-table-with-settings"))

  app_driver$stop()
})

test_that("e2e - tm_tbl_summary: Verify default values and settings (data_extracts) for data selection", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_tbl_summary()

  # default variable selection
  testthat::expect_equal(
    app_driver$get_active_module_input("by-variables-selected"),
    "Plant"
  )

  testthat::expect_equal(
    app_driver$get_active_module_input("include-variables-selected"),
    c("Type", "Treatment")
  )

  # new variable selection
  set_picks_slot_selected(app_driver, "by", "Treatment")
  set_picks_slot_selected(app_driver, "include", c("Plant", "Type", "conc"))
  app_driver$expect_no_validation_error()

  app_driver$stop()
})

test_that("e2e - tm_tbl_summary: Change table settings", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_tbl_summary()

  app_driver$click(
    selector = sprintf("%s .radio:last-child input", app_driver$namespaces(TRUE)$module("custom-missing"))
  )
  app_driver$expect_no_validation_error()

  app_driver$click(
    selector = sprintf("%s .radio:last-child input", app_driver$namespaces(TRUE)$module("custom-percent"))
  )
  app_driver$expect_no_validation_error()

  app_driver$stop()
})
