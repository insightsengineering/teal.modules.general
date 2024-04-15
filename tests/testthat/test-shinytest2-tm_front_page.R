app_driver_tm_front_page <- function() {
  data <- simple_cdisc_data()
  data <- within(data, {
    attr(ADSL, "metadata") <- list("Author" = "NEST team", "data_source" = "synthetic data")
  })

  TealAppDriver$new(
    data = data,
    modules = tm_front_page(
      label = "Data Table",
      header_text = c(
        "Important information" = "It can go here.",
        "Other information" = "Can go here."
      ),
      tables = list("MTCARS" = head(mtcars, 5), "IRIS" = head(iris, 5)),
      additional_tags = HTML("Additional HTML or shiny tags go here"),
      footnotes = "This is a footnote",
      show_metadata = TRUE
    ),
    timeout = 3000
  )
}

test_that("e2e: tm_front_page initializes without errors and html elements", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_front_page()

  app_driver$expect_no_shiny_error()

  # header text
  testthat::expect_match(
    app_driver$get_text(selector = "#front_page_headers"),
    "Important information"
  )
  # additional tags
  testthat::expect_match(
    app_driver$get_text(selector = "#front_page_custom_html"),
    "Additional HTML or shiny tags go here"
  )
  app_driver$stop()
})

test_that("e2e: tm_front_page displays tables", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_front_page()
  # tables
  testthat::expect_match(app_driver$get_active_module_output("table_1"), "MTCARS")
  testthat::expect_true(app_driver$is_visible(selector = app_driver$active_module_element("table_1")))

  testthat::expect_match(app_driver$get_active_module_output("table_2"), "IRIS")
  testthat::expect_true(app_driver$is_visible(selector = app_driver$active_module_element("table_2")))
  app_driver$stop()
})

test_that("e2e: tm_front_page displays metadata", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_front_page()

  # show metadata
  app_driver$click(NS(app_driver$active_module_ns(), "metadata_button"))
  testthat::expect_true(app_driver$is_visible(selector = app_driver$active_module_element("metadata_table")))

  app_driver$stop()
})
