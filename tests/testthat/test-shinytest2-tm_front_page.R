app_driver_tm_front_page <- function() {
  data <- simple_cdisc_data()
  data <- within(data, {
    attr(ADSL, "metadata") <- list("Author" = "NEST team", "data_source" = "synthetic data")
  })

  init_teal_app_driver(
    data = data,
    modules = tm_front_page(
      label = "Front page",
      datanames = "all",
      header_text = c(
        "Important information" = "It can go here.",
        "Other information" = "Can go here."
      ),
      tables = list("MTCARS" = head(mtcars, 5), "IRIS" = head(iris, 5)),
      additional_tags = HTML("Additional HTML or shiny tags go here"),
      footnotes = "This is a footnote"
    ),
    timeout = 3000
  )
}

test_that("e2e - tm_front_page: Initializes without errors and check html elements", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_front_page()

  app_driver$expect_no_shiny_error()

  testthat::expect_equal(
    app_driver$get_text("#teal-teal_modules-active_tab .active"),
    "Front page"
  )

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

test_that("e2e - tm_front_page: Verify the module displays tables", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_front_page()
  # tables
  testthat::expect_match(app_driver$get_active_module_output("table_1"), "MTCARS")
  testthat::expect_true(app_driver$is_visible(sprintf("%s table", app_driver$active_module_element("table_1"))))

  testthat::expect_match(app_driver$get_active_module_output("table_2"), "IRIS")
  testthat::expect_true(app_driver$is_visible(sprintf("%s table", app_driver$active_module_element("table_2"))))
  app_driver$stop()
})

test_that("e2e - tm_front_page: Verify the module displays metadata", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_front_page()

  # show metadata
  app_driver$click(NS(app_driver$active_module_ns(), "metadata_button"))
  testthat::expect_true(app_driver$is_visible(sprintf("%s table", app_driver$active_module_element("metadata_table"))))

  app_driver$stop()
})
