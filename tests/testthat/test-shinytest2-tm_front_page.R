test_that("e2e: tm_front_page initializes without errors", {
  skip_if_too_deep(5)
  require(shinytest2)

  data <- simple_cdisc_data()
  data <- within(data, {
    attr(ADSL, "metadata") <- list("Author" = "NEST team", "data_source" = "synthetic data")
  })

  app <- TealAppDriver$new(
    data = data,
    modules = teal::modules(
      tm_front_page(
        header_text = c(
          "Important information" = "It can go here.",
          "Other information" = "Can go here."
        ),
        tables = list("MTCARS" = head(mtcars, 5), "IRIS" = head(iris, 5)),
        additional_tags = HTML("Additional HTML or shiny tags go here <br>"),
        footnotes = c("X" = "is the first footnote", "Y is the second footnote"),
        show_metadata = TRUE
      )
    ),
    timeout = 3000
  )

  app$wait_for_idle()
  app$expect_no_shiny_error()

  # header text
  testthat::expect_identical(
    rlang::hash(app$get_html("#front_page_headers")),
    "2b2ab743c68213927d0578b7e39f5873"
  )
  testthat::expect_match(app$get_html("#front_page_headers"), "Important information")

  # tables
  testthat::expect_identical(
    rlang::hash(app$get_active_module_output("table_1")),
    "5b29b8b4d8c0b0b23244c9b7cb5d94e8"
  )
  testthat::expect_match(app$get_active_module_output("table_1"), "MTCARS")

  testthat::expect_identical(
    rlang::hash(app$get_active_module_output("table_2")),
    "537b1c08d7559f0f6ee9def27a7b0ef1"
  )
  testthat::expect_match(app$get_active_module_output("table_2"), "IRIS")


  # additional tags
  testthat::expect_match(
    app$get_text("#front_page_custom_html"),
    "Additional HTML or shiny tags go here"
  )
  # show metadata
  app$click(NS(app$active_module_ns(), "metadata_button"))
  testthat::expect_identical(
    rlang::hash(app$get_html("#DataTables_Table_0_wrapper")),
    "ed8058e0f1823def26cd7aa07e667269"
  )
  testthat::expect_match(app$get_html("#DataTables_Table_0_wrapper"), "NEST team")

  app$stop()
})
