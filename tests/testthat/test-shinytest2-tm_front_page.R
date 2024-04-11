test_that("e2e: tm_front_page initializes without errors", {
  skip_if_too_deep(9)
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

  app$expect_no_shiny_error()

  # header text
  app$expect_html(selector = "#front_page_headers")

  # tables
  app$expect_screenshot(selector = app$active_module_element("table_1"))
  testthat::expect_match(app$get_active_module_output("table_1"), "MTCARS")

  app$expect_screenshot(selector = app$active_module_element("table_2"))
  testthat::expect_match(app$get_active_module_output("table_2"), "IRIS")


  # additional tags
  app$expect_html("#front_page_custom_html")

  # show metadata
  app$click(NS(app$active_module_ns(), "metadata_button"))
  app$expect_screenshot(selector = app$active_module_element("metadata_table"))

  app$stop()
})
