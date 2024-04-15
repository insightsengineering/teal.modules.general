app_driver_tm_file_viewer <- function() {
  TealAppDriver$new(
    data = simple_teal_data(),
    modules = tm_file_viewer(
      label = "File Viewer Module",
      input_path = list(
        folder = system.file("sample_files", package = "teal.modules.general"),
        png = system.file("sample_files/sample_file.png", package = "teal.modules.general"),
        txt = system.file("sample_files/sample_file.txt", package = "teal.modules.general"),
        url = "https://fda.gov/files/drugs/published/Portable-Document-Format-Specifications.pdf"
      )
    ),
    timeout = 3000
  )
}

test_that("e2e: tm_file_viewer initializes without errors and shows files tree specified in input_path argument", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_file_viewer()

  app_driver$expect_no_shiny_error()

  # encoding are visible
  testthat::expect_true(app_driver$is_visible(selector = app_driver$active_module_element("tree")))
  list_files <- app_driver$get_html_rvest(selector = app_driver$active_module_element("tree")) %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  testthat::expect_setequal(list_files,
                         c("folder", "png", "txt", "url"))


  app_driver$stop()
})


test_that("e2e: tm_file_viewer shows selected image file", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_file_viewer()

  app_driver$click(selector = "[id= '4_anchor']")
  testthat::expect_true(app_driver$is_visible(selector = app_driver$active_module_element("output")))
})
test_that("e2e: tm_file_viewer shows selected text file", {})
