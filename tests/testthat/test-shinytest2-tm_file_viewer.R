nest_logo_url <- "https://raw.githubusercontent.com/insightsengineering/hex-stickers/main/PNG/nest.png"

app_driver_tm_file_viewer <- function() {
  init_teal_app_driver(
    teal::init(
      data = simple_teal_data(),
      modules = tm_file_viewer(
        label = "File Viewer Module",
        input_path = list(
          folder = system.file("sample_files", package = "teal.modules.general"),
          png = system.file("sample_files/sample_file.png", package = "teal.modules.general"),
          txt = system.file("sample_files/sample_file.txt", package = "teal.modules.general"),
          url = nest_logo_url
        )
      )
    ),
    timeout = 10000
  )
}

test_that("e2e - tm_file_viewer: Initializes without errors and shows files tree specified in input_path argument", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_file_viewer()

  app_driver$expect_no_shiny_error()
  # check valid path as no file is selected
  app_driver$expect_validation_error()
  # encoding are visible
  testthat::expect_true(app_driver$is_visible(selector = app_driver$namespaces(TRUE)$module("tree")))
  list_files <- app_driver$get_html_rvest(selector = app_driver$namespaces(TRUE)$module("tree")) %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  testthat::expect_setequal(
    list_files,
    c("folder", "png", "txt", "url")
  )

  testthat::expect_equal(app_driver$get_text(".teal-modules-tree .active"), "File Viewer Module")

  app_driver$stop()
})

test_that("e2e - tm_file_viewer: Shows selected image file", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_file_viewer()

  # Find and click the png file anchor by text content
  tree_selector <- app_driver$namespaces(TRUE)$module("tree")
  tree_html <- app_driver$get_html_rvest(selector = tree_selector)
  anchors <- rvest::html_nodes(tree_html, "a")
  anchor_texts <- rvest::html_text(anchors)
  png_idx <- which(anchor_texts == "png")
  png_id <- rvest::html_attr(anchors[png_idx], "id")
  app_driver$click(selector = paste0("[id='", png_id, "']"))
  testthat::expect_true(app_driver$is_visible(app_driver$namespaces(TRUE)$module("output img")))

  img_src <- app_driver$get_html_rvest(app_driver$namespaces(TRUE)$module("output")) %>%
    rvest::html_element("img") %>%
    rvest::html_attr("src")

  testthat::expect_match(img_src, "png$")

  app_driver$stop()
})

test_that("e2e - tm_file_viewer: Shows selected text file", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_file_viewer()

  # Find and click the txt file anchor by text content
  tree_selector <- app_driver$namespaces(TRUE)$module("tree")
  tree_html <- app_driver$get_html_rvest(selector = tree_selector)
  anchors <- rvest::html_nodes(tree_html, "a")
  anchor_texts <- rvest::html_text(anchors)
  txt_idx <- which(anchor_texts == "txt")
  txt_id <- rvest::html_attr(anchors[txt_idx], "id")
  app_driver$click(selector = paste0("[id='", txt_id, "']"))
  testthat::expect_true(app_driver$is_visible(app_driver$namespaces(TRUE)$module("output pre")))

  pre_text <- app_driver$get_html_rvest(app_driver$namespaces(TRUE)$module("output")) %>%
    rvest::html_element("pre") %>%
    rvest::html_text()

  testthat::expect_true(!is.na(pre_text))
  testthat::expect_true(length(pre_text) > 0)

  testthat::expect_match(
    attr(app_driver$get_active_module_input("tree")$txt, "ancestry"),
    "txt$"
  )

  app_driver$stop()
})

test_that("e2e - tm_file_viewer: Shows selected url", {
  skip_if_too_deep(5)
  app_driver <- app_driver_tm_file_viewer()

  # Find and click the url anchor by text content
  tree_selector <- app_driver$namespaces(TRUE)$module("tree")
  tree_html <- app_driver$get_html_rvest(selector = tree_selector)
  anchors <- rvest::html_nodes(tree_html, "a")
  anchor_texts <- rvest::html_text(anchors)
  url_idx <- which(anchor_texts == "url")
  url_id <- rvest::html_attr(anchors[url_idx], "id")
  app_driver$click(selector = paste0("[id='", url_id, "']"))
  testthat::expect_true(app_driver$is_visible(app_driver$namespaces(TRUE)$module("output img")))

  testthat::expect_equal(
    attr(app_driver$get_active_module_input("tree")$url, "ancestry"),
    nest_logo_url
  )

  app_driver$stop()
})
