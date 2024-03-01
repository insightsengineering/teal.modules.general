testthat::test_that("tm_g_bivariate creates a `teal_module` object", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_s3_class(
    tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE),
      row_facet = mock_data_extract_spec(select_multiple = FALSE),
      col_facet = mock_data_extract_spec(select_multiple = FALSE),
      facet = TRUE,
      color_setting = TRUE,
      use_density = TRUE,
      free_x_scales = TRUE,
      free_y_scales = TRUE,
      plot_height = c(400, 100, 600),
      plot_width = c(600, 100, 600),
      rotate_xaxis_labels = TRUE,
      swap_axes = TRUE
    ),
    "teal_module"
  )
})

testthat::test_that("tm_g_bivariate creates a `teal_module` object with default options", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_s3_class(
    tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE)
    ),
    "teal_module"
  )
})

testthat::test_that("tm_g_bivariate creates a `teal_module` object with multiple data extract specs", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_s3_class(
    tm_g_bivariate(
      "a label",
      list(mock_data_extract_spec(select_multiple = FALSE), mock_data_extract_spec(select_multiple = FALSE)),
      list(mock_data_extract_spec(select_multiple = FALSE), mock_data_extract_spec(select_multiple = FALSE)),
      plot_height = c(400, 100, 600),
      plot_width = c(600, 100, 600)
    ),
    "teal_module"
  )
})

testthat::test_that("tm_g_bivariate creates a module with datanames taken from data extracts", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  mod <- tm_g_bivariate(
    "a label",
    list(
      mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
      mock_data_extract_spec(dataname = "B", select_multiple = FALSE)
    ),
    mock_data_extract_spec(dataname = "C", select_multiple = FALSE)
  )

  expect_setequal(
    mod$datanames,
    c("A", "B", "C")
  )
})

# Test `x` and `y` arguments with invalid data_extract_spec

testthat::test_that("tm_g_bivariate fails when `x` contains a spec with multiple selection", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      list(
        mock_data_extract_spec(select_multiple = TRUE)
      ),
      list()
    ),
    "'x' should not allow multiple selection"
  )
})

testthat::test_that("tm_g_bivariate fails when `x` contains multiple spec with (at least one ) multiple selection", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      list(
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = TRUE)
      ),
      list(mock_data_extract_spec(select_multiple = FALSE))
    ),
    "'x' should not allow multiple selection"
  )
})

testthat::test_that("tm_g_bivariate fails when `x` contains multiple spec with (at least one ) multiple selection", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      list(
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = TRUE)
      )
    ),
    "'y' should not allow multiple selection"
  )
})

testthat::test_that("tm_g_bivariate fails when `y` contains multiple spec with (at least one ) multiple selection", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      list(
        mock_data_extract_spec(select_multiple = FALSE),
        mock_data_extract_spec(select_multiple = TRUE)
      )
    ),
    "'y' should not allow multiple selection"
  )
})

# Test `plot_height` and `plot_width` arguments

testthat::test_that("tm_g_bivariate fails when `plot_height` is not valid", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE),
      plot_height = c(100, 10, 20)
    ),
    "Assertion on 'plot_height' failed: Element 1 is not <= 20"
  )
})

testthat::test_that("tm_g_bivariate fails when `plot_height` is not valid", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE),
      plot_height = c(1, 10, 20)
    ),
    "Assertion on 'plot_height' failed: Element 1 is not >= 10"
  )
})

testthat::test_that("tm_g_bivariate fails when `plot_height` is not valid", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE),
      plot_height = 100
    ),
    "Assertion on 'plot_height' failed: Must have length 3, but has length 1"
  )
})

testthat::test_that("tm_g_bivariate fails when `plot_width` is not valid", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE),
      plot_width = c(100, 10, 20)
    ),
    "Assertion on 'plot_width' failed: Element 1 is not <= 20"
  )
})

testthat::test_that("tm_g_bivariate fails when `plot_width` is not valid", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE),
      plot_width = c(1, 10, 20)
    ),
    "Assertion on 'plot_width' failed: Element 1 is not >= 10"
  )
})

testthat::test_that("tm_g_bivariate fails when `plot_width` is not valid", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE),
      plot_width = 100
    ),
    "Assertion on 'plot_width' failed: Must have length 3, but has length 1"
  )
})

# Test `color_settings` argument

testthat::test_that("tm_g_bivariate fails when `color_setting` is FALSE and `color` is supplied", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE),
      color_setting = FALSE,
      color = mock_data_extract_spec()
    ),
    "'color_settings' argument needs to be set to TRUE if 'color', 'fill', and/or 'size' is/are supplied."
  )
})

testthat::test_that("tm_g_bivariate fails when `color_setting` is FALSE and `size` is supplied", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE),
      color_setting = FALSE,
      size = mock_data_extract_spec()
    ),
    "'color_settings' argument needs to be set to TRUE if 'color', 'fill', and/or 'size' is/are supplied."
  )
})

testthat::test_that("tm_g_bivariate fails when `color_setting` is FALSE and `fill` is supplied", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      mock_data_extract_spec(select_multiple = FALSE),
      mock_data_extract_spec(select_multiple = FALSE),
      color_setting = FALSE,
      fill = mock_data_extract_spec()
    ),
    "'color_settings' argument needs to be set to TRUE if 'color', 'fill', and/or 'size' is/are supplied."
  )
})

testthat::test_that("tm_g_bivariate determines `color`, `size` and `fill` when `color_setting` is TRUE", {
  local_logger_threshold(logger::FATAL) # Suppress logger messages

  mod <- tm_g_bivariate(
    "a label",
    mock_data_extract_spec(select_multiple = FALSE),
    mock_data_extract_spec(select_multiple = FALSE),
    color_setting = TRUE
  )

  testthat::expect_contains(
    vapply(
      unlist(mod$ui_args[c("color", "size", "fill")], recursive = FALSE),
      class,
      character(1)
    ),
    "data_extract_spec"
  )
})
