testthat::test_that("assert_single_selection succeeds if all elements have multiple choices disabled", {
  # Suppress logger messages
  local_logger_threshold(logger::FATAL)

  mock_spec <- data_extract_spec(
    dataname = "MOCK_DATASET",
    select = teal.transform::select_spec(choices = c("A", "B"), multiple = FALSE)
  )

  testthat::expect_s3_class(
    tm_g_bivariate(
      "a label",
      mock_spec,
      mock_spec
    ),
    "teal_module"
  )

  testthat::expect_s3_class(
    tm_g_bivariate(
      "a label",
      list(mock_spec),
      mock_spec
    ),
    "teal_module"
  )

  testthat::expect_s3_class(
    tm_g_bivariate(
      "a label",
      list(mock_spec, mock_spec),
      mock_spec
    ),
    "teal_module"
  )
})

testthat::test_that("assert_single_selection fails when multiple selection is selected in any of the specs", {
  # Suppress logger messages
  local_logger_threshold(logger::FATAL)

  mock_spec <- data_extract_spec(
    dataname = "MOCK_DATASET",
    select = teal.transform::select_spec(choices = c("A", "B"), multiple = TRUE)
  )
  mock_spec2 <- data_extract_spec(
    dataname = "MOCK_DATASET",
    select = teal.transform::select_spec(choices = c("A", "B"), multiple = FALSE)
  )

  x <- list(mock_spec)

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      x,
      x
    ),
    "'x' should not allow multiple selection"
  )

  testthat::expect_error(
    tm_g_bivariate(
      "a label",
      list(mock_spec2, mock_spec, data_extract_spec("EMPTY")),
      x
    ),
    "'x' should not allow multiple selection"
  )
})
