testthat::test_that("assert_single_selection succeeds if all elements have multiple choices disabled", {
  mock_spec <- data_extract_spec(
    dataname = "MOCK_DATASET",
    select = teal.transform::select_spec(choices = c("A", "B"), multiple = FALSE)
  )

  testthat::expect_true(assert_single_selection(list(mock_spec)))
  testthat::expect_true(assert_single_selection(list(mock_spec, mock_spec)))
})

testthat::test_that("assert_single_selection fails when multiple selection is selected in any of the specs", {
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
    assert_single_selection(x),
    "'x' should not allow multiple selection"
  )
  testthat::expect_error(
    assert_single_selection(list(mock_spec2, mock_spec), .var.name = "x"),
    "'x' should not allow multiple selection"
  )
})

testthat::test_that("assert_single_selection fails when multiple selection is enabled", {
  mock_spec <- data_extract_spec(
    dataname = "MOCK_DATASET",
    select = teal.transform::select_spec(choices = c("A", "B"), multiple = TRUE)
  )

  # Suppress logger messages
  logger::with_log_threshold(
    testthat::expect_error(
      tm_g_bivariate(
        "a label",
        mock_spec,
        mock_spec
      ),
      "'x' should not allow multiple selection"
    ),
    threshold = logger::FATAL,
    namespace = "teal.modules.general"
  )
})

testthat::test_that("assert_single_selection succeeds when multiple selection is disabled", {
  mock_spec <- data_extract_spec(
    dataname = "ADSL",
    select = teal.transform::select_spec(
      choices = c("USUBJID", "AGE"),
      multiple = FALSE
    )
  )

  # Suppress logger messages
  logger::with_log_threshold(
    testthat::expect_s3_class(
      tm_g_bivariate(
        "a label",
        mock_spec,
        mock_spec
      ),
      "teal_module"
    ),
    threshold = logger::FATAL,
    namespace = "teal.modules.general"
  )
})
