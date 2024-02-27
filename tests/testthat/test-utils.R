testthat::test_that("assert_single_selection will pass if all elements have multiple choices disabled", {
  mock_spec <- list(select = list(multiple = FALSE))

  testthat::expect_true(assert_single_selection(list(mock_spec)))
  testthat::expect_true(assert_single_selection(list(mock_spec, mock_spec)))
})

testthat::test_that("assert_single_selection fails when multiple selection is selected in any of the specs", {
  mock_spec <- list(select = list(multiple = TRUE))
  mock_spec2 <- list(select = list(multiple = FALSE))

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
