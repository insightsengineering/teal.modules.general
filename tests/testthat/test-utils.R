test_that("check_range_slider returns logical with valid default arguments", {
  expect_true(check_range_slider(c(0, 0, 2)))
  expect_true(check_range_slider(c(1, 0, 2)))
  expect_true(check_range_slider(c(2, 0, 2)))
})

test_that("check_range_slider returns logical with valid arguments looking for integers", {
  expect_true(check_range_slider(c(0L, 0L, 2L), test_fun = checkmate::test_integer))
  expect_true(check_range_slider(c(1L, 0L, 2L), test_fun = checkmate::test_integer))
  expect_true(check_range_slider(c(2L, 0L, 2L), test_fun = checkmate::test_integer))
})

test_that("check_range_slider returns logical with valid arguments looking for integerish", {
  expect_true(check_range_slider(c(0, 0, 2), test_fun = checkmate::test_integerish))
  expect_true(check_range_slider(c(1L, 0L, 2L), test_fun = checkmate::test_integerish))
  expect_true(check_range_slider(c(2, 0L, 2L), test_fun = checkmate::test_integerish))
  expect_true(check_range_slider(c(2, 0, 2L), test_fun = checkmate::test_integerish))
  expect_true(check_range_slider(c(2L, 0L, 2), test_fun = checkmate::test_integerish))
})

test_that("check_range_slider fails when it is looking for integerish and there is a double", {
  regex <- "Must be a numeric vector of length 3 with `c\\(value, min, max\\)`"

  expect_match(check_range_slider(c(0, 0, 2.2), test_fun = checkmate::test_integerish), regex)
  expect_match(check_range_slider(c(1, 0.1, 2.2), test_fun = checkmate::test_integerish), regex)
  expect_match(check_range_slider(c(1.1, 0, 2), test_fun = checkmate::test_integerish), regex)
})

test_that("check_range_slider fails when looking for strict integers", {
  regex <- "Must be a numeric vector of length 3 with `c\\(value, min, max\\)`"

  expect_match(check_range_slider(c(0, 0, 2), test_fun = checkmate::test_integer), regex)
  expect_match(check_range_slider(c(0L, 0L, 2), test_fun = checkmate::test_integer), regex)
  expect_match(check_range_slider(c(0L, 0, 2L), test_fun = checkmate::test_integer), regex)
  expect_match(check_range_slider(c(0, 0L, 2L), test_fun = checkmate::test_integer), regex)
  expect_match(check_range_slider(c(0L, 0, 2), test_fun = checkmate::test_integer), regex)
  expect_match(check_range_slider(c(0, 0L, 2), test_fun = checkmate::test_integer), regex)
  expect_match(check_range_slider(c(0, 0L, 2L), test_fun = checkmate::test_integer), regex)

  expect_match(check_range_slider(c(0.1, 0L, 2L), test_fun = checkmate::test_integer), regex)
})

test_that("check_range_slider returns character on vector with wrong size", {
  regex <- "Must be a numeric vector of length 3 with `c\\(value, min, max\\)`"
  expect_match(check_range_slider(1), regex)
  expect_match(check_range_slider(c(2, 1)), regex)
  expect_match(check_range_slider(c(2, 1, 3, 4)), regex)
  expect_match(check_range_slider(c(2, 1, 3, 4, 5)), regex)
})

test_that("check_range_slider returns character on wrong c(`value`, `min`, `max`) structure", {
  regex <- "Must be a numeric vector of length 3 with `c\\(value, min, max\\)`"
  expect_match(check_range_slider(c(2, 1, 1)), regex)
  expect_match(check_range_slider(c(2, 3, 1)), regex)
  expect_match(check_range_slider(c(0, 1, 3)), regex)
  expect_match(check_range_slider(c(5, 1, 3)), regex)
})
