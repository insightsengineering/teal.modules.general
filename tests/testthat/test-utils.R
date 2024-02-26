test_that("check_slider returns logical valid arguments", {
  expect_true(check_slider(c(0, 0, 2)))
  expect_true(check_slider(c(1, 0, 2)))
  expect_true(check_slider(c(2, 0, 2)))
})

test_that("check_slider returns character on vector with wrong size", {
  regex <- "Must be a numeric vector of length 3 with `c\\(value, min, max\\)`"
  expect_match(check_slider(1), regex)
  expect_match(check_slider(c(2, 1)), regex)
  expect_match(check_slider(c(2, 1, 3, 4)), regex)
  expect_match(check_slider(c(2, 1, 3, 4, 5)), regex)
})

test_that("check_slider returns character on wrong c(`value`, `min`, `max`) structure", {
  regex <- "Must be a numeric vector of length 3 with `c\\(value, min, max\\)`"
  expect_match(check_slider(c(2, 1, 1)), regex)
  expect_match(check_slider(c(2, 3, 1)), regex)
  expect_match(check_slider(c(0, 1, 3)), regex)
  expect_match(check_slider(c(5, 1, 3)), regex)
})
