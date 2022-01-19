testthat::test_that("remove_outliers_from does not remove outliers when outlier_definition is zero", {
  var <- c(-100, 5, 10, 10, 20, 30, 40, 40, 40.5, 10000)
  testthat::expect_equal(remove_outliers_from(var, outlier_definition = 0), var)
})

testthat::test_that("remove_outliers_from removes outliers when outlier definition is non-zero", {
  var <- c(-100, 5, 10, 10, 20, 30, 40, 40, 40, 40.5, 10000)
  testthat::expect_equal(remove_outliers_from(var, outlier_definition = 1), c(5, 10, 10, 20, 30, 40, 40, 40, 40.5))
})

testthat::test_that("remove_outliers_from keeps missing values unchanged", {
  var <- c(-100, 5, NA, 10, 10, 20, 30, 40, 40, 40, 40.5, 10000)
  testthat::expect_equal(remove_outliers_from(var, outlier_definition = 1), c(5, NA, 10, 10, 20, 30, 40, 40, 40, 40.5))
})
