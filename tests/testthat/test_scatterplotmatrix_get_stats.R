testthat::test_that("get_scatterplotmatrix_stats() x-y numeric", {
  set.seed(1)
  x <- runif(25, 0, 1)
  y <- runif(25, 0, 1)
  x_na <- x
  x_na[c(3, 10, 18)] <- NA

  corr <- get_scatterplotmatrix_stats(x_na, y, .f = cor.test, .f_args = list(method = "pearson"))
  testthat::expect_true(is.character(corr))

  corr <- get_scatterplotmatrix_stats(x_na, y, .f = cor.test, .f_args = list(method = "pearson", na.action = na.fail))
  testthat::expect_true(corr == "NA")

  corr <- get_scatterplotmatrix_stats(x, y, .f = cor.test, .f_args = list(method = "pearson", na.action = na.fail))
  testthat::expect_true(corr != "NA")
})

testthat::test_that("get_scatterplotmatrix_stats() x-y character", {
  x <- LETTERS[runif(25, 0, 10)]
  y <- LETTERS[runif(25, 0, 10)]

  x_na <- x
  x_na[c(3, 10, 18)] <- NA

  corr <- get_scatterplotmatrix_stats(x, y, .f = cor.test, .f_args = list(method = "pearson"))
  testthat::expect_true(startsWith(corr, "cor"))
  corr <- get_scatterplotmatrix_stats(x, y, .f = cor.test, .f_args = list(method = "kendall"))
  testthat::expect_true(startsWith(corr, "tau"))
  corr <- get_scatterplotmatrix_stats(x, y, .f = cor.test, .f_args = list(method = "spearman"))
  testthat::expect_true(startsWith(corr, "rho"))
})
