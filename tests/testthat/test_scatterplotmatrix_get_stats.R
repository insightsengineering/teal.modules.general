test_that("get_scatterplotmatrix_stats() x-y numeric", {
  set.seed(1)
  x <-  runif(25, 0, 1)
  y <-  runif(25, 0, 1)
  x_na <- x
  x_na[c(3, 10, 18)] <-  NA

  corr <- get_scatterplotmatrix_stats(x_na, y, .f = cor.test, .f_args = list(method = "pearson"))
  expect_true(is.character(corr))

  corr <- get_scatterplotmatrix_stats(x_na, y, .f = cor.test, .f_args = list(method = "pearson", na.action = na.fail))
  expect_true(corr == "NA")

  corr <- get_scatterplotmatrix_stats(x, y, .f = cor.test, .f_args = list(method = "pearson", na.action = na.fail))
  expect_true(corr != "NA")

})

test_that("get_scatterplotmatrix_stats() x-y character", {
  x <- LETTERS[runif(25, 0, 10)]
  y <- LETTERS[runif(25, 0, 10)]

  x_na <-  x
  x_na[c(3, 10, 18)] <-  NA

  corr <-  get_scatterplotmatrix_stats(x, y, .f = cor.test, .f_args = list(method = "pearson"))
  expect_true(startsWith(corr, "cor"))
  corr <-  get_scatterplotmatrix_stats(x, y, .f = cor.test, .f_args = list(method = "kendall"))
  expect_true(startsWith(corr, "tau"))
  corr <-  get_scatterplotmatrix_stats(x, y, .f = cor.test, .f_args = list(method = "spearman"))
  expect_true(startsWith(corr, "rho"))

})
