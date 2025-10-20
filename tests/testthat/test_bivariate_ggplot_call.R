testthat::test_that("bivariate_ggplot_call with numerics", {
  testthat::expect_match(
    bivariate_ggplot_call("numeric", "numeric", alpha = quote(.alpha), size = quote(.size)) %>%
      deparse(width.cutoff = 300),
    "geom_point\\(alpha = \\.alpha\\, size = \\.size\\, pch = 21)"
  )
  testthat::expect_match(
    bivariate_ggplot_call("numeric", "factor") %>% deparse(width.cutoff = 300),
    "geom_boxplot()"
  )
  testthat::expect_match(
    bivariate_ggplot_call("numeric", "factor") %>% deparse(width.cutoff = 300),
    "geom_boxplot()"
  )
  testthat::expect_match(
    bivariate_ggplot_call("factor", "numeric") %>% deparse(width.cutoff = 300),
    "geom_boxplot()"
  )
})

testthat::test_that("bivariate_ggplot_call with factor, char, logical", {
  error_message <- "Classes for 'x' and 'y' are currently not supported."
  testthat::expect_error(
    bivariate_ggplot_call("factor", "factor") %>% deparse(width.cutoff = 300),
    error_message
  )
  testthat::expect_error(
    bivariate_ggplot_call("logical", "factor") %>% deparse(width.cutoff = 300),
    error_message
    )
  testthat::expect_error(
    bivariate_ggplot_call("character", "factor") %>% deparse(width.cutoff = 300),
    error_message
  )
  testthat::expect_error(
    bivariate_ggplot_call("logical", "character") %>% deparse(width.cutoff = 300),
    error_message
  )
  testthat::expect_error(
    bivariate_ggplot_call("character", "logical") %>% deparse(width.cutoff = 300),
    error_message
  )
  testthat::expect_error(
    bivariate_ggplot_call("logical", "logical") %>% deparse(width.cutoff = 300),
    error_message
  )
  testthat::expect_error(
    bivariate_ggplot_call("character", "character") %>% deparse(width.cutoff = 300),
    error_message
  )
})

testthat::test_that("bivariate_ggplot_call with single data numeric", {
  testthat::expect_match(
    bivariate_ggplot_call("numeric", "NULL") %>% deparse(width.cutoff = 300),
    "geom\\_histogram"
  )
  testthat::expect_match(
    bivariate_ggplot_call("numeric", "NULL", TRUE) %>% deparse(width.cutoff = 300),
    "Frequency"
  )
  testthat::expect_match(
    bivariate_ggplot_call("numeric", "NULL", FALSE) %>% deparse(width.cutoff = 300),
    "Density"
  )

  testthat::expect_match(
    bivariate_ggplot_call("NULL", "numeric") %>% deparse(width.cutoff = 300),
    "geom\\_histogram"
  )
  testthat::expect_match(
    bivariate_ggplot_call("NULL", "numeric", TRUE) %>% deparse(width.cutoff = 300),
    "Frequency"
  )
  testthat::expect_match(
    bivariate_ggplot_call("NULL", "numeric", FALSE) %>% deparse(width.cutoff = 300),
    "Density"
  )
})

testthat::test_that("bivariate_ggplot_call with single data factor", {
  testthat::expect_match(
    bivariate_ggplot_call("factor", "NULL") %>% deparse(width.cutoff = 300),
    "geom\\_bar"
  )
  testthat::expect_match(
    bivariate_ggplot_call("factor", "NULL", FALSE) %>% deparse(width.cutoff = 300),
    "Fraction"
  )
  testthat::expect_match(
    bivariate_ggplot_call("NULL", "factor") %>% deparse(width.cutoff = 300),
    "geom\\_bar"
  )
  testthat::expect_match(
    bivariate_ggplot_call("NULL", "factor", FALSE) %>% deparse(width.cutoff = 300),
    "Fraction"
  )
})
