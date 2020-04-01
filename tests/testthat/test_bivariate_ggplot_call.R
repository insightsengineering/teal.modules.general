context("bivariate_ggplot_call")

test_that("bivariate_ggplot_call with numerics", {
  expect_match(
    bivariate_ggplot_call("numeric", "numeric") %>% deparse(width.cutoff = 300),
    "geom_point\\(alpha = \\.alpha\\)"
  )
  expect_match(
    bivariate_ggplot_call("numeric", "factor") %>% deparse(width.cutoff = 300),
    "geom_boxplot()"
  )
  expect_match(
    bivariate_ggplot_call("numeric", "factor") %>% deparse(width.cutoff = 300),
    "coord_flip()"
  )
  expect_match(
    bivariate_ggplot_call("factor", "numeric") %>% deparse(width.cutoff = 300),
    "geom_boxplot()"
  )
})

test_that("bivariate_ggplot_call with factor, char, logical", {
  expect_match(
    bivariate_ggplot_call("factor", "factor") %>% deparse(width.cutoff = 300),
    "geom_mosaic"
  )
  expect_match(
    bivariate_ggplot_call("logical", "factor") %>% deparse(width.cutoff = 300),
    "geom_mosaic"
  )
  expect_match(
    bivariate_ggplot_call("character", "factor") %>% deparse(width.cutoff = 300),
    "geom_mosaic"
  )
  expect_match(
    bivariate_ggplot_call("logical", "character") %>% deparse(width.cutoff = 300),
    "geom_mosaic"
  )
  expect_match(
    bivariate_ggplot_call("character", "logical") %>% deparse(width.cutoff = 300),
    "geom_mosaic"
  )
  expect_match(
    bivariate_ggplot_call("logical", "logical") %>% deparse(width.cutoff = 300),
    "geom_mosaic"
  )
  expect_match(
    bivariate_ggplot_call("character", "character") %>% deparse(width.cutoff = 300),
    "geom\\_mosaic"
  )
})

test_that("bivariate_ggplot_call with single data numeric", {
  expect_match(
    bivariate_ggplot_call("numeric", "NULL") %>% deparse(width.cutoff = 300),
    "geom\\_histogram"
  )
  expect_match(
    bivariate_ggplot_call("numeric", "NULL", TRUE) %>% deparse(width.cutoff = 300),
    "Frequency"
  )
  expect_match(
    bivariate_ggplot_call("numeric", "NULL", FALSE) %>% deparse(width.cutoff = 300),
    "Density"
  )

  expect_match(
    bivariate_ggplot_call("NULL", "numeric") %>% deparse(width.cutoff = 300),
    "geom\\_histogram"
  )
  expect_match(
    bivariate_ggplot_call("NULL", "numeric", TRUE) %>% deparse(width.cutoff = 300),
    "Frequency"
  )
  expect_match(
    bivariate_ggplot_call("NULL", "numeric", FALSE) %>% deparse(width.cutoff = 300),
    "Density"
  )
})
test_that("bivariate_ggplot_call with single data factor", {
  expect_match(
    bivariate_ggplot_call("factor", "NULL") %>% deparse(width.cutoff = 300),
    "geom\\_bar"
  )
  expect_match(
    bivariate_ggplot_call("factor", "NULL", FALSE) %>% deparse(width.cutoff = 300),
    "Proportion"
  )
  expect_match(
    bivariate_ggplot_call("NULL", "factor") %>% deparse(width.cutoff = 300),
    "geom\\_bar"
  )
  expect_match(
    bivariate_ggplot_call("NULL", "factor", FALSE) %>% deparse(width.cutoff = 300),
    "Proportion"
  )
})
