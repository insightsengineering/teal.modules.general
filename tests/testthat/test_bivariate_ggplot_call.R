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

testthat::describe("bivariate_ggplot_call with arguments:", {
  possible_classes <- c("factor", "logical", "character")
  comb <- expand.grid(a = possible_classes, b = possible_classes, stringsAsFactors = FALSE)
  apply(
    comb,
    1,
    function(x) {
      it(sprintf("%s and %s", x[[1]], x[[2]]), {
        testthat::expect_match(
          deparse(
            bivariate_ggplot_call(x[[1]], x[[2]], data_name = "ANL", x = "x", y = "y"),
            width.cutoff = 300
          ),
          "mosaic_data <- ",
          all = FALSE
        )
      })
    }
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
