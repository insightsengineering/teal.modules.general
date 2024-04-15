testthat::test_that("coloring_ggplot_call can work with all combinations", {
  testthat::expect_equal(
    coloring_ggplot_call(character(0), character(0), character(0), FALSE),
    NULL
  )

  testthat::expect_equal(
    coloring_ggplot_call("ADSL", character(0), character(0), FALSE) %>% deparse(),
    "ggplot2::aes(colour = ADSL)"
  )

  testthat::expect_equal(
    coloring_ggplot_call("ADSL", "ADSL", character(0), FALSE) %>% deparse(),
    "ggplot2::aes(colour = ADSL, fill = ADSL)"
  )
  testthat::expect_equal(
    coloring_ggplot_call("ADSL", "ADSL", "ADSL", FALSE) %>% deparse(),
    "ggplot2::aes(colour = ADSL, fill = ADSL)"
  )
  testthat::expect_equal(
    coloring_ggplot_call("ADSL", "ADSL", character(0), TRUE) %>% deparse(),
    "ggplot2::aes(colour = ADSL, fill = ADSL)"
  )

  testthat::expect_equal(
    coloring_ggplot_call("ADSL", "ADSL", "ADSL", TRUE) %>% deparse(),
    "ggplot2::aes(colour = ADSL, fill = ADSL, size = ADSL)"
  )
  testthat::expect_equal(
    coloring_ggplot_call("ADSL", character(0), "ADSL", TRUE) %>% deparse(),
    "ggplot2::aes(colour = ADSL, size = ADSL)"
  )
})
testthat::test_that("coloring_ggplot_call can work with all combinations without colour_var", {
  testthat::expect_equal(
    coloring_ggplot_call(character(0), character(0), character(0), FALSE),
    NULL
  )

  testthat::expect_equal(
    coloring_ggplot_call(character(0), "ADSL", character(0), FALSE) %>% deparse(),
    "ggplot2::aes(fill = ADSL)"
  )
  testthat::expect_equal(
    coloring_ggplot_call(character(0), "ADSL", "ADSL", FALSE) %>% deparse(),
    "ggplot2::aes(fill = ADSL)"
  )
  testthat::expect_equal(
    coloring_ggplot_call(character(0), "ADSL", character(0), TRUE) %>% deparse(),
    "ggplot2::aes(fill = ADSL)"
  )
  testthat::expect_equal(
    coloring_ggplot_call(character(0), "ADSL", "ADSL", TRUE) %>% deparse(),
    "ggplot2::aes(colour = ADSL, fill = ADSL, size = ADSL)"
  )
})

testthat::test_that("coloring_ggplot_call can work size_var", {
  testthat::expect_equal(
    coloring_ggplot_call(character(0), character(0), "ADSL", TRUE) %>% deparse(),
    "ggplot2::aes(size = ADSL)"
  )
  testthat::expect_equal(
    coloring_ggplot_call(character(0), character(0), character(0), TRUE),
    NULL
  )
  testthat::expect_equal(
    coloring_ggplot_call(character(0), character(0), "ADSL", FALSE),
    NULL
  )
})
