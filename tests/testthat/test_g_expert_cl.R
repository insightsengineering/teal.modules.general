test_that("coloring_ggplot_call can work with all combinations", {
  expect_equal(
    coloring_ggplot_call(character(0), character(0), character(0), FALSE),
    NULL
  )

  expect_equal(
    coloring_ggplot_call("ADSL", character(0), character(0), FALSE) %>% deparse(),
    "aes(colour = ADSL)"
  )

  expect_equal(
    coloring_ggplot_call("ADSL", "ADSL", character(0), FALSE) %>% deparse(),
    "aes(colour = ADSL, fill = ADSL)"
  )
  expect_equal(
    coloring_ggplot_call("ADSL", "ADSL", "ADSL", FALSE) %>% deparse(),
    "aes(colour = ADSL, fill = ADSL)"
  )
  expect_equal(
    coloring_ggplot_call("ADSL", "ADSL", character(0), TRUE) %>% deparse(),
    "aes(colour = ADSL, fill = ADSL)"
  )

  expect_equal(
    coloring_ggplot_call("ADSL", "ADSL", "ADSL", TRUE) %>% deparse(),
    "aes(colour = ADSL, fill = ADSL, size = ADSL)"
  )
  expect_equal(
    coloring_ggplot_call("ADSL", character(0), "ADSL", TRUE) %>% deparse(),
    "aes(colour = ADSL, size = ADSL)"
  )
})
test_that("coloring_ggplot_call can work with all combinations without colour_var", {
  expect_equal(
    coloring_ggplot_call(character(0), character(0), character(0), FALSE),
    NULL
  )

  expect_equal(
    coloring_ggplot_call(character(0), "ADSL", character(0), FALSE) %>% deparse(),
    "aes(fill = ADSL)"
  )
  expect_equal(
    coloring_ggplot_call(character(0), "ADSL", "ADSL", FALSE) %>% deparse(),
    "aes(fill = ADSL)"
  )
  expect_equal(
    coloring_ggplot_call(character(0), "ADSL", character(0), TRUE) %>% deparse(),
    "aes(fill = ADSL)"
  )
  expect_equal(
    coloring_ggplot_call(character(0), "ADSL", "ADSL", TRUE) %>% deparse(),
    "aes(color = ADSL, fill = ADSL, size = ADSL)"
  )
})

test_that("coloring_ggplot_call can work size_var", {
  expect_equal(
    coloring_ggplot_call(character(0), character(0), "ADSL", TRUE) %>% deparse(),
    "aes(size = ADSL)"
  )
  expect_equal(
    coloring_ggplot_call(character(0), character(0), character(0), TRUE),
    NULL
  )
  expect_equal(
    coloring_ggplot_call(character(0), character(0), "ADSL", FALSE),
    NULL
  )
})
