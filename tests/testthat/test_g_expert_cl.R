context("expert_ggplot_call")

test_that("expert_ggplot_call can work with all combinations", {
  expect_equal(
    expert_ggplot_call(NULL, NULL, NULL, FALSE),
    NULL
  )

  expect_equal(
    expert_ggplot_call("ADSL", NULL, NULL, FALSE) %>% deparse(),
    "aes(colour = ADSL)"
  )

  expect_equal(
    expert_ggplot_call("ADSL", "ADSL", NULL, FALSE) %>% deparse(),
    "aes(colour = ADSL, fill = ADSL)"
  )
  expect_equal(
    expert_ggplot_call("ADSL", "ADSL", "ADSL", FALSE) %>% deparse(),
    "aes(colour = ADSL, fill = ADSL)"
  )
  expect_equal(
    expert_ggplot_call("ADSL", "ADSL", NULL, TRUE) %>% deparse(),
    "aes(colour = ADSL, fill = ADSL)"
  )

  expect_equal(
    expert_ggplot_call("ADSL", "ADSL", "ADSL", TRUE) %>% deparse(),
    "aes(colour = ADSL, fill = ADSL, size = ADSL)"
  )
  expect_equal(
    expert_ggplot_call("ADSL", NULL, "ADSL", TRUE) %>% deparse(),
    "aes(colour = ADSL, size = ADSL)"
  )
})
test_that("expert_ggplot_call can work with all combinations without colour_var", {
  expect_equal(
    expert_ggplot_call(NULL, NULL, NULL, FALSE),
    NULL
  )

  expect_equal(
    expert_ggplot_call(NULL, "ADSL", NULL, FALSE) %>% deparse(),
    "aes(fill = ADSL)"
  )
  expect_equal(
    expert_ggplot_call(NULL, "ADSL", "ADSL", FALSE) %>% deparse(),
    "aes(fill = ADSL)"
  )
  expect_equal(
    expert_ggplot_call(NULL, "ADSL", NULL, TRUE) %>% deparse(),
    "aes(fill = ADSL)"
  )
  expect_equal(
    expert_ggplot_call(NULL, "ADSL", "ADSL", TRUE) %>% deparse(),
    "aes(fill = ADSL, size = ADSL)"
  )
})

test_that("expert_ggplot_call can work size_var", {
  expect_equal(
    expert_ggplot_call(NULL, NULL, "ADSL", TRUE) %>% deparse(),
    "aes(size = ADSL)"
  )
  expect_equal(
    expert_ggplot_call(NULL, NULL, NULL, TRUE),
    NULL
  )
  expect_equal(
    expert_ggplot_call(NULL, NULL, "ADSL", FALSE),
    NULL
  )
})
