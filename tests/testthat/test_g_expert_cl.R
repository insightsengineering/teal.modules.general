context("expert_ggplot_call")

test_that("expert_ggplot_call can work with all combinations", {
  expect_equal(
    expert_ggplot_call(NULL, NULL, NULL, FALSE),
    NULL
  )

  expect_equal(
    expert_ggplot_call("ASL", NULL, NULL, FALSE) %>% deparse(),
    "aes(colour = ASL)"
  )

  expect_equal(
    expert_ggplot_call("ASL", "ASL", NULL, FALSE) %>% deparse(),
    "aes(colour = ASL, fill = ASL)"
  )
  expect_equal(
    expert_ggplot_call("ASL", "ASL", "ASL", FALSE) %>% deparse(),
    "aes(colour = ASL, fill = ASL)"
  )
  expect_equal(
    expert_ggplot_call("ASL", "ASL", NULL, TRUE) %>% deparse(),
    "aes(colour = ASL, fill = ASL)"
  )

  expect_equal(
    expert_ggplot_call("ASL", "ASL", "ASL", TRUE) %>% deparse(),
    "aes(colour = ASL, fill = ASL, size = ASL)"
  )
  expect_equal(
    expert_ggplot_call("ASL", NULL, "ASL", TRUE) %>% deparse(),
    "aes(colour = ASL, size = ASL)"
  )
})
test_that("expert_ggplot_call can work with all combinations without colour_var", {
  expect_equal(
    expert_ggplot_call(NULL, NULL, NULL, FALSE),
    NULL
  )

  expect_equal(
    expert_ggplot_call(NULL, "ASL", NULL, FALSE) %>% deparse(),
    "aes(fill = ASL)"
  )
  expect_equal(
    expert_ggplot_call(NULL, "ASL", "ASL", FALSE) %>% deparse(),
    "aes(fill = ASL)"
  )
  expect_equal(
    expert_ggplot_call(NULL, "ASL", NULL, TRUE) %>% deparse(),
    "aes(fill = ASL)"
  )
  expect_equal(
    expert_ggplot_call(NULL, "ASL", "ASL", TRUE) %>% deparse(),
    "aes(fill = ASL, size = ASL)"
  )
})

test_that("expert_ggplot_call can work size_var", {
  expect_equal(
    expert_ggplot_call(NULL, NULL, "ASL", TRUE) %>% deparse(),
    "aes(size = ASL)"
  )
  expect_equal(
    expert_ggplot_call(NULL, NULL, NULL, TRUE),
    NULL
  )
  expect_equal(
    expert_ggplot_call(NULL, NULL, "ASL", FALSE),
    NULL
  )
})
