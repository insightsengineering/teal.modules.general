
testthat::test_that("ggplot_args validation", {
  testthat::expect_error(ggplot_args(labs = NULL, theme = NULL), "labs has to be a list")
  testthat::expect_silent(ggplot_args(labs = list(), theme = list()))
  testthat::expect_silent(ggplot_args(labs = list()))
  testthat::expect_silent(ggplot_args(labs = list(title = "SOME TITLE")))
  testthat::expect_error(ggplot_args(labs = list(title_wrong = "SOME TITLE")), "Please validate labs arguments names")
})

testthat::test_that("ggplot_args class", {
  testthat::expect_true(inherits(ggplot_args(labs = list(), theme = list()), c("list", "ggplot_args")))
})


labs_base <- list(
  title = "TEST",
  caption = "SOMTHING",
  subtitle = "aa"
)

ggplot2_args_single <- ggplot_args(labs = labs_base)

ggplot2_args_multi <- list(
  default = ggplot_args(labs = labs_base),
  plot1 = ggplot_args(labs = labs_base)
)

ggplot2_args_multi_wrong <- list(
  default_WRONG = ggplot_args(labs = labs_base),
  plot1 = ggplot_args(labs = labs_base)
)

testthat::test_that("validate_ggplot2_args", {
  testthat::expect_silent(validate_ggplot2_args(structure(list(), class = "ggplot_args")))
  testthat::expect_silent(validate_ggplot2_args(ggplot2_args_single))
  testthat::expect_silent(validate_ggplot2_args(ggplot2_args_multi, c("plot1")))
  testthat::expect_silent(validate_ggplot2_args(list()))

  error_msg <- paste0(
    "Error: is_ggplot2_args || (is_nested_ggplot2_args && (all(names(ggplot2_args)",
    '%in% c("default", plot_names)))) is not TRUE\n'
  )

  testthat::expect_error(validate_ggplot2_args(ggplot2_args_multi), error_msg)
  testthat::expect_error(validate_ggplot2_args(ggplot2_args_multi_wrong), error_msg)
  testthat::expect_error(validate_ggplot2_args(ggplot2_args_multi_wrong, "plot1"), error_msg)
})

testthat::test_that("get_expr_ggplot2_args, miniaml requirement is to provide ggplot2_args", {
  testthat::expect_identical(get_expr_ggplot2_args(ggplot2_args = list()), quote(gg))
  testthat::expect_identical(
    get_expr_ggplot2_args(ggplot2_args = ggplot2_args_single),
    quote(gg + labs(title = "TEST", caption = "SOMTHING", subtitle = "aa"))
  )
  testthat::expect_identical(
    get_expr_ggplot2_args(ggplot2_args = ggplot2_args_multi),
    quote(gg + labs(title = "TEST", caption = "SOMTHING", subtitle = "aa"))
  )
})

testthat::test_that("get_expr_ggplot2_args, validation and assumed that validate_ggplot2_args is already used", {
  testthat::expect_silent(get_expr_ggplot2_args(ggplot2_args = list()))
})

nest_ggplot2_args <- ggplot_args(labs = list(
  x = quote(paste0("Fitted values\nlm(", reg_form, ")")),
  y = "Residuals",
  title = "Residuals vs Fitted"
))

testthat::test_that("get_expr_ggplot2_args  ggtheme = NULL and all theme empty", {
  testthat::expect_identical(
    get_expr_ggplot2_args(
      plot_name = "STH",
      chunk_plot_name = as.name("g"),
      ggplot2_args = ggplot2_args_single,
      nest_ggplot2_args = nest_ggplot2_args,
      ggtheme = NULL
    ),
    quote(g + labs(
      title = "TEST", caption = "SOMTHING", subtitle = "aa",
      x = paste0("Fitted values\nlm(", reg_form, ")"), y = "Residuals"
    ))
  )

  testthat::expect_identical(
    get_expr_ggplot2_args(
      plot_name = "default",
      chunk_plot_name = as.name("g"),
      ggplot2_args = ggplot2_args_multi,
      nest_ggplot2_args = NULL,
      ggtheme = NULL
    ),
    quote(g + labs(title = "TEST", caption = "SOMTHING", subtitle = "aa"))
  )

  testthat::expect_identical(
    get_expr_ggplot2_args(
      plot_name = "WRONG_NAME",
      chunk_plot_name = as.name("g"),
      ggplot2_args = ggplot2_args_multi,
      nest_ggplot2_args = NULL,
      ggtheme = NULL
    ),
    quote(g + labs(title = "TEST", caption = "SOMTHING", subtitle = "aa"))
  )
})
