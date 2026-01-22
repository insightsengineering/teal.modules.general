testthat::describe("add_facet_labels", {
  it("returns ggplotGrob when both labels are NULL", {
    p <- ggplot2::ggplot(mtcars) +
      ggplot2::aes(x = mpg, y = disp) +
      ggplot2::geom_point()

    result <- add_facet_labels(p, xfacet_label = NULL, yfacet_label = NULL)
    testthat::expect_s3_class(result, "gtable")
  })

  it("adds x facet label when provided", {
    p <- ggplot2::ggplot(mtcars) +
      ggplot2::aes(x = mpg, y = disp) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(gear ~ cyl)

    result <- add_facet_labels(p, xfacet_label = "cylinders", yfacet_label = NULL)
    testthat::expect_s3_class(result, "gTree")
  })

  it("adds y facet label when provided", {
    p <- ggplot2::ggplot(mtcars) +
      ggplot2::aes(x = mpg, y = disp) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(gear ~ cyl)

    result <- add_facet_labels(p, xfacet_label = NULL, yfacet_label = "gear")
    testthat::expect_s3_class(result, "gTree")
  })

  it("adds both x and y facet labels when both provided", {
    p <- ggplot2::ggplot(mtcars) +
      ggplot2::aes(x = mpg, y = disp) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(gear ~ cyl)

    result <- add_facet_labels(p, xfacet_label = "cylinders", yfacet_label = "gear")
    testthat::expect_s3_class(result, "gTree")
  })

  it("joins multiple x facet labels with &", {
    p <- ggplot2::ggplot(mtcars) +
      ggplot2::aes(x = mpg, y = disp) +
      ggplot2::geom_point() +
      ggplot2::facet_grid(gear ~ cyl)

    result <- add_facet_labels(p, xfacet_label = c("cylinders", "type"), yfacet_label = NULL)
    testthat::expect_s3_class(result, "gTree")
  })

  it("fails when p is not a ggplot object", {
    testthat::expect_error(
      add_facet_labels("not a ggplot", xfacet_label = "test"),
      "Assertion on 'p' failed"
    )
  })

  it("fails when xfacet_label is not character", {
    p <- ggplot2::ggplot(mtcars) +
      ggplot2::aes(x = mpg, y = disp) +
      ggplot2::geom_point()

    testthat::expect_error(
      add_facet_labels(p, xfacet_label = 123),
      "Assertion on 'xfacet_label' failed"
    )
  })

  it("fails when yfacet_label is not character", {
    p <- ggplot2::ggplot(mtcars) +
      ggplot2::aes(x = mpg, y = disp) +
      ggplot2::geom_point()

    testthat::expect_error(
      add_facet_labels(p, yfacet_label = 123),
      "Assertion on 'yfacet_label' failed"
    )
  })
})

