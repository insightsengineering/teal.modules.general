# `teal` module: Distribution analysis

Module is designed to explore the distribution of a single variable
within a given dataset. It offers several tools, such as histograms, Q-Q
plots, and various statistical tests to visually and statistically
analyze the variable's distribution.

## Usage

``` r
tm_g_distribution(
  label = "Distribution Module",
  dist_var = teal.picks::picks(teal.picks::datasets(), teal.picks::variables(is.numeric),
    teal.picks::values()),
  strata_var = NULL,
  group_var = NULL,
  freq = FALSE,
  ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
  ggplot2_args = teal.widgets::ggplot2_args(),
  bins = c(30L, 1L, 100L),
  plot_height = c(600, 200, 2000),
  plot_width = NULL,
  pre_output = NULL,
  post_output = NULL,
  transformators = list(),
  decorators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- dist_var:

  (`picks` or `list` of multiple `picks`) Variable(s) for which the
  distribution will be analyzed.

- strata_var:

  (`picks` or `list` of multiple `picks`) Categorical variable used to
  split the distribution analysis.

- group_var:

  (`picks` or `list` of multiple `picks`) Variable used for faceting
  plot into multiple panels.

- freq:

  (`logical`) optional, whether to display frequency (`TRUE`) or density
  (`FALSE`). Defaults to density (`FALSE`).

- ggtheme:

  (`character`) optional, `ggplot2` theme to be used by default.
  Defaults to `"gray"`.

- ggplot2_args:

  (`ggplot2_args`) optional, object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for all the plots or named list of `ggplot2_args`
  objects for plot-specific settings. The argument is merged with
  options variable `teal.ggplot2_args` and default module setup.

  List names should match the following:
  `c("default", "Histogram", "QQplot")`.

  For more details see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

- bins:

  (`integer(1)` or `integer(3)`) optional, specifies the number of bins
  for the histogram.

  - When the length of `bins` is one: The histogram bins will have a
    fixed size based on the `bins` provided.

  - When the length of `bins` is three: The histogram bins are
    dynamically adjusted based on vector of `value`, `min`, and `max`.
    Defaults to `c(30L, 1L, 100L)`.

- plot_height:

  (`numeric`) optional, specifies the plot height as a three-element
  vector of `value`, `min`, and `max` intended for use with a slider UI
  element.

- plot_width:

  (`numeric`) optional, specifies the plot width as a three-element
  vector of `value`, `min`, and `max` for a slider encoding the plot
  width.

- pre_output:

  (`shiny.tag`) optional,\
  with text placed before the output to put the output into context. For
  example a title.

- post_output:

  (`shiny.tag`) optional, with text placed after the output to put the
  output into context. For example the
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html)
  elements are useful.

- transformators:

  (`list` of `teal_transform_module`) that will be applied to transform
  module's data input. To learn more check
  [`vignette("transform-input-data", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).

- decorators:

  **\[experimental\]** (named `list` of lists of
  `teal_transform_module`) optional, decorator for tables or plots
  included in the module output reported. The decorators are applied to
  the respective output objects.

## Value

Object of class `teal_module` to be used in `teal` applications.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators::

- `histogram_plot` (`ggplot`)

- `qq_plot` (`ggplot`)

- `summary_table`
  ([`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html))

- `test_table`
  ([`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_distribution(
       ..., # arguments for module
       decorators = list(
         histogram_plot = teal_transform_module(...), # applied only to `histogram_plot` output
         qq_plot = teal_transform_module(...) # applied only to `qq_plot` output
         summary_table = teal_transform_module(...) # applied only to `summary_table` output
         test_table = teal_transform_module(...) # applied only to `test_table` output

       )
    )

For additional details and examples of decorators, refer to the vignette
[`vignette("decorate-module-output", package = "teal.modules.general")`](https://insightsengineering.github.io/teal.modules.general/articles/decorate-module-output.md).

To learn more please refer to the vignette
[`vignette("transform-module-output", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-module-output.html)
or the
[`teal::teal_transform_module()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)
documentation.

## Reporting

This module returns an object of class `teal_module`, that contains a
`server` function. Since the server function returns a `teal_report`
object, this makes this module reportable, which means that the
reporting functionality will be turned on automatically by the `teal`
framework.

For more information on reporting in `teal`, see the vignettes:

- [`vignette("reportable-shiny-application", package = "teal.reporter")`](https://insightsengineering.github.io/teal.reporter/latest-tag/articles/reportable-shiny-application.html)

- `vignette("adding-support-for-reporting-to-custom-modules", package = "teal")`

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6urSMtKJxVTWiSgC+ihBKaKj1KnnsFf4ZugC8A0G4-XxCInUjdKKkfRCVlaQwiRLJtaQ19IKa1v3L-luJWizDugkYqLQEANaiiIg396KLR0cponCkbwpg1Vq-zk4yWHxWcG411uDyeZxqUHo03YtQwEEE8BqBBBhw+VxesMQZ2ogjEaTBRzalP6bTatBMuhRqmYlh0Nls5TBokKEFYAEF0OxOgASQS0Uoi76MHSMNrNJRgZoAXSAA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dfihSKF04AA9YVBElUPCDYy5qAH1EqBsEsIijXQB3WlIACxV2TNxdECVdXQBBHwCAGWTdVIxMxERGRpalAF9FCAArIhU0gGs4VlEK3Nt8-jgTKGFSNIJ+WlECNLGJ6dngaHg5zLkAXTdodDaVYvZakNzdAF4X8NxnviERUXeul+wjETwgdTqpBgaQkGR2pEYDEEmmszwhIXhaS0LEBHVQtAIk1E3XxhLmaPRnygojgpHJYD6zQUYDk33BlMhcG4GFJRO62MRUHo-3YBCeYAAQgBZLAAaSwAEZmVVmfUAOKuFmsino7HUQSgmDrWhxPQfABi9WaAWcww52vZ6NECNyWJxHzxBL5iF55MdlMyNLp4sZzIdHM53N9-JYtCFIrFqqwUuVumZXgA8o4AHIOACaqeZNoAGmGqjSRBo4PxAdnHM1mnbKeHKRJGERBKg3YxcVyeNGfV6-RGqUH6aGWWyR56yTHBcLQYmGcnC2BMzn86uS2XdBW4FWax86w2m+jT7om8NhrQTLp2CpyMxLDobLYauzRGUIKx6uh2GhUAAEkEWgqgAwCaUYHRGGGAYlDAAZLiAA)

## Examples

``` r
# general data example
data <- teal_data()
data <- within(data, {
  iris <- iris
})

app <- init(
  data = data,
  modules = list(
    tm_g_distribution(
      dist_var = teal.picks::picks(
        datasets("iris"),
        teal.picks::variables(is.numeric),
        teal.picks::values()
      )
    )
  )
)
#> Initializing tm_g_distribution
if (interactive()) {
  shinyApp(app$ui, app$server)
}

# CDISC data example
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_distribution(
      dist_var = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(c("BMRKR1", "AGE")),
        values(multiple = FALSE)
      ),
      strata_var = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(c("ARM", "COUNTRY", "SEX"), selected = NULL)
      ),
      group_var = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(c("ARM", "COUNTRY", "SEX"), selected = NULL)
      )
    )
  )
)
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Initializing tm_g_distribution
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
