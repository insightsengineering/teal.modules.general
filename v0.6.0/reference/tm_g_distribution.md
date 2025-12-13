# `teal` module: Distribution analysis

Module is designed to explore the distribution of a single variable
within a given dataset. It offers several tools, such as histograms, Q-Q
plots, and various statistical tests to visually and statistically
analyze the variable's distribution.

## Usage

``` r
tm_g_distribution(
  label = "Distribution Module",
  dist_var,
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

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  Variable(s) for which the distribution will be analyzed.

- strata_var:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  Categorical variable used to split the distribution analysis.

- group_var:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  Variable used for faceting plot into multiple panels.

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

  (`shiny.tag`) optional,  
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

  See section "Decorating Module" below for more details.

## Value

Object of class `teal_module` to be used in `teal` applications.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators::

- `histogram_plot` (`ggplot`)

- `qq_plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_distribution(
       ..., # arguments for module
       decorators = list(
         histogram_plot = teal_transform_module(...), # applied only to `histogram_plot` output
         qq_plot = teal_transform_module(...) # applied only to `qq_plot` output
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6urSMtKJxVTWiSgC+ihBKaKj1KnnsFf4ZugC8A0G4-XxCInUjdKKkfRCVlaQwiRLJtaQ19IKa1v3L-luJWizDo1CJYdvqpImiqHAEi0dHKdDwFwpg1bU-4yWb0qojgIg0F1B4Puj2e7DONSg9BEiQI+SItAIYj6v0aPzkpR+AAU4EEeAAZCgSAr4w7LNpHBm6NptWgmXTsFTkZiWHQ2WzlIGiQoQVgAQXQ7E6ABJBLRSjLQYwdIw2s0lGBmgBdIA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dfihSKF04AA9YVBElUPCDYy5qAH1EqBsEsIijXQB3WlIACxV2TNxdECVdXQBBHwCAGWTdVIxMxERGRpalAF9FCAArIhU0gGs4VlEK3Nt8-jgTKGFSNIJ+WlECNLGJ6dngaHg5zLkAXTcIJTRUNpVi9lqQ3N0AXjfw3Fe+IREok+un+wjELwgdTqpBgaQkGR2pEYDEEmmsryhIURaS0LGBmTS0SR6g2olQcAIEMxmMypz0XwUYD6zUZv0h1LqojgIg0wK5PNJ5MpGI5ugIJXGBDEwNxyKg9BEmwltClc0ZzNZYpeTIA4q48LpGQAhACyWAA0lgAIyMuRyNmiqH8inkfjA41my02vAijkwda0OL03QAMXqzQCzgdjtMtCicDdXzDEdc7I5w3T0cxomJ4RxeK+BKJzA0aTJFKpotpsGD6qaLJ9aepJlo1HIjGBLbbMjLQsrjtlQK+4slYjL3JdCf7MdltHlipHKvBdf6BuFTKwJs1jK8AHlHAA5BwATW3YEjAA1bfbfaLnRoE8CD45miym5nb5j-W3AyJgQ4XE-XQM2pG9310CRGCIQRUHzDtC1yQkomJUty2FcC6mreB3SZetWSArt207Vt217CsgLqQdgUXVVxwFKcKKhWd5zgJVRzVXDVyqdd6k3M890PE8z0va8sxje9XRw3itzARiwJjEEAyDf8nFTR0QMxDTgNeYZhloExdHYFR2xJbQ4BsWwanZUQyggVh6nQdh7gAEkEWgqhcrlGB0RhhgGJQwAGS4gA)

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
      dist_var = data_extract_spec(
        dataname = "iris",
        select = select_spec(variable_choices("iris"), "Petal.Length")
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
      dist_var = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices("ADSL", c("AGE", "BMRKR1")),
          selected = "BMRKR1",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      strata_var = data_extract_spec(
        dataname = "ADSL",
        filter = filter_spec(
          vars = choices_selected(
            variable_choices("ADSL", c("ARM", "COUNTRY", "SEX")),
            selected = NULL
          ),
          multiple = TRUE
        )
      ),
      group_var = data_extract_spec(
        dataname = "ADSL",
        filter = filter_spec(
          vars = choices_selected(
            variable_choices("ADSL", c("ARM", "COUNTRY", "SEX")),
            selected = "ARM"
          ),
          multiple = TRUE
        )
      )
    )
  )
)
#> Initializing tm_g_distribution
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
