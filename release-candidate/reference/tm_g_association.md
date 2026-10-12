# `teal` module: Stack plots of variables and show association with reference variable

Module provides functionality for visualizing the distribution of
variables and their association with a reference variable. It supports
configuring the appearance of the plots, including themes and whether to
show associations.

## Usage

``` r
tm_g_association(
  label = "Association",
  ref = teal.picks::picks(teal.picks::datasets(), teal.picks::variables(choices =
    is.numeric | teal.picks::is_categorical(min.len = 2, max.len = 10), selected = 1L),
    teal.picks::values()),
  vars,
  show_association = TRUE,
  plot_height = c(600, 400, 5000),
  plot_width = NULL,
  distribution_theme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic",
    "void"),
  association_theme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic",
    "void"),
  pre_output = NULL,
  post_output = NULL,
  ggplot2_args = teal.widgets::ggplot2_args(),
  transformators = list(),
  decorators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- ref:

  (`picks`) Reference variable specification created using
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html).

- vars:

  (`picks`) Variables to be associated with the reference variable,
  specified using
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html).

- show_association:

  (`logical`) optional, whether show association of `vars` with
  reference variable. Defaults to `TRUE`.

- plot_height:

  (`numeric`) optional, specifies the plot height as a three-element
  vector of `value`, `min`, and `max` intended for use with a slider UI
  element.

- plot_width:

  (`numeric`) optional, specifies the plot width as a three-element
  vector of `value`, `min`, and `max` for a slider encoding the plot
  width.

- distribution_theme, association_theme:

  (`character`) optional, `ggplot2` themes to be used by default.
  Default to `"gray"`.

- pre_output:

  (`shiny.tag`) optional, text or UI element to be displayed before the
  module's output, providing context or a title. with text placed before
  the output to put the output into context. For example a title.

- post_output:

  (`shiny.tag`) optional, text or UI element to be displayed after the
  module's output, adding context or further instructions. Elements like
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html) are
  useful.

- ggplot2_args:

  (`ggplot2_args`) optional, object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for all the plots or named list of `ggplot2_args`
  objects for plot-specific settings. The argument is merged with
  options variable `teal.ggplot2_args` and default module setup.

  List names should match the following:
  `c("default", "Bivariate1", "Bivariate2")`.

  For more details see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

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

## Note

For more examples, please see the vignette "Using association plot" via
[`vignette("using-association-plot", package = "teal.modules.general")`](https://insightsengineering.github.io/teal.modules.general/articles/using-association-plot.md).

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `plot` (`grob` created with
  [`ggplot2::ggplotGrob()`](https://ggplot2.tidyverse.org/reference/ggplotGrob.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_association(
       ..., # arguments for module
       decorators = list(
         plot = teal_transform_module(...) # applied to the `plot` output
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6uoxwAI6CtNXsPqKkxNREjIoQlQDCAPIATHG6-QMVpuqkHaLD0PCi7ABitNTkjOy0og4upVpokRyjpZsYJpMdpe0StATc7ACMADJyL129g8BnGtMAusPU+2ohw+XymjFEP1KUHEBHyLEmMi6AF8ukp9sMVHl2OMUroALz+DK4cZ8IQiGYE0nCMTY7qVXSkGCJCSJaGiIgEWiBKwQWn0+nVEz4hlwbgYVA3ADWokQiAlBGlfP59JSojgpAWCjAoy1cmJdOVIrF8ulsr2jC59HJSsNlVhRBuYmFBGxYAACgCyFrSlq7KxUHBvbpfdVAvAvWA9eNbbo1SINHB+MKtR6oBHo8q3pn9YbzRSjTwTTK5VLNQblar1ZrtYNdTnbQlxaWzSxLdaM4b7Y78y6U57SEHff7A3hg2A7KHGRQB5H6zG43AE0mCSHRVOI3PbTBhJpInoCdtXOX+VmTxms10urQhRtVMxLDobLZynTRIUIKwAILodj7AAk9RQugf5qowOidEoSJKGASI-EAA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dfihSKF04AA9YVBElUPCDYy5qAH1EqBsEsIijXQB3WlIACxV2TNxdECVdXUY4AEdBWgb2CDFSYmoiRkUIOoBBHwCAGWTdVIxMxERGYbGlAF9+gCsiFTSAazhWUQrc23z+OBMoYVI0gn5aUQI09c2dveBoeH3MuQBdN2h0CZUxXYtRCuV0AF5QeFcCC+EIRKIIbo4cIxMCBnVJjA0hI0lBRKIiARaGErBB0ZjMQ0TEipqhaAQtqJZvTGfsQZS6plRHBSOywAtRgowHIYRjOZM4NwMKymbMtCwSfQERSJZiCCUNgQxEiCMCwAFnAANYVVYVYQZeVx4XTCrwAeUcADkHABNU22gVYACyHuFAQcgzsgwAjH6DYHgwAmcN+Ox2ABiQptwoAQt6sABpLAxkVitWYnkiDRwfhI82W60czn9CWi6t1BWMRGQukMuWIWXs8Wc7m8-mC4X1nuUtts+WKqDKtENzkarU6yF6-3G8MWq3hh3Ot3hwY+8MBi3BsMpiNHwa5s1gOOJ5NX9NZnND-MF3RFuAlsuQtMZ7OX2eUjAFy0HEeiQg4LgAbotY1tWMH9P0tA0uwKjkMwlg6DYtg1BiohlBArCDOg7BoKgAAkLRVKRZE8owOh9MsShgEsXxAA)

## Examples

``` r
# general data example
data <- teal_data()
data <- within(data, {
  require(nestcolor)
  CO2 <- CO2
  factors <- names(Filter(isTRUE, vapply(CO2, is.factor, logical(1L))))
  CO2[factors] <- lapply(CO2[factors], as.character)
})

app <- init(
  data = data,
  modules = modules(
    tm_g_association(
      ref = teal.picks::picks(
        datasets("CO2"),
        teal.picks::variables(
          choices = c("Plant", "Type", "Treatment"),
          selected = "Plant"
        )
      ),
      vars = teal.picks::picks(
        datasets("CO2"),
        teal.picks::variables(
          choices = c("Plant", "Type", "Treatment"),
          selected = "Treatment",
          multiple = TRUE
        )
      )
    )
  )
)
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Initializing tm_g_association
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
if (interactive()) {
  shinyApp(app$ui, app$server)
}

# CDISC data example
data <- teal_data()
data <- within(data, {
  require(nestcolor)
  ADSL <- teal.data::rADSL
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_association(
      ref = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(
          choices = c("SEX", "RACE", "COUNTRY", "ARM", "STRATA1", "STRATA2", "ITTFL", "BMRKR2"),
          selected = "RACE"
        )
      ),
      vars = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(
          choices = c("SEX", "RACE", "COUNTRY", "ARM", "STRATA1", "STRATA2", "ITTFL", "BMRKR2"),
          selected = "BMRKR2",
          multiple = TRUE
        )
      )
    )
  )
)
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Initializing tm_g_association
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
