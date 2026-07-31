# `teal` module: Outliers analysis

Module to analyze and identify outliers using different methods such as
IQR, Z-score, and Percentiles, and offers visualizations including box
plots, density plots, and cumulative distribution plots to help
interpret the outliers.

## Usage

``` r
tm_outliers(
  label = "Outliers Module",
  outlier_var = teal.picks::picks(teal.picks::datasets(),
    teal.picks::variables(is.numeric, 1L, multiple = FALSE)),
  categorical_var = teal.picks::picks(teal.picks::datasets(),
    teal.picks::variables(choices = teal.picks::is_categorical(min.len = 1, max.len =
    10), selected = 1L, multiple = TRUE)),
  ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
  ggplot2_args = teal.widgets::ggplot2_args(),
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

- outlier_var:

  (`picks` or `list` of multiple `picks`) Specifies variable(s) to be
  analyzed for outliers.

- categorical_var:

  (`picks` or `list` of multiple `picks`) optional, specifies the
  categorical variable(s) to split the selected outlier variables on.

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
  `c("default", "Boxplot", "Density Plot", "Cumulative Distribution Plot")`.

  For more details see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

- plot_height:

  (`numeric`) optional, specifies the plot height as a three-element
  vector of `value`, `min`, and `max` intended for use with a slider UI
  element.

- plot_width:

  (`numeric`) optional, specifies the plot width as a three-element
  vector of `value`, `min`, and `max` for a slider encoding the plot
  width.

- pre_output:

  (`shiny.tag`) optional, text or UI element to be displayed before the
  module's output, providing context or a title. with text placed before
  the output to put the output into context. For example a title.

- post_output:

  (`shiny.tag`) optional, text or UI element to be displayed after the
  module's output, adding context or further instructions. Elements like
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html) are
  useful.

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
place using decorators:

- `box_plot` (`ggplot`)

- `density_plot` (`ggplot`)

- `cumulative_plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_outliers(
       ..., # arguments for module
       decorators = list(
         box_plot = teal_transform_module(...), # applied only to `box_plot` output
         density_plot = teal_transform_module(...), # applied only to `density_plot` output
         cumulative_plot = teal_transform_module(...) # applied only to `cumulative_plot` output
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokrcQAxLqkQ11XfxQpFC6cAAesKgiSoHBBsZc1AD6sVA2MUEhRroA7rSkABYq7Km4ugDCAPIATPEVNYoQSmiodSr57Eq6AZm6ALw9wbhdunxCIqL9owLCYp0Q3d2kMElEgqR0MqLzi4trG7QySVosU4kYqLQEANaiiIiXN9sju92ponCkz2BV1Qp4un+v3+cmGC1eSzg3AuV1u9xOjFoUHoE3YBE6hBIBH+ZX+glQwWucBBuLA+MJxLAoJer3OjzhiBO1EEc0aEOp4N2BCCcAkRER3OSCLOUJ49LuD1hz05r3en2+wIBQJqJJpuzpUvhLCRKLm6P+AAVqFAyDjAWA7KxUJTSXZGFDlhRSCTzUaTc6qWCIZDoeKtczWWrdGzFiHGo1aCZdOwVORmJYdDZbCARqIihBWABBdDsFoAEkEtDK+Y+jB0jEaAF8lGBKwBdIA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokrcQAxLoDCAEQCSAMreuvxQpFC6cAAesKgiSmERBsZc1AD6SVA2ieGRRroA7rSkABYq7Fm4uiBKuroAgr6BADIpumkYWYiIjE2tSgC+ihAAVkQq6QDWcKyilXm2BfxwJlDCpOkE-LSiBOnjkzNzwNDw81lyALruaKjtKiXsdaF5ugC8rxG4L3xCIqIPro-sIxM8IPV6qQYOkiIJSHQZPMXpDdHCEbQZOktCwgZ1ULQCFNRD0CUTkRDUZCsqI4KRkWB+i0FHhdCymSy5D9KVSOnBuBgycSejjGLQoPQAewCM9GQBxVyslkAIQAslgANJYACMnOq7IVnJRkK5xvqBHCcAkRDFFoyorx-J4QpJiBd4N59RpdIZHKVjOazLApp5VPxhOFiFF4slYPD5J6uy2luttu47BgKgwIghn211RgUGi2YoQO1AAY5CNedWTS8RiNaCZdOwVORmJYdDZbLVKaJyhBWA10Ow7gASQS0arj2mMHSMEaDJRgQZXIA)

## Examples

``` r

# general data example
data <- teal_data()
data <- within(data, CO2 <- CO2)

app <- init(
  data = data,
  modules = modules(
    tm_outliers(
      outlier_var = teal.picks::picks(
        datasets("CO2", "CO2"),
        teal.picks::variables(c("conc", "uptake"), "uptake"),
        teal.picks::values()
      ),
      categorical_var = teal.picks::picks(
        datasets("CO2", "CO2"),
        teal.picks::variables(c("Plant", "Type", "Treatment"), "Plant"),
        teal.picks::values()
      )
    )
  )
)
#> Initializing tm_outliers
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
    tm_outliers(
      outlier_var = teal.picks::picks(
        datasets("ADSL", "ADSL"),
        teal.picks::variables(c("AGE", "BMRKR1"), "AGE")
      ),
      categorical_var = teal.picks::picks(
        datasets("ADSL", "ADSL"),
        teal.picks::variables(teal.picks::is_categorical(min.len = 1, max.len = 10))
      )
    )
  )
)
#> Initializing tm_outliers
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
