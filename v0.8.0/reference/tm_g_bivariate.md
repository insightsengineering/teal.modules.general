# `teal` module: Univariate and bivariate visualizations

Module enables the creation of univariate and bivariate plots,
facilitating the exploration of data distributions and relationships
between two variables.

## Usage

``` r
tm_g_bivariate(
  label = "Bivariate Plots",
  x = teal.picks::picks(teal.picks::datasets(), teal.picks::variables(choices =
    is.numeric | teal.picks::is_categorical(min.len = 2, max.len = 10), selected = 1L),
    teal.picks::values()),
  y,
  row_facet,
  col_facet,
  facet,
  color = NULL,
  fill = NULL,
  size = NULL,
  use_density = FALSE,
  color_settings = FALSE,
  free_x_scales = FALSE,
  free_y_scales = FALSE,
  plot_height = c(600, 200, 2000),
  plot_width = NULL,
  rotate_xaxis_labels = FALSE,
  swap_axes = FALSE,
  ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
  ggplot2_args = teal.widgets::ggplot2_args(),
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

- x:

  (`picks`) Variable specification for the x-axis. Created using
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html).
  Can be numeric, factor or character. No empty selections are allowed.

- y:

  (`picks`) Variable specification for the y-axis. Created using
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html).
  Can be numeric, factor or character.

- row_facet:

  (`picks`) optional, specification of the data variable(s) to use for
  faceting rows. Created using
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html).

- col_facet:

  (`picks`) optional, specification of the data variable(s) to use for
  faceting columns. Created using
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html).

- facet:

  (`logical`) optional, specifies whether the facet encodings `ui`
  elements are toggled on and shown to the user by default. Defaults to
  `TRUE` if either `row_facet` or `column_facet` are supplied.

- color:

  (`picks`) optional, specification of the data variable(s) selected for
  the outline color inside the coloring settings. It will be applied
  when `color_settings` is set to `TRUE`. Created using
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html).

- fill:

  (`picks`) optional, specification of the data variable(s) selected for
  the fill color inside the coloring settings. It will be applied when
  `color_settings` is set to `TRUE`. Created using
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html).

- size:

  (`picks`) optional, specification of the data variable(s) selected for
  the size of `geom_point` plots inside the coloring settings. It will
  be applied when `color_settings` is set to `TRUE`. Created using
  [`teal.picks::picks()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html).

- use_density:

  (`logical`) optional, indicates whether to plot density (`TRUE`) or
  frequency (`FALSE`). Defaults to frequency (`FALSE`).

- color_settings:

  (`logical`) Whether coloring, filling and size should be applied and
  `UI` tool offered to the user.

- free_x_scales:

  (`logical`) optional, whether X scaling shall be changeable. Does not
  allow scaling to be changed by default (`FALSE`).

- free_y_scales:

  (`logical`) optional, whether Y scaling shall be changeable. Does not
  allow scaling to be changed by default (`FALSE`).

- plot_height:

  (`numeric`) optional, specifies the plot height as a three-element
  vector of `value`, `min`, and `max` intended for use with a slider UI
  element.

- plot_width:

  (`numeric`) optional, specifies the plot width as a three-element
  vector of `value`, `min`, and `max` for a slider encoding the plot
  width.

- rotate_xaxis_labels:

  (`logical`) optional, whether to rotate plot X axis labels. Does not
  rotate by default (`FALSE`).

- swap_axes:

  (`logical`) optional, whether to swap X and Y axes. Defaults to
  `FALSE`.

- ggtheme:

  (`character`) optional, `ggplot2` theme to be used by default.
  Defaults to `"gray"`.

- ggplot2_args:

  (`ggplot2_args`) object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for the module plot. The argument is merged with options
  variable `teal.ggplot2_args` and default module setup.

  For more details see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html)

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

## Details

This is a general module to visualize 1 & 2 dimensional data.

## Note

For more examples, please see the vignette "Using bivariate plot" via
[`vignette("using-bivariate-plot", package = "teal.modules.general")`](https://insightsengineering.github.io/teal.modules.general/articles/using-bivariate-plot.md).

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_bivariate(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6uoxwAI6CtNXsPqKkxNREjIoQlQDCAPIATHH+GRgmzPDs-QNdAL5dSmiowyp57BUjsQC8m1C4G3xCIqK6O6QwiRKJ9NostIFw692VutRQ9HB+OwpgAEK3jHu5F0AAV2qRRD99s9KqFTroEhhULQCABrUSIRDItGiJ4vF4pURwCHrMDTH5yaH4yqI7HozFaO7vY7sIkiDRwfjwn7ECwUjaVSkC3SseG0lH0rES3HCyqE4kysmDClU6ninEMpn0FlsuAcrnfMCCVBBVFwfkw3RCy2MIjZRImdTEsVwbhI6WYukyy1yjJEkk-clga3UhGunhezWA5liVmfPXkA26H52Vioc3B4Uhl5tB1O0gut2RqU4vHU+UBpUDFWysNFj2IRnR7Wx3X67lgOzVQLwMgW-FdQVKLq0Ey6dgqcjMSw6Gy2crPUSFCCsACC6HYSwAJPVStuiYwdJ0lLMlGBZgBdIA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dfihSKF04AA9YVBElUPCDYy5qAH1EqBsEsIijXQB3WlIACxV2TNxdECVdXUY4AEdBWgb2CDFSYmoiRkUIOoBBHwCAGWTdVIxMxERGYbGlAF9+gCsiFTSAazhWUQrc23z+OBMoYVI0gn5aUQI09c2dveBoeH3MuQBdN2h0CZUxXYtRCuV0AF5QeFcCC+EIRKIIZMYGkJGl6NoWLQwnBgQM6rpqFB6HBqEiFGAAEKYxjY8i6AAKPVIogpMPxdSiSKmqFoBC2olmvP5+xBBKhUFEcBZwLAC1GFLk7PFdR5fIFsy0WOJCPYUpEGjg-HJcoA4q4wP1xUqxbpWNy4NwMMKNYgXaKOQTMlKZRT5YrlSq1SLNdr6Lr9XBDcbIRSAs4ABqK202z2MIgFNJnAjSh1O91C9UelUSn2iuUjBWWwPi4OurW0nViPWkqPkGO6P1YACyyc9qfF3Sz6lzkLrgrdRbxJe90vL-urttVjp4BcQDex4ebkejJq8AHlHAA5BwATT71pB-X6tBMunYKnIzEsOhsthq+NEZQgrEG6HYaCoAAJC0VSAUBUqMDofTLEoYBLF8QA)

## Examples

``` r
# general data example
data <- teal_data()
data <- within(data, {
  require(nestcolor)
  CO2 <- data.frame(CO2)
})

app <- init(
  data = data,
  modules = tm_g_bivariate(
    label = "Bivariate Plots",
    x = teal.picks::picks(
      datasets("CO2"),
      teal.picks::variables(selected = "conc")
    ),
    y = teal.picks::picks(
      datasets("CO2"),
      teal.picks::variables(selected = "uptake")
    ),
    row_facet = teal.picks::picks(
      datasets("CO2"),
      teal.picks::variables(selected = "Type")
    ),
    col_facet = teal.picks::picks(
      datasets("CO2"),
      teal.picks::variables(selected = "Treatment")
    )
  )
)
#> Warning: teal.picks::variables(selected = "conc")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
#> Initializing tm_g_bivariate
#> Warning: teal.picks::variables(selected = "uptake")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
#> Warning: teal.picks::variables(selected = "Type")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
#> Warning: teal.picks::variables(selected = "Treatment")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
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
  modules = tm_g_bivariate(
    label = "Bivariate Plots",
    x = teal.picks::picks(
      datasets("ADSL"),
      teal.picks::variables(selected = "AGE")
    ),
    y = teal.picks::picks(
      datasets("ADSL"),
      teal.picks::variables(selected = "SEX")
    ),
    row_facet = teal.picks::picks(
      datasets("ADSL"),
      teal.picks::variables(selected = "ARM")
    ),
    col_facet = teal.picks::picks(
      datasets("ADSL"),
      teal.picks::variables(selected = "COUNTRY")
    )
  )
)
#> Warning: teal.picks::variables(selected = "AGE")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
#> Initializing tm_g_bivariate
#> Warning: teal.picks::variables(selected = "SEX")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
#> Warning: teal.picks::variables(selected = "ARM")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
#> Warning: teal.picks::variables(selected = "COUNTRY")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
