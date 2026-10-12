# `teal` module: Response plot

Generates a response plot for a given `response` and `x` variables. This
module allows users customize and add annotations to the plot depending
on the module's arguments. It supports showing the counts grouped by
other variable facets (by row / column), swapping the coordinates, show
count annotations and displaying the response plot as frequency or
density.

## Usage

``` r
tm_g_response(
  label = "Response Plot",
  response = teal.picks::picks(teal.picks::datasets(), teal.picks::variables(choices =
    teal.picks::is_categorical(min.len = 2, max.len = 10)), teal.picks::values()),
  x,
  row_facet = NULL,
  col_facet = NULL,
  coord_flip = FALSE,
  count_labels = TRUE,
  rotate_xaxis_labels = FALSE,
  freq = FALSE,
  plot_height = c(600, 400, 5000),
  plot_width = NULL,
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

- response:

  (`picks`) Which variable to use as the response. The `picks` must not
  allow multiple variable selection in this case.

- x:

  (`picks` ) Specifies which variable to use on the X-axis of the
  response plot. The `picks` must not allow multiple selection in this
  case.

- row_facet:

  (`picks`) optional specification of the data variable(s) to use for
  faceting rows.

- col_facet:

  (`picks`) optional specification of the data variable(s) to use for
  faceting columns.

- coord_flip:

  (`logical(1)`) Indicates whether to flip coordinates between `x` and
  `response`. The default value is `FALSE` and it will show the `x`
  variable on the x-axis and the `response` variable on the y-axis.

- count_labels:

  (`logical(1)`) Indicates whether to show count labels. Defaults to
  `TRUE`.

- rotate_xaxis_labels:

  (`logical`) optional, whether to rotate plot X axis labels. Does not
  rotate by default (`FALSE`).

- freq:

  (`logical(1)`) Indicates whether to display frequency (`TRUE`) or
  density (`FALSE`). Defaults to density (`FALSE`).

- plot_height:

  (`numeric`) optional, specifies the plot height as a three-element
  vector of `value`, `min`, and `max` intended for use with a slider UI
  element.

- plot_width:

  (`numeric`) optional, specifies the plot width as a three-element
  vector of `value`, `min`, and `max` for a slider encoding the plot
  width.

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

## Note

For more examples, please see the vignette "Using response plot" via
[`vignette("using-response-plot", package = "teal.modules.general")`](https://insightsengineering.github.io/teal.modules.general/articles/using-response-plot.md).

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_response(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6uoxwAI6CtNXsPqKkxNREjIoQlTCtLKJxur0E-RWmHbrsGFq6KroE7AqErNRLpUtaomu6S7DbS1IsS3K25d2VQ32MosDA0wC694NQ4ibqpB3sw-23D-ddlQAvkpAV0lGhUIMVHlFucUroALz+DK4MZ8IQiAZI9HCMSwi66UgwRISRLVUSoEiiOD4gm6ahQehwPxIpZYMSUiDU3QABXapC2eDGBPJnO5SISGFQtAIAGtRIhENK5aJaXTCXBuFKZfLFSlqQLFmBvtdjqjzurJcrdYgtCxaIzMWr1fN8kQZWJEfMjQQVvswIdGGbhS7qSINHB+F6lr7VkKLS6YMJNJE9EiAGIAQQAMgBlZzml0XEy0UKRr1ZvOuBMEuSFl1WnUK23cQR4gF0ushyqhL2NlWK62q7sXfVwQ1LE2Crs1i79m12xgO+hOkcEghuj1Y70bQXrFAwYOzglhuARqOssCbNZri5J6gpkQVnP5+tFktli+6Sv528zovzs2drUG2qodrWIYdl0XS0CYkwqOQzCWDoNinGMoiFBArCZug7AQgAJPUpQEdSjA6J0IJKGAgL3EAA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dfihSKF04AA9YVBElUPCDYy5qAH1EqBsEsIijXQB3WlIACxV2TNxdECVdXUY4AEdBWgb2CDFSYmoiRkUIOoBBHwCAGWTdVIxMxERGYbGlAF9+gCsiFTSAazhWUQrc23z+OBMoYVI0gn5aUQI09c2dveBoeH3MuQBdN2h0CZUxXYtRCuV0AF5QeFcCC+EIRKIIbo4cIxMCBnVJjA0hI0g1RKgSKI4OjMZjqFB6HBqEiFGAsGJCRBiboAAo9UiiOkwjFk-FMlmQqaoWgELaiWYisX7EFkuqZYmc4FgBajOlyHlyzHC0Xi2ZaFi0SkI0lazEEEobAhiJEEZUAIQAslgANJYABM3N0dK8AHlHAA5BwATXVmrNdWJIg0cH4tLATtdHrpsrlGtTZJ10v13EEaP6WvTvMxUSRWb1iCl4tNWoVcCVdNVYYz2rg3AwVYliANjCN9BNLbJFqtNshdrpAWcAA0vXSsIMvK4wEWI5HqXAY3HIXOF0vB7oV2by12DdQ8-sC2nU5f+v1aCZdOwVORmJYdDZbDUMaIyhBWIN0HYNBUAAEhaKpgJA4lGB0PpliUMAli+IA)

## Examples

``` r
# general data example
data <- teal_data()
data <- within(data, {
  require(nestcolor)
  mtcars <- mtcars
  for (.v in c("cyl", "vs", "am", "gear")) {
    mtcars[[.v]] <- as.factor(mtcars[[.v]])
  }
})

app <- init(
  data = data,
  modules = modules(
    tm_g_response(
      label = "Response Plots",
      response = teal.picks::picks(
        teal.picks::datasets("mtcars"),
        teal.picks::variables(
          choices = c("cyl", "gear"),
          selected = "cyl",
          multiple = FALSE,
          fixed = FALSE
        ),
        teal.picks::values()
      ),
      x = teal.picks::picks(
        datasets("mtcars"),
        teal.picks::variables(
          choices = c("vs", "am"),
          selected = "vs",
          multiple = FALSE,
          fixed = FALSE
        ),
        teal.picks::values()
      )
    )
  )
)
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Initializing tm_g_response
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
    tm_g_response(
      label = "Response Plots",
      response = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(
          choices = c("BMRKR2", "COUNTRY"),
          selected = "BMRKR2"
        ),
        teal.picks::values()
      ),
      x = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(
          choices = c("SEX", "RACE"),
          selected = "RACE"
        ),
        teal.picks::values()
      )
    )
  )
)
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Initializing tm_g_response
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
