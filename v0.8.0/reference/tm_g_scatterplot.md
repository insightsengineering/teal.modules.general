# `teal` module: Scatterplot

Generates a customizable scatterplot using `ggplot2`. This module allows
users to select variables for the x and y axes, color and size
encodings, faceting options, and more. It supports log transformations,
trend line additions, and dynamic adjustments of point opacity and size
through UI controls.

## Usage

``` r
tm_g_scatterplot(
  label = "Scatterplot",
  x = teal.picks::picks(teal.picks::datasets(), teal.picks::variables(is.numeric),
    teal.picks::values()),
  y,
  color_by = NULL,
  size_by = NULL,
  row_facet = NULL,
  col_facet = NULL,
  plot_height = c(600, 200, 2000),
  plot_width = NULL,
  alpha = c(1, 0, 1),
  shape = shape_names,
  size = c(5, 1, 15),
  max_deg = 5L,
  rotate_xaxis_labels = FALSE,
  ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
  pre_output = NULL,
  post_output = NULL,
  table_dec = 4,
  ggplot2_args = teal.widgets::ggplot2_args(),
  transformators = list(),
  decorators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- x:

  (`picks` or `list` of multiple `picks`) Specifies variable names
  selected to plot along the x-axis by default.

- y:

  (`picks` or `list` of multiple `picks`) Specifies variable names
  selected to plot along the y-axis by default.

- color_by:

  (`picks` or `list` of multiple `picks`) optional, defines the color
  encoding. If `NULL` then no color encoding option will be displayed.

- size_by:

  (`picks` or `list` of multiple `picks`) optional, defines the point
  size encoding. If `NULL` then no size encoding option will be
  displayed.

- row_facet:

  (`picks` or `list` of multiple `picks`) optional, specifies the
  variable(s) for faceting rows.

- col_facet:

  (`picks` or `list` of multiple `picks`) optional, specifies the
  variable(s) for faceting columns.

- plot_height:

  (`numeric`) optional, specifies the plot height as a three-element
  vector of `value`, `min`, and `max` intended for use with a slider UI
  element.

- plot_width:

  (`numeric`) optional, specifies the plot width as a three-element
  vector of `value`, `min`, and `max` for a slider encoding the plot
  width.

- alpha:

  (`integer(1)` or `integer(3)`) optional, specifies point opacity.

  - When the length of `alpha` is one: the plot points will have a fixed
    opacity.

  - When the length of `alpha` is three: the plot points opacity are
    dynamically adjusted based on vector of `value`, `min`, and `max`.

- shape:

  (`character`) optional, character vector with the names of the shape,
  e.g. `c("triangle", "square", "circle")`. It defaults to
  `shape_names`. This is a complete list from
  [`vignette("ggplot2-specs", package="ggplot2")`](https://ggplot2.tidyverse.org/articles/ggplot2-specs.html).

- size:

  (`integer(1)` or `integer(3)`) optional, specifies point size.

  - When the length of `size` is one: the plot point sizes will have a
    fixed size.

  - When the length of `size` is three: the plot points size are
    dynamically adjusted based on vector of `value`, `min`, and `max`.

- max_deg:

  (`integer`) optional, maximum degree for the polynomial trend line.
  Must not be less than 1.

- rotate_xaxis_labels:

  (`logical`) optional, whether to rotate plot X axis labels. Does not
  rotate by default (`FALSE`).

- ggtheme:

  (`character`) optional, `ggplot2` theme to be used by default.
  Defaults to `"gray"`.

- pre_output:

  (`shiny.tag`) optional, text or UI element to be displayed before the
  module's output, providing context or a title. with text placed before
  the output to put the output into context. For example a title.

- post_output:

  (`shiny.tag`) optional, text or UI element to be displayed after the
  module's output, adding context or further instructions. Elements like
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html) are
  useful.

- table_dec:

  (`integer`) optional, number of decimal places used to round numeric
  values in the table.

- ggplot2_args:

  (`ggplot2_args`) object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for the module plot. The argument is merged with options
  variable `teal.ggplot2_args` and default module setup.

  For more details see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html)

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

For more examples, please see the vignette "Using scatterplot" via
[`vignette("using-scatterplot", package = "teal.modules.general")`](https://insightsengineering.github.io/teal.modules.general/articles/using-scatterplot.md).

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_scatterplot(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6uoxwAI6CtNXsPqKkxNREjIoQlQDCAPIATHG6-QNKAL5dSmiowyp57BX+GboAvMtBuEt8QiKia7o7wmKL3ZW6pDCJEomiBIHkjJFEpKfn59RQ9HB+6wpgAGV7qRHs9SCN8kRaAQxP8tmd3qEDgkMKhoQBrUSIRBogiYt7vc4pURwUiiRZgUb-OTwwnnFG4zHYrQsWhfPYEumVAiQ6FiA4ECnECxw3T-QSoILouDU2lcyokkQaOD8A7-YUEf5LOk07WEhkYrGIFnUQQnLo6uXvVjIuDcVGG7GM8l697E0kuymDWWu+l2njO5ms9knX3vHlQmH7daC9UkTV4MVgCVSmVgXUIrmKuDK1V-ZOSqDSrWZ94Z+UGvFGk1m8kWwnlwltDqJeg29aVpk4w2cwnuskUqnpq36-0OqtBxhs+gcsPnCN86O6WNgAAKnzIov+dlYqDTpW31UC8E3ibjIrPBdTPtLhOzuYOADlHAAZF9zxt0zvV7i1tK3z9zlEWgAC84FbdsLjHQNuyrXs3QyEkB3+IdAPeb9J2nDkFyjAUhXjLcryLNMaV0e9yDzJMU2Im8K2gx1jV-c1fTQqoiGyRITHUUlbXtGDnXgolEI9QdvWHOcMMYqcQxdW9w15XCYwpdcoFPA8wB3PdCLsI9LgoUhaPlMifhzCin1fd85N0VjKkkmtmIAkduSIJIuJhcEO3oidYPxOd+09VCnL9PiGJZaSZxOHD+SU-4VLUpNNP3BLdJPAzh2MpUzPWZ83xsqCQu8+y6xYvV6y6LpaBMXR2BUR51E0HQbFscozlEQoIFYABBdB2BmAASepSn6klGB0ToJiUMBxgAXSAA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dfihSKF04AA9YVBElUPCDYy5qAH1EqBsEsIijXQB3WlIACxV2TNxdECVdXUY4AEdBWgb2CDFSYmoiRkUIOoBBHwCAGWTdVIxMxERGYbGlAF9+gCsiFTSAazhWUQrc23z+OBMoYVI0gn5aUQI09c2dveBoeH3MuQBdN2h0CZUxXYtRCuV0AF5QeFcCC+EIRKIIbo4cIxMCBnVJjA0hI0ncwuRGHEiKR0ZjMdQoPQ4NQkQowAECASZMTSN4ShsCGJ6TCMeSokipqhaAQtqJZsLRfsQeS6plRHBSNKwAtRvS5LzZZihSKxbMtCxaFSEewCByRWIkQRgSqAOKuPC6ekAIQAslgANJYACMPKdYDdnqwACZ1VUFSINHB+HS7Q6NTLZTqpfruII0f0tQm+ZjWIK4NwMJK9Yhi9Kc+T5Yrlaqw4nycmSwbGEb6CazZzLZDrfTBva-S73V7fY7B0HQ2ANboI3AozHIWPh3WK9qCzwy6nqOn9pnZdmtd1eml6HnIY3xaXdeWtXLcgqlTba5PNTfz6mW8a0T24wOA0Ofb+gZehOVT0lggxeA6oFgFgzi2n4ADyAByI5TjOc5IkhjijKM+6vmuRZXpu27ZCueHkqItAAF5wMep6TARG6XlKZI3lWD69iMarPvWq6Fkxzatu25pcoi3aPv2o5-kGqHhjSs7kPO-p9vGL5am+iAGluGa8eRmKMEQBRpGcXJsmejFEcxYqsVq7E1lxy43gx-GWYJn77B2Fpibo35ASGv7gZBAVwYhKFhtO8kYZCWE4XpDYWSmmlpjpZFqZi3TGeoir5i5iVljZsp2Y+Dk8SudQaW5bZfiJXY+Tafkgf6gVQU1IXIbJEWRopmHYbhaXxblTbJTuumJru-T9LQJi6OwKiEuomg6DYtg1BiohlBArCDOg7BoKgAAkLRVHt+0KowOh9MsShgEsXxAA)

## Examples

``` r
# general data example
data <- teal_data()
data <- within(data, {
  require(nestcolor)
  CO2 <- CO2
})

app <- init(
  data = data,
  modules = modules(
    tm_g_scatterplot(
      label = "Scatterplot Choices",
      x = teal.picks::picks(
        datasets("CO2"),
        teal.picks::variables(
          choices = c("conc", "uptake"),
          selected = "conc"
        ),
        teal.picks::values()
      ),
      y = teal.picks::picks(
        datasets("CO2"),
        teal.picks::variables(
          choices = c("conc", "uptake"),
          selected = "uptake"
        ),
        teal.picks::values()
      ),
      color_by = teal.picks::picks(
        datasets("CO2"),
        teal.picks::variables(
          choices = c("Plant", "Type", "Treatment", "conc", "uptake"),
          selected = NULL
        ),
        teal.picks::values()
      ),
      size_by = teal.picks::picks(
        datasets("CO2"),
        teal.picks::variables(choices = c("conc", "uptake"), selected = "uptake"),
        teal.picks::values()
      ),
      row_facet = teal.picks::picks(
        datasets("CO2"),
        teal.picks::variables(
          choices = c("Plant", "Type", "Treatment"),
          selected = NULL
        ),
        teal.picks::values()
      ),
      col_facet = teal.picks::picks(
        datasets("CO2"),
        teal.picks::variables(choices = c("Plant", "Type", "Treatment"), selected = NULL),
        teal.picks::values()
      )
    )
  )
)
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Initializing tm_g_scatterplot
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
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
    tm_g_scatterplot(
      label = "Scatterplot Choices",
      x = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(choices = c("AGE", "BMRKR1", "BMRKR2"), selected = "AGE"),
        teal.picks::values()
      ),
      y = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(choices = c("AGE", "BMRKR1", "BMRKR2"), selected = "BMRKR1"),
        teal.picks::values()
      ),
      color_by = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(c("AGE", "BMRKR1", "BMRKR2", "RACE", "REGION1"), selected = NULL),
        teal.picks::values()
      ),
      size_by = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(choices = c("AGE", "BMRKR1"), selected = "AGE"),
        teal.picks::values()
      ),
      row_facet = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(choices = c("BMRKR2", "RACE", "REGION1"), selected = NULL),
        teal.picks::values()
      ),
      col_facet = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(choices = c("BMRKR2", "RACE", "REGION1"), selected = NULL),
        teal.picks::values()
      )
    )
  )
)
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Initializing tm_g_scatterplot
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
