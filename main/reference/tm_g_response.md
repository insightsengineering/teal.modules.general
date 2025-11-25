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
  response,
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

  (`data_extract_spec` or `list` of multiple `data_extract_spec`) Which
  variable to use as the response. You can define one fixed column by
  setting `fixed = TRUE` inside the `select_spec`.

  The `data_extract_spec` must not allow multiple selection in this
  case.

- x:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  Specifies which variable to use on the X-axis of the response plot.
  Allow the user to select multiple columns from the `data` allowed in
  teal.

  The `data_extract_spec` must not allow multiple selection in this
  case.

- row_facet:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  optional specification of the data variable(s) to use for faceting
  rows.

- col_facet:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  optional specification of the data variable(s) to use for faceting
  columns.

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

  See section "Decorating Module" below for more details.

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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6uoxwAI6CtNXsPqKkxNREjIoQlTCtLKJxur0E-RWmHbrsWroqugTsCoSs1Iuli1qiq7qLsFuLUiyLcrbl3ZVDfYyiwMBaALp3g1DiJuqkHezD-Tf3d12VAF8lACuko0KhBio8gszildABefwZXBjPhCEQDRFo4RiGHnXSkGCJCSJaqiVAkURwPH43TUKD0OB+RGLLBiCkQKm6AAK7VImzwY3xZI5XMRKUSYVIzA0iXJcHmQtpKWg8AR2zAXyuqyV+KpIg06v1CtIctQCpptPO9MZzI1AGUmSbdFoWLQGSJEDqzlbKgR8kRaAQxOrXYx3fQRIl-YHg6JihkbostQKHqVFUsVngNQdGEc5Cifb7jRo4Px1YsCMtvb7zjBhJpInpEQAxACCABl7c5C7XKiZaKEy+r213XEX8f9aQXdbpQuqJVKZab5YqJ+cVbBmxqUzXi07DYiSyvzWu+3SGUyK2BHQbSC63R64F7Bev8TGgyHEWGI1GP3GEyCJNNUuVM7nTBYwA2PYUBgfNez7Y9hxZKCBQQ2t62oRsRBHTtu3Q30ByHctWzw8daynSclSnLouloExJhUchl20aljjKMZREKCBWDbdB2HBAASepSiEqlGB0TpgSUMAATuIA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dfihSKF04AA9YVBElUPCDYy5qAH1EqBsEsIijXQB3WlIACxV2TNxdECVdXUY4AEdBWgb2CDFSYmoiRkUIOoBBHwCAGWTdVIxMxERGYbGlAF9+gCsiFTSAazhWUQrc23z+OBMoYVI0gn5aUQI09c2dveBoeH3MuQBdN2h0CZUxXYtRCuV0AF5QeFcCC+EIRKIIbo4cIxMCBnVJjA0hI0g1RKgSKI4OjMZjqFB6HBqEiFGAsGJCRBiboAAo9UiiOkwjFk-FMlmQzJpaKkZgaNIEuAEUlkzGZN56SF0hajbkguW6YkiDRI7XSy5SmUazW6ClUmnKsABakG3RaFi0SkiRDq3mmgglDYEMRIh2MJ30ERXL20H0fXLAYAqkZqsBfL5VY1gABCAFksABpLAAJm5ujpXgA8o4AHIOACadLkch5psx+o0cH4tNTGezebwJs1MAutDiSt0ADFBqMAs46-XTLQos2kSOx653WT+pra93dFEkcLReLDahpbLNQrYIOY4su8uG7bdZDG-vDxuyebqa2bTrSPbHc64K7L1PdE9b1fUhf1A2DICwzRTIo3POMEyTYFrWcAANfM6SwQYvFcMAa0net7znK1MOwt0AN7ahNAHedR3HfDTRMGciOHWil3rVc5Q4uoOP6fpaBMXR2BUcg920Eka2qEFRDKCBWEGdB2DQVAABIWiqJTlOJRgdD6ZYlDAJYviAA)

## Examples

``` r
# general data example
data <- teal_data()
data <- within(data, {
  require(nestcolor)
  mtcars <- mtcars
  for (v in c("cyl", "vs", "am", "gear")) {
    mtcars[[v]] <- as.factor(mtcars[[v]])
  }
})

app <- init(
  data = data,
  modules = modules(
    tm_g_response(
      label = "Response Plots",
      response = data_extract_spec(
        dataname = "mtcars",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["mtcars"]], c("cyl", "gear")),
          selected = "cyl",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      x = data_extract_spec(
        dataname = "mtcars",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["mtcars"]], c("vs", "am")),
          selected = "vs",
          multiple = FALSE,
          fixed = FALSE
        )
      )
    )
  )
)
#> Initializing tm_g_response
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
      response = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["ADSL"]], c("BMRKR2", "COUNTRY")),
          selected = "BMRKR2",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      x = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["ADSL"]], c("SEX", "RACE")),
          selected = "RACE",
          multiple = FALSE,
          fixed = FALSE
        )
      )
    )
  )
)
#> Initializing tm_g_response
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
