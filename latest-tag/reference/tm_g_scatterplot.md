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
  x,
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

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  Specifies variable names selected to plot along the x-axis by default.

- y:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  Specifies variable names selected to plot along the y-axis by default.

- color_by:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  optional, defines the color encoding. If `NULL` then no color encoding
  option will be displayed.

- size_by:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  optional, defines the point size encoding. If `NULL` then no size
  encoding option will be displayed.

- row_facet:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  optional, specifies the variable(s) for faceting rows.

- col_facet:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  optional, specifies the variable(s) for faceting columns.

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

  See section "Decorating Module" below for more details.

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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6uoxwAI6CtNXsPqKkxNREjIoQlQDCAPIATHG6-QNKAL5dSmiowyp57BX+GboAvMtBuEt8QiKia7o7wmKL3ZW6pDCJEomiBIHkjJFEpKfn59RQ9HB+6wpgAGV7qRHs9SCN8kRaAQxP8tmd3qEDilEmFSMwNLdUHACG93ucUtB4Ad-qM4Ut8bpRD8ceD1tSRJjRNjcRTKbpPt9frp-gCaRpdFoWLQviJEOSEeyCJDoWIDkLGCL6CJEtKoTDRMUMsBgKTBv8ALoG0qswgkAhwnlgQSoIIAazg-zkcnh7PODNpcH4JLNFglbsqMGEmkienWADEAIIAGQBzldAZMtFCXoOUdjrkl7y6lJdbMqrGRGVRoXR6lIWJxeMphNgYatZLw+fd-LpVNbldZWfxnJ+Pr5jPBCqVYv9AbVsv262HorgqplGq1QR1erGYCNJsWvoteCtNvtjrAzoTbo9GlTf2ttqgDrHbqD1BDIjTMbjJ-ZSZT3ojr8zbpz+J5t2bQdIk9CFusKJohiFbMlWzaVLWxKXo277vGebYYZ21bsr23K8q2grCrO4pNt27wThq8rEcqc6UScCH4ikK5gI2G6MRRW4AAqfGQlr-HYrDYvxYB2NUgTwHxu7-MQfrSVeB5OhxuhAQG7aDheugAHKONG0ZofiD5PvW6Zvspn6aaZf7sgB2YGaItAAF5zuBRZBCWZZMiyOHvEh9arne+IYQcWFwV2al4f2hEzrRpEGRRC5ytONEqvRmrMbqrH6uuxq6Kask7qU-z7jeh7HspGGacV163mRalGbQoYvhm8XnBZ366FZjG2ecqn4owRDZIkJjqHAbZQaWMHYYxfk+qhjHBfSHZhT5PZfH2l4DrSRGKiRgWUml1G7bR87qicGUBTlm7-DxUBSUVolCYeD1iXAEkUKQTp9aeraaTpemtYGwaNc+P4teZyaWb+3XNt95xtMNo3jcW0HltN5EbHddZzdlrWLeptLoxF634YC0UpXAcXKYdyXHaliXpdqmVsblpq3fdVqCcJ8mve9UnlRjlSVR1-36cpDVNWDZmC6YkMdV1GM9ZUPUAV0XS0CYujsCojzltocA2LY5RnKIhQQKwkboOwMwACT1KUtvUowOidBMShgOMBpAA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dfihSKF04AA9YVBElUPCDYy5qAH1EqBsEsIijXQB3WlIACxV2TNxdECVdXUY4AEdBWgb2CDFSYmoiRkUIOoBBHwCAGWTdVIxMxERGYbGlAF9+gCsiFTSAazhWUQrc23z+OBMoYVI0gn5aUQI09c2dveBoeH3MuQBdN2h0CZUxXYtRCuV0AF5QeFcCC+EIRKIIbo4cIxMCBnVJjA0hI0ncwuRGHEiKR0ZjMdQoPQ4NQkQowAECASZMTSN4ShsCGJ6TCMeSokjMmloqRmBo8ag4AQyeTMZk3npIfSFqMeSDZbpRDSpWzIVqROLRJLpeqNbpKdTaUqGdqNLotCxaFSRIg1XyzQQObQuYjIQ7GE76CIrl6fQdwsBgMqRqqwF8vlUTWBBgBxVx4XT0gBCAFksABpLAARh5mbAuYLWAATPS5HJeWbMfqdXB+HTk2m3Y26jALrQ4ordAAxQajALOBvdky0KKtpEjseud3k-oa+umuqsQW5YVRUXqS5GqUyjXy2CD6OLPAbpu23Wau8S4838kWmntgJ3+2O51wV3X5dZU9TkxCRf1A2DYDvTRTJI0vWN40TYEO3TKpszzQsSwzdDKxrMA60nRtmw0OdrQrTCu27XtqE0Ad51HcdCLNadZzbSEF3HF9dFXWV10A7pejSegt0hIURTFQ9jRPWUz3gdsVUojViPvZSnxNQDXypd9rU-A02XA39-yYjUoJ9MCfyDOAQxA-YuPJWCo2TGN6UQuzMSTVNULLcji1LHDCzwtCwCwQYvC8+ksGcFM-AAeQAOSwnjGz47sHz00jdDixxRlGYzZWo2iRHoxc8vJFiMo4pdkpvFLZVEWgAC8rOE7dwl3fdDSkrjZIvJyr1KtKdSRVSj3U1K3ytMtdKGgzLKMtzTNAv0LMg0MYNyOC+oQhNdA8ztsPLDDfPw2qiLvDLlX2gaez7Oj2IYic3PKtjhweriksxU7MUYIgCjSM4uXvMS9wktTpPs3IFXk5yALOvThsfUbwdlCaPy-WaXUUj01t9b8A1-azoI+DbHIUuMdqTHzArLEKwr84KotihLay+pTzperKcuu5FbqK+6SqemcKrejTuJqvLun+9Q4CBndxIPMHush89of6rjlIRvTFdFupUZ09GVr-LGTJx8z8cswmwwc+CXIp5Cqfp2nwoZ6L4sS1m6vZpFOdytyCv7PnXoFnXTCFl7KvemrTR4-p+loExdHYFRCQPbQ4BsWwagxUQyggVhBnQdg0FQAASFoqmLkutUYHQ+mWJQwCWL4gA)

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
      x = data_extract_spec(
        dataname = "CO2",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["CO2"]], c("conc", "uptake")),
          selected = "conc",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      y = data_extract_spec(
        dataname = "CO2",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["CO2"]], c("conc", "uptake")),
          selected = "uptake",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      color_by = data_extract_spec(
        dataname = "CO2",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(
            data[["CO2"]],
            c("Plant", "Type", "Treatment", "conc", "uptake")
          ),
          selected = NULL,
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      size_by = data_extract_spec(
        dataname = "CO2",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["CO2"]], c("conc", "uptake")),
          selected = "uptake",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      row_facet = data_extract_spec(
        dataname = "CO2",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["CO2"]], c("Plant", "Type", "Treatment")),
          selected = NULL,
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      col_facet = data_extract_spec(
        dataname = "CO2",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["CO2"]], c("Plant", "Type", "Treatment")),
          selected = NULL,
          multiple = FALSE,
          fixed = FALSE
        )
      )
    )
  )
)
#> Initializing tm_g_scatterplot
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
      x = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1", "BMRKR2")),
          selected = "AGE",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      y = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1", "BMRKR2")),
          selected = "BMRKR1",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      color_by = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(
            data[["ADSL"]],
            c("AGE", "BMRKR1", "BMRKR2", "RACE", "REGION1")
          ),
          selected = NULL,
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      size_by = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
          selected = "AGE",
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      row_facet = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["ADSL"]], c("BMRKR2", "RACE", "REGION1")),
          selected = NULL,
          multiple = FALSE,
          fixed = FALSE
        )
      ),
      col_facet = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Select variable:",
          choices = variable_choices(data[["ADSL"]], c("BMRKR2", "RACE", "REGION1")),
          selected = NULL,
          multiple = FALSE,
          fixed = FALSE
        )
      )
    )
  )
)
#> Initializing tm_g_scatterplot
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
