# `teal` module: Scatterplot and regression analysis

Module for visualizing regression analysis, including scatterplots and
various regression diagnostics plots. It allows users to explore the
relationship between a set of regressors and a response variable,
visualize residuals, and identify outliers.

## Usage

``` r
tm_a_regression(
  label = "Regression Analysis",
  regressor,
  response,
  plot_height = c(600, 200, 2000),
  plot_width = NULL,
  alpha = c(1, 0, 1),
  size = c(2, 1, 8),
  ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
  ggplot2_args = teal.widgets::ggplot2_args(),
  pre_output = NULL,
  post_output = NULL,
  default_plot_type = 1,
  default_outlier_label = "USUBJID",
  label_segment_threshold = c(0.5, 0, 10),
  transformators = list(),
  decorators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- regressor:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  Regressor variables from an incoming dataset with filtering and
  selecting.

- response:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  Response variables from an incoming dataset with filtering and
  selecting.

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

- size:

  (`integer(1)` or `integer(3)`) optional, specifies point size.

  - When the length of `size` is one: the plot point sizes will have a
    fixed size.

  - When the length of `size` is three: the plot points size are
    dynamically adjusted based on vector of `value`, `min`, and `max`.

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
  `c("default", "Response vs Regressor", "Residuals vs Fitted", "Scale-Location", "Cook's distance", "Residuals vs Leverage", "Cook's dist vs Leverage")`.

  For more details see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

- pre_output:

  (`shiny.tag`) optional, text or UI element to be displayed before the
  module's output, providing context or a title. with text placed before
  the output to put the output into context. For example a title.

- post_output:

  (`shiny.tag`) optional, text or UI element to be displayed after the
  module's output, adding context or further instructions. Elements like
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html) are
  useful.

- default_plot_type:

  (`numeric`) optional, defaults to "Response vs Regressor".

  1.  Response vs Regressor

  2.  Residuals vs Fitted

  3.  Normal Q-Q

  4.  Scale-Location

  5.  Cook's distance

  6.  Residuals vs Leverage

  7.  Cook's dist vs Leverage

- default_outlier_label:

  (`character`) optional, default column selected to label outliers.

- label_segment_threshold:

  (`numeric(1)` or `numeric(3)`) Minimum distance between label and
  point on the plot that triggers the creation of a line segment between
  the two. This may happen when the label cannot be placed next to the
  point as it overlaps another label or point. The value is used as the
  `min.segment.length` parameter to the
  [`ggrepel::geom_text_repel()`](https://ggrepel.slowkow.com/reference/geom_text_repel.html)
  function.

  It can take the following forms:

  - `numeric(1)`: Fixed value used for the minimum distance and the
    slider is not presented in the UI.

  - `numeric(3)`: A slider is presented in the UI (under "Plot
    settings") to adjust the minimum distance dynamically.

    It takes the form of `c(value, min, max)` and it is passed to the
    `value_min_max` argument in
    [`teal.widgets::optionalSliderInputValMinMax`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/optionalSliderInputValMinMax.html).

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

For more examples, please see the vignette "Using regression plots" via
[`vignette("using-regression-plots", package = "teal.modules.general")`](https://insightsengineering.github.io/teal.modules.general/articles/using-regression-plots.md).

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_a_regression(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokrcQAxLqkQ11XfxQpFC6cAAesKgiSoHBBsZc1AD6sVA2MUEhRroA7rSkABYq7Km4uiBKurqMcACOgrQ17L6ipMTURIyKEFUAwgDyAEzxugODSgC+3UpoqCMq+eyVAZm6ALwrwbjLfEIiouu6u8JiSz1VuqQwSVBJNRI1oqJWEGcXF9RQ9HD+GwpgWDgDzEzxI-2253ej1QJFEeg2qSS4VIzA0SVEqDgBDe7wuqWg8EO-zG4OWuN0cJEGkOlKxpHRmOxZPJuk+31+un+AGUfnTdFoWLQviJEKTISyCAUiLQCGIiWBBKhggBrOBilkXWkaOD8eWKlVqvDM8kwYSaKLw3QAMQAggAZLnOCEaqomWhhHWHBwuY0XbrkuTO3H3R6iTqHRHI1H0jFYnHk-GwS3Eobqlla0g03lo2NM8Xktk-eU8qmZgWMIX0faio353GS6Wyg4bcuVkRJBsy06pYDAFPjMAAXUHZSZhBIBHBnLAdhqQXgZH+ckDvveGc9f3HFjTGtN1HNIi9Tidq4ubo9uo2todrjrVX9uIf9+W3W6tBMunYKnI0e0cBstgVOcohFBArA2ug7CzAAJA0ZQwXCjA6F0kxKGAEyDkAA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dfihSKF04AA9YVBElUPCDYy5qAH1EqBsEsIijXQB3WlIACxV2TNxdECVdXUY4AEdBWgb2CDFSYmoiRkUIOoBBHwCAGWTdVIxMxERGYbGlAF9+gCsiFTSAazhWUQrc23z+OBMoYVI0gn5aUQI09c2dveBoeH3MuQBdN2h0CZUxXYtRCuV0AF5QeFcCC+EIRKIIbo4cIxMCBnVJjA0lA0g0JA1RKIrBB0ZjMdQoPQ4NQkQowFg4ASxMSSPSYRjyYTUCRRHpIZk0tFSMwNGlRKg4AQyeTMZk3vzdPSFqN2SDZbo+SINEitVLLhKpTKNXVKdTaZD6QEafrdFoWLQqSJEGrOSaCCUNgQxHSwAAhACyWAA0lgAIyuk2YvUaOD8X2BkPhyNR5EXWhxRUAMUGowCzg5qdMtCicaRDhc6tl-Q1ckLsvxhNEvSRguFooNkulVfJ8tgiuVI1VeB70ZtOshMc7RtH5LNNN91u1pDtDqdYhdI7dGo9Xp9kPtjEd9BEV09tG9H1ywGAg8WYC+Xyq3bAgwA4q48EqwPmABrs78sEGLxPzkOtZzHZcy0tV8PxTKMYHTTNyycAsILqEwS2g3QczzVxt0xGtqyrIj+n6WgTF0dgVHIDttDgGxbBqDFRDKCBWEGdB2DQVAABIWiqHjeL5RgdD6ZYlDAJYviAA)

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
    tm_a_regression(
      label = "Regression",
      response = data_extract_spec(
        dataname = "CO2",
        select = select_spec(
          label = "Select variable:",
          choices = "uptake",
          selected = "uptake",
          multiple = FALSE,
          fixed = TRUE
        )
      ),
      regressor = data_extract_spec(
        dataname = "CO2",
        select = select_spec(
          label = "Select variables:",
          choices = variable_choices(data[["CO2"]], c("conc", "Treatment")),
          selected = "conc",
          multiple = TRUE,
          fixed = FALSE
        )
      )
    )
  )
)
#> Initializing tm_a_regression
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
    tm_a_regression(
      label = "Regression",
      response = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Select variable:",
          choices = "BMRKR1",
          selected = "BMRKR1",
          multiple = FALSE,
          fixed = TRUE
        )
      ),
      regressor = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          label = "Select variables:",
          choices = variable_choices(data[["ADSL"]], c("AGE", "SEX", "RACE")),
          selected = "AGE",
          multiple = TRUE,
          fixed = FALSE
        )
      )
    )
  )
)
#> Initializing tm_a_regression
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
