# `teal` module: Missing data analysis

This module analyzes missing data in `data.frame`s to help users explore
missing observations and gain insights into the completeness of their
data. It is useful for clinical data analysis within the context of
`CDISC` standards and adaptable for general data analysis purposes.

## Usage

``` r
tm_missing_data(
  label = "Missing data",
  plot_height = c(600, 400, 5000),
  plot_width = NULL,
  datanames = "all",
  parent_dataname = "ADSL",
  ggtheme = c("classic", "gray", "bw", "linedraw", "light", "dark", "minimal", "void"),
  ggplot2_args = list(`Combinations Hist` = teal.widgets::ggplot2_args(labs =
    list(caption = NULL)), `Combinations Main` = teal.widgets::ggplot2_args(labs =
    list(title = NULL))),
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

- plot_height:

  (`numeric`) optional, specifies the plot height as a three-element
  vector of `value`, `min`, and `max` intended for use with a slider UI
  element.

- plot_width:

  (`numeric`) optional, specifies the plot width as a three-element
  vector of `value`, `min`, and `max` for a slider encoding the plot
  width.

- datanames:

  (`character`) Names of the datasets relevant to the item. There are 2
  reserved values that have specific behaviors:

  - The keyword `"all"` includes all datasets available in the data
    passed to the teal application.

  - `NULL` hides the sidebar panel completely.

  - If `transformators` are specified, their `datanames` are
    automatically added to this `datanames` argument.

- parent_dataname:

  (`character(1)`) Specifies the parent dataset name. Default is `ADSL`
  for `CDISC` data. If provided and exists, enables additional analysis
  "by subject". For non-`CDISC` data, this parameter can be ignored.

- ggtheme:

  (`character`) optional, specifies the default `ggplot2` theme for
  plots. Defaults to `classic`.

- ggplot2_args:

  (`ggplot2_args`) optional, object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for all the plots or named list of `ggplot2_args`
  objects for plot-specific settings. The argument is merged with
  options variable `teal.ggplot2_args` and default module setup.

  List names should match the following:
  `c("default", "Summary Obs", "Summary Patients", "Combinations Main", "Combinations Hist", "By Subject")`.

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
place using decorators:

- `summary_plot` (`ggplot`)

- `combination_plot` (`grob` created with
  [`ggplot2::ggplotGrob()`](https://ggplot2.tidyverse.org/reference/ggplotGrob.html))

- `by_subject_plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_missing_data(
       ..., # arguments for module
       decorators = list(
         summary_plot = teal_transform_module(...), # applied only to `summary_plot` output
         combination_plot = teal_transform_module(...), # applied only to `combination_plot` output
         by_subject_plot = teal_transform_module(...) # applied only to `by_subject_plot` output
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrquuAAesKgiuvxQpFBK4ZEGxlzUAPoxUDbREVBxugDutKQAFirsKbi6IEq6uoxwAI6CtNXsPqKkxNREjIoQFbpQ-PyJ0KJZZhaa1gG25RCVlQHAosEi7KK1idwkEuyTpSbtHewiEBIF27YAVFXmtCbsAIylGAAMAKyPdwDsct8AulkAcgBBHpzHoAXzcM10DVowyM0MYsJ6MFaLDhxhRBDRkMqMNEwD+8OoaBCHDxpT6AyGXUqmLRBKyxPQ1A4dMYogp-UGUFENN0bPxwAUhFY1GFP0Jxh5GBM6lIBwFwCFIrFYAlfMVyqkLHFkt64llGgVqPZSuF2sYuq6YK6ShJWRUeXYPRSugAvGEMrhkQJhGJ3fzfSJRM6oZVSDBEjBYaIVBJkhl2KgWBRSAnItB4AHhQLhXyul0brp2CpyMxLDobFMeqJChBWID0OwSQASeoU9At1aMHSdJQQiBgME-IA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dOAAPWFQRXX4oUiglGLiDYy5qAH1EqBsE2Khk3QB3WlIACxV2TNxdECVdXUY4AEdBWgb2CDFSYmoiRkUIOoBBHwCAGXzUjEzEREZhsdrdYawA-LmfFaUAX36AKyIVNIBrOFZRCtzbI2i4EyhhUjSCflpRAjT9w5Oz4Gh4c8ycgAum5oOh8ipiuxFpldABeaK5XCLPhCESieG6VHCMTQgZ1XSkGBpGCvUQqCQZXLZfH9fq0Ey6dgqcjMSw6Gy2Gr40RlCCsQbodhoVAAEhaVRFotEMh0fW2SjAWyBQA)

## Examples

``` r
# general example data
data <- teal_data()
data <- within(data, {
  require(nestcolor)

  add_nas <- function(x) {
    x[sample(seq_along(x), floor(length(x) * runif(1, .05, .17)))] <- NA
    x
  }

  iris <- iris
  mtcars <- mtcars

  iris[] <- lapply(iris, add_nas)
  mtcars[] <- lapply(mtcars, add_nas)
  mtcars[["cyl"]] <- as.factor(mtcars[["cyl"]])
  mtcars[["gear"]] <- as.factor(mtcars[["gear"]])
})

app <- init(
  data = data,
  modules = modules(
    tm_missing_data(parent_dataname = "mtcars")
  )
)
#> Initializing tm_missing_data
if (interactive()) {
  shinyApp(app$ui, app$server)
}

# CDISC example data
data <- teal_data()
data <- within(data, {
  require(nestcolor)
  ADSL <- teal.data::rADSL
  ADRS <- rADRS
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_missing_data()
  )
)
#> Initializing tm_missing_data
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
