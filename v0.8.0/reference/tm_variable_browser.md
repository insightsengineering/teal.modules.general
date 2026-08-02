# `teal` module: Variable browser

Module provides provides a detailed summary and visualization of
variable distributions for `data.frame` objects, with interactive
features to customize analysis.

## Usage

``` r
tm_variable_browser(
  label = "Variable Browser",
  datasets_selected = deprecated(),
  datanames = if (missing(datasets_selected)) "all" else datasets_selected,
  parent_dataname = "ADSL",
  pre_output = NULL,
  post_output = NULL,
  ggplot2_args = teal.widgets::ggplot2_args(),
  transformators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- datasets_selected:

  (`character`) **\[deprecated\]** vector of datasets to show, please
  use the `datanames` argument.

- datanames:

  (`character`) Names of the datasets relevant to the item. There are 2
  reserved values that have specific behaviors:

  - The keyword `"all"` includes all datasets available in the data
    passed to the teal application.

  - `NULL` hides the sidebar panel completely.

  - If `transformators` are specified, their `datanames` are
    automatically added to this `datanames` argument.

- parent_dataname:

  (`character(1)`) string specifying a parent dataset. If it exists in
  `datanames` then an extra checkbox will be shown to allow users to not
  show variables in other datasets which exist in this `dataname`. This
  is typically used to remove `ADSL` columns in `CDISC` data. In non
  `CDISC` data this can be ignored. Defaults to `"ADSL"`.

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

## Value

Object of class `teal_module` to be used in `teal` applications.

## Details

Numeric columns with fewer than 30 distinct values can be treated as
either discrete or continuous with a checkbox allowing users to switch
how they are treated(if \< 6 unique values then the default is discrete,
otherwise it is continuous).

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6urSMtKJxVTWiFbowpAQsdVmt7YxNEJXZRPD9WYPDzSZQeflmflmT07PNAMIA8gBM9WvrSgC+ihBKaKj1KnnszSm6ALz+GbjNfEIidbdPwmIX-ZW6pDCJWhYtCg9BEiSYRGyohkXx+P2oILgfluCjAADUgSCRLoIVCZKjmpUDkSlAdaCZdOwVORmJYdDZbOVvqJChBWABBdDsY4AEkEtFKvOhjB0jAOuyUYF2AF0gA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dOAAPWFQRXX4oUiglOiYWDlFUFgBrOgg4RQgYuINjLmoAfXyoGyVywt0Ad1pSAAsVdnLcXRAlXV0AQR8AgBka4oxyxERGPsGu3p87O2dhuG5R2KhxybmFpQBfXIArIhUS9LhWUVa12yNouBMoYVISgn5aUQISw+PT8+BoeAu5TkAF03NB0DUVA12DNqgBeaJrXAzPhCESiXQI1HCMQwiDdbqkGAlLQsWhQegiEpMIi1UQyPEEgnUClwaiY3QKMAANTJFKiNLpMi5M26uTFSlytBMunYKnIzEsOhstk6+N0omaEFYPXQ7DQqAAJIJaO0DYb6YwdIxcjslGAdsCgA)

## Examples

``` r
# general data example
data <- teal_data()
data <- within(data, {
  iris <- iris
  mtcars <- mtcars
  women <- women
  faithful <- faithful
  CO2 <- CO2
})

app <- init(
  data = data,
  modules = modules(
    tm_variable_browser(
      label = "Variable browser"
    )
  )
)
#> Initializing tm_variable_browser
if (interactive()) {
  shinyApp(app$ui, app$server)
}

# CDISC example data
library(sparkline)
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
  ADTTE <- teal.data::rADTTE
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_variable_browser(
      label = "Variable browser"
    )
  )
)
#> Initializing tm_variable_browser
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
