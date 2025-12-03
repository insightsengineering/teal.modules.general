# Plot variable

Creates summary plot with statistics relevant to data type.

## Usage

``` r
plot_var_summary(
  qenv,
  wrap_character = NULL,
  numeric_as_factor,
  display_density = FALSE,
  remove_NA_hist = FALSE,
  outlier_definition,
  records_for_factor,
  ggplot2_args
)
```

## Arguments

- qenv:

  teal_data object where code should be evaluated.

- wrap_character:

  (`numeric`) number of characters at which to wrap text values of `var`

- numeric_as_factor:

  (`logical`) should the numeric variable be treated as a factor

- display_density:

  (`logical`) should density estimation be displayed for numeric values

- remove_NA_hist:

  (`logical`) should `NA` values be removed for histogram of factor like
  variables

- outlier_definition:

  if 0 no outliers are removed, otherwise outliers (those more than
  outlier_definition\*IQR below/above Q1/Q3 be removed)

- records_for_factor:

  (`numeric`) if the number of factor levels is \>= than this value then
  a graph of the factors isn't shown, only a list of values

- ggplot2_args:

  (`ggplot2_args`) object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for the module plot. The argument is merged with options
  variable `teal.ggplot2_args` and default module setup.

  For more details see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html)

## Value

plot
