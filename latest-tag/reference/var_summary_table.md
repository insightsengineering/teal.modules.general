# Summarizes variable

Creates html summary with statistics relevant to data type. For numeric
values it returns central tendency measures, for factor returns level
counts, for Date date range, for other just number of levels.

## Usage

``` r
var_summary_table(x, numeric_as_factor, dt_rows, outlier_definition)
```

## Arguments

- x:

  vector of any type

- numeric_as_factor:

  `logical` should the numeric variable be treated as a factor

- dt_rows:

  `numeric` current/latest `DT` page length

- outlier_definition:

  If 0 no outliers are removed, otherwise outliers (those more than
  `outlier_definition*IQR below/above Q1/Q3` be removed)

## Value

text with simple statistics.
