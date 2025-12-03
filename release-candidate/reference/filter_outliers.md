# Logical vector

Returns a logical vector. Suitable for
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
and data.frames.

## Usage

``` r
filter_outliers(var, outlier_definition)
```

## Arguments

- var:

  (`numeric`) a numeric vector

- outlier_definition:

  (`numeric`) if `0` then no outliers are removed, otherwise outliers
  (those more than `outlier_definition*IQR below/above Q1/Q3`) are
  removed
