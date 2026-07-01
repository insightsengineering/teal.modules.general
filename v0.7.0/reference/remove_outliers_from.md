# Removes the outlier observation from an array

Removes the outlier observation from an array

## Usage

``` r
remove_outliers_from(var, outlier_definition)
```

## Arguments

- var:

  (`numeric`) a numeric vector

- outlier_definition:

  (`numeric`) if `0` then no outliers are removed, otherwise outliers
  (those more than `outlier_definition*IQR below/above Q1/Q3`) are
  removed

## Value

(`numeric`) vector without the outlier values
