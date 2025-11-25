# Get stats for x-y pairs in scatterplot matrix

Uses [`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html) per
default for all numerical input variables and converts results to
character vector. Could be extended if different stats for different
variable types are needed. Meant to be called from
[`lattice::panel.text()`](https://rdrr.io/pkg/lattice/man/llines.html).

## Usage

``` r
get_scatterplotmatrix_stats(
  x,
  y,
  .f = stats::cor.test,
  .f_args = list(),
  round_stat = 2,
  round_pval = 4
)
```

## Arguments

- x, y:

  (`numeric`) vectors of data values. `x` and `y` must have the same
  length.

- .f:

  (`function`) function that accepts x and y as formula input `~ x + y`.
  Default [`stats::cor.test`](https://rdrr.io/r/stats/cor.test.html).

- .f_args:

  (`list`) of arguments to be passed to `.f`.

- round_stat:

  (`integer(1)`) optional, number of decimal places to use when rounding
  the estimate.

- round_pval:

  (`integer(1)`) optional, number of decimal places to use when rounding
  the p-value.

## Value

Character with stats. For
[`stats::cor.test()`](https://rdrr.io/r/stats/cor.test.html) correlation
coefficient and p-value.

## Details

Presently we need to use a formula input for
[`stats::cor.test`](https://rdrr.io/r/stats/cor.test.html) because
`na.fail` only gets evaluated when a formula is passed (see below).

    x = c(1,3,5,7,NA)
    y = c(3,6,7,8,1)
    stats::cor.test(x, y, na.action = "na.fail")
    stats::cor.test(~ x + y,  na.action = "na.fail")

## Examples

``` r
set.seed(1)
x <- runif(25, 0, 1)
y <- runif(25, 0, 1)
x[c(3, 10, 18)] <- NA

get_scatterplotmatrix_stats(x, y, .f = stats::cor.test, .f_args = list(method = "pearson"))
#> [1] "cor:0.11\nP:0.6255"
get_scatterplotmatrix_stats(x, y, .f = stats::cor.test, .f_args = list(
  method = "pearson",
  na.action = na.fail
))
#> [1] "NA"
```
