
<!-- README.md is generated from README.Rmd. Please edit that file -->

# teal.modules.general

<!-- badges: start -->
<!-- badges: end -->

This package contains a set of standard `teal` modules for you to
include in your `teal` applications. These include modules for:

-   viewing data (including `tm_variable_browser`, `tm_data_table` and
    `tm_file_viewer`)
-   visualizing data (`tm_g_scatterplot`, `tm_g_association` and many
    more)
-   understanding missing and outlier values within your data
    (`tm_missing_data` and `tm_outliers`)
-   performing simple data analysis (`tm_a_pca`, `tm_g_distribution` and
    `tm_a_regression`)

These modules work with both `CDISC` and general relational data.

## Installation

This repository requires a personal access token to install see here
[creating and using
PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token).
Once this is set up, to install the latest released version of the
package run:

``` r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
install.packages("devtools")
devtools::install_github("insightsengineering/teal.modules.general@*release")
```

\<\< Some comment about handling dependencies here \>\>

## Example

This is a basic example app showing a few modules from this package

``` r
library(teal.modules.general)
#> Loading required package: ggmosaic
#> Warning: package 'ggmosaic' was built under R version 4.1.1
#> Loading required package: ggplot2
#> Loading required package: magrittr
#> Loading required package: shiny
#> Warning: package 'shiny' was built under R version 4.1.2
#> Loading required package: shinyTree
#> Warning: package 'shinyTree' was built under R version 4.1.1
#> Loading required package: teal
#> 
#> You are using teal version 0.10.1.9006

app <- init(
  data = teal_data(
    dataset("IRIS", x = iris)
  ),
  modules = modules(
    tm_data_table(),
    tm_variable_browser(),
    tm_t_crosstable(
      x = teal::data_extract_spec("IRIS",
        select = teal::select_spec(
          choices = c("Sepal.Length", "Sepal.Width"),
          multiple= TRUE
        )
      ),
      y = teal::data_extract_spec("IRIS",
        select = teal::select_spec(
          choices = "Species",
          multiple = FALSE,
          fixed = TRUE
        )
      )
    )
  )
)
#> [INFO] 2022-02-18 13:12:19.8448 pid:15184 token:[] teal.modules.general Initializing tm_data_table
#> [INFO] 2022-02-18 13:12:19.8499 pid:15184 token:[] teal.modules.general Initializing tm_variable_browser
#> [INFO] 2022-02-18 13:12:19.8729 pid:15184 token:[] teal.modules.general Initializing tm_t_crosstable
```

Which is run using:

``` r
runApp(app)
```

# Further Information

See package vignettes
`browseVignettes(package = "teal.modules.general")` for examples using
other modules from this package.
