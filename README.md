# teal.modules.general

This package contains a set of standard `teal` modules for you to include in your `teal` applications.
These include modules for:

- viewing data (including `tm_variable_browser`, `tm_data_table` and `tm_file_viewer`)
- visualizing data (`tm_g_scatterplot`, `tm_g_association` and many more)
- understanding missing and outlier values within your data (`tm_missing_data` and `tm_outliers`)
- performing simple data analysis (`tm_a_pca`, `tm_g_distribution` and `tm_a_regression`).

These modules work with both `CDISC` and general relational data.  

## Installation

This repository requires a personal access token to install see here [creating and using PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token). Once this is set up, to install the latest released version of the package run:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
install.packages("devtools")
devtools::install_github("insightsengineering/teal.modules.general@*release", dependencies = FALSE)
```

Currently, it is necessary to manually install all of the package dependencies before installing this package as without
the `dependencies = FALSE` argument to the `install_github` function it errors. 

See package vignettes `browseVignettes(package = "teal.modules.general")` for usage of this package.
