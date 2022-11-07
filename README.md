# teal.modules.general

<!-- start badges -->
[![Code Coverage](https://raw.githubusercontent.com/insightsengineering/teal.modules.general/_xml_coverage_reports/data/main/badge.svg)](https://raw.githubusercontent.com/insightsengineering/teal.modules.general/_xml_coverage_reports/data/main/coverage.xml)
<!-- end badges -->

This package contains a set of standard `teal` modules for you to include in applications.
These include modules for:

<!-- markdownlint-disable MD007 MD030 -->
-   viewing data (including `tm_variable_browser`, `tm_data_table`, `tm_file_viewer`, ...)
-   visualizing data (`tm_g_scatterplot`, `tm_g_association`, ...)
-   understanding missing and outlier values within your data (`tm_missing_data`, `tm_outliers`, ...)
-   performing simple data analysis (`tm_a_pca`, `tm_g_distribution`, `tm_a_regression`, ...).
<!-- markdownlint-enable MD007 MD030 -->

These modules work with `CDISC` data, independent datasets and general relational data.

## Installation

For releases from August 2022 it is recommended that you [create and use a Github PAT](https://docs.github.com/en/github/authenticating-to-github/keeping-your-account-and-data-secure/creating-a-personal-access-token) to install the latest version of this package. Once you have the PAT, run the following:

```r
Sys.setenv(GITHUB_PAT = "your_access_token_here")
if (!require("remotes")) install.packages("remotes")
remotes::install_github("insightsengineering/teal.modules.general@*release")
```

In order to run many of the examples you will also need to install the [`scda`](https://insightsengineering.github.io/scda/) package.

A stable release of all `NEST` packages from June 2022 is also available [here](https://github.com/insightsengineering/depository#readme).

See package vignettes `browseVignettes(package = "teal.modules.general")` for usage of this package.

## Acknowledgment

This package is a result of a joint efforts by many developers and stakeholders. We would like to thank everyone who contributed so far!

[![Stargazers repo roster for @insightsengineering/teal.modules.general](https://reporoster.com/stars/insightsengineering/teal.modules.general)](https://github.com/insightsengineering/teal.modules.general/stargazers)
[![Forkers repo roster for @insightsengineering/teal.modules.general](https://reporoster.com/forks/insightsengineering/teal.modules.general)](https://github.com/insightsengineering/teal.modules.general/network/members)

## Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.modules.general.svg)](https://starchart.cc/insightsengineering/teal.modules.general)
