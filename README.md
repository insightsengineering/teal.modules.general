# teal.modules.general

<!-- start badges -->
[![Check 🛠](https://github.com/insightsengineering/teal.modules.general/actions/workflows/check.yaml/badge.svg)](https://insightsengineering.github.io/teal.modules.general/main/unit-test-report/)
[![Docs 📚](https://github.com/insightsengineering/teal.modules.general/actions/workflows/docs.yaml/badge.svg)](https://insightsengineering.github.io/teal.modules.general/)
[![Code Coverage 📔](https://raw.githubusercontent.com/insightsengineering/teal.modules.general/_xml_coverage_reports/data/main/badge.svg)](https://insightsengineering.github.io/teal.modules.general/main/coverage-report/)

![GitHub forks](https://img.shields.io/github/forks/insightsengineering/teal.modules.general?style=social)
![GitHub repo stars](https://img.shields.io/github/stars/insightsengineering/teal.modules.general?style=social)

![GitHub commit activity](https://img.shields.io/github/commit-activity/m/insightsengineering/teal.modules.general)
![GitHub contributors](https://img.shields.io/github/contributors/insightsengineering/teal.modules.general)
![GitHub last commit](https://img.shields.io/github/last-commit/insightsengineering/teal.modules.general)
![GitHub pull requests](https://img.shields.io/github/issues-pr/insightsengineering/teal.modules.general)
![GitHub repo size](https://img.shields.io/github/repo-size/insightsengineering/teal.modules.general)
![GitHub language count](https://img.shields.io/github/languages/count/insightsengineering/teal.modules.general)
[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Current Version](https://img.shields.io/github/r-package/v/insightsengineering/teal.modules.general/main?color=purple\&label=package%20version)](https://github.com/insightsengineering/teal.modules.general/tree/main)
[![Open Issues](https://img.shields.io/github/issues-raw/insightsengineering/teal.modules.general?color=red\&label=open%20issues)](https://github.com/insightsengineering/teal.modules.general/issues?q=is%3Aissue+is%3Aopen+sort%3Aupdated-desc)
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

Please see [`teal` gallery](https://github.com/insightsengineering/teal.gallery) and [TLG Catalog](https://github.com/insightsengineering/tlg-catalog) to see examples of `teal` apps with modules from this package.

## Installation

From July 2023 `insightsengineering` packages are available on [r-universe](https://r-universe.dev/).

```r
# stable versions
install.packages('teal.modules.general', repos = c('https://insightsengineering.r-universe.dev', 'https://cloud.r-project.org'))

# beta versions
install.packages('teal.modules.general', repos = c('https://pharmaverse.r-universe.dev', 'https://cloud.r-project.org'))
```

See package vignettes `browseVignettes(package = "teal.modules.general")` for usage of this package.

## Acknowledgment

This package is a result of a joint efforts by many developers and stakeholders. We would like to thank everyone who contributed so far!

[![Stargazers repo roster for @insightsengineering/teal.modules.general](https://reporoster.com/stars/insightsengineering/teal.modules.general)](https://github.com/insightsengineering/teal.modules.general/stargazers)
[![Forkers repo roster for @insightsengineering/teal.modules.general](https://reporoster.com/forks/insightsengineering/teal.modules.general)](https://github.com/insightsengineering/teal.modules.general/network/members)

## Stargazers over time

[![Stargazers over time](https://starchart.cc/insightsengineering/teal.modules.general.svg)](https://starchart.cc/insightsengineering/teal.modules.general)
