# `teal` module: GT Summary table

Summary table from a given dataset, using `gtsummary`.

## Usage

``` r
tm_gtsummary(
  label = "Summary table",
  by,
  include,
  ...,
  col_label = NULL,
  pre_output = NULL,
  post_output = NULL,
  transformators = list(),
  decorators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- by:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`) An
  object with all available choices and with a pre-selected option on
  how to split rows.

  `data_extract_spec` multiple selection: not allowed

- include:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`) An
  object with all available choices and with a pre-selected option that
  picks columns to include as rows.

  `data_extract_spec` multiple selection: allowed

- ...:

  Arguments passed on to
  [`crane::tbl_roche_summary`](https://insightsengineering.github.io/crane/latest-tag/reference/tbl_roche_summary.html)

  `statistic`

  :   ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.html))  
      Specifies summary statistics to display for each variable. The
      default is
      `list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)")`.
      See below for details.

  `digits`

  :   ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.html))  
      Specifies how summary statistics are rounded. Values may be either
      integer(s) or function(s). If not specified, default formatting is
      assigned via `assign_summary_digits()`. See below for details.

  `type`

  :   ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.html))  
      Specifies the summary type. Accepted value are
      `c("continuous", "continuous2", "categorical", "dichotomous")`. If
      not specified, default type is assigned via
      `assign_summary_type()`. See below for details.

  `value`

  :   ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.html))  
      Specifies the level of a variable to display on a single row. The
      gtsummary type selectors, e.g. `all_dichotomous()`, cannot be used
      with this argument. Default is `NULL`. See below for details.

  `nonmissing,nonmissing_text,nonmissing_stat`

  :   Arguments dictating how and if missing values are presented:

      - `nonmissing`: must be one of `c("always", "ifany", "no")`

      - `nonmissing_text`: string indicating text shown on non-missing
        row. Default is `"n"`

      - `nonmissing_stat`: statistic to show on non-missing row. Default
        is `"{N_nonmiss}"`. Possible values are `N_nonmiss`, `N_miss`,
        `N_obs`, `p_nonmiss` `p_miss`.

  `sort`

  :   ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.html))  
      Specifies sorting to perform for categorical variables. Values
      must be one of `c("alphanumeric", "frequency")`. Default is
      `all_categorical(FALSE) ~ "alphanumeric"`.

- col_label:

  Used to override default labels in summary table, e.g.
  `list(age = "Age, years")`. The default for each variable is the
  column label attribute, `attr(., 'label')`. If no label has been set,
  the column name is used.

- pre_output:

  (`shiny.tag`) optional, text or UI element to be displayed before the
  module's output, providing context or a title. with text placed before
  the output to put the output into context. For example a title.

- post_output:

  (`shiny.tag`) optional, text or UI element to be displayed after the
  module's output, adding context or further instructions. Elements like
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html) are
  useful.

- transformators:

  (`list` of `teal_transform_module`) that will be applied to transform
  module's data input. To learn more check
  [`vignette("transform-input-data", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).

- decorators:

  **\[experimental\]** (named `list` of lists of
  `teal_transform_module`) optional, decorator for tables or plots
  included in the module output reported. The decorators are applied to
  the respective output objects.

  See section "Decorating Module" below for more details.

## Value

Object of class `teal_module` to be used in `teal` applications.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `table` (`gtsummary` - output of
  [`crane::tbl_roche_summary()`](https://insightsengineering.github.io/crane/latest-tag/reference/tbl_roche_summary.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_gtsummary(
       ..., # arguments for module
       decorators = list(
         table = teal_transform_module(...) # applied to the `table` output
       )
    )

For additional details and examples of decorators, refer to the vignette
[`vignette("decorate-module-output", package = "teal.modules.general")`](https://insightsengineering.github.io/teal.modules.general/articles/decorate-module-output.md).

To learn more please refer to the vignette
[`vignette("transform-module-output", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-module-output.html)
or the
[`teal::teal_transform_module()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)
documentation.

## statistic argument

The statistic argument specifies the statistics presented in the table.
The input dictates the summary statistics presented in the table. For
example, `statistic = list(age ~ "{mean} ({sd})")` would report the mean
and standard deviation for age;
`statistic = list(all_continuous() ~ "{mean} ({sd})")` would report the
mean and standard deviation for all continuous variables.

The values are interpreted using
[`glue::glue()`](https://glue.tidyverse.org/reference/glue.html) syntax:
a name that appears between curly brackets will be interpreted as a
function name and the formatted result of that function will be placed
in the table.

For categorical variables, the following statistics are available to
display: `{n}` (frequency), `{N}` (denominator), `{p}` (percent).

For continuous variables, **any univariate function may be used**. The
most commonly used functions are `{median}`, `{mean}`, `{sd}`, `{min}`,
and `{max}`. Additionally, `{p##}` is available for percentiles, where
`##` is an integer from 0 to 100. For example,
`p25: quantile(probs=0.25, type=2)`.

When the summary type is `"continuous2"`, pass a vector of statistics.
Each element of the vector will result in a separate row in the summary
table.

For both categorical and continuous variables, statistics on the number
of missing and non-missing observations and their proportions are
available to display.

- `{N_obs}` total number of observations

- `{N_miss}` number of missing observations

- `{N_nonmiss}` number of non-missing observations

- `{p_miss}` percentage of observations missing

- `{p_nonmiss}` percentage of observations not missing

## digits argument

The digits argument specifies the the number of digits (or formatting
function) statistics are rounded to.

The values passed can either be a single integer, a vector of integers,
a function, or a list of functions. If a single integer or function is
passed, it is recycled to the length of the number of statistics
presented. For example, if the statistic is `"{mean} ({sd})"`, it is
equivalent to pass `1`, `c(1, 1)`, `label_style_number(digits=1)`, and
`list(label_style_number(digits=1), label_style_number(digits=1))`.

Named lists are also accepted to change the default formatting for a
single statistic, e.g. `list(sd = label_style_number(digits=1))`.

## type and value arguments

There are four summary types. Use the `type` argument to change the
default summary types.

- `"continuous"` summaries are shown on a *single row*. Most numeric
  variables default to summary type continuous.

- `"continuous2"` summaries are shown on *2 or more rows*

- `"categorical"` *multi-line* summaries of nominal data. Character
  variables, factor variables, and numeric variables with fewer than 10
  unique levels default to type categorical. To change a numeric
  variable to continuous that defaulted to categorical, use
  `type = list(varname ~ "continuous")`

- `"dichotomous"` categorical variables that are displayed on a *single
  row*, rather than one row per level of the variable. Variables coded
  as `TRUE`/`FALSE`, `0`/`1`, or `yes`/`no` are assumed to be
  dichotomous, and the `TRUE`, `1`, and `yes` rows are displayed.
  Otherwise, the value to display must be specified in the `value`
  argument, e.g. `value = list(varname ~ "level to show")`

## Reporting

This module returns an object of class `teal_module`, that contains a
`server` function. Since the server function returns a `teal_report`
object, this makes this module reportable, which means that the
reporting functionality will be turned on automatically by the `teal`
framework.

For more information on reporting in `teal`, see the vignettes:

- [`vignette("reportable-shiny-application", package = "teal.reporter")`](https://insightsengineering.github.io/teal.reporter/latest-tag/articles/reportable-shiny-application.html)

- `vignette("adding-support-for-reporting-to-custom-modules", package = "teal")`

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqX8opKAeMB3WqQAFiqccNwYHl6IiFzUAPqRUDa4uiBKuroAggAiAMoAMj66sRGeUNGMOQVKAL6KEABWRCpxANZwrKLsibZGuvxwJlDCpHEE-LSiBHFNLe2dwNDwXT0AukpoqEUqAezp-WW6ALwHXrj7fEIiose6l8JiexAZGaQwcRKkooIwMCwc+xeunorFuJVIzAgohMREYMGiiTicAAHhD1KNRKg4AQnkCgYklnoTgowFV8iTzs88RlRHARBowWEeGioTC4dFafSMVicYDqboCIFmgQxLdeWBcs4ABoU3QkgDCAHlHAA5BwATVlJNyAEk7M4ddktaT5XZMlgALIkuSU-kvTnY8j8W7a6UUvnUmAjWioES3ABimXyko9L3q1JtoZUBGoggGjPCLOhsPhiERKLRGjimOxuP5BNgRLlpLy5LwoZpdMdCeZkOT7MQDqzOd5VLtguFopO4t1+sNxqVqo1xsypvNVrAkbb-KbTpdEr1BqN5ennu9vqLDhctrtployLgzpOgeDrlXYdD4YvVPq9VoJl07BU5GYlh0NlsaSpomCEFYmXQdhNgAEkEWgUhA2lGB0Rh6hqMAalWIA)

## Examples

``` r
data <- within(teal.data::teal_data(), {
  ADSL <- teal.data::rADSL
})
join_keys(data) <- default_cdisc_join_keys[names(data)]
app <- init(
  data = data,
  modules = modules(
    tm_gtsummary(
      by = teal.transform::data_extract_spec(
        dataname = "ADSL",
        select = teal.transform::select_spec(
          choices = c("SEX", "COUNTRY", "SITEID", "ACTARM"),
          selected = "SEX",
          multiple = FALSE
        )
      ),
      include = teal.transform::data_extract_spec(
        dataname = "ADSL",
        select = teal.transform::select_spec(
          choices = c("SITEID", "COUNTRY", "ACTARM"),
          selected = "SITEID",
          multiple = TRUE,
          fixed = FALSE
        )
      )
    )
  )
)
#> Initializing tm_gtsummary
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
