# `teal` module: GT Summary table

Summary table from a given dataset, using `gtsummary`.

## Usage

``` r
tm_tbl_summary(
  label = "Summary table",
  by = NULL,
  include = teal.picks::picks(teal.picks::datasets(), teal.picks::variables(selected =
    dplyr::everything(), multiple = TRUE)),
  dataname = NULL,
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

  (`picks` or
  [dplyr::dplyr_tidy_select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))
  When using picks object it should have
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  to identify a single column from the data.

- include:

  (`picks` or
  [dplyr::dplyr_tidy_select](https://dplyr.tidyverse.org/reference/dplyr_tidy_select.html))
  When using picks object it should have
  [`teal.picks::variables()`](https://insightsengineering.github.io/teal.picks/latest-tag/reference/picks.html)
  to identify one or more columns from the data.

- dataname:

  (`string` or `NULL`) Name of the dataset to be used in the module if
  and only if `picks` is not used for other arguments.

- ...:

  Arguments passed on to
  [`gtsummary::tbl_summary`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html)

  `statistic`

  : ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.html))\
    Specifies summary statistics to display for each variable. The
    default is
    `list(all_continuous() ~ "{median} ({p25}, {p75})", all_categorical() ~ "{n} ({p}%)")`.
    See below for details.

  `digits`

  : ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.html))\
    Specifies how summary statistics are rounded. Values may be either
    integer(s) or function(s). If not specified, default formatting is
    assigned via `assign_summary_digits()`. See below for details.

  `type`

  : ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.html))\
    Specifies the summary type. Accepted value are
    `c("continuous", "continuous2", "categorical", "dichotomous")`. If
    not specified, default type is assigned via `assign_summary_type()`.
    See below for details.

  `value`

  : ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.html))\
    Specifies the level of a variable to display on a single row. The
    gtsummary type selectors, e.g. `all_dichotomous()`, cannot be used
    with this argument. Default is `NULL`. See below for details.

  `missing,missing_text,missing_stat`

  : Arguments dictating how and if missing values are presented:

    - `missing`: must be one of `c("ifany", "no", "always")`.

    - `missing_text`: string indicating text shown on missing row.
      Default is `"Unknown"`.

    - `missing_stat`: statistic to show on missing row. Default is
      `"{N_miss}"`. Possible values are `N_miss`, `N_obs`, `N_nonmiss`,
      `p_miss`, `p_nonmiss`.

  `sort`

  : ([`formula-list-selector`](https://www.danieldsjoberg.com/gtsummary/reference/syntax.html))\
    Specifies sorting to perform for categorical variables. Values must
    be one of `c("alphanumeric", "frequency")`. Default is
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

## Value

Object of class `teal_module` to be used in `teal` applications.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `table` (`gtsummary` - output of
  [`gtsummary::tbl_summary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_summary.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_tbl_summary(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQHFps3XAAPWFQRJX4oUigDYy5qAH1wyJswiKijXQB3WlIACxV2RKhcXQBhAHkAJmjSysVodGqVbPYlXV1C3QBedtTcVt0+IRFRLoGBYTEWiDa20hg40np40UEYGBYOfpndelZR2IxUWgIAa1FERCPT0Snt7cLROFIbhTByitfi1-fXuT7pu5tLQsWhQJaTR4iDRwfijV4ABWoUDIvy2Mz+aLaKgI1EE-D03QOVzOF2JLwBgIeTxeb0qn103zpYAxFLuwMYoPBN0hcGhsO6BBaYDsrFQcHprzsjDgEXgKOZxRgwk0IQJ9iczjqgK16P6dTqtBMunYKnIzEsOhsthA-VEeQgrAAguh2GhUAASQS0Ypu92PRg6Rh1AC+SjAwYAukA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dfihSKF04AA9YVBElUPCDYwB3WlIACxVOOG4MRKhERC5qAH0Cm1xdECVdXQBBHwCAGWTdEvywwsRGRpbahp87O2c2joKi3qGRpQBfRQgAKyIVUoBrOFZRdgLbIxC4EyhhUlKCflpRAlLl1Y2t4Gh4bd2AXSU0VDaVdPYBgt0AF4Ql1cAM+EIRKIgboIcIxH8IHU6qQYKVSPQyqJBDAYCwOANkbp6KwYR1ULQCGtREUKVTtoSiSDwqI4KRtgQ-mA+s0FHhdHzGsNXGA5FVBdMRWLGUStCxaFBMQjOXyAs4ABp88VgLwAeUcADkHABNLUCsABPwjPw+M2Crx2epYACydp1BoCWDdAAUnfVnb4+WLzWrNaKZbppUimSoCNRBPw9MDyZTqbTUwzo0zmVBWez2CruU1efyJcKg9qeRWI3U5YwFUqOVzLdbbaWdfqjVhTe36g6na726GKyGrc4bWaYCdaHEk-YnM4Ftml8iVwsFrQTLp2CpyMxLDobLYatHRFkIKx6uh2J8ACSCWhVO+sxg6RgLWZgWavIA)

## Examples

``` r
# General example
data <- teal_data()
data <- within(data, CO2 <- CO2)
app <- init(
  data = data,
  modules = modules(
    tm_tbl_summary(
      by = teal.picks::picks(
        datasets("CO2", "CO2"),
        variables(selected = "Plant")
      ),
      include = teal.picks::picks(
        datasets("CO2", "CO2"),
        variables(selected = c("Type", "Treatment"), multiple = TRUE)
      )
    )
  )
)
#> Initializing tbl_summary
#> Warning: variables(selected = "Plant")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
#> Warning: variables(selected = c("Type", "Treatment"), multiple = TRUE)
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
if (interactive()) {
  shinyApp(app$ui, app$server)
}

# CDISC data example
data <- within(teal.data::teal_data(), {
  ADSL <- teal.data::rADSL
  ADTTE <- teal.data::rADTTE
})
join_keys(data) <- default_cdisc_join_keys[names(data)]
app <- init(
  data = data,
  modules = modules(
    tm_tbl_summary(
      by = teal.picks::picks(
        datasets(c("ADSL", "ADTTE"), "ADTTE"),
        variables(c("SEX", "COUNTRY", "SITEID", "ACTARM", "CNSR", "PARAMCD"), "SEX")
      ),
      include = teal.picks::picks(
        datasets(c("ADSL", "ADTTE"), "ADSL"),
        variables(c("SITEID", "COUNTRY", "ACTARM", "SEX"), "SITEID", multiple = TRUE)
      )
    )
  )
)
#> Initializing tbl_summary
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
