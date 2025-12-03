# `teal` module: Data table viewer

Module provides a dynamic and interactive way to view `data.frame`s in a
`teal` application. It uses the `DT` package to display data tables in a
paginated, searchable, and sortable format, which helps to enhance data
exploration and analysis.

## Usage

``` r
tm_data_table(
  label = "Data Table",
  variables_selected = list(),
  datasets_selected = deprecated(),
  datanames = if (missing(datasets_selected)) "all" else datasets_selected,
  dt_args = list(),
  dt_options = list(searching = FALSE, pageLength = 30, lengthMenu = c(5, 15, 30, 100),
    scrollX = TRUE),
  server_rendering = FALSE,
  pre_output = NULL,
  post_output = NULL,
  transformators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- variables_selected:

  (`named list`) Character vectors of the variables (i.e. columns) which
  should be initially shown for each dataset. Names of list elements
  should correspond to the names of the datasets available in the app.
  If no entry is specified for a dataset, the first six variables from
  that dataset will initially be shown.

- datasets_selected:

  (`character`) **\[deprecated\]** A vector of datasets which should be
  shown and in what order. Use `datanames` instead.

- datanames:

  (`character`) Names of the datasets relevant to the item. There are 2
  reserved values that have specific behaviors:

  - The keyword `"all"` includes all datasets available in the data
    passed to the teal application.

  - `NULL` hides the sidebar panel completely.

  - If `transformators` are specified, their `datanames` are
    automatically added to this `datanames` argument.

- dt_args:

  (`named list`) Additional arguments to be passed to
  [`DT::datatable()`](https://rdrr.io/pkg/DT/man/datatable.html) (must
  not include `data` or `options`).

- dt_options:

  (`named list`) The `options` argument to
  [`DT::datatable`](https://rdrr.io/pkg/DT/man/datatable.html). By
  default
  `list(searching = FALSE, pageLength = 30, lengthMenu = c(5, 15, 30, 100), scrollX = TRUE)`

- server_rendering:

  (`logical`) should the data table be rendered server side (see
  `server` argument of
  [`DT::renderDataTable()`](https://rdrr.io/pkg/DT/man/dataTableOutput.html))

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

## Value

Object of class `teal_module` to be used in `teal` applications.

## Details

The `DT` package has an option `DT.TOJSON_ARGS` to show `Inf` and `NA`
in data tables. Configure the `DT.TOJSON_ARGS` option via
`options(DT.TOJSON_ARGS = list(na = "string"))` before running the
module. Note though that sorting of numeric columns with `NA`/`Inf` will
be lexicographic not numerical.

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6uoxwAI6CtNXsPqKkxNREjIoQlQ20onG6vaJKAL5dSmioAyp57BX+GboAvAtBuPN8QiL9K5vCYnPdlbqkMMkZiUH0IofHx1ostFDXYomicCIacPzLunQttzuxyGvwIczAAGU4KhuBgADIUCQFBR4XQoqEwngAdVo-GRqJRAAU4EEeAiIEj8ijSkSSbCcXiqQTIag4ARaGIUV0gbo5Osjnc8YkWBIdn8+qR2AQ0JoSL8UQBJLAKiH2Z4iXQAYRlVggXPmlW5vKUXVoJl07BU5GYlh0Nls5SOokKEFYAEF0OxJgASeqlH3vRg6TqjJRgEYAXSAA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dfihSKF04AA9YVBElUPCDYy5qAH1EqBsEsIijXQB3WlIACxV2TNxdECVdXUY4AEdBWgb2CDFSYmoiRkUIOoBBHwCAGWTdVIxMxERGYbGlAF9+gCsiFTSAazhWUQrc23z+OBMoYVI0gn5aUQI09c2dveBoeH3MuQBdN2h0CZUxXYtRCuV0AF5QeFcCC+EIRKIIbo4cIxMCBnVJjAMrk0uF6CJ0ZjMVoWLQoASxGlRHARBo4PwkXRRKR2AtxpCCMCwAE7I4fABNPw+BR4XSixwBRwAIQAUsLRVVRVK5Qqxcq-HZnGqlWBBgBxVzqnnOAAaorkchhGOJ-EuLAkiMhzNZBDQmhISNF7PsFJE3ndVggFpBdX6YaU-VoJl07BU5GYlh0NlsNQxojKEFYg3Q7DQqAAJC0qvmCzTGDo+sslGAll8gA)

## Examples

``` r
# general data example
data <- teal_data()
data <- within(data, {
  require(nestcolor)
  iris <- iris
})

app <- init(
  data = data,
  modules = modules(
    tm_data_table(
      variables_selected = list(
        iris = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
      ),
      dt_args = list(caption = "IRIS Table Caption")
    )
  )
)
#> Initializing tm_data_table
if (interactive()) {
  shinyApp(app$ui, app$server)
}

# CDISC data example
data <- teal_data()
data <- within(data, {
  require(nestcolor)
  ADSL <- teal.data::rADSL
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_data_table(
      variables_selected = list(ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX")),
      dt_args = list(caption = "ADSL Table Caption")
    )
  )
)
#> Initializing tm_data_table
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
