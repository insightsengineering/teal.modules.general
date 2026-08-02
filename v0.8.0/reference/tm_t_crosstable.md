# `teal` module: Cross-table

Generates a simple cross-table of two variables from a dataset with
custom options for showing percentages and sub-totals.

## Usage

``` r
tm_t_crosstable(
  label = "Cross Table",
  x = teal.picks::picks(teal.picks::datasets(), teal.picks::variables(choices =
    teal.picks::is_categorical(min.len = 2, max.len = 10), selected = 1L, multiple =
    TRUE, ordered = TRUE)),
  y,
  show_percentage = TRUE,
  show_total = TRUE,
  remove_zero_columns = FALSE,
  pre_output = NULL,
  post_output = NULL,
  basic_table_args = teal.widgets::basic_table_args(),
  transformators = list(),
  decorators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- x:

  (`picks` or `list` of `picks`) Object with all available choices with
  pre-selected option for variable X - row values. In case of `picks`
  use `teal.picks::variables(..., ordered = TRUE)` if table elements
  should be rendered according to selection order.

- y:

  (`picks` or `list` of multiple `picks`) Object with all available
  choices with pre-selected option for variable Y - column values.

  `picks` must not allow multiple selection in this case.

- show_percentage:

  (`logical(1)`) Indicates whether to show percentages (relevant only
  when `x` is a `factor`). Defaults to `TRUE`.

- show_total:

  (`logical(1)`) Indicates whether to show total column. Defaults to
  `TRUE`.

- remove_zero_columns:

  (`logical(1)`) Indicates whether to remove columns that contain only
  zeros from the output table. Defaults to `FALSE`.

- pre_output:

  (`shiny.tag`) optional, text or UI element to be displayed before the
  module's output, providing context or a title. with text placed before
  the output to put the output into context. For example a title.

- post_output:

  (`shiny.tag`) optional, text or UI element to be displayed after the
  module's output, adding context or further instructions. Elements like
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html) are
  useful.

- basic_table_args:

  (`basic_table_args`) object created by
  [`teal.widgets::basic_table_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/basic_table_args.html)
  with settings for the module table. The argument is merged with
  options variable `teal.basic_table_args` and default module setup.

  For more details see the vignette:
  [`vignette("custom-basic-table-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-basic-table-arguments.html)

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

## Note

For more examples, please see the vignette "Using cross table" via
[`vignette("using-cross-table", package = "teal.modules.general")`](https://insightsengineering.github.io/teal.modules.general/articles/using-cross-table.md).

## Table Settings

The module provides several table settings that can be adjusted:

- `Show column percentage`: Shows column percentages when enabled

- `Show total column`: Shows a total column when enabled

- `Remove zero-only columns`: Removes columns that contain only zeros
  from the output table

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `table` (`ElementaryTable` - output of
  [`rtables::build_table`](https://rdrr.io/pkg/rtables/man/build_table.html))

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_t_crosstable(
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6ujCkBCyicVU1dRWmRIy67Fq6KroE7AqErNQDpQNaoiO6A7CTA1IsA3K25RCVldW1jKLAwFoAunsNUOIm6qRt7Bt1O-t7iqu6AL4tV1s7A6iMtDAsrIkA1nBWAMDg1RHAAI6JEQQdgQRhEbKXJpbJZKR73ABWRBUAKBomKGVsWWxuMBrAJpIgeI4A1eEzwUzA9NmYE+31+NMW9yUaFQDRUeX6DxSugAvP4MrgXgJhGJxVVZSICS1KqQYIlSIkCAjRKIgvQRMK1mtqFB6HA-BKBgBhXX1OzmqJ4VVrUIKhIYVC0Aj-USIRDe30qh4myopcGkFXMlEMuTS0Nhz1Bv0BrQsWhOsTGsMmgj5HEEeUS9NfLPags+7Mpd4xzYMg6lPoDAhDVnjVkzRlzOALMBLBO5k3gkQaOD8BXNwbDbtgeaMRaDofrYSaSJ6CUOFxL5dtfgyccKrfOHdDky0UKHiUAMQAggAZADKrkTJvjrpNyZ9qcQ6eogmze5c3fV9dFYD1ex4FN-UDb8Q2XCM4Cjfo62aftT0-SCvTgtMMyzeDl16Ssi3qEs8MNOAK0LasMlrFkwEbXoUNbGdRjADtZy7Nj525DCwxHOAxwna12IZPiTRgVdaHXBU7yfE8PzDc9L2E3Q5OfRTKhA5cv2DXD-0AxSgLWYz7nuWgTA6FRyGYSwdBsZYWlEQoIFYW90HYPkABJBFoUpvPBRgdEYe5nggMBHj2IA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dfihSKF04AA9YVBElUPCDYy5qAH1EqBsEsIijXQB3WlIACxV2TNxdECVdXQBBHwCAGWTdVIxMxERGRpalAF9FCAArIhU0gGs4VlEK3Nt8-jgTKGFSNIJ+WlECNLGJ6dngaHg5zLkAXTdodDaVYvZakNzdAF4X8NxnviERUXeul+wjETwgdTqpBgaQ2BEYRFEonC9BEYIhEOoUHocGogIUYC88MR9ix8Twz3RUUBHVQtAIk1E3Vp9LmFPRnygojgpFZYD6zXxcm+4PZkLg3AwzIZ3S0LFopNBbNFBBK4wIYkBssY8pRcE2qrpoMywGA+P5+MulyqokE9C5pEBZgsmmsF2qStFulo-Cp+QAhLL0NQOJUvRASjJiqIqtQiBI6dx2ABGIW6AhPMA+MJwfFVfEABQA8gE-AANDS53QF4tl6ikQXDT3sxjcwSMCDsU5GhbAb1RS6Npu6IbCoe6LkiDRwfh4gmFxwAOQcAE1cx70TB1rQ4noPg4XKOh0RGMsWzO905nIemyZaFFp4CAGL1ZoBVwi0VC9ftcU8KWMxBZWoQRQUHdEvw-CFWGpX9JTpaVEH-NFPUye1eXNMAIKbGl4IArUdX+ZCmxVNUNQ+fCFX1UjzlyE0zSaAUwEta1bXtR1zEsV0FndSCmz7NpAziENciqHYMFWDRjxjOME2oZM5DAocW1INsOy7GjwjkXsfQHb86hHPTxxxOAp3PKswDfUs1149lNzrbcRCfF832vT1b3vMzn1fd8mywz0cJZGVuBAuZFLqRTB2GYZaBMXR2BUchmEsHQbFsGoRVEMoIFYep0HYNBUAAEkEWgqgKwquUYHRGGGAYlDAAZLiAA)

## Examples

``` r
# general data example
data <- teal_data()
data <- within(data, {
  mtcars <- mtcars
  for (v in c("cyl", "vs", "am", "gear")) {
    mtcars[[v]] <- as.factor(mtcars[[v]])
  }
  mtcars[["primary_key"]] <- seq_len(nrow(mtcars))
})
join_keys(data) <- join_keys(join_key("mtcars", "mtcars", "primary_key"))

app <- init(
  data = data,
  modules = modules(
    tm_t_crosstable(
      label = "Cross Table",
      x = teal.picks::picks(
        datasets("mtcars"),
        teal.picks::variables(
          choices = variable_choices(data[["mtcars"]], c("cyl", "vs", "am", "gear")),
          selected = c("cyl", "gear"),
          multiple = TRUE,
          ordered = TRUE,
          fixed = FALSE
        ),
        teal.picks::values()
      ),
      y = teal.picks::picks(
        datasets("mtcars"),
        teal.picks::variables(
          choices = variable_choices(data[["mtcars"]], c("cyl", "vs", "am", "gear")),
          selected = "vs",
          multiple = FALSE,
          fixed = FALSE
        ),
        teal.picks::values()
      )
    )
  )
)
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Initializing tm_t_crosstable
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
if (interactive()) {
  shinyApp(app$ui, app$server)
}

# CDISC data example
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_t_crosstable(
      label = "Cross Table",
      x = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(
          choices = variable_choices(data[["ADSL"]], subset = function(data) {
            idx <- !vapply(data, inherits, logical(1), c("Date", "POSIXct", "POSIXlt"))
            return(names(data)[idx])
          }),
          selected = "COUNTRY",
          multiple = TRUE,
          ordered = TRUE,
          fixed = FALSE
        ),
        teal.picks::values()
      ),
      y = teal.picks::picks(
        datasets("ADSL"),
        teal.picks::variables(
          choices = variable_choices(data[["ADSL"]], subset = function(data) {
            idx <- vapply(data, is.factor, logical(1))
            return(names(data)[idx])
          }),
          selected = "SEX",
          multiple = FALSE,
          fixed = FALSE
        ),
        teal.picks::values()
      )
    )
  )
)
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Initializing tm_t_crosstable
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
