# `teal` module: Outliers analysis

Module to analyze and identify outliers using different methods such as
IQR, Z-score, and Percentiles, and offers visualizations including box
plots, density plots, and cumulative distribution plots to help
interpret the outliers.

## Usage

``` r
tm_outliers(
  label = "Outliers Module",
  outlier_var,
  categorical_var = NULL,
  ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
  ggplot2_args = teal.widgets::ggplot2_args(),
  plot_height = c(600, 200, 2000),
  plot_width = NULL,
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

- outlier_var:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  Specifies variable(s) to be analyzed for outliers.

- categorical_var:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  optional, specifies the categorical variable(s) to split the selected
  outlier variables on.

- ggtheme:

  (`character`) optional, `ggplot2` theme to be used by default.
  Defaults to `"gray"`.

- ggplot2_args:

  (`ggplot2_args`) optional, object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for all the plots or named list of `ggplot2_args`
  objects for plot-specific settings. The argument is merged with
  options variable `teal.ggplot2_args` and default module setup.

  List names should match the following:
  `c("default", "Boxplot", "Density Plot", "Cumulative Distribution Plot")`.

  For more details see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

- plot_height:

  (`numeric`) optional, specifies the plot height as a three-element
  vector of `value`, `min`, and `max` intended for use with a slider UI
  element.

- plot_width:

  (`numeric`) optional, specifies the plot width as a three-element
  vector of `value`, `min`, and `max` for a slider encoding the plot
  width.

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

- `box_plot` (`ggplot`)

- `density_plot` (`ggplot`)

- `cumulative_plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_outliers(
       ..., # arguments for module
       decorators = list(
         box_plot = teal_transform_module(...), # applied only to `box_plot` output
         density_plot = teal_transform_module(...), # applied only to `density_plot` output
         cumulative_plot = teal_transform_module(...) # applied only to `cumulative_plot` output
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokrcQAxLqkQ11XfxQpFC6cAAesKgiSoHBBsZc1AD6sVA2MUEhRroA7rSkABYq7Km4uiBKuroAwgDyAEzxNQ2VzfXAwApgqIy0MCysSQDWcKxdALrjTaJwAI5JIhDsEIxEOex19XKKEAC+OwBWRCrDo6IlmbbZRycjrOc3EKccXZtdZa8teLpdPX0Dzy623cWhYoiaBAKxwIYiSMxEGjg-HYoN6UHoIiSkOhYguwQ6n3qE3GZQI7C6AAVqFAyO8fmA7KxUHA6V07Iw4EF4LSwNsdko0Kgmip8uSIFVUroALwBTK4Vp8IQicEyxXCXGtKqkGBJIiCUh0GTnTVVXR6g20GRJVHS3R0USkMWm52y4JJcKkZgaOHMskml2ummwPQywnvf0u+FwDS2qPe0S+p0Bl3U+hwfyhsAAZXT0dIulRtHRIkQ4fFyed2NoMJVBZYRYxcCxUOruNSBLAbzAk1J5MIJAIrLAglQwRGQLk8vLFaqcfI-FtXRHY5ZeAjAZgwk0URDugAYgBBAAyWecU5nppMtDCSNth5PrmnyZ2FZfLsnEYIQTgEiIvS-yQ2jK9qOuugbumEnrqKQPrRkmFapNA8CLp2XznjOV7UOQjC2ph2GwX6T4VqitYkehF5VjWtqgtQghNpRbaZB2XY9nWjCiAAJHOSIfkRybcQuMo0XRzY4uc7adKhRLdiSbGcQJvEXlUm5YbQO62g4LhgVUb4Brppr6W+Ow7LQJi6OwKjYdB2hwDYtgVOWohFBArAHug7CChxgi0GUnkzIwOiMDsuxKGAuzjEAA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokrcQAxLoDCAEQCSAMreuvxQpFC6cAAesKgiSmERBsZc1AD6SVA2ieGRRroA7rSkABYq7Fm4uiBKuroAgr6BADIpumkYWYiIjE2tSgC+ihAAVkQq6QDWcKyilXm2BfxwJlDCpOkE-LSiBOnjkzNzwNDw81lyALruaxrpWiyi6VD8otTtZ2LsAGK01ORGOxdg4XNVRGh4hwssBgAowP0WvCrldqrsMHdSERGHJcUpHoxRO0CKUJgQxOlRHARBo4Px2ATaFB6CItqTaOSLnlYfDEcjUaZ1JsCc9Xu88RB3JD2ioSuw6qE8roALyKiK4BV8IQiImqrXCb4K+qkGDpIiCUh0GTzI31XTmy20GQPFgq3R0USkeUQO2+tVQdIxUjMe6iVBwAjev1+rJfN285pIvC26NUmmkN1piObMMRqPRv3UZnU+NgQLU7O6RnMkSIeEan0F30ksliN3VllwNmtrkRHkIxP86qR3kAcVceF08IAQgBZLAAaSwAEZ4biG03fVnafxSw1x-WUwWYBtaPE9Kqfg0WoFnBvN-UTLRonS3Veb65G02Rt+j3J7824RwBI2IctwLqMG6Hpeke9RZIG0TBkKlLhiOX4FrGsAXlOA4DMm6HRk+AIyG6RGAiheawX6Irtk8AGbi2HJtqqjzUIIXaMZyCx9nCuFJii1QigAJNu5D8P+VFbhWO60WxHHslxMK8XyYACVWTwidJYkSQRx6nuebqgp+D4-gWpm+uZuimSMIy0CYujAqoIaaDoNi2LUjaiOUECsA06DsJCQmCLQ1SBVSjA6DiQxKGAgxXEAA)

## Examples

``` r

# general data example
data <- teal_data()
data <- within(data, {
  CO2 <- CO2
  CO2[["primary_key"]] <- seq_len(nrow(CO2))
})
join_keys(data) <- join_keys(join_key("CO2", "CO2", "primary_key"))

vars <- choices_selected(variable_choices(data[["CO2"]], c("Plant", "Type", "Treatment")))

app <- init(
  data = data,
  modules = modules(
    tm_outliers(
      outlier_var = list(
        data_extract_spec(
          dataname = "CO2",
          select = select_spec(
            label = "Select variable:",
            choices = variable_choices(data[["CO2"]], c("conc", "uptake")),
            selected = "uptake",
            multiple = FALSE,
            fixed = FALSE
          )
        )
      ),
      categorical_var = list(
        data_extract_spec(
          dataname = "CO2",
          filter = filter_spec(
            vars = vars,
            choices = value_choices(data[["CO2"]], vars$selected),
            selected = value_choices(data[["CO2"]], vars$selected),
            multiple = TRUE
          )
        )
      )
    )
  )
)
#> Initializing tm_outliers
if (interactive()) {
  shinyApp(app$ui, app$server)
}


# CDISC data example
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

fact_vars_adsl <- names(Filter(isTRUE, sapply(data[["ADSL"]], is.factor)))
vars <- choices_selected(variable_choices(data[["ADSL"]], fact_vars_adsl))


app <- init(
  data = data,
  modules = modules(
    tm_outliers(
      outlier_var = list(
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            label = "Select variable:",
            choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
            selected = "AGE",
            multiple = FALSE,
            fixed = FALSE
          )
        )
      ),
      categorical_var = list(
        data_extract_spec(
          dataname = "ADSL",
          filter = filter_spec(
            vars = vars,
            choices = value_choices(data[["ADSL"]], vars$selected),
            selected = value_choices(data[["ADSL"]], vars$selected),
            multiple = TRUE
          )
        )
      )
    )
  )
)
#> Initializing tm_outliers
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
