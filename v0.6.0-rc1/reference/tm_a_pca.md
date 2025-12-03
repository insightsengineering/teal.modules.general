# `teal` module: Principal component analysis

Module conducts principal component analysis (PCA) on a given dataset
and offers different ways of visualizing the outcomes, including elbow
plot, circle plot, biplot, and eigenvector plot. Additionally, it
enables dynamic customization of plot aesthetics, such as opacity, size,
and font size, through UI inputs.

## Usage

``` r
tm_a_pca(
  label = "Principal Component Analysis",
  dat,
  plot_height = c(600, 200, 2000),
  plot_width = NULL,
  ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
  ggplot2_args = teal.widgets::ggplot2_args(),
  rotate_xaxis_labels = FALSE,
  font_size = c(12, 8, 20),
  alpha = c(1, 0, 1),
  size = c(2, 1, 8),
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

- dat:

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  specifying columns used to compute PCA.

- plot_height:

  (`numeric`) optional, specifies the plot height as a three-element
  vector of `value`, `min`, and `max` intended for use with a slider UI
  element.

- plot_width:

  (`numeric`) optional, specifies the plot width as a three-element
  vector of `value`, `min`, and `max` for a slider encoding the plot
  width.

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
  `c("default", "Elbow plot", "Circle plot", "Biplot", "Eigenvector plot")`.

  For more details see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html).

- rotate_xaxis_labels:

  (`logical`) optional, whether to rotate plot X axis labels. Does not
  rotate by default (`FALSE`).

- font_size:

  (`numeric`) optional, specifies font size. It controls the font size
  for plot titles, axis labels, and legends.

  - If vector of `length == 1` then the font sizes will have a fixed
    size.

  - while vector of `value`, `min`, and `max` allows dynamic adjustment.

- alpha:

  (`integer(1)` or `integer(3)`) optional, specifies point opacity.

  - When the length of `alpha` is one: the plot points will have a fixed
    opacity.

  - When the length of `alpha` is three: the plot points opacity are
    dynamically adjusted based on vector of `value`, `min`, and `max`.

- size:

  (`integer(1)` or `integer(3)`) optional, specifies point size.

  - When the length of `size` is one: the plot point sizes will have a
    fixed size.

  - When the length of `size` is three: the plot points size are
    dynamically adjusted based on vector of `value`, `min`, and `max`.

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

- `elbow_plot` (`ggplot`)

- `circle_plot` (`ggplot`)

- `biplot` (`ggplot`)

- `eigenvector_plot` (`ggplot`)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_a_pca(
       ..., # arguments for module
       decorators = list(
         elbow_plot = teal_transform_module(...), # applied to the `elbow_plot` output
         circle_plot = teal_transform_module(...), # applied to the `circle_plot` output
         biplot = teal_transform_module(...), # applied to the `biplot` output
         eigenvector_plot = teal_transform_module(...) # applied to the `eigenvector_plot` output
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokrcQAxLqkQ11XfxQpFC6cAAesKgiSoHBBsZc1AD6sVA2MUEhRroA7rSkABYq7Km4uiBKurqMcACOgrQ17L6ipMTURIyKEFWOAMoAgow1raLxuv1DI6SiSgC+3UpoqOMq+eyVAZm6ALxbwbibfEIiY3vHwmIbPVW6pDBJUEmoBGmbt7oKYAAKAMIDX0ONw+sV2+ye4VIzA0SVEqDgBGuHxBmWg8DBX0mwzEM0B72RojgIg0YMJxNIsPhiPxyN0BAKRFoBDEYK0LFoUHoIiS9MZzNESNpyNSYNSwGAmMG2NGXwAurKytSwABZQSMfgyQGfMADUSiKDCUhazGMehQCDfIioY1gLBoOBfbpC25yIHOqpkhHkfhgpWq9WavDa3X6w2Ot3umCG2hRPR7BwuGkuiO0ky0ajkRhggByjgAMnmk06XZtut1aCZdOwVJn1JodDZbBUbqIihBWAN0OxlgASBplXuExg6LrzJRgOayoA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokrcQAxLoDCAEQCSAMreuvxQpFC6cAAesKgiSmERBsZc1AD6SVA2ieGRRroA7rSkABYq7Fm4uiBKurqMcACOgrSN7BBipMTURIyKEPUAgr6BADIpumkYWYiIjCPjSgC+AwBWRCrpANZwrKKVebYF-HAmUMKk6QT8tKIE6Rtbu-vA0PAHWXIAuu5oqJMVCV2HVQnldABeMERXCgvhCESiSG6eHCMQgwb1KYwdJQdKoAjZUFY3QKMAABW8QzJsMxJKSyKy6RipGYGnSolQcAIGJJ9Ly7z0ULJizGNOJfNEcBEGmRUplV053N5fJJBFKmwIYmRWhYtCg9BE1w1tC1BwlqvqWUZeWAwBFozFYG+32qPLJACEALJYADSWAAjDTSWAhgBxVx4EPOADygV8AE0yQNLVi5LTU-V5dzyPxke6wN6-YHgyKI8mM5mYJdaPEhfYnK46Xz0xasSZaNRyIxkQA5RxjMXN+optOggYDWgmXTsFTd9SaHQ2Wy1TGicoQVhDdDsf4AEla1X3UsYOn6KyUYGW3yAA)

## Examples

``` r
# general data example
data <- teal_data()
data <- within(data, {
  require(nestcolor)
  USArrests <- USArrests
})

app <- init(
  data = data,
  modules = modules(
    tm_a_pca(
      "PCA",
      dat = data_extract_spec(
        dataname = "USArrests",
        select = select_spec(
          choices = variable_choices(
            data = data[["USArrests"]], c("Murder", "Assault", "UrbanPop", "Rape")
          ),
          selected = c("Murder", "Assault"),
          multiple = TRUE
        ),
        filter = NULL
      )
    )
  )
)
#> Initializing tm_a_pca
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
    tm_a_pca(
      "PCA",
      dat = data_extract_spec(
        dataname = "ADSL",
        select = select_spec(
          choices = variable_choices(
            data = data[["ADSL"]], c("BMRKR1", "AGE", "EOSDY")
          ),
          selected = c("BMRKR1", "AGE"),
          multiple = TRUE
        ),
        filter = NULL
      )
    )
  )
)
#> Initializing tm_a_pca
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
