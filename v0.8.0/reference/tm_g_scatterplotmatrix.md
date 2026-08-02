# `teal` module: Scatterplot matrix

Generates a scatterplot matrix from selected `variables` from datasets.
Each plot within the matrix represents the relationship between two
variables, providing the overview of correlations and distributions
across selected data.

## Usage

``` r
tm_g_scatterplotmatrix(
  label = "Scatterplot Matrix",
  variables = list(teal.picks::picks(teal.picks::datasets(),
    teal.picks::variables(selected = seq(1L, 5L), multiple = TRUE))),
  min_n_variables = 2L,
  max_n_variables = 5L,
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

- variables:

  (`picks` or `list` of `picks`) Specifies plotting variables from an
  incoming dataset with filtering and selecting. In case of `picks` use
  `teal.picks::variables(..., ordered = TRUE)` if plot elements should
  be rendered according to selection order.

- min_n_variables:

  (`integer(1)`) Minimum number of variables that must be selected.

- max_n_variables:

  (`integer(1)`) Maximum number of variables that can be selected.

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

## Value

Object of class `teal_module` to be used in `teal` applications.

## Note

For more examples, please see the vignette "Using scatterplot matrix"
via
[`vignette("using-scatterplot-matrix", package = "teal.modules.general")`](https://insightsengineering.github.io/teal.modules.general/articles/using-scatterplot-matrix.md).

When *Add Correlation* is enabled, a simple **Omit NAs** checkbox
controls NA handling (checked = `"pairwise.complete.obs"`, matching the
historical default). Unchecking it reveals a dropdown with all five
[`stats::cor()`](https://rdrr.io/r/stats/cor.html) `use` options for
advanced control.

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `plot` (`ggplot` - a `patchwork` assembled from individual `ggplot`
  panels)

A Decorator is applied to the specific output using a named list of
`teal_transform_module` objects. The name of this list corresponds to
the name of the output to which the decorator is applied. See code
snippet below:

    tm_g_scatterplotmatrix(
       ..., # arguments for module
       decorators = list(
         plot = teal_transform_module(...) # applied to the `plot` output
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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6usTmpIy0YnH+GRgmzPDsFZW6tPy6ALxVHWAAIq54ugpgAGJYk6WTAJJ2cxNgzgDKK5MACsvjkwDis-tgAHIAMltgAEJj82DnjlcAgntyuJ2VEkQ6jBDwZH6pnUpCIjA6EC6XQI7AATKV4bpEQBGBFo3SojGlTGw96fLrUKD0ODUURAmGTACyJBYBHyrCuWDgqEE9DoBEmikhXTx3MqhIgEkEUCkiRMsFo1FYQPFGjBEKhlRhmIAzKU1boNRqUdj1djeYrdITiaTyUMDjIYFBlByTgAJEkiG2MohWixwTn4g1dVBEFmEzQkM0ADg1ADYAOylMMABlKABYoxi4xjMcik8jMTGMGHSgBOb2VFhwYIDGEqgCsSYrFdVMcxFZjFfzKJViPjdYROdKocLXn4qDNKow8a7SdhGDTI9K2Y12cR2aT2ebumzWZHff4cHopDNE5xGG106R3ZPR9HZ67F4n565lTvulE3AaWRSLTacAVXR6QORiEbHx8lURA1GwiQ-gMT4wJEn74kqQyjFcMxXEsVwbFcuxXEcVwXFctxXI8LxvIBhqiLQABeegDABcG6IwzKEgQVH2E4rhAX2rAlowQKiGCpDsFBMFwvWMaILCMbiaUAF0Qx6jMQ4LhyH2OgQIIzGCSI7AUmAohWtQ1BXPA-C0IIMBXISjBSFcJB0D4nJSSm9GRHJQIKc4fYENaTH6XAvSQREmkwm5pRTM85zrO5DmlE5jHyaxfYAI7CmQeTSgMfxgjA7DSciMYpuJHlEKIpBkulECZdlKbBvlMZ9qgjBECYeRAhljBZdJknJg+XIAL5cgAVkQKiJAA1nArCiMUGS2Fkg3DWNE1fnNECjeNQzVGQdRiFcG21PUohXD09mdMtq0cJMT4iAdJyXdtJxHWA3qnQt60gZt+1XLd12lNpD1ApMu1gQ9SlKFyShoIOWQqHkX4pECJSdHwQhXUCSPCGIX6VKQMCJBIiSiJ5pDkIwkRENjgR1KEmMEkSJL-WA6yE8TpO7lae2hHMtFaCwtBEijAx0MV1NQgkGCoLQBAjaIiCIOLkuTbRUIpKIcAla9oEfY9JGGl0oty1LMvc3UfMY4rip0kNTGlbomj8BNJJwBoMtwL8rAFCoEg2NrOtdCrIgaL5ZqTMWUBXBIA5XFuO7HUBPswMImgwa5rHez7uhglu9F+SxLhm-eqeGnrEsG4g3PUGpk0PoafYiyWPD69LsvFwrseKsrqsKzpz7XTXipF-Lhs8ybLdp9C+SWw0ZZDEl1qaKQDInMQxXfas9WNXkMej5UfuO+Q2faTPKXzztRUlZvW-x9QiciMnLgFzrGcyIHAxuXnuhV4qH-57RtTWqIJiZUCGCa2gt+JmwSIkX+EB-6ZTFJKYmws+51zFs3GWDdEGGnbmrC63dz6j37iXI2vM2Sm0IG9WorBwL8DwWnAhjcy4VzSK3KEX8eS0Q-t1UGShaAmF0OwFQxMQTaE-EpMonRRCFAgKwZ46B2AQwACSCFoKUBRKtGC-F6koMAPUAC6QA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dfihSKF04AA9YVBElUPCDYy5qAH1EqBsEsIijXQB3WlIACxV2TNxdECVdXQBBHwCAGWTdVIxMxERGRpbahp8sALaOrp7G4aUAX0UIACsiFTSAazhWUQrc23z+OBMoYVI0gn5aUQI0xeW1jeBoeE3MuQBdN2h0NpVi9gHM3QAvCFcrgBnwhCJRIDdODhGJfhA6nVSDA0hI0hcwuRGHEiCiwoxaFEEUikdQoPQ4NRoQowAECFiZLjSDCCUTaaDEaTdFoWLQKZDoXRRKQSdzkXBuBhULQCCtRN0ZXLNgNxXVMqI4KQVWA+s1aXJOWqkR0lfLurzCQL4arjboCCUlgQxNDNPwNlS4BpunAdGxSioJDYjXa6pqRBo4PxoQRfrqAOKuPC6WlYepeJNVWkBZwADQNIdDMCOtDieiBDhchbtREYe0YUehlec1eNJiJjaBADF6s0c7bxYaB9zTbLzYhedRBPC5sah1zxaPlYqxyqF2qNVqdZMAgXh6Sl+PLfz6JCxaGHU6XUC3R6I6QfX7WAGIEH56Gw57I9GgbHafUADVew5FNdR8ABNPd12NYtqE0MsmycFt925Wt607ewkNbNV2yiDCez7VxoO5Wc1VI0l3xHZgIFEExaxgMJayhIFhVFFDUjSUhqNo+i0nbOCZHPNVDwVRAzTXD92klHhxO6TdtTjHcoMkkSLT5a0dQABXqNMAFlfGUj9VInbhp02cMvXIH9QIAIWcAJhk0vwDRQupyJI4d3NIuY5loExdHYFRsXUTQdBsWwai5UQyggVh6nQdg0FQAASQRaCqJLks1Rg-TmaYlDAaYXiAA)

## Examples

``` r
# general data example
data <- teal_data()
data <- within(data, {
  countries <- data.frame(
    id = c("DE", "FR", "IT", "ES", "PT", "GR", "NL", "BE", "LU", "AT"),
    government = factor(
      c(2, 2, 2, 1, 2, 2, 1, 1, 1, 2),
      labels = c("Monarchy", "Republic")
    ),
    language_family = factor(
      c(1, 3, 3, 3, 3, 2, 1, 1, 3, 1),
      labels = c("Germanic", "Hellenic", "Romance")
    ),
    population = c(83, 67, 60, 47, 10, 11, 17, 11, 0.6, 9),
    area = c(357, 551, 301, 505, 92, 132, 41, 30, 2.6, 83),
    gdp = c(3.4, 2.7, 2.1, 1.4, 0.3, 0.2, 0.7, 0.5, 0.1, 0.4),
    debt = c(2.1, 2.3, 2.4, 2.6, 2.3, 2.4, 2.3, 2.4, 2.3, 2.4)
  )
  sales <- data.frame(
    id = 1:50,
    country_id = sample(
      c("DE", "FR", "IT", "ES", "PT", "GR", "NL", "BE", "LU", "AT"),
      size = 50,
      replace = TRUE
    ),
    year = sort(sample(2010:2020, 50, replace = TRUE)),
    venue = sample(c("small", "medium", "large", "online"), 50, replace = TRUE),
    cancelled = sample(c(TRUE, FALSE), 50, replace = TRUE),
    quantity = rnorm(50, 100, 20),
    costs = rnorm(50, 80, 20),
    profit = rnorm(50, 20, 10)
  )
})
join_keys(data) <- join_keys(
  join_key("countries", "countries", "id"),
  join_key("sales", "sales", "id"),
  join_key("countries", "sales", c("id" = "country_id"))
)

app <- init(
  data = data,
  modules = modules(
    tm_g_scatterplotmatrix(
      label = "Scatterplot matrix",
      variables = list(
        teal.picks::picks(
          datasets("countries"),
          teal.picks::variables(
            choices = tidyselect::everything(),
            selected = c("area", "gdp", "debt"),
            multiple = TRUE,
            ordered = TRUE
          ),
          teal.picks::values()
        ),
        teal.picks::picks(
          datasets("sales"),
          teal.picks::variables(
            choices = c("quantity", "costs", "profit"),
            selected = c("quantity", "costs"),
            multiple = TRUE,
            ordered = TRUE
          )
        )
      ),
      transformators = list(
        teal_transform_filter(
          teal.picks::picks(
            datasets("sales"),
            teal.picks::variables("country_id"),
            teal.picks::values()
          )
        )
      )
    )
  )
)
#> Warning: teal.picks::variables(choices = tidyselect::everything(), selected = c("area", "gdp", "debt"), multiple = TRUE, ordered = TRUE)
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
#> Initializing tm_g_scatterplotmatrix
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
if (interactive()) {
  shinyApp(app$ui, app$server)
}

# CDISC data example
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
  ADRS <- teal.data::rADRS
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

app <- init(
  data = data,
  modules = modules(
    tm_g_scatterplotmatrix(
      label = "Scatterplot matrix",
      variables = list(
        teal.picks::picks(
          datasets("ADSL"),
          teal.picks::variables(
            choices = tidyselect::everything(),
            selected = c("AGE", "RACE", "SEX"),
            multiple = TRUE,
            ordered = TRUE,
            fixed = FALSE
          ),
          teal.picks::values()
        ),
        teal.picks::picks(
          datasets("ADRS"),
          teal.picks::variables(
            choices = tidyselect::everything(),
            selected = c("AVAL", "ADY"),
            multiple = TRUE,
            ordered = TRUE,
            fixed = FALSE
          )
        )
      ),
      transformators = list(
        teal_transform_filter(
          teal.picks::picks(
            teal.picks::datasets("ADRS"),
            teal.picks::variables("PARAMCD"),
            teal.picks::values(selected = "BESRSPI")
          )
        )
      )
    )
  )
)
#> Warning: teal.picks::variables(choices = tidyselect::everything(), selected = c("AGE", "RACE", "SEX"), multiple = TRUE, ordered = TRUE, fixed = FALSE)
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
#> Warning: teal.picks::variables(choices = tidyselect::everything(), selected = c("AVAL", "ADY"), multiple = TRUE, ordered = TRUE, fixed = FALSE)
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
#> Initializing tm_g_scatterplotmatrix
#> Warning: rlang::dots_list(..., .ignore_empty = "trailing")
#>  - Setting explicit `selected` while `choices` are delayed (set using `tidyselect`) doesn't guarantee that `selected` is a subset of `choices`.
#> Warning: variables has eager choices (character) while datasets has dynamic choices. It is not guaranteed that explicitly defined choices will be a subset of data selected in a previous element.
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
