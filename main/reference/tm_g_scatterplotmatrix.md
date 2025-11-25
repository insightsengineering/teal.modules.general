# `teal` module: Scatterplot matrix

Generates a scatterplot matrix from selected `variables` from datasets.
Each plot within the matrix represents the relationship between two
variables, providing the overview of correlations and distributions
across selected data.

## Usage

``` r
tm_g_scatterplotmatrix(
  label = "Scatterplot Matrix",
  variables,
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

  (`data_extract_spec` or `list` of multiple `data_extract_spec`)
  Specifies plotting variables from an incoming dataset with filtering
  and selecting. In case of `data_extract_spec` use
  `select_spec(..., ordered = TRUE)` if plot elements should be rendered
  according to selection order.

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

## Note

For more examples, please see the vignette "Using scatterplot matrix"
via
[`vignette("using-scatterplot-matrix", package = "teal.modules.general")`](https://insightsengineering.github.io/teal.modules.general/articles/using-scatterplot-matrix.md).

## Decorating Module

This module generates the following objects, which can be modified in
place using decorators:

- `plot` (`trellis` - output of
  [`lattice::splom`](https://rdrr.io/pkg/lattice/man/splom.html))

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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXVIhrqu-lCkULpwAB6wqCJKAUEGxlzUAPoxUDbRgcFGugDutKQAFirsKbi6IEq6usTmpIy0YnH+GRgmzPDsFZW6tPy6ALxVHWAAIq54ugpgAGJYk6WTAJJ2cxNgzgDKK5MACsvjkwDis-tgAHIAMltgAEJj82DnjlcAgntyuJ2VEkQ6jBDwZH6pnUpCIjA6EC6XQI7AATKV4bpEQBGBFo3SojGlTGw96fLrUKD0ODUURAmGTACyJBYBHyrCuWDgqEE9DoBEmikhXTx3MqhIgEkEUCkiRMsFo1FYQPFGjBEKhlRhmIAzKU1boNRqUdj1djeYrdITiaTyUMDjIYFBlByTgAJEkiG2MohWixwTn4g1dVBEFmEzQkM0ADg1ADYAOylMMABlKABYoxi4xjMcik8jMTGMGHSgBOb2VFhwYIDGEqgCsSYrFdVMcxFZjFfzKJViPjdYROdKocLXn4qDNKow8a7SdhGDTI9K2Y12cR2aT2ebumzWZHff4cHopDNE5xGG106R3ZPR9HZ67F4n565lTvulE3AaWRSLTacAVXR6QORiEbHx8lURA1GwiQ-gMT4wJEn74kqQyjFcMxXEsVwbFcuxXEcVwXFctxXI8LxvIBhqiLQABeegDABcG6IwzKEgQVH2E4rhAX2rAlowQKiGCpDsFBMFwvWMaILCMbiaUAF0Qx6jMQ4LhyH2OgQIIzGCSI7AUmAohWtQ1BXPA-C0IIMBXISjBSFcJB0D4nJSSm9GRHJQIKc4fYENaTH6XAvSQREmkwm5pRTM85zrO5DmlE5jHyaxfYAI7CmQeTSgMfxgjA7DSciMYpuJHlEKIpBkulECZdlKbBvlMZ9qgjBECYeRAhljBZdJknJg+XIAL5cgAVkQKiJAA1nArCiMUGS2Fkg3DWNE1fnNECjeNQzVGQdRiFcG21PUohXD09mdMtq0cJMT4iAdJyXdtJxHWA3qnQt60gZt+1XLd12lNpD1ApMu1gQ9SlKFyShoIOWQqHkX4pECJSdHwQhXUCSPCGIX6VKQMCJBIiSiJ5pDkIwkRENjgR1KEmMEkSJL-WA6yE8TpO7lae2hHMtFaCwtBEijAx0MV1NQikiRhLUIL46gcAUkBhopNA8D04DH14LRUKiCSMu7pBWsaFLMvC4aRq034AyTOseu7tzdR82IiCc3Lxt0kNTGlboNu82ycCJC7tBu1NQTAMAANvXtd0ALoR32xuPlbvlmpMxZQFcEgDlcW47sdTuGjAwiaDBrmsSRsddGCW70X5LEuCXpemLQoQJwMoXhWxdcPoaMddKL4vMProjS7LdcK7AzEXc+33q10TXUMTMqSsTBtD3XJsmvTlsiBoHs83bDtqznio2+7oegaw4H8I7K9VPkrsNAM3PUGpvs3-7GMpMH49XZMUf3IDZ-A7XUumtN7kCrtpRCJxkInFQicTCJxsInFwicfCJxCInFeNnK+edZ60ELgMNyU97yANIlbHiVsl5G0NMaOm5sGakM9nbUQe9iHOxfm7IEDDvbP1vpNd+IcdIT2-hHH6QwkrWk0KQBkJxiDFW+qseqjU8ici7iQkBTdBiTDESlSRO0iolSuAopqpBMEr2wQXEQRca6ELLowCu6jgrWMqE1RuVcW4RWsR3RUnj7xek6FyLktATC6HYCoYmIJtCfiUmUToohCgQFYM8dA7AIYABJBC0FKKkzWjBfi9SUGAHqEcgA)

- example-2:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqUBiXQGEAIgEkAZS9dfihSKF04AA9YVBElUPCDYy5qAH1EqBsEsIijXQB3WlIACxV2TNxdECVdXQBBHwCAGWTdVIxMxERGRpbahp8sALaOrp7G4aUAX0UIACsiFTSAazhWUQrc23z+OBMoYVI0gn5aUQI0xeW1jeBoeE3MuQBdN2h0NpVi9gHM3QAvCFcrgBnwhCJRIDdODhGJfhA6nVSDA0hI0hcwuRGHEiCiwoxaFEEUikdQoPQ4NRoQowAECFiZLjSDCCUTaaDEaTdFoWLQKZDoXRRKQSdy6pk0tFSMwNBjUHACGLxRLcg89EDaX1mhyBirdKIqYqWUDDSI5aIFUq9frdOTKdTNXSjRoeXyBWJELqubbdAQSksCGJobzCR6TgHaEGnrlgMAtU0dWAXi85JzfXUzca4PxodawPUAOKuPC6WlYepeEtVWkBZwADVpaZt+pgR1ocQ19icznTGaIjD2jBz0IcLj7vpMRJHQIAYvVmnWW9y5vrmz7uZLpbLjpbFcqVZl1TSC0MAt7J7RqNjoVPrzJ5fvl+L7VSTwEXSyKPxUEsyKIvTwZ9uVDKEgXzAAFeoKwAWV8DkywLAA1QI-DsJsJ1tf1A2DIFeWoQQ4AjHCY3COMEymZMXiqSDoPqOCfAQrUUICNCML9X4wCg2CmOQ1D0LAOR1wzA1PxnRC-AAOSQ5w7F0YxnEknxdAAeVnXQpJ8RwvDsPwVMki8Mzba8OxEUce2AuphNtLNXVNT9H2tDd9VfR1EI-c0WVDfl6EhQDMP1bCo1wt0w18oigujLYyPjU9KJTazfVs8hc3Azii2rRD6iQhdeMaABNDDLKRYzNE7czx2KuoByHcSx17KrTGnVLdHnRdXGcldgNXLqNx6uY5loExdHYFRsXUTQdBsWwai5UQyggVh6nQdg0FQAASQRaCqNb1sNRgdEYOZpiUMBpheIA)

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
        data_extract_spec(
          dataname = "countries",
          select = select_spec(
            label = "Select variables:",
            choices = variable_choices(data[["countries"]]),
            selected = c("area", "gdp", "debt"),
            multiple = TRUE,
            ordered = TRUE,
            fixed = FALSE
          )
        ),
        data_extract_spec(
          dataname = "sales",
          filter = filter_spec(
            label = "Select variable:",
            vars = "country_id",
            choices = value_choices(data[["sales"]], "country_id"),
            selected = c("DE", "FR", "IT", "PT", "GR", "NL", "BE", "LU", "AT"),
            multiple = TRUE
          ),
          select = select_spec(
            label = "Select variables:",
            choices = variable_choices(data[["sales"]], c("quantity", "costs", "profit")),
            selected = c("quantity", "costs", "profit"),
            multiple = TRUE,
            ordered = TRUE,
            fixed = FALSE
          )
        )
      )
    )
  )
)
#> Initializing tm_g_scatterplotmatrix
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
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            label = "Select variables:",
            choices = variable_choices(data[["ADSL"]]),
            selected = c("AGE", "RACE", "SEX"),
            multiple = TRUE,
            ordered = TRUE,
            fixed = FALSE
          )
        ),
        data_extract_spec(
          dataname = "ADRS",
          filter = filter_spec(
            label = "Select endpoints:",
            vars = c("PARAMCD", "AVISIT"),
            choices = value_choices(data[["ADRS"]], c("PARAMCD", "AVISIT"), c("PARAM", "AVISIT")),
            selected = "INVET - END OF INDUCTION",
            multiple = TRUE
          ),
          select = select_spec(
            label = "Select variables:",
            choices = variable_choices(data[["ADRS"]]),
            selected = c("AGE", "AVAL", "ADY"),
            multiple = TRUE,
            ordered = TRUE,
            fixed = FALSE
          )
        )
      )
    )
  )
)
#> Initializing tm_g_scatterplotmatrix
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
