# `teal` module: R Markdown render

**\[experimental\]**

Module to render R Markdown files using the data provided in the
`teal_data` object.

The R Markdown file should be designed to accept variables available in
the data names of the module.

## Usage

``` r
tm_rmarkdown(
  label = "RMarkdown Module",
  rmd_content,
  datanames = "all",
  allow_download = TRUE,
  pre_output = NULL,
  post_output = NULL,
  transformators = list(),
  extra_transform = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- rmd_content:

  (`character`) Content of the R Markdown file to be rendered. This can
  be the value of `readLines("path/to/file.Rmd")`.

- datanames:

  (`character`) Names of the datasets relevant to the item. There are 2
  reserved values that have specific behaviors:

  - The keyword `"all"` includes all datasets available in the data
    passed to the teal application.

  - `NULL` hides the sidebar panel completely.

  - If `transformators` are specified, their `datanames` are
    automatically added to this `datanames` argument.

- allow_download:

  (`logical`) whether to allow downloading of the R Markdown file.
  Defaults to `TRUE`.

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

- extra_transform:

  (`list`) of
  [`teal::teal_transform_module()`](https://insightsengineering.github.io/teal/latest-tag/reference/teal_transform_module.html)
  that will be added in the module's UI. This can be used to create
  interactive inputs that modify the parameters in R Markdown rendering.

## Value

Object of class `teal_module` to be used in `teal` applications.

## Details

For example, if the `teal_data` object contains datasets named `mtcars`
and `iris`, the R Markdown file can use these as variables as they will
be available in the R Markdown environment.

The libraries used in the R Markdown file must be available in the
deployed shiny app environment.

When developing the R Markdown file, the working data can be simulated
on a code chunk, which in turn can look for the presence of `.raw_data`
object to determine if it is being run inside the `teal` module or not.

Example R markdown file:

    ---
    title: "R Markdown Report"
    output: html_document
    ---

    ```{r eval=!exists(".raw_data")}
    mtcars <- datasets::mtcars
    iris <- datasets::iris
    ```

    ```{r}
    summary(mtcars) |> print()
    summary(iris) |> print()
    ```

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
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokrcQAxLqkQ11XfxQpFC6cAAesKgiSoHBBsZc1AD6sVA2MUEhRroA7rSkABYq7Km4uiBKuroAwgDyAEzxNQ1KAL6KEEpoqE0q+eyVAZm6ALxDwbiDfEIioqO608JiAxBVVaQwSYwwLADW-EQ5ECtra9RQ9HD+YwpgWACyewdHuvcCS7eTq6e62-xJxFUZHmBBOPyqt0MUM+g3Bt00pBEiF0ClRd1eT0OqywcFQREYpFRtxh3zhYCIglIqEpyIKG2SBwIgngZBJ4IhYChhjZ7OJeFhP1uAAMRSBGK0eWTRMydmx2HV6rYAD4APl0qEYKlI6X5pMFYBFQuJeqqHVOZt0HQ6tBMunYWrUlh0NlsFW+oiKEFYAEF0OxugASQS0MqB0QyHSMDoSiBgVoAXSAA)

## Examples

``` r

# general data example
data <- teal_data()
data <- within(data, {
  CO2 <- CO2
})

app <- init(
  data = data,
  modules = modules(
    tm_rmarkdown(
      label = "RMarkdown Module",
      rmd_content = c(
        "---",
        "title: \"R Markdown Report\"",
        "output: html_document",
        "---",
        "",
        "```{r}",
        "summary(CO2) |> print()",
        "```"
      )
    )
  )
)
#> Initializing tm_rmarkdown
if (interactive()) {
  shinyApp(app$ui, app$server)
}

nrow_transform <- teal_transform_module(
  label = "N Rows selector",
  ui = function(id) {
    ns <- NS(id)
    tags$div(
      numericInput(ns("n_rows"), "Show n rows", value = 40, min = 0, max = 200, step = 5)
    )
  },
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      reactive({
        req(data())
        within(data(),
          {
            n_rows <- n_rows_value
          },
          n_rows_value = input$n_rows
        )
      })
    })
  }
)

app <- init(
  data = data,
  modules = modules(
    tm_rmarkdown(
      label = "RMarkdown Module",
      rmd_content = readLines(
        system.file(
          file.path("sample_files", "co2_example.Rmd"),
          package = "teal.modules.general"
        )
      ),
      allow_download = FALSE,
      extra_transform = list(nrow_transform)
    )
  )
)
#> Initializing tm_rmarkdown

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
