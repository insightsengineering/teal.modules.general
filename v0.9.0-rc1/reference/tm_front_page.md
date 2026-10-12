# `teal` module: Front page

Creates a simple front page for `teal` applications, displaying
introductory text, tables, additional `html` or `shiny` tags, and
footnotes.

## Usage

``` r
tm_front_page(
  label = "Front page",
  header_text = character(0),
  tables = list(),
  additional_tags = tagList(),
  footnotes = character(0),
  show_metadata = deprecated(),
  datanames = if (missing(show_metadata)) NULL else "all",
  transformators = list()
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- header_text:

  (`character` vector) text to be shown at the top of the module, for
  each element, if named the name is shown first in bold as a header
  followed by the value. The first element's header is displayed larger
  than the others.

- tables:

  (`named list` of `data.frame`s) tables to be shown in the module.

- additional_tags:

  (`shiny.tag.list` or `html`) additional `shiny` tags or `html` to be
  included after the table, for example to include an image,
  `tagList(tags$img(src = "image.png"))` or to include further `html`,
  `HTML("html text here")`.

- footnotes:

  (`character` vector) of text to be shown at the bottom of the module,
  for each element, if named the name is shown first in bold, followed
  by the value.

- show_metadata:

  (`logical`) **\[deprecated\]** indicating whether the metadata of the
  datasets be available on the module. Metadata shown automatically when
  `datanames` set.

- datanames:

  (`character`) Names of the datasets relevant to the item. There are 2
  reserved values that have specific behaviors:

  - The keyword `"all"` includes all datasets available in the data
    passed to the teal application.

  - `NULL` hides the sidebar panel completely.

  - If `transformators` are specified, their `datanames` are
    automatically added to this `datanames` argument.

- transformators:

  (`list` of `teal_transform_module`) that will be applied to transform
  module's data input. To learn more check
  [`vignette("transform-input-data", package = "teal")`](https://insightsengineering.github.io/teal/latest-tag/articles/transform-input-data.html).

## Value

Object of class `teal_module` to be used in `teal` applications.

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqX8opKAeNdqAfQ8vG3dPbyNdAHdaUgALFXYgqFxdECVdXUY4AEdBWiz2CDFSYmoiRkUIDIBBABEAZQAZH10-DCTEREY6pvTdT1JGdh7GlIUweC8k8dsIulFSdnHqwTjy8d0AXl1xgDlnertWuFhxsbAk-1EiQUYCOA3t8dFWMli4TQJdabBKgF9KgArIgqfwAazgrFEiTCs2M-DgJigwlI-gI-FoogI-mBoIhUOA0Hg0KScgAum4yFB6CJ-ABGFpJDAmZjwdgASQgJiIW10BCWYGqZx2YAAQjMUnY4AAPUi8-nLYXjcW-SpeGlwfwAJkZYWZrLg7AABgBhIjUQQwKp0o3ygUmpVgWoS3Sm82Wqpa23bfkAVgwvpSADYMEG5Ck3RarboAMzevkCoV4EUquRq6m0mO6rz62CGzncu3jVzJ8YAMRdUtlRbAAHFHQAJGaVJTq2kqVCrFrzRZ9cZ2DN6OmPVqD+m4PtgAca3RakdtzVaidVEXTkSx+djmNKFvQdAtFQxJYrpK8pLLjJ8IQiUS8q-CMTHjIZUgwfwskio1BQKRP58Zd4oARRh-HIasfT-f8MnGdkYFQcovDIXQVG5RgYE8KwIBHGC5QIKAqgkHl3iyDAzj6KCRQAeTiGRkK5cp0M0EhsLAE18N0QjdGIuBSLAcjn3DfiX0HW9tgXfwO1WC8oKAjEmOgAIvAkUTdAbOwAFlGkTfg5Mw7hVI05pyl0UR4ggVhR2UjiiJkPR9CYAA+CUhNMIgiFICB3LEGsAA0WMxVp3lMfIFlc9zPPIR0AE1kNvGiTLgYgIH4MKPK8mZ+MqDJKlsAAfJyVyvWgTFYfxAOAzgf1EAASWI6QFepYFQddqnQOg8Pk5tdHyvoipK983PIIYlJq1BEza2gOsw1KZGbSlit0dgVCG9RNB0GxbDSFdTJUVhWrGtBUGqvIUkO6rRBkHQKiUP4lDAP4ySAA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  require(nestcolor)
  ADSL <- teal.data::rADSL
  attr(ADSL, "metadata") <- list("Author" = "NEST team", "data_source" = "synthetic data")
})
join_keys(data) <- default_cdisc_join_keys[names(data)]

table_1 <- data.frame(Info = c("A", "B"), Text = c("A", "B"))
table_2 <- data.frame(`Column 1` = c("C", "D"), `Column 2` = c(5.5, 6.6), `Column 3` = c("A", "B"))
table_3 <- data.frame(Info = c("E", "F"), Text = c("G", "H"))

table_input <- list(
  "Table 1" = table_1,
  "Table 2" = table_2,
  "Table 3" = table_3
)

app <- init(
  data = data,
  modules = modules(
    tm_front_page(
      header_text = c(
        "Important information" = "It can go here.",
        "Other information" = "Can go here."
      ),
      tables = table_input,
      additional_tags = HTML("Additional HTML or shiny tags go here <br>"),
      footnotes = c("X" = "is the first footnote", "Y is the second footnote")
    )
  )
) |>
  modify_header(tags$h1("Sample Application")) |>
  modify_footer(tags$p("Application footer"))
#> Initializing tm_front_page

if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
