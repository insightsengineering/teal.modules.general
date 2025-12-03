# Create internal markdown object for use in reporter

Creates an object of class `markdown_internal` that contains the content
of a markdown file.

## Usage

``` r
.markdown_internal(markdown_file, rendered_html)

# S3 method for class 'markdown_internal'
toHTML(x, ...)

# S3 method for class 'markdown_internal'
to_rmd(block, figures_dir = "figures", ...)
```

## Arguments

- markdown_file:

  (`character(1)`) path to markdown file.

- rendered_html:

  (`shiny.tag`) rendered HTML content.

- x:

  An object to display.

- ...:

  Arguments that will be passed to the next method.

- block:

  (`any`) content which can be represented in Rmarkdown syntax.

- figures_dir:

  (`character(1)`) directory where the R markdown auxiliary files will
  be saved.

## Value

`markdown_internal` object

## Details

This package registers S3 methods for `toHTML` and `to_rmd` for this
class to facilitate rendering in `teal.reporter`.

## Functions

- `toHTML(markdown_internal)`: Custom
  [`tools::toHTML()`](https://rdrr.io/r/tools/toHTML.html) method for
  markdown_internal class that uses a cached rendering of the module.

- `to_rmd(markdown_internal)`: Custom
  [`teal.reporter::to_rmd()`](https://insightsengineering.github.io/teal.reporter/latest-tag/reference/to_rmd.html)
  method for `markdown_internal` object will be used to render the
  report.
