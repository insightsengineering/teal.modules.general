# `teal` module: File viewer

The file viewer module provides a tool to view static files. Supported
formats include text formats, `PDF`, `PNG` `APNG`, `JPEG` `SVG`, `WEBP`,
`GIF` and `BMP`.

## Usage

``` r
tm_file_viewer(
  label = "File Viewer Module",
  input_path = list(`Current Working Directory` = ".")
)
```

## Arguments

- label:

  (`character(1)`) Label shown in the navigation item for the module or
  module group. For `modules()` defaults to `"root"`. See `Details`.

- input_path:

  (`list`) of the input paths, optional. Each element can be:

  Paths can be specified as absolute paths or relative to the running
  directory of the application. Default to the current working directory
  if not supplied.

## Value

Object of class `teal_module` to be used in `teal` applications.

## Examples in Shinylive

- example-1:

  [Open in
  Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQAdCLTIyoBUrQBucAAQAeALS6AZoIgbaJdnN0AVLAFUAokqX8opKAeNdqAfQ8vG3dPbyNdAHdaUgALFXYgqFxdECVdXSSfTLCME2Z4dgBGRQgAX1KlNFRslRj2dJyvXQBeJuTGviERUVbdLuExBogMjNIYfxNaEX8tWjhImWHR0ZVUQVJ-VE9YvrpRUmWV0ZMian4ZPtFWA7gYPOm4BrBRWFQZqZ6FPF1tggBrKBSPrfPy8ASDcRSCBqajfOS4RrHX4QCRXG7ke6fJ7fV4wd5wSaPUQAejxBKJIgwqFR3xSf0BwLaoLg3HB3TEkmksnhiJGyN0pAAHqR0bcsY9nuSPsSyW8ZVThaQ6b91Iy9MywGCBj0uTCeWAEUjjoJGNQ+tjqTsjgLdN9YqRSKhRIgSSTIh68h5JEQtCTsaT+IxBBJSet6PtYnB+HTjcjvgAFIiMLwRuCGAAiRAIgngZEMADFkzBPIYAMqoOAEWhTAieKwQcSofgmb5x0alZGdjuNUqlGu6dgqcjMSw6Gy2NL80TxCCsACC6HY1QAJIJaClV6IZDpGKUykowGUALpAA)

## Examples

``` r
data <- teal_data()
data <- within(data, {
  data <- data.frame(1)
})

app <- init(
  data = data,
  modules = modules(
    tm_file_viewer(
      input_path = list(
        folder = system.file("sample_files", package = "teal.modules.general"),
        png = system.file("sample_files/sample_file.png", package = "teal.modules.general"),
        txt = system.file("sample_files/sample_file.txt", package = "teal.modules.general"),
        url = file.path(
          "https://www.fda.gov/files/drugs/published",
          "Portable-Document-Format-Specifications.pdf"
        )
      )
    )
  )
)
#> Initializing tm_file_viewer
if (interactive()) {
  shinyApp(app$ui, app$server)
}
```
