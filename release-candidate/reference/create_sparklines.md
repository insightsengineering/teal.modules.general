# S3 generic for `sparkline` widget HTML

Generates the `sparkline` HTML code corresponding to the input array.
For numeric variables creates a box plot, for character and factors -
bar plot. Produces an empty string for variables of other types.

## Usage

``` r
create_sparklines(arr, width = 150, ...)

# S3 method for class 'logical'
create_sparklines(arr, ...)

# S3 method for class 'numeric'
create_sparklines(arr, width = 150, ...)

# S3 method for class 'character'
create_sparklines(arr, ...)

# S3 method for class 'factor'
create_sparklines(arr, width = 150, bar_spacing = 5, bar_width = 20, ...)

# S3 method for class 'Date'
create_sparklines(arr, width = 150, bar_spacing = 5, bar_width = 20, ...)

# S3 method for class 'POSIXct'
create_sparklines(arr, width = 150, bar_spacing = 5, bar_width = 20, ...)

# S3 method for class 'POSIXlt'
create_sparklines(arr, width = 150, bar_spacing = 5, bar_width = 20, ...)

# Default S3 method
create_sparklines(arr, width = 150, ...)
```

## Arguments

- arr:

  vector of any type and length

- width:

  `numeric` the width of the `sparkline` widget (pixels)

- ...:

  `list` additional options passed to bar plots of `jquery.sparkline`;
  see
  [`jquery.sparkline docs`](https://omnipotent.net/jquery.sparkline/#common)

- bar_spacing:

  `numeric` the spacing between the bars (in pixels)

- bar_width:

  `numeric` the width of the bars (in pixels)

## Value

Character string containing HTML code of the `sparkline` HTML widget.
