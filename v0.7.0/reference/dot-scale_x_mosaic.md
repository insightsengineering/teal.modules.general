# Determining scales for mosaics

Determining scales for mosaics

## Usage

``` r
.scale_x_mosaic(
  breaks = unique,
  minor_breaks = NULL,
  labels = unique,
  na.value = NA_real_,
  position = "bottom",
  ...
)
```

## Arguments

- breaks, labels, minor_breaks:

  One of:

  - `NULL` for no breaks / labels.

  - [`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
    for the default breaks / labels computed by the scale.

  - A numeric / character vector giving the positions of the breaks /
    labels.

  - A function. See
    [`ggplot2::scale_x_continuous()`](https://ggplot2.tidyverse.org/reference/scale_continuous.html)
    for more details.

- na.value:

  The value to be used for `NA` values.

- position:

  For position scales, The position of the axis. left or right for y
  axes, top or bottom for x axes.

- ...:

  other arguments passed to
  [`continuous_scale()`](https://ggplot2.tidyverse.org/reference/continuous_scale.html).
