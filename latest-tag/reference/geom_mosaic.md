# Mosaic Rectangles Layer for ggplot2

Adds a mosaic-style rectangles layer to a ggplot, visualizing the joint
distribution of categorical variables. Each rectangle's size reflects
the proportion of observations for combinations of `x` and `fill`.

## Usage

``` r
geom_mosaic(
  mapping = NULL,
  data = NULL,
  stat = "mosaic",
  position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = TRUE,
  inherit.aes = TRUE
)

.calculate_coordinates(data)
```

## Arguments

- mapping:

  Set of aesthetic mappings created by
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html). Must
  specify `x` and `fill`.

- data:

  The data to be displayed in this layer.

- stat:

  The statistical transformation to use on the data. Defaults to
  `"rects"`.

- position:

  Position adjustment. Defaults to `"identity"`.

- ...:

  Other arguments passed to
  [`layer()`](https://ggplot2.tidyverse.org/reference/layer.html).

- na.rm:

  Logical. Should missing values be removed?

- show.legend:

  Logical. Should this layer be included in the legends?

- inherit.aes:

  Logical. If `FALSE`, overrides default aesthetics.

## Value

A ggplot2 layer that adds mosaic rectangles to the plot.

## Functions

- `.calculate_coordinates()`: Computes the coordinates for rectangles in
  a mosaic plot based on combinations of `x` and `fill` variables. For
  each unique `x` and `fill`, calculates the proportional widths and
  heights, stacking rectangles within each `x` group.

  ### Value

  A data frame with columns: `x`, `fill`, `xmin`, `xmax`, `ymin`,
  `ymax`, representing the position and size of each rectangle.

## Examples

``` r
df <- data.frame(RACE = c("Black", "White", "Black", "Asian"), SEX = c("M", "M", "F", "F"))
library(ggplot2)
ggplot(df) +
  geom_mosaic(aes(x = RACE, fill = SEX))
```
