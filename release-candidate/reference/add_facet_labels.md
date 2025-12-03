# Add labels for facets to a `ggplot2` object

Enhances a `ggplot2` plot by adding labels that describe the faceting
variables along the x and y axes.

## Usage

``` r
add_facet_labels(p, xfacet_label = NULL, yfacet_label = NULL)
```

## Arguments

- p:

  (`ggplot2`) object to which facet labels will be added.

- xfacet_label:

  (`character`) Label for the facet along the x-axis. If `NULL`, no
  label is added. If a vector, labels are joined with " & ".

- yfacet_label:

  (`character`) Label for the facet along the y-axis. Similar behavior
  to `xfacet_label`.

## Value

Returns `grid` or `grob` object (to be drawn with `grid.draw`)

## Examples

``` r
library(ggplot2)
library(grid)

p <- ggplot(mtcars) +
  aes(x = mpg, y = disp) +
  geom_point() +
  facet_grid(gear ~ cyl)

xfacet_label <- "cylinders"
yfacet_label <- "gear"
res <- add_facet_labels(p, xfacet_label, yfacet_label)
grid.newpage()
grid.draw(res)


grid.newpage()
grid.draw(add_facet_labels(p, xfacet_label = NULL, yfacet_label))

grid.newpage()
grid.draw(add_facet_labels(p, xfacet_label, yfacet_label = NULL))

grid.newpage()
grid.draw(add_facet_labels(p, xfacet_label = NULL, yfacet_label = NULL))

```
