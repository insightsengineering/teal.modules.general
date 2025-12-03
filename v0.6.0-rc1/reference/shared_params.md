# Shared parameters documentation

Defines common arguments shared across multiple functions in the package
to avoid repetition by using `inheritParams`.

## Arguments

- plot_height:

  (`numeric`) optional, specifies the plot height as a three-element
  vector of `value`, `min`, and `max` intended for use with a slider UI
  element.

- plot_width:

  (`numeric`) optional, specifies the plot width as a three-element
  vector of `value`, `min`, and `max` for a slider encoding the plot
  width.

- rotate_xaxis_labels:

  (`logical`) optional, whether to rotate plot X axis labels. Does not
  rotate by default (`FALSE`).

- ggtheme:

  (`character`) optional, `ggplot2` theme to be used by default.
  Defaults to `"gray"`.

- ggplot2_args:

  (`ggplot2_args`) object created by
  [`teal.widgets::ggplot2_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/ggplot2_args.html)
  with settings for the module plot. The argument is merged with options
  variable `teal.ggplot2_args` and default module setup.

  For more details see the vignette:
  [`vignette("custom-ggplot2-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-ggplot2-arguments.html)

- basic_table_args:

  (`basic_table_args`) object created by
  [`teal.widgets::basic_table_args()`](https://insightsengineering.github.io/teal.widgets/latest-tag/reference/basic_table_args.html)
  with settings for the module table. The argument is merged with
  options variable `teal.basic_table_args` and default module setup.

  For more details see the vignette:
  [`vignette("custom-basic-table-arguments", package = "teal.widgets")`](https://insightsengineering.github.io/teal.widgets/latest-tag/articles/custom-basic-table-arguments.html)

- pre_output:

  (`shiny.tag`) optional, text or UI element to be displayed before the
  module's output, providing context or a title. with text placed before
  the output to put the output into context. For example a title.

- post_output:

  (`shiny.tag`) optional, text or UI element to be displayed after the
  module's output, adding context or further instructions. Elements like
  [`shiny::helpText()`](https://rdrr.io/pkg/shiny/man/helpText.html) are
  useful.

- alpha:

  (`integer(1)` or `integer(3)`) optional, specifies point opacity.

  - When the length of `alpha` is one: the plot points will have a fixed
    opacity.

  - When the length of `alpha` is three: the plot points opacity are
    dynamically adjusted based on vector of `value`, `min`, and `max`.

- size:

  (`integer(1)` or `integer(3)`) optional, specifies point size.

  - When the length of `size` is one: the plot point sizes will have a
    fixed size.

  - When the length of `size` is three: the plot points size are
    dynamically adjusted based on vector of `value`, `min`, and `max`.

- decorators:

  **\[experimental\]** (named `list` of lists of
  `teal_transform_module`) optional, decorator for tables or plots
  included in the module output reported. The decorators are applied to
  the respective output objects.

  See section "Decorating Module" below for more details.

## Value

Object of class `teal_module` to be used in `teal` applications.
