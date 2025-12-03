# Wrappers around `srv_transform_teal_data` that allows to decorate the data

Wrappers around `srv_transform_teal_data` that allows to decorate the
data

## Usage

``` r
srv_decorate_teal_data(id, data, decorators, expr)

ui_decorate_teal_data(id, decorators, ...)
```

## Arguments

- id:

  (`character(1)`) `shiny` module instance id.

- data:

  (`teal_data`, `teal_data_module`, or `reactive` returning `teal_data`)
  The data which application will depend on.

- expr:

  (`reactive`) with expression to evaluate on the output of the
  decoration. It must be compatible with `code` argument of
  [`teal.code::eval_code()`](https://insightsengineering.github.io/teal.code/latest-tag/reference/eval_code.html).
  Default is `NULL` which won't evaluate any appending code.

- ...:

  ([`dots`](https://rdrr.io/r/base/dots.html)) additional arguments
  passed to future methods.

## Details

`srv_decorate_teal_data` is a wrapper around `srv_transform_teal_data`
that allows to decorate the data with additional expressions. When
original `teal_data` object is in error state, it will show that error
first.

`ui_decorate_teal_data` is a wrapper around `ui_transform_teal_data`.
