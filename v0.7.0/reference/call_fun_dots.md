# Call a function with a character vector for the `...` argument

Call a function with a character vector for the `...` argument

## Usage

``` r
call_fun_dots(fun, str_args)
```

## Arguments

- fun:

  (`character`) Name of a function where the `...` argument shall be
  replaced by values from `str_args`.

- str_args:

  (`character`) A character vector that the function shall be executed
  with

## Value

Value of call to `fun` with arguments specified in `str_args`.
