# Validates the variable browser inputs

Validates the variable browser inputs

## Usage

``` r
validate_input(input, plot_var, data)
```

## Arguments

- input:

  (`session$input`) the `shiny` session input

- plot_var:

  (`list`) list of a data frame and an array of variable names

- data:

  (`teal_data`) the datasets passed to the module

## Value

`logical` TRUE if validations pass; a `shiny` validation error otherwise
