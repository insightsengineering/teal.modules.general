# Creates observers updating the currently selected column

The created observers update the column currently selected in the
left-hand side tabset panel.

## Usage

``` r
establish_updating_selection(datanames, input, plot_var, columns_names)
```

## Arguments

- datanames:

  (`character`) the name of the dataset

- input:

  (`session$input`) the `shiny` session input

- plot_var:

  (`list`) the list containing the currently selected dataset (tab) and
  its column names

- columns_names:

  (`environment`) the environment containing bindings for each dataset

## Note

Creates an observer for each dataset (each tab in the tabset panel).
