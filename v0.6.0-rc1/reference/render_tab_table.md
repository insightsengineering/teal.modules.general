# Renders the table for a single dataset in the left-hand side tabset panel

The table contains column names, column labels, small summary about NA
values and `sparkline` (if appropriate).

## Usage

``` r
render_tab_table(
  dataset_name,
  parent_dataname,
  output,
  data,
  input,
  columns_names,
  plot_var
)
```

## Arguments

- dataset_name:

  (`character`) the name of the dataset

- parent_dataname:

  (`character`) the name of a parent `dataname` to filter out variables
  from

- output:

  (`session$output`) the `shiny` session output

- data:

  (`teal_data`) the object containing all datasets

- input:

  (`session$input`) the `shiny` session input

- columns_names:

  (`environment`) the environment containing bindings for each dataset

- plot_var:

  (`list`) the list containing the currently selected dataset (tab) and
  its column names
