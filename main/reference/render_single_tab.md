# Renders a single tab in the left-hand side tabset panel

Renders a single tab in the left-hand side tabset panel. The rendered
tab contains information about one dataset out of many presented in the
module.

## Usage

``` r
render_single_tab(
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

  (`character`) the name of the dataset contained in the rendered tab

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
