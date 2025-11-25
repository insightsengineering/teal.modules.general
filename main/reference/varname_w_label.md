# Generate a string for a variable including its label

Generate a string for a variable including its label

## Usage

``` r
varname_w_label(
  var_names,
  dataset,
  wrap_width = 80,
  prefix = NULL,
  suffix = NULL
)
```

## Arguments

- var_names:

  (`character`) Name of variable to extract labels from.

- dataset:

  (`dataset`) Name of analysis dataset.

- wrap_width:

  (`numeric`) Number of characters to wrap original label to. Defaults
  to 80.

- prefix, suffix:

  (`character`) String to paste to the beginning/end of the variable
  name with label.

## Value

(`character`) String with variable name and label.
