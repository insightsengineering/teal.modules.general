# JavaScript expression to check if a tab is active

JavaScript expression to check if a tab is active

## Usage

``` r
is_tab_active_js(id, name)
```

## Arguments

- id:

  (`character(1)`) the id of the tab panel with tabs.

- name:

  (`character(1)`) the name of the tab.

## Value

JavaScript expression to be used in
[`shiny::conditionalPanel()`](https://rdrr.io/pkg/shiny/man/conditionalPanel.html)
to determine if the specified tab is active.
