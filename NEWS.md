# teal.modules.general 0.2.6.9000

* Added `shape` arguments to scatterplot and point color input via `colourpicker::colourInput`.
* Added `size_by` variable to scatterplot to size points by a numeric variable.
* Implemented trend line for scatterplot.
* Integrated `is_single_dataset` argument for `data_extract_input` function calls.
* Add remove outliers option to `tm_variable_browser`.
* Display number of non-missing rows `n` in statistics table for numeric variables in `tm_variable_browser`.
* In `tm_variable_browser` numeric variables can now be treated as categorical.  
* Bivariate plots with two continuous variables now allow adding lines.
* Move `code` argument to `cdisc_dataset` (from `cdisc_data`) in examples and vignettes.
* `tm_variable_browser` shows 30 up to 50 levels divided in 2 columns for character/factor variables with more than 30 unique levels.
* `tm_a_pca` updated to include `Plot settings` and `Plot specific settings`
* `tm_variable_browser` now display character variable summary statistics table in decreasing order of occurrences instead of alphabetical. 
* `tm_variable_browser` displays histograms and statistics table for variables of type `Date`, `POSIXct` and `POSIXlt`.
* Remove keys from missing data combinations plot in `tm_missing_data` when they have not been selected. 
* Add marginal density plots to `tm_g_scatterplot`.
* Add rug plot to both axis option in `tm_g_scatterplot`.
* Require `ggmosaic` version >= 0.3.0.
* Show labels on x axis when selecting a categorical variable in the `tm_g_bivariate` module and fixed issues when deselecting variables and datasets in the encoding panel (added meaningful warnings).
* Add bar chart to missing data combination plot.
* issues with no news:

# teal.modules.general 0.2.6

* Adds additional plot settings such as themes for `tm_g_association`, `tm_g_response`, and `tm_g_scatterplot`, as well as point size and opacity settings for `tm_g_association` and `tm_g_scatterplot`.
* Replace `pickerInput` with `optionalSelectInput` for `tm_data_table`.
* Adds new `facet` parameter to `tm_g_bivariate` to specify whether the facet encodings elements should be visible to the user by default.
* Replace `plot_with_height` module with new `plot_with_settings` module. 
* Missing data module now uses chunks to get reproducible R code.
* New theme for missing data module and new label positioning.
* `tm_data_table`: allow developers to pass arguments to `DT::dataTable` and use `pickerInput` to select variables.
* `tm_g_response` a new argument `count_labels`. Counts might be assessed for the frequency plot too.
* Moved missing data module summary plot footer elements to tool tip in encodings panels.
* Replaced `base` plots in `tm_a_regression` with `ggplot2`.
* Added optional slider to adjust `width` in `plot_with_settings`.
* Handle `Inf` in data gracefully.
* `tm_variable_browser` now outputs the summary table for factors with all missing values.

# teal.modules.general 0.2.5

* Adds `ggplot` call inside chunks in modules.
* Pass on dataname to `get_rcode_srv`.
* Uses utils function to standardize plot label generation.
* Improved handling of variable labels.
* Removed "Add as filter variable" button from variable browser module.
* Scatterplot matrix module now automatically converts characters to factors with a message.

# teal.modules.general 0.2.4

* New PCA module.
* Documentation fix for cross table module.
* Display variable labels in drop-down menu of data table (#393).
* Optional subsetting and ordering datasets for tm_data_table.
* Use `teal.devel::chunks_push_data_merge` to include merge code into reproducibility code.
* More consistent coloring behavior between bivariate plots.
* Updates to missing data module.
* Updated graph axis labels for the following modules: bivariate plot, missing data, response plot, regression module, scatterplot, scatterplot matrix.
* Simplify scatterplot matrix to display plot within one dataset only.

# teal.modules.general 0.2.3

* Fixed naming in tm_variable_browser.
* Correlation added to Scatter Plot Matrix.
* Opacity slider added to tm_bivariate.
* Fixes due to bug in teal.devel#313.
* Performance enhancements to missing data module.
* Fix bug with reactivity in scatterplot matrix module.

# teal.modules.general 0.2.2

* New module to summarise missing data.
* Refactor of variable browser module.
* Correct display of density / frequency option in bivariate plot.

# teal.modules.general 0.2.1

* Fix `magrittr` loading in reproducible code.

# teal.modules.general 0.2.0

* Refactor functions with data extract and data merge.
* Create sample_app.R demonstrating teal.modules.general modules.
* Rename tm_table on tm_cross_table.
* Include show R code in all modules.
* Merge tm_cross_table and tm_t_percentage_table into one module.

# teal.modules.general 0.1.0

* Initial release.
