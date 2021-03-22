# teal.modules.general 0.2.8.9000
Issues with no news:

# teal.modules.general 0.2.8

### New Module
#### `tm_outliers`
* Added new module `tm_outliers` to analyse outliers in datasets.

### Enhancements
#### `tm_g_scatterplot`
* Added `max_deg` optional argument to scatterplot to allow users to choose the maximum smoothing degree for the trend line.
* Added column and row facetting functionality to the scatterplot.
* Added possibility to specify type of join in `tm_t_crosstable`. Incorporate missings resulting from non inner join.

### Bug Fixes
* Fixed edge case errors in `tm_missing_data` caused by input `dataset` not having categorical variables or being a `data.frame` instead of `tibble`.
* Fixed `tm_a_pca` biplot plot when coloring with a factor/character/numeric with < 6 values variable.
* Fixed typo in outlier definition in `tm_variable_browser`.

### Miscellaneous
* Sparklines no longer shown for numeric variables with more than 100000 rows.
* Colors legend for each plot.
* Replaced "Show variables other than in ADSL" checkbox to more general "Show parent dataset variables".
* Capitalize default module labels.
* Some of the outputs of `tm_missing_data` are conditional to the data being a CDISC data.
* Added `pre_output` and `post_output` arguments to `tm_data_table`, `tm_missing_data`, `tm_variable_browser`.
* Fixed issue in `tm_data_table` when selecting "show distinct rows" if variable names had non-alphanumeric characters. 

# teal.modules.general 0.2.7

### Enhancements
#### `tm_variable_browser` 
* Introduced `sparklines` for quick, inline variable summaries.
* Added option to remove outliers.
* Histograms and statistics tables are now displayed for variables of type `Date`, `POSIXct` and `POSIXlt`.
* Character variable summary statistics tables are now displayed in decreasing order of occurrences instead of alphabetical. 
* Display number of non-missing rows `n` in statistics table for numeric variables.
* Added ability to treat numeric variables as categorical.
* `tm_variable_browser` shows from 30 up to 50 levels divided in 2 columns for character/factor variables with more than 30 unique levels.

#### `tm_a_regression`
* Added support for outlier labels to `tm_a_regression` and provided an additional optional argument, `default_outlier_label` which can be used to specify the default column used to label outliers.

#### `tm_g_scatterplot`
* Implemented trend line.
* Added marginal density plots.
* Added option to display rug plot to both axes.
* Added `shape` and `point color` arguments. The latter users `colourpicker::colourInput`.
* Added `size_by` variable to scatterplot and allowed for point size to be mapped to a numeric variable.

#### `tm_missing_data`
* Added bar chart to missing data combination plot.
* Removed keys from missing data combinations plot in when they have not been selected. 

#### `tm_g_bivariate`
* Bivariate plots with two continuous variables now allow adding lines.
* Show labels on x axis when selecting a categorical variable and fixed issues when deselecting variables and datasets in the encodings panel (including the addition of meaningful warnings).

#### `tm_a_pca`
* Updated to include `Plot settings` and `Plot specific settings`.

### Miscellaneous
* Integrated `is_single_dataset` argument for `data_extract_input` function calls to simplify encodings panel UI.
* Moved `code` argument to `cdisc_dataset` (from `cdisc_data`) in examples and vignettes.
* Require `ggmosaic` version >= 0.3.0.

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
