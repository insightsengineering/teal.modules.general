# teal.modules.general 0.3.0.9038

* Removed `Show Warnings` modals from modules.

### Enhancements
* Added `teal.logger` functionality for logging changes in shiny inputs in all modules.

* Users can now provide their own card functions to specify the content that modules send to reports.

# teal.modules.general 0.3.0

### Enhancements
* Updated the package docs and vignettes with the new way of specifying data for `teal::init()`. The `data` argument will accept a `teal_data` object

### Bug fixes
* Outlier labels no longer appear out of bounds in `tm_a_regression`.
* Fixed a bug in `tm_outliers` when changing the selected variable would cause a popup.

### Miscellaneous
* Removed `teal.slice` dependencies.
* Specified minimal version of package dependencies.

# teal.modules.general 0.2.16

### Breaking changes
* Replaced `chunks` with simpler `qenv` class.
* Replaced `datasets` argument containing `FilteredData` with the new arguments `data` (`tdata` object) and `filter_panel_api` (`FilterPanelAPI`).

### Enhancements
* Added `parent_dataname` argument to `tm_variable_browser` and `tm_missing_data` to allow specification of parent dataset for these modules.
* Improved `UI` labels and plot panel title in `tm_g_association`.
* Added inputs `tm_variable_browser` module for text size and plot theme.
* Forced `ggplot` theme to be always selected in all modules.
* Updated encodings input checks to use `shinyvalidate::InputValidator` instead of `shiny::validate` for better `UI` experience.

### Bug fixes
* Fixed a bug in `tm_g_scatterplot` when selected x and y facets were the same.
* Fixed a bug in `tm_g_distribution` to plot the theoretical distribution with newer `ggplot2` version.
* Fixed a bug in `tm_g_bivariate` when adding lines checkbox was available if one of x or y was deselected.
* Fixed a bug in `tm_variable_browser` when changing filters would reset the selected variable to the first on the list.

### Miscellaneous
* Removed `scda` package dependency from examples.
* Replaced deprecated `ggplot2` functions `..count..`, `..density..` and `..prop..`.
* Version bump on `forcats` dependency.
* Replaced `scda` data generation functions with `random.cdisc.data`

# teal.modules.general 0.2.15

### Enhancements
* Added the `teal.reporter` functionality to all modules.
* Implemented `nestcolor` in the examples, refactored `tm_a_pca` and `tm_missing_data` to allow using `nestcolor`.
* Added log transformation options to `tm_g_scatterplot`.
* Added `server_rendering` flag to `tm_data_table` to control whether the table is rendered server or client side.

### Bug fixes
* Fixed the overflow of very wide `tm_a_pca` tables.
* Fixed the join type functionality in `tm_t_crosstable`.
* Fixed a bug in `tm_missing_data` when selecting only variables with missings.
* Fixed a bug in `tm_missing_data` when using `any_na`.

### Miscellaneous
* Moved packages only used in one module from `Imports` to `Suggests` in the `DESCRIPTION` file.
* Moved `magrittr` package from `Depends` to `Imports` in the `DESCRIPTION` file.

# teal.modules.general 0.2.14

### Enhancements
* New `teal` module `tm_front_page` to simplify creating a front page for `teal` apps.
* Added a slider widget to control the font size of the label in `tm_g_scatterplot`.
* Output integers without decimal places when selecting points in `tm_g_scatterplot`.
* Improved the names of the code chunks shown in `Debug Info`.
* Improved a validation message when the number of regressors is too big in `tm_a_regression.R`.

### Bug fixes
* Fixed a wrong validation in `tm_a_pca`.
* Fixed a crash when deselecting the categorical factor in `tm_outliers`.

### Miscellaneous
* Added a template to the `pkgdown` site.
* Updated package authors.

# teal.modules.general 0.2.13

### Enhancements
* Rewrote modules to use `moduleServer` and updated call to `plot_with_settings_srv` after changes in `teal.devel`.
* Fixed tracking of the selection order in `data_extract_ui`. All selectors can return ordered selection if one specifies `ordered = TRUE` in `select_spec`.
* Switched order of `tm_missing_data` combination plot to show data in descending order.
* Changed `By variable levels` output in `tm_missing_data` to allow numerical sorting.

### Miscellaneous
* Removed unneeded `n` row in `tm_t_crosstable`.
* Replaced calls to `teal::root_modules` with `teal::modules` following deprecation of `teal::root_modules`.
* Adjusted package imports to take into account changes to the `teal` framework.
* Added the "Getting started with teal.modules.general" vignette.
* Updated `README` file.
* Moved the `ggmosaic` package from `Depends` to `Imports` in the `DESCRIPTION` file.

# teal.modules.general 0.2.12

### Enhancements
* Added support for logging with the `logger` package and added info level logs upon initialization of a module.
* Added support for custom arguments for `ggplot2::labs` and `ggplot2::theme` in plot based modules.
* Added support for custom arguments for `rtables::basic_table` in `tm_t_crosstable`.
* Updated `tm_outliers`, `tm_g_scatterplotmatrix`, `tm_g_association`, and `tm_t_crosstable` modules to adopt the new `teal.transform::data_merge_srv` and `teal.transform::data_extract_multiple_srv` modules.
* Distinguished bars representing `NA` in plot depicting counts of `tm_variable_browser` with a different color fill.
* Modified the summary statistics table for numeric columns in `tm_variable_browser` to reflect the plot after `outliers` are removed.
* Added an option to remove missing values in a `tm_variable_browser` histogram for factor like variables.
* Added ability to sort by `Variable` and `Type` in `tm_variable_browser` by separating the variable type icons into their own column.
* Updated the `Grouped by Subject` tab of the `tm_missing_data` module to present data the same way the `Summary` tab does.
* Added support for `NA` level in grouping variable in `By variable levels` table in `tm_missing_data` module.
* Added informative labels for each level of grouping variable in `By variable levels` table in `tm_missing_data` module.
* Added a checkbox to `tm_g_scatterplot` to toggle the option to free up the x and y axis scales whenever faceting arguments are provided.
* Used browser-side processing in `tm_data_table` so that `Buttons` extension could download full table. Added example for advanced usage of `DT` in the module.

### Bug fixes
* Fixed an error in `tm_variable_browser` when the selected column is `logical(1)`.
* Fixed bugs in the modules' vignette examples.

### Miscellaneous
* Added R version requirement `R >= 3.6`.
* Removed `input_id` argument from `teal.transform::data_merge_module` calls in all modules.
* Refactored the defunct `teal.devel::data_extract_input` into its replacement `teal.transform::data_extract_ui`.
* Updated `teal.transform::data_merge_srv` to use `dplyr::inner_join` instead of `dplyr::left_join` in `tm_outliers` module.
* Removed the overlay statistics table in `tm_g_distribution` module.
* Removed dependency on `test.nest` package.
* Removed dependency on `utils.nest` package and replaced its functions with equivalents from the `checkmate` package.

# teal.modules.general 0.2.11

### New features
* A new module, `tm_file_viewer`, was added for the visualization of static files.
* A new module, `tm_g_distribution`, was added for distribution analysis.

### Bug fixes
* Fixed the bar plot order for factor variables in `tm_variable_browser`.

### Enhancements
#### `tm_variable_browser`
* Added `sparkline` support for `Date`/`POSIXct`/`POSIXlt` variable types.
* Fixed histograms to contain at least two bars.

#### `tm_outliers`
* Added `filter_spec` support.
* Simplified returned R code.
* Support `select_spec` for categorical variables.

#### `tm_g_scatterplotmatrix`
* Added ordered select input support.
* Decoupled the dataset list from the variables list parameters.

#### `tm_g_association`
* Added ordered select input support.

#### `tm_t_crosstable`
* Added ordered select input support.

#### `tm_g_scatterplot`
* Modified the trend line feature of the scatterplot to display the raw equation that will equal the actual `y` value when the actual `x` values are plugged in.

### Miscellaneous
* Updated `LICENCE` and `README` with new package references.
* Added `error_on_lint: TRUE` to `.lintr`.
* Updated quantile `type = 2` argument in `tm_variable_browser` to be in line with STREAM.

# teal.modules.general 0.2.10
### Enhancements
* Refactored the internal code of all the modules to optimize their performance.
* Refactored `tm_g_scatterplot` to compute trend line statistics using `ggpmisc::stat_poly_eq` to simplify code and to have labels for each group on the plot.
* Refactored `tm_g_scatterplot` to enable filtering via the Encoding Panel using the new `filter_spec` functionality.
* Added support for logical variables in `tm_variable_browser`.
* Updated `tm_outliers` to handle non-`CDISC` datasets.
* Updated `UI` of `tm_variable_browser` for factor and character variables with more than 30 levels.
* Updated `sparklines` to remove NA values for numeric variables.
* Added checkbox to display number of observations on plot to `tm_g_scatterplot`.
* Added validation statements to `tm_g_scatterplot` handling users choosing more than one facet variable.
* Improved performance of `tm_variable_browser` by a more efficient `sparkline` generation.
* Added the html code type for all datasets labels inside `tm_missing_data`.
* Improved the styling of tool tips in `tm_a_regression`, `tm_missing_data` and `tm_variable_browser`.

# teal.modules.general 0.2.9
### Enhancements
* Added download, enlarge and resize graph options to `tm_variable_browser` module.
* Added download and expand options to the table in `tm_t_crosstable`.
* Enabled selection of custom datasets for `tm_variable_browser` with `datasets_selected` argument.
* Allowed all dataset variables to be used as grouping variables in `by variable levels` tab of `tm_missing_data`, not just those selected to be displayed as rows in the table.
* Enabled the brushing of points in NA categories when facetting in `tm_g_scatterplot`.

### Bug fixes
* Added persistence for the number of entries displayed in data tables.
* Removed 'treat variable as factor' checkbox in `tm_variable_browser` if no graph shown.
* Fixed `DT` length reset in `tm_variable_browser` on "Show parent dataset variables" checkbox tick.

# teal.modules.general 0.2.8

### New Module
#### `tm_outliers`
* Added new module `tm_outliers` to analyze outliers in datasets.

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
* `Sparklines` no longer shown for numeric variables with more than 100000 rows.
* Colors legend for each plot.
* Replaced `"Show variables other than in ADSL"` checkbox to more general `"Show parent dataset variables"`.
* Capitalize default module labels.
* Some of the outputs of `tm_missing_data` are conditional to the data being a `CDISC` data.
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
* Integrated `is_single_dataset` argument for `data_extract_input` function calls to simplify encodings panel `UI`.
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
* Pass on `dataname` to `get_rcode_srv`.
* Uses utils function to standardize plot label generation.
* Improved handling of variable labels.
* Removed "Add as filter variable" button from variable browser module.
* Scatterplot matrix module now automatically converts characters to factors with a message.

# teal.modules.general 0.2.4

* New PCA module.
* Documentation fix for cross table module.
* Display variable labels in drop-down menu of data table (#393).
* Optional subsetting and ordering datasets for `tm_data_table`.
* Use `teal.code::chunks_push_data_merge` to include merge code into reproducibility code.
* More consistent coloring behavior between bivariate plots.
* Updates to missing data module.
* Updated graph axis labels for the following modules: bivariate plot, missing data, response plot, regression module, scatterplot, scatterplot matrix.
* Simplify scatterplot matrix to display plot within one dataset only.

# teal.modules.general 0.2.3

* Fixed naming in `tm_variable_browser`.
* Correlation added to Scatter Plot Matrix.
* Opacity slider added to `tm_bivariate`.
* Fixes due to bug in teal.devel#313.
* Performance enhancements to missing data module.
* Fix bug with reactivity in scatterplot matrix module.

# teal.modules.general 0.2.2

* New module to summarize missing data.
* Refactor of variable browser module.
* Correct display of density / frequency option in bivariate plot.

# teal.modules.general 0.2.1

* Fix `magrittr` loading in reproducible code.

# teal.modules.general 0.2.0

* Refactor functions with data extract and data merge.
* Create sample_app.R demonstrating teal.modules.general modules.
* Rename `tm_table` on `tm_cross_table`.
* Include show R code in all modules.
* Merge `tm_cross_table` and `tm_t_percentage_table` into one module.

# teal.modules.general 0.1.0

* Initial release.
