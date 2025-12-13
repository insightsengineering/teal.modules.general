# Using scatterplot matrix

## `teal` application to use scatter plot matrix with various datasets types

This vignette will guide you through the four parts to create a `teal`
application using various types of datasets using the scatter plot
matrix module
[`tm_g_scatterplotmatrix()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_g_scatterplotmatrix.md):

1.  Load libraries
2.  Create data sets
3.  Create an `app` variable
4.  Run the app

### 1 - Load libraries

``` r
library(teal.modules.general) # used to create the app
library(dplyr) # used to modify data sets
library(lattice)
```

### 2 - Create data sets

Inside this app 4 datasets will be used

1.  `ADSL` A wide data set with subject data
2.  `ADRS` A long data set with response data for subjects at different
    time points of the study
3.  `ADTTE` A long data set with time to event data
4.  `ADLB` A long data set with lab measurements for each subject

``` r
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL %>%
    mutate(TRTDUR = round(as.numeric(TRTEDTM - TRTSDTM), 1))
  ADRS <- teal.data::rADRS
  ADTTE <- teal.data::rADTTE
  ADLB <- teal.data::rADLB %>%
    mutate(CHGC = as.factor(case_when(
      CHG < 1 ~ "N",
      CHG > 1 ~ "P",
      TRUE ~ "-"
    )))
})
join_keys(data) <- default_cdisc_join_keys[names(data)]
```

### 3 - Create an `app` variable

This is the most important section. We will use the
[`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)
function to create an app. The data will be handed over using
[`teal.data::teal_data()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html).
The app itself will be constructed by multiple calls of
[`tm_g_scatterplotmatrix()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_g_scatterplotmatrix.md)
using different combinations of data sets.

``` r
# configuration for the single wide dataset
mod1 <- tm_g_scatterplotmatrix(
  label = "Single wide dataset",
  variables = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]]),
      selected = c("AGE", "RACE", "SEX", "BMRKR1", "BMRKR2"),
      multiple = TRUE,
      fixed = FALSE,
      ordered = TRUE
    )
  )
)

# configuration for the one long datasets
mod2 <- tm_g_scatterplotmatrix(
  "One long dataset",
  variables = data_extract_spec(
    dataname = "ADTTE",
    select = select_spec(
      choices = variable_choices(data[["ADTTE"]], c("AVAL", "BMRKR1", "BMRKR2")),
      selected = c("AVAL", "BMRKR1", "BMRKR2"),
      multiple = TRUE,
      fixed = FALSE,
      ordered = TRUE,
      label = "Select variables:"
    )
  )
)

# configuration for the two long datasets
mod3 <- tm_g_scatterplotmatrix(
  label = "Two long datasets",
  variables = list(
    data_extract_spec(
      dataname = "ADRS",
      select = select_spec(
        label = "Select variables:",
        choices = variable_choices(data[["ADRS"]]),
        selected = c("AVAL", "AVALC"),
        multiple = TRUE,
        fixed = FALSE,
        ordered = TRUE,
      ),
      filter = filter_spec(
        label = "Select endpoints:",
        vars = c("PARAMCD", "AVISIT"),
        choices = value_choices(data[["ADRS"]], c("PARAMCD", "AVISIT"), c("PARAM", "AVISIT")),
        selected = "OVRINV - SCREENING",
        multiple = FALSE
      )
    ),
    data_extract_spec(
      dataname = "ADTTE",
      select = select_spec(
        label = "Select variables:",
        choices = variable_choices(data[["ADTTE"]]),
        selected = c("AVAL", "CNSR"),
        multiple = TRUE,
        fixed = FALSE,
        ordered = TRUE
      ),
      filter = filter_spec(
        label = "Select parameters:",
        vars = "PARAMCD",
        choices = value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
        selected = "OS",
        multiple = TRUE
      )
    )
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    modules(
      label = "Scatterplot matrix",
      mod1,
      mod2,
      mod3
    )
  )
)
```

### 4 - Run the app

A simple
[`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html) call
will let you run the app. Note that app is only displayed when running
this code inside an `R` session.

``` r
shinyApp(app$ui, app$server, options = list(height = 1024, width = 1024))
```

### 5 - Try it out in Shinylive

[Open in
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQACAMQLBouPwWkiCgozhRyWgBZwFaVAB0IdJiw79U1Vo0Uq1GrTr79aAM1YFfkMoBXVSUWtbZjZ2akNSWgI4OWtg0lCAHgBaLQNqAH10qHZUiGKFHIUAd1pSY1oIdmLcBRBrBQUAQQARAGUAGUrcrh5ixERGXsGFAFIAPlmOzoUYQQzydgAVLC2egFUsBQBeBUYiQXL2KHEIQXhGJO3dgFEerYBZBVydrb73j5yVoARjkZU6vSwfWGeW4GHGk0hfWWvS2WxeMNG8JCEym73RKJ6AwAQpj8tiMrjeiS5otlp01hs4OwAMIACQA4iyTuZxH4oARtIx2AQbnACtVTE16St2RzKgpgQoAH4KSxgABy6twMs6coU80VKrVYAACtrdQodvsMar1dl1TKwWUAL5lABWREaBQA1nBWKJmiFFFV+HB+cJSAUCL5RAQCp7vX6A8BoPBA8U5ABdawqYgQPy0CSCZiJEgKPxERgmMyiRoSEQ1WhhoIhcLWHxKqqkGAFCQFOMJGROIg9wyPAAe7GW8XocGoPPVfXrjdqLeK7bwywAbixaFB6CJRDzigU4BPSMxBQPUHACNOICtWxk02ZTurpgMLY+VuoRIKeT-O8o1EW970tWd50XMA+nnYCFF3R4DyPRBvyfToCGML1kmPU5EP3Q9xUw7CxCDDJgGAD9+i-MAsyzIFLSAwVPFOcCwC6DkXm1E0sC6FkuLwE0+heAANbj1WJD4sAAaSwYFxLASSZKwAAmdUGJ-J81moRInDfK0sBtHVNJWQsJxYhQADEugGYTjPQhQqzDfRNFOa0uJM8EFDKMpc10EhC2LUtaHLStq3qMwSDMagSAkZ8xQiDsBBUzFe37QdSHIRgRzHS9aCnZZ1QAeQgaLYvizd7IQvdkLEE8QjPC8rxAsCHyfYpX2g1ECS3EymNIQC4OvUC7za9DiKSOq8JqwjoywyaMxCCiqLRAS6NaNiugANRshSlNk+TBIkqTZLUsAwSqp9+oszadpo1pjuUw6HsUk7VPUy6Vm03TGzcwyXk+zozIs6zbIBy0nJkCz3MBhRIIXd8YKGgb8Nq0RULAJ1lh86w-PzQKS0MELHzCmstGqHQYogOKNzgRKIB8ABmVK+wHUVMuHGLcsnMb4egrYKbh8racS3rOlRwjcLh2hRFIMbOlPc9LwFFrRstDrYH0qioTQ9D+sG-9VfAkynz5xHYMN6qkMljHYYw+acJ5CWRDmkjFvIyj2J6HXaPou2wmRm7p3Yu6FO2myWQ+y0GUjWg9J5GHo4rfKQZsuyk8hlyE-+2GNIcwsdJkHkC6ym81ZNlYzaE5GFAoRwvTIdHdYcxCpbY00ul4j4WR6MOtoAST6futijiv7bdp3uEEIiHdI4plq9n31t0YOO67nu+8H4ePpX9U166D5N6HkfzrzhyA8Niziq2rB+41LbvgUPoWSwF4Xg1O+OWb9Dvrj36rLTh5ByXlOhnwVg1JWzUy7GwchreAXV8QCVhvrU4-VoHy3QlXJcNdnZiFtknCajtprWxdoQueS1PbdTWn7JO11XK7xDrtI6YAWQaj6FgUe59Vix3jn9IySdgb0NBunMejlGDOWhv9S0YDTK0ELtWU4JcZDoKTlgpGltUAsE1llJuYtz6t2gvvbuvc9EOTIVLXc1Bp6uwWmRKAC8qHqmXnvTuB8N7MKMZw8+dDoJFWRKYn+PD-7uWkVjH8OMIB+UaHUfcdAABeZgIrmHQNYCwMJoly2WBUU4LRlg+GEFNVYAgCmBhlPko8GDOhqL6OzLKOVVjjnyt-BkAhgSwx8CpdpAhGZhNAdYMoogGgQFYF0dA1x0AABJBC0FaBYCZ6hGDbhkK0IgqAywQClnQWW7BTBFmMANU4wIAAMKkAAsrQ1z1B5Mcs5YIwAuizEAA)
