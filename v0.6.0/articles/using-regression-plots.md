# Using regression plots

## `teal` application to use regression plot with various datasets types

This vignette will guide you through the four parts to create a `teal`
application using various types of datasets using the regression plot
module
[`tm_a_regression()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_a_regression.md):

1.  Load libraries
2.  Create data sets
3.  Create an `app` variable
4.  Run the app

### 1 - Load libraries

``` r
library(teal.modules.general) # used to create the app
library(dplyr) # used to modify data sets
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
[`tm_a_regression()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_a_regression.md)
using different combinations of data sets.

``` r
# configuration for the single wide dataset
mod1 <- tm_a_regression(
  label = "Single wide dataset",
  response = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("BMRKR1", "BMRKR2")),
      selected = "BMRKR1",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  regressor = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "SEX", "RACE")),
      selected = "AGE",
      multiple = TRUE,
      fixed = FALSE
    )
  )
)

# configuration for the two wide datasets
mod2 <- tm_a_regression(
  label = "Two wide datasets",
  default_plot_type = 2,
  response = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("BMRKR1", "BMRKR2")),
      selected = "BMRKR1",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  regressor = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "SEX", "RACE")),
      selected = c("AGE", "RACE"),
      multiple = TRUE,
      fixed = FALSE
    )
  )
)

# configuration for the same long datasets (same subset)
mod3 <- tm_a_regression(
  label = "Same long datasets (same subset)",
  default_plot_type = 2,
  response = data_extract_spec(
    dataname = "ADTTE",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADTTE"]], c("AVAL", "CNSR")),
      selected = "AVAL",
      multiple = FALSE,
      fixed = FALSE
    ),
    filter = filter_spec(
      label = "Select parameter:",
      vars = "PARAMCD",
      choices = value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
      selected = "PFS",
      multiple = FALSE
    )
  ),
  regressor = data_extract_spec(
    dataname = "ADTTE",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADTTE"]], c("AGE", "CNSR", "SEX")),
      selected = c("AGE", "CNSR", "SEX"),
      multiple = TRUE
    ),
    filter = filter_spec(
      label = "Select parameter:",
      vars = "PARAMCD",
      choices = value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
      selected = "PFS",
      multiple = FALSE
    )
  )
)

# configuration for the wide and long datasets
mod4 <- tm_a_regression(
  label = "Wide and long datasets",
  response = data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
        selected = levels(data[["ADLB"]]$PARAMCD)[2],
        multiple = TRUE,
        label = "Select measurement:"
      ),
      filter_spec(
        vars = "AVISIT",
        choices = levels(data[["ADLB"]]$AVISIT),
        selected = levels(data[["ADLB"]]$AVISIT)[2],
        multiple = TRUE,
        label = "Select visit:"
      )
    ),
    select = select_spec(
      label = "Select variable:",
      choices = "AVAL",
      selected = "AVAL",
      multiple = FALSE,
      fixed = TRUE
    )
  ),
  regressor = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]], c("BMRKR1", "BMRKR2", "AGE")),
      selected = "AGE",
      multiple = TRUE,
      fixed = FALSE
    )
  )
)

# configuration for the same long datasets (different subsets)
mod5 <- tm_a_regression(
  label = "Same long datasets (different subsets)",
  default_plot_type = 2,
  response = data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
        selected = levels(data[["ADLB"]]$PARAMCD)[1],
        multiple = TRUE,
        label = "Select lab:"
      ),
      filter_spec(
        vars = "AVISIT",
        choices = levels(data[["ADLB"]]$AVISIT),
        selected = levels(data[["ADLB"]]$AVISIT)[1],
        multiple = TRUE,
        label = "Select visit:"
      )
    ),
    select = select_spec(
      choices = "AVAL",
      selected = "AVAL",
      multiple = FALSE,
      fixed = TRUE
    )
  ),
  regressor = data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
        selected = levels(data[["ADLB"]]$PARAMCD)[1],
        multiple = FALSE,
        label = "Select labs:"
      ),
      filter_spec(
        vars = "AVISIT",
        choices = levels(data[["ADLB"]]$AVISIT),
        selected = levels(data[["ADLB"]]$AVISIT)[1],
        multiple = FALSE,
        label = "Select visit:"
      )
    ),
    select = select_spec(
      choices = variable_choices(data[["ADLB"]], c("AVAL", "AGE", "BMRKR1", "BMRKR2", "SEX", "ARM")),
      selected = c("AVAL", "BMRKR1"),
      multiple = TRUE
    )
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    modules(
      label = "Regression plots",
      mod1,
      mod2,
      mod3,
      mod4,
      mod5
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
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQACAMQLBouPwWkiCgozhRyWgBZwFaVAB0IdJiw79U1Vo0Uq1GrTr79aAM1YFfkMoBXVSUWtg0lCAHgBaLQNqAH1oqHY5KJCFBIUAd1pSY1oIdnTcBRBrBQUAQQARAGUAGVzErh50xERGRtaFAFIAPkGa2oUYQRjydgAVLDmGgFUsBQBeBUYiQQh+dihxCEF4RloCecWAUQa5gFkFRIW5ptu7uUqARjksiFrGrBNdpJbgYbq9AFNcaNOZzK7AzpgkI9Pq3OHQhotABCCOSSJiKMa2KGo3GtSmMzg7AAwgAJADi1I25nEfigBG0jHYBEOcBS+VMZTJEzp9NyCk+CgAfgpLGAAHJy3DC2qihTDCXS2VgAAKSpVCgWy3hMrl8Tlwp+vwAvr8AFZEUopADWcFYonKIUUeX4cDZwlIKQIvlEBBSDqdrvdwGg8A96TkAF1rCpiBA-LQJIJmKRaCQFH4iIwTGZRKUJCICrRfUEQuFrD5JXlSDAUlAUvoJPpRGWSOxxtQoPQ4NRmXKmuXK4Ua+l63hxt3UCR1Mz0ik4AAPUjMDkpUSoOAXYXpWNmTZy-otfV-CbqEQc5l3w+B-eH-s3iYKQfD0fnsBNEdnwUAA3FhaCHEREGvT9agIYxHQIMRmVAs4IL5OCELET0YmAYAL2aK8wETRNKiPMAsTuLAAGksE+JVtQo6isAAJjlH5lQ-W9AI5Tw-0Ymi6PnTjyQDWgnDPBQADE6haJorg4mCC1oDdeKkmS5MtcYPgXOAuzEUQi1XEJ1y3HcXwPI9OJPWAJPw1poK4+9SEfbjzLfA1vxHMd-1ckCwLQ0QoKExSMPOJDNhQ8D6BEIN4LC+MQlwuzCOI0j+zAOp6SuejxyuAANHKwCwOpqWysB2INJ8eM0P9MrKhSYKmahc3E5kjXkg0MxUmq1Nk7LON+Wpfl+FNdBIDMsxzPM-kLYtijMUh8h0aczFnOAIgbARmIRVt207bteyFG9PN-bU5iWqsZzrdbImCoI-SgAMUicIhA1IVgD2ZZiGsXZcJLXTdt3ZNzLM-az4G8y8HNqKrnM2WG9ws99FJO7yAKcvzUOiuAgoaz9QsQ0RkP87HYswhKcLwjKCLlVLdHS-jaMKxnWPK7ThLCVzVLlRnBLxiYmpaytNmkvr+dqLrVNFjSBq0n7dIOwzNgB0zgcR9yrJCU9IZpu6Yd8+HXPV0GYNRv90aAyKAtxg0CfCzGopiu2KagJLqfsoiSPpi8ssKuSCrwbVitKtj2cU2HVLIurCuDsqw8a0TWs2drxaU7rmWl-rP0GhRhusUa0wm7NDGmgtDPmsIbK-EgJFrGJwiJ9hRCr0RBHocJfh8ABmHa2w7BX9Om5GvyHLzzar6ga7r3kIgUJuW7bjuHN9f1mueye3o+iTvp0-c-qMmITKB3dXxN2pwds6nYXq4VYZcpzjeHz8ze1C2Hyt7GbY553iaxp24sJthV2VMYTok9mlC8AA1GShVqTyiaFgUOqcI49SgTAvWAtE7C16nJVOkseqZ0tPzDMzUZDMhIeQRgj8PKj1OuOXyqAWA2UoV-RSKEiZ-h1HUYqdxqQNGhhMH+EVuCCHQgArC6Q3agLKnTOUXCeF8MKvIuodw2LIK5qg3UkkoQYJEs1MS2DCGyxvPHfa+klbTyPmZahmsYja1qmiG+nE76GwfqfJ+EwX70Ixh-SCAjYLiI4Q7NCZN4pAKkY42mXso6+0DnKOBCC-b5SQZVDRzIYn1W1AkxBcT-zJPKqnQWBiJLtSIcKChZDNgVKoe4mhP40YMKYfAFh-jMZBLkdwlRijdG6ECcTagojQmAMkSAyJ4DtTKN4fw3Jky1GpKctzLROjClYIkkY7OWlrAjQgKmcamZi65nzLNEsl0zBQD2NXCAtc1obQgD4AALL3PaA8exDwHLQ7yAB1asZyLmTyudPBuDlfoQBXMrYygNrG1NsecquDjsQCOqcyOgohSAeIlrQUhNSkYGlqOw7ykzumpwCeTfpgznbhNGQi8ZHSFHTMqLSlRcyOb6wWT1EQwERwuwidS4iAASQlDQ5DAGYiRXFkxVltSwMaYlI96nm18vAQ42Y4DwDIEFA08dPzVJsYpPFLB2kZUgQASSaMauYrSSXxWRXATl1BuVUqxFEvldQTVmrmFqmCKCbV2odfhXliYXVuvNcK0VsqilJ0NNKjqLK5Vj1fr5YCtAyykA1RzHOQ1+YuM5m4nFHMvE+R8STPxPShHaldeg9RbLIbQMIis-RkbM54OUqpUpxjM06T0j2CxKtj4gw8RfHWHss0Gxzc+XVpsPkKqLX-MQrCYJlt8WI8mlLkpRIgeRSiAlmZbpYoVaObMq3PkWQe+tQsSnRubenEW6ks4TBznnCABc9mTRLkc8uphK4Q3+dc66s9yj+D8DICgzlW7txup3AQABWJ5-cu2HWHgWpoE8p43Mbr4PwQH9BkDCIvCDy8HpPRepvT6mwd43hBWCyxkK1bQrBlrOF5bMROowUizYKK0WdUxZQidbCDUEs6VMy1vTSXCIGcusJIz-UsdkbqQTRKJmCeZXqsd1UfVctXdTANAr5NCuAJ8MVsaI3YJTuKpDvlvxpsUp6jFWLeMwXxbVYNFqelWsJup+1mmiQycDa601IbZXevY7ajTUmtM+aDf5j1+nDMqeMxemVZmp0Jp8cmooVmYIZtziOjGrjx10YXX0pzlb5nHs0RWutBp4sZ1vVe1t0bNImPlvBntELVYnzzfRuxjHpOIu45Ur8ybOMcx1QVvjjBDWCuE4ukREnhmJUdeuxTdKlFKYKeKoLX4QuebC9551grhUGfDZKm9YskvypS0Bb8gULTpubXZsbDn+NOai9NorW3fVeeY86vz7qbOOTKx5v14WfvOcO7FvV1XTu4PO-G7xls0uptu9ZspzjR0I0e25+2S6hkSIW718ZUda37tiQyzdTE+YMV3azMn-t91YFUYe0ranNhE8rVTinymE4NpMw19tuctn5x2QoUoRRwJ0AAF4LU-RYawFhgSi+G+fHI4KYgNR8MIe2GuRAemFNrrCdS4dFReYdBQxHbqFIEJ8S3-ByOKW7jb+5NuoONaGoL0FJQICsDqOgA46A+WCFoJUCwfL1CME5YwSoRBUCHNBciob7BTCZmMHDCUAAGZiTvLrFGZJ8DP9yfhgGtImIAA)
