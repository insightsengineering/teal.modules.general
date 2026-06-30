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
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQACAMQLBouPwWkiCgozhRyWgBZwFaVAB0IdJiw79U1Vo0Uq1GrTr79aAM1YFfkMoBXVSUWtg0lCAHgBaLQNqAH1oqHY5KJCFBIUAd1pSY1oIdnTcBRBrBQUAQQARAGUAGVzErh50xERGRtaFAFIAPkGa2oUYQRjydgAVLDmGgFUsBQBeBUYiQQh+dihxCEF4RloCecWAUQa5gFkFRIW5ptu7uUqARjksiFrGrBNdpJbgYbq9AFNcaNOZzK7AzpgkI9Pq3OHQhotABCCOSSJiKMa2KGo3GtSmMzg7AAwgAJADi1I25nEfigBG0jHYBEOcBS+VMZTJEzp9NyCk+CgAfgpLGAAHJy3DC2qihTDCXS2VgAAKSpVCgWy3hMrl8Tlwp+vwAvr8AFZEUopADWcFYonKIUUeX4cDZwlIKQIvlEBBSDqdrvdwGg8A96TkAF1rCpiBA-LQJIJmKRaCQFH4iIwTGZRKUJCICrRfUEQuFrD5JXlSDAUhIUqHDORGE4iC3DGcAB7scbUKD0ODUZlyprlyuFGvpet4cYANxYtHHIlEzPSKTgg9IzA5HdQcAuwvSsbMmzl-Ra+r+E3UIg5zJf58DojPF6fEwUY4TlOt5gE0k6fgo65nFuYiII+-61AQxiOgQYjMlBm70CIQbIecYiejEwDAHezQPmAiaJh8BofhynibBeYB1PSVxKtqWB1NSLF4NqTRXAAGqxcpYncWAANJYJ8glgMJYlYAATHKVF-hMUzULmTg3oaWDGsqym1Bmg50QoABidQtLxukIQoRa+vomibEaLHKb8tS-L8Ka6CQGZZjmeZ-IWxbFGYJBmNQJASLWMThJEEA+HJCKtu2nakN2vb9ketDDuMcoAPIQKF4WRbypDwZBG4wTumx7geR7sl+P4jspV6wJpJGwlxlnPuBb6bDR9Xno1VlIShaGbBhME4SN8YhERbXouRiaVAxdQAGpmVJMniZJ3FCSJ4kKWAPydf+fVGcta1kZUu2ydtV3SXt8mKcdKkBrQGnMo5z36ZlRmmeZVxfdZjC2UZn0GoBk7TqB3WkGV0FYbBFrOeMbnWB5abedmhh+QWRYllo+Q6GFEARUucARA2AgAMwJW2HY8ilMhpTAA6ZYNAHjpDIFzITAGFWTFMrk+40I5VAG0KIpDs7U1WHse-W-lZzXwFDkKlSdMPvjDp4DQatQQ8BPGayL25wULVmIbhqFiybfLDXh02EcRjENICcoUUpFthDDZ0joxF1SatZnUk9euTK970OdpANhwZv1mRZYc2TIoPR4DnsIRmakyMyWfdjrisWwbUNga+sMUI4jpkKIZuA7UUFiwxOp1OxdzUg0gcrQAkk0XdzKHekTPb1vodwgh21b+HpLNLtuwtS1+83rft53Pd909uiLy3dR3Kvvf94dGdWad9najlK1YF38orY8ChNNSWBXFc8pX-S6sIap6mVpsf28QaLkTCPkVfccs6oF2lhMZWrUXbtXfrUPqWsy7gLDsXECpcIK2xrnAoek8bblQRpNB2BEoAzxhPND2ddvZl19neAOO0wDUnlE0LAA8vafzet-LSOlY4-VPr-GOg9ajJzsh9aO-9AZ5xzpsSRjBkGCI5kBEumtUAsBat2LB5sLYNyhkvHeK9NFDVwaPag49CHW2IaQtEXEKJ3V0W3Du9C7GsItifKGOUoQGI-hHThjl-6WhRtYdyEAVClCKJuOgAAvMwQVzDoGsBYYEoSpbjHSLuEInUfDCFGpMAQWSPTCkyduCB+tOaGxnAzVKYVYYswyoObBjZAZxUadTfxT5UYQFECUCArA6joAOOgAAJIIWglQLADPUIwVcMhKhEFQLmEgYs6CS3YKYTMxhYabE+AABjkgAFkqAuYozJtl7J+GAa0iYgA)
