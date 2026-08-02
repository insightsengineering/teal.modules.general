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
  variables = picks(
    datasets("ADSL", "ADSL"),
    variables(
      selected = c("AGE", "RACE", "SEX", "BMRKR1", "BMRKR2"),
      multiple = TRUE
    )
  )
)

# configuration for the one long datasets
mod2 <- tm_g_scatterplotmatrix(
  "One long dataset",
  variables = picks(
    datasets("ADTTE", "ADTTE"),
    variables(
      c("AVAL", "BMRKR1", "BMRKR2"),
      c("AVAL", "BMRKR1", "BMRKR2"),
      multiple = TRUE
    )
  )
)

# configuration for the two long datasets
mod3 <- tm_g_scatterplotmatrix(
  label = "Two long datasets",
  variables = list(
    picks(
      datasets("ADRS", "ADRS"),
      variables(
        selected = c("AVAL", "AVALC"),
        multiple = TRUE
      )
    ),
    picks(
      datasets("ADTTE", "ADTTE"),
      variables(
        selected = c("AVAL", "CNSR"),
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
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQACAMQLBouPwWkiCgozhRyWgBZwFaVAB0IdJiw79U1Vo0Uq1GrTr79aAM1YFfkMoBXVSUWtg0lCAHgBaLQNqAH1oqHY5KJCFBIUAd1pSY1oIdnTcBRBrBQUAQQARAGUAGVzErh50xERGRtaFAFIAPkGa2oUYQRjydgAVLDmGgFUsBQBeBUYiQQh+dihxCEF4RloCecWAUQa5gFkFRIW5ptu7uUqARjksiFrGrBNdpJbgYbq9AFNcaNOZzK7AzpgkI9Pq3OHQhotABCCOSSJiKMa2KGo3GtSmMzg7AAwgAJADi1I25nEfigBG0jHYBEOcBS+VMZTJEzp9NyCk+CgAfgpLGAAHJy3DC2qihTDCXS2VgAAKSpVCgWy3hMrl8Tlwp+vwAvr8AFZEUopADWcFYonKIUUeX4cDZwlIKQIvlEBBSDqdrvdwGg8A96TkAF1rCpiBA-LQJIJmKRaCQFH4iIwTGZRKUJCICrRfUEQuFrD5JXlSDAUhIUqHDORGE4iC3DGcAB7scbUKD0ODUZlyprlyuFGvpet4cYANxYtHHIlEzNQ52dHuFS7gERHYH6LSV2ovco+wvXZy3YhHfwmtXUIg5nk2FzldXpVxXnKWB1NSgF4NqTRXAAGkBYBYncWAANJYJ8cEIchWAAEy3sqr5vlM1C5k4ZibEagH4Qovy1L8vwproJAZlmOZ5n8hbFsUZgkGY1AkBItYxOEkQQD4WEIq27adqQ3a9v2pBDi+tRygA8hAPF8QJvKkPqr4Ppu9Dbru+6HpRx6nn+aLgZUFmwuBd6UXpT4mW+tS-ueABqdSXhBcoYShaE+fBiEoThYD2S5uhnnUnnedZQWYQFcV+dhuEGoRxGVmRWDGpa4y0dY9Fpkx2aGKxBZFiWWj5DovEQPxZnCT4ADM4lth2PLSTIskwAOtDDqO46TtOYBzNVCi1fVdYnpEK66RuTnMnQoikIpEx7gQB6rW+DVRQ0gJwZCqWUbUjkGc+BrvpOcBfpoP5RTFB0xdSR0ReSAa0CRzLkQa1ETOFa3GVtEw7TZ6KBTCYP-W+p3bkDb4ftd5C3ZFf4PYF1Lyk0WAva96UfZlhrZRREW-TReXWHREAqKURSbnQABeZiceY6DWBYwI0yt4zpMyFTjD4whiMyAuw8KIvncd42DVOmwzh1Mm8aQky9YOOkRY2eHqwIWGay5zW5a++UQKIJQQKwdToAc6AACSCLQlQWNb6iMKuMiVEQqC5iQO6bEtK2mJmxhK5snwAAxYQALJUC7FMyYeRz8YDWomQA)
