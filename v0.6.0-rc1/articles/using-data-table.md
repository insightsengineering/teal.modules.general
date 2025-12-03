# Using data table

## `teal` application to display data table with various datasets types

This vignette will guide you through the four parts to create a `teal`
application using various types of datasets using the data table module
[`tm_data_table()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_data_table.md):

1.  Load libraries
2.  Create data sets
3.  Create an `app` variable
4.  Run the app

### 1 - Load libraries

``` r
library(teal.modules.general) # used to create the app
```

### 2 - Create data sets

Inside this app 3 datasets will be used

1.  `ADSL` A wide data set with subject data
2.  `ADTTE` A long data set with time to event data
3.  `ADLB` A long data set with lab measurements for each subject

``` r
data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
  ADTTE <- teal.data::rADTTE
  ADLB <- teal.data::rADLB
})
join_keys(data) <- default_cdisc_join_keys[names(data)]
```

### 3 - Create an `app` variable

This is the most important section. We will use the
[`teal::init()`](https://insightsengineering.github.io/teal/latest-tag/reference/init.html)
function to create an app. The data will be handed over using
[`teal.data::teal_data()`](https://insightsengineering.github.io/teal.data/latest-tag/reference/teal_data.html).
The app itself will be constructed by multiple calls of
[`tm_data_table()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_data_table.md)
using different combinations of data sets.

``` r
# configuration for the two-datasets example
mod1 <- tm_data_table(
  label = "Two datasets",
  variables_selected = list(
    ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"),
    ADTTE = c(
      "STUDYID", "USUBJID", "SUBJID", "SITEID",
      "PARAM", "PARAMCD", "ARM", "ARMCD", "AVAL", "CNSR"
    )
  )
)

# configuration for the subsetting or changing order of datasets
mod2 <- tm_data_table(
  label = "Datasets order",
  variables_selected = list(
    ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"),
    ADLB = c(
      "STUDYID", "USUBJID", "SUBJID", "SITEID",
      "PARAM", "PARAMCD", "AVISIT", "AVISITN", "AVAL", "CHG"
    )
  ),
  datanames = c("ADTTE", "ADLB", "ADSL")
)

# configuration for the advanced usage of DT options and extensions
mod3 <- tm_data_table(
  label = "Advanced DT usage",
  dt_args = list(extensions = c("Buttons", "ColReorder", "FixedHeader")),
  dt_options = list(
    searching = FALSE,
    pageLength = 30,
    lengthMenu = c(5, 15, 25, 50, 100),
    scrollX = FALSE,
    dom = "lBrtip",
    buttons = c("copy", "csv", "excel", "pdf", "print"),
    colReorder = TRUE,
    fixedHeader = TRUE
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    mod1,
    mod2,
    mod3
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
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQACAMQLBouPwWkiCgozhRyWgBZwFaVAB0I-Q1AUAeALRaD1APq3SUdnOtf7ZwUAd1pSY1oIdgDcBRBrBQUAQQARAGUAGUcXLh4AxERGVMyE5JSAFXKAUWzXbgx8wtTKqtLUjIAhWtyGuwKilM7rAF8-CAArIkj3AGs4VlFou0Ug-jgAMyhhUncCflpRAndJ6bmF4Gh4RYC5AF1rFWIIddoJQWZSWhIFdaJGEzMpGCRCcAXUpFECjgAA9YKgRNY+PwAIzdGCeOzubz0ETsUrUKD0ODUBQAXgUljA5WBCjBcAhlNwpQAbixaISRKJ3OoRARyJpyXRRKQ8RBEolillyQQ8WA0uUAKopACaAEkUoyKWAFWkFR0AFLqzWU3UGo14LVpVXVc2xSlJADirQtJqqAA1KXImWLxc1qmTdKLxeKTYqVbatTq9YaNS65dGIybrVVbaVg5SAApJLBJACyxrAWZzuYAwrG7WBs-m41WywWkgA1JIZAslgByaSwlLTCjGiTGYweuhILzeHy+Yt+-3CZlEgno4M+EAkCj+umMUGXkRXfzW-yI61pdnBokRAgATGiMd4sRy4EGFASiSTyZSUsf6ZDdzJGSy2XeuR5OA+Q0AMhRFHtJQDGVQyVNVy0jU0YwLJDEzlZM0MdZ0KzSd1PW9YN2i6aUH3TOUw3ggsozNBCTQTWj0Jtcse0STNszzAsizzOsawbVUrXKes+IEtshObVsAAkHW7H1+1KL1SgCS4xGg2U-WwrUiPrdIWzAAdrCHJ5R3eQwJx+NcZ3MfhWQgAhQLUKApFXQ8KlXVBPhISFN00GFyAgUQJ1PCAkQAZivAJbxxe98UJYkA3tazNzszRXIcqRfx9fgdhYCRIUFA4RV8igAs81TKQ6QRSG0fzWyIagsDgb9GALAAxWhoQ0CSDD3T0FMynYiHcwKwIK0iFHUFgCAiZcAxa5tcII8VUEcuAMgoCRwgDEKAAZFsSERl3CXMKEEVSAFZYmRC6FHPa6zt2hRkW27a+uDQ5GDq6g3Vm+aqj22kiBgeKaA6RhPisPAe3oSrqrywNKWIVBWALAhRGZAsYTs6gC1Qfh1hxxhIlIfCe2IerGsYPcA3KLAFT+nsXg6-guqgKnyRpun5OsQcIBUSIwnZOgAC9AVMcx0GsCxan5iDMrsAMYlKJFhBU8llc5MakWRf6kXPHWBBCrmIDGURptYJJ0HYCwABJBFoWIbfURhmRkWJBo8-yRuFdhTFeYxSADJ7zwAFliUIsuMQPtpDuQ5DAYZbiAA)
