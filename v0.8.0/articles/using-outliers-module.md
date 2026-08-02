# Using outliers module

## `teal` application to analyze and report outliers with various datasets types.

This vignette will guide you through the four parts to create a `teal`
application using various types of datasets using the outliers module
[`tm_outliers()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_outliers.md):

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

Inside this app 3 datasets will be used

1.  `ADSL` A wide data set with subject data
2.  `ADRS` A long data set with response data for subjects at different
    time points of the study
3.  `ADLB` A long data set with lab measurements for each subject

``` r

data <- teal_data()
data <- within(data, {
  ADSL <- teal.data::rADSL
  ADRS <- teal.data::rADRS
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
[`tm_outliers()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_outliers.md)
using different combinations of data sets.

``` r

# configuration for the single wide dataset
mod1 <- tm_outliers(
  label = "Single wide dataset",
  outlier_var = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
      selected = "AGE",
      fixed = FALSE
    )
  ),
  categorical_var = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(
        data[["ADSL"]],
        subset = names(Filter(isTRUE, sapply(data[["ADSL"]], is.factor)))
      ),
      selected = "RACE",
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the wide and long datasets
mod2 <- tm_outliers(
  label = "Wide and long datasets",
  outlier_var = list(
    data_extract_spec(
      dataname = "ADSL",
      select = select_spec(
        label = "Select variable:",
        choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
        selected = "AGE",
        fixed = FALSE
      )
    ),
    data_extract_spec(
      dataname = "ADLB",
      select = select_spec(
        label = "Select variable:",
        choices = variable_choices(data[["ADLB"]], c("AVAL", "CHG2")),
        selected = "AVAL",
        multiple = FALSE,
        fixed = FALSE
      )
    )
  ),
  categorical_var =
    data_extract_spec(
      dataname = "ADSL",
      select = select_spec(
        label = "Select variables:",
        choices = variable_choices(
          data[["ADSL"]],
          subset = names(Filter(isTRUE, sapply(data[["ADSL"]], is.factor)))
        ),
        selected = "RACE",
        multiple = FALSE,
        fixed = FALSE
      )
    )
)

# configuration for the multiple long datasets
mod3 <- tm_outliers(
  label = "Multiple long datasets",
  outlier_var = list(
    data_extract_spec(
      dataname = "ADRS",
      select = select_spec(
        label = "Select variable:",
        choices = variable_choices(data[["ADRS"]], c("ADY", "EOSDY")),
        selected = "ADY",
        fixed = FALSE
      )
    ),
    data_extract_spec(
      dataname = "ADLB",
      select = select_spec(
        label = "Select variable:",
        choices = variable_choices(data[["ADLB"]], c("AVAL", "CHG2")),
        selected = "AVAL",
        multiple = FALSE,
        fixed = FALSE
      )
    )
  ),
  categorical_var = list(
    data_extract_spec(
      dataname = "ADRS",
      select = select_spec(
        label = "Select variables:",
        choices = variable_choices(data[["ADRS"]], c("ARM", "ACTARM")),
        selected = "ARM",
        multiple = FALSE,
        fixed = FALSE
      )
    ),
    data_extract_spec(
      dataname = "ADLB",
      select = select_spec(
        label = "Select variables:",
        choices = variable_choices(
          data[["ADLB"]],
          subset = names(Filter(isTRUE, sapply(data[["ADLB"]], is.factor)))
        ),
        selected = "RACE",
        multiple = FALSE,
        fixed = FALSE
      )
    )
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    # tm_outliers ----
    modules(
      label = "Outliers module",
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
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQACAMQLBouPwWkiCgozhRyWgBZwFaVAB0IdJiw79U1Vo0Uq1GrTr79aAM1YFfkMoBXVSUWtg0lCAHgBaLQNqAH1oqHY5KJCFBIUAd1pSY1oIdnTcBRBrBQUAQQARAGUAGVzErh50xERGRtaa+oasJvak7gxu3saRwcaWgCExzsmQnr6GxesAXyyIACsiUpSAazhWUXKQxTz+OD8oYVIUgl9RAhTD47OL4Gh4S7pOQAXWsKmIED8tAkgmYpFoJAUfiIjBMZlEpQkIgKtDuQRC4WsPgAjMsYCkiIJSHQZJdBtQoPQ4NQFABeBSWMBNTHYwp49KEvCDSnU2gyFIANxYbPxMRScAAHqRmARnqJUHACOxBrV0v8zOzOf0WpzcDqwszNaQZeoRKqUurNdqILVXQoGUyWYauZbVQopYxaIyRIhTebagRjEcCGIZQGg-QRC8o7QY4CQsBgEbmiawMDgZUtUaAOIAUVNHLACwAslgANJYYmcuRyM0ut0Wu3kTTeuplsPtt1QhWedkAMTqLSa5cHCj2tVbgwIhjgEhRqe4kul7PS8qVKrVGqLs71sANleNA7dtqtNt9h6d4fdjOZMs5TXv-pYCZEolDQtnCMUzTONv2DOBk2jMRnQ7V10kzbMBjzAsn1qURBHocIZX1S4x1oahyEYdhaFEAAVLAAFVS0qUQLGcK4YgQsBL2QyoSIwB5VRRFt5w7RdAM7K1R0rLA6gAYXLADYJgJ5aCcc8Jynain2HYTFOnc1eL2PYwV0EgoRhOEERdZFUWKMw+TMKAIE0agSAkWUoHCSIIB8AAmMkKSpGlGDpdsPVfb0AHVcSsmz3XsxznKvBQRR8rdUXZOhRFIGC4JCPdlSge1HWPWDT3gN9mJzGLr0-dkbxyo80tggKvUrD8uy-QNwP-NtYKAqDRFAlrEwgyMuoYqAmJY-NC21Zj+zwSsa3rRtm34jrBNVYSS0k9qOtUnsFHUmdYN4hcNt1DLFSyqrHwEgrz2zLYpI7SrrQq+8HWq1Dn09IrGtveNWtKjsBtTWN2R+vrIMB9NGKzYrbrG3QJrqAA1ScK05MSAAlizchajvu+9VuYpHcxxt0ZIIuTsXHSdp2J10tplXanwOudBkW3QVzXQNl1SAM2XNXdToPF6LvykJ9SKliaYeu8uyFvKOrqz7PxB382regGQOBsDQfV6C3uOyHENzMa9bCDCsPZHD2DwgiZGIsjKOosI6IcDModGgsFHYzjtFcFs3tZ2CHvx0SJL+knZPk+mqeUgTajpymlMZzTrB0iBwX06FYUMYykRRNEFFJ+FI7siAHIFOAIiJAQAGZPLisVfLShXvWrCPsRLsuCQryI7vr8UeaSkjUr5k792yh85bdK7xeGJow+Wx6F9lmqO2bhqla1kN5868Gep-frgOg+C3dnzlYaLYqAE0UbAUsAHkmgaa+wBbGm0Lx7bs2ft+kVoEdtoZgJJmAdHKZUFrlFe+trJnhnrdSW5Ul4QLemvd8G9epbzurBHW3VNboIPoNY+N0Fhnw9hfRGyNpqowxljF+ICypdnxuQomb1C7kwUtHH+8cdrRyTrOLSG1lzkA5hubm253RDxXvzMe51J7pRiGLXsp9MGuilk9GWSDY7vUCuvJqysxCq00dgve4EwZpiGiNJR594ZYGrDfcSpE6g2Oxm9IOn9mJOOUeHMmkcE7UzelwwB+1NI4ykWdCekDHIKIvJsYhniF7SytMvZBL56qoN0ZvfR29dCHxwc1fepjdaaKgRYmGKEimm0whXbCZ5cL4UInbciVEaLO3MSfUpbFxDe24n7TRdCVEfyKiHdaLC27sKUpwv+akeFAOTu2bS1hdKlCKEGOgAAvMw5lzDoGsBYMYSzh7tnSDKCogwfDCCBgXAQ5y-JuhUKQckfdfIKHiC8+I5ozm-giSgsAd9vIN26h8uA88SQ03cqCmusyFwp2sKIEoEBWB1HQOwCwAASQQtBKiovUIwCUMhKhEFQPCEguTkqpVMNCYwi9iQAAY3IABZKh8mKDKGl9KWxgG2MCIAA)
