# Using association plot

## `teal` application to use association plot with various datasets types

This vignette will guide you through the four parts to create a `teal`
application using various types of datasets using the association plot
module
[`tm_g_association()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_g_association.md):

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
[`tm_g_association()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_g_association.md)
using different combinations of data sets.

``` r
# configuration for a single wide dataset
mod1 <- tm_g_association(
  label = "Single wide dataset",
  ref = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]]),
      selected = "AGE",
      fixed = FALSE
    )
  ),
  vars = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]]),
      selected = "BMRKR1",
      multiple = TRUE,
      fixed = FALSE
    )
  )
)

# configuration for two wide datasets
mod2 <- tm_g_association(
  label = "Two wide datasets",
  ref = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "SEX", "STRATA1", "RACE")),
      selected = "STRATA1",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  vars = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "SEX", "RACE", "COUNTRY")),
      selected = c("AGE", "COUNTRY", "RACE"),
      multiple = TRUE,
      fixed = FALSE
    )
  )
)

# configuration for multiple long datasets
mod3 <- tm_g_association(
  label = "Multiple different long datasets",
  ref = data_extract_spec(
    dataname = "ADTTE",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADTTE"]]),
      selected = "AVAL",
      multiple = FALSE,
      fixed = FALSE
    ),
    filter = filter_spec(
      label = "Select endpoint:",
      vars = "PARAMCD",
      choices = value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
      selected = c("PFS", "EFS"),
      multiple = TRUE
    )
  ),
  vars = data_extract_spec(
    dataname = "ADRS",
    reshape = TRUE,
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADRS"]], c("AVALC", "BMRKR1", "BMRKR2", "ARM")),
      selected = "AVALC",
      multiple = TRUE,
      fixed = FALSE
    ),
    filter = list(
      filter_spec(
        label = "Select endpoints:",
        vars = "PARAMCD",
        choices = value_choices(data[["ADRS"]], "PARAMCD", "PARAM"),
        selected = "BESRSPI",
        multiple = TRUE
      ),
      filter_spec(
        label = "Select endpoints:",
        vars = "AVISIT",
        choices = levels(data[["ADRS"]]$AVISIT),
        selected = "SCREENING",
        multiple = TRUE
      )
    )
  )
)

# configuration for wide and long datasets
mod4 <- tm_g_association(
  label = "Wide and long datasets",
  ref = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AVAL", "AVALC")),
      selected = "AVALC",
      multiple = FALSE,
      fixed = FALSE,
      label = "Selected variable:"
    ),
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data[["ADRS"]], "PARAMCD", "PARAM"),
        selected = levels(data[["ADRS"]]$PARAMCD),
        multiple = TRUE,
        label = "Select response"
      ),
      filter_spec(
        vars = "AVISIT",
        choices = levels(data[["ADRS"]]$AVISIT),
        selected = levels(data[["ADRS"]]$AVISIT),
        multiple = TRUE,
        label = "Select visit:"
      )
    )
  ),
  vars = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("SEX", "AGE", "RACE", "COUNTRY", "BMRKR1", "STRATA1", "ARM")),
      selected = "AGE",
      multiple = TRUE,
      fixed = FALSE,
      label = "Select variable:"
    )
  )
)

# configuration for the same long dataset (same subsets)
mod5 <- tm_g_association(
  label = "Same long datasets (same subsets)",
  ref = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]]),
      selected = "AVALC",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  vars = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]]),
      selected = "PARAMCD",
      multiple = TRUE,
      fixed = FALSE,
      label = "Select variable:"
    )
  )
)

# configuration for the same long dataset (different subsets)
mod6 <- tm_g_association(
  label = "Same long datasets (different subsets)",
  ref = data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
        selected = levels(data[["ADLB"]]$PARAMCD)[1],
        multiple = FALSE,
        label = "Select lab:"
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
      choices = variable_choices(data[["ADLB"]], c("AVAL", "CHG2", "PCHG2")),
      selected = "AVAL",
      multiple = FALSE
    )
  ),
  vars = data_extract_spec(
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
      choices = variable_choices(data[["ADLB"]]),
      selected = "STRATA1",
      multiple = TRUE
    )
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    # tm_g_association ----
    modules(
      label = "Association plot",
      mod1,
      mod2,
      mod3,
      mod4,
      mod5,
      mod6
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
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQACAMQLBouPwWkiCgozhRyWgBZwFaVAB0IdJiw79U1Vo0Uq1GrTr79aAM1YFfkMoBXVSUWtg0lCAHgBaLQNqAH1oqHY5KJCFBIUAd1pSY1oIdnTcBRBrBQUAQQARAGUAGVzErh50xERGRtaFAFIAPkGa2oUYQRjydgAVLDmGgFUsBQBeBUYiQQh+dihxCEF4RloCecWAUQa5gFkFRIW5ptu7uUqARjksiFrGrBNdpJbgYbq9AFNcaNOZzK7AzpgkI9Pq3OHQhotABCCOSSJiKMa2KGo3GtSmMzg7AAwgAJADi1I25nEfigBG0jHYBEOcBS+VMZTJEzp9NyCk+CgAfgpLGAAHJy3DC2qihTDCXS2VgAAKSpVCgWy3hMrl8Tlwp+vwAvr8AFZEUopADWcFYonKIUUeX4cDZwlIKQIvlEBBSDqdrvdwGg8A96TkAF1rCpiBA-LQJIJmKRaCQFH4iIxzGFShIRAVaL6giFwtYfJK8qQYCkJClDqIiARaIY80K-gpqFB6HBqMy5U0yxXCtX0nW8ON9H5mekUnAAB6kZgclKiVBwC7C9KxsybOX9Fr6ge1dQiDnM28HwN7g-sA1Dkdjs9gJqjp8KAA3Fge3oERECvCYJgIYxHQIMRmSAs5hxEIMYPOMRPRiYBgHPZpLzARNEw+A1Hw5TxvzqekrggyCC1oddyIUAAxOoWiaajrwUX5amIgdENEFcQjXTdt2ffdD0449YFPbULxom8-3vTZSLE1932HUdxx-RTSEA4DkLEcCF042poNg+DNkQkCULM9D4xCbDcNaOVCN42iwh0xi5SxO4sAAaSwT55ImKZqFzJwZKNK5lRMuiGM0TYWLYjjIO4rjrF+FNdBIDMsxzPsCyLLR8h0GczDnOAInrAQACYERbNsOy7HtcxIN8Bw-TTvzmErK1nWtKsiYzaiXQSYmErd2VUiTIKk+AtLk4aJhUh8dN3cT2vczqv21X8710qyDKMmL3NsuCBMs-TQL5M6MPSRywEWwjKkPR6qKVXargADQ+icFjqOY6iCvBtSwOpqWosAfhO2iVK8n9-sB4GYcg0LworRLWPYlGJgzeLmSS9jLXGNy9MYC6a3GjdJp3F8ZomOaZKc-CcZW5S1rpzbaO2rS9v-Q7rtEY6DVuimBZstDzswqAHqexMXrfN7IcqCdvt+sAwYh9XqQAeWWeUFgATTlaGSM8hLdEVyjle1XX9aN9XNch0nUYDWgIuZKKcdqPHGMJlKJjS35MogVMcszbNe3zQtizR92K2oEgJEp3kqogHwAGZ6tbdtRE7bso-7Woee-O43Y93w-D8GQKF0xOIGTiqqqW0bNlXanRPWtTJJCE8FrRZXhTZjz9q7+nIJL3adLJ6zDOC0zJYsmeDNQ8z7KwnDHoHlyiO9ken3huoADVWPnyZy4x5iseig1fYt-3LRxjMwpkZln-IRgx65ieNJ2idp4oI4R0ZBhaxX4lpHUdQwZ3GpA0M+osELcEEDdRe68ZabxhOiAi8ttSQOgbA9WeC6h3BNnvOGFtXo6iYlCEGcorjUNIQaOOHtNhRWJgOUm4C25CQ7lNL+R5e7SX7oCYK+hRDGDQJFLAxpWbT3ZqPTm6lPy82nuLOAoDTqoMQUha6q87LS1lg0ER2CFbnhPi0ak6sfL+UClY3yAUarqygSQqGLtlrmwWuYyxS1XZhXjlImRt96J+2vo-YU79X6bDoKIUg39ca0Bfp-RRsVi6-xUftBQgDUDAIiBo9yZMKZyiITAuBPjaIIMutQZBeipb3QwUYmhz1cFQOIQQ2huoWkuLcZBchWksRXCaICHUABJM+5IL4BIDqlPeESkkbQNKk5R34+b3iyTkoWYyCmeOGU0YZcxNkVMHHAACo40GGOMYRAAJMfHZezunuP2vDJo1IsBXCuPKYZ8p6SbOYZfNhsU0o8RJhlawWU0y5Ujq1P4Mc+pmCgHsQcScU7hEiOnAQAAWbOjU87NULlzSecoADqVY4UIvro3AazcYatxThNTuyTZqCPmhRBpwVh4qX4bFQ5aial3QcvUi5ODXrH1Pu0kVFiTb3N6RRLxZ9fkyX9jM4J99r57wJdpR5mg1FGTCZxWZzJomxKCYkzl+StnfmKW0veC816IKqSgteBiBWNJwUUzpVrmnQMYSk-eZELYiBOdQM5zqd6XMtQ0e54y-EsMNNIm+Pr1UrN0mI7JEB1AWgBUqk1DL8lcNkkfW5+yymQUOQG05TrcKCuuQW3ZcxI2+vIP6455a6mVpddWwt9b5WezjdawcaTlmqNoKIIoOqAXsJ4jDPN7cRJ8JzbURm-dnJlPZRzeZXKtGXR0RLR1rat7Lqaa9diP0xXvXaU7bWesDZYGNu06xAVka7URkDJxWAulSo8RRM9e9u2sN7UE-GmNkpqoHVPDJ2qM2pWBRAEOYd0wR3ytHIqxQzCiCEeS5FlUFDsDQ-NUQgh6Aot+D4AArFi3O+cWp9nxaBic6GkVNwEjhoR+HCODSyC3P0Y0oB0rneuxlMQ+4suMbIjJ8inymvKZu5eujRYVq3oKj9mrPGsW8T+iZBNVUAZCcBpRXUwP8yumBSDgcSZTpYBTGdNNppxMXcJmhon-zidpvxqTtqt2z15cGttO8lMHwtm6-BpT1PRr+f+2Kd9NO6diom1RRn1EmaBRwkFEAwXhzyoXQqxYUNhHow3TDulyj+GrvoMgYQCNEeqvwAAbORpqBcoU0aWbtPLFKYgouw5XErtdytsYiBx6lXHuFU1nS57uAn4VCJZdiYK+qonDqNRFhJH9JPuTzYF1pwWFm6Gk0Be1Xn5NEixDvFWHSguEM6d6s10qjmBu81vGb2Cw3uojcAT48ttu-qvtFs1sWMkfjHe5e5szVu0XW49GteyDnSbLUGw7mJjtPZubW+tN3Yf3aO6G5Hdy3sfZ9V9xV22-v82HaOxLpnOJuNXQo1zJbdvxYO3uzHJjLZmNFad0Ujj2k6k55Kshn783s6YRpoDRNKdmfGNOnho2bMCME1N2SCPZvLciYOBbcSfYq7meN3NFmIEveh+5vS+25NM6VyzjbJSLteqhn2tHza4dm8e1c8Nchcd9oJ1phNtGNX-g-BssABpgda9B5BcH2Oi19tLQ7jH5urkR9RwL9H8PneJg7Sj93n2RffexkTn3SbAKk9IID2igKuKOaUg20PO2jc8tN-y3Cqe-N+t5s+5GwvQuTInelGDoLQ4KFKEUHsdAABeZgcsWGsBYYEg-FsLpyMNqAMMfDCCXivkQHphQqGbDnerVH8zxEP+aTi6+MJ6b-o9HFDWCpOCIKQOVAhPg-tqs--gGdX-otfyR1-1Xu-B2sOIqUKwHUOgAcOgJcoILQJUBYJcuoIwCcowJUEQKgFChTIauwKYJmMYLpJsJ8AAAw1Sf59TFDMj4GEE-BgDWiJhAA)
