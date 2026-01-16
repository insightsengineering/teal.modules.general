# Using bivariate plot

## `teal` application to use bivariate plot with various datasets types

This vignette will guide you through the four parts to create a `teal`
application using various types of datasets using the bivariate plot
module
[`tm_g_bivariate()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_g_bivariate.md):

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
[`tm_g_bivariate()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_g_bivariate.md)
using different combinations of data sets.

``` r

# configuration for the single wide dataset
mod1 <- tm_g_bivariate(
  label = "Single wide dataset",
  x = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]]),
      selected = "BMRKR1",
      fixed = FALSE
    )
  ),
  y = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]]),
      selected = "SEX",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  row_facet = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]]),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  col_facet = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]]),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the two wide datasets
mod2 <- tm_g_bivariate(
  label = "Two wide datasets",
  x = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("BMRKR1", "AGE", "SEX", "STRATA1", "RACE")),
      selected = c("BMRKR1"),
      multiple = FALSE
    )
  ),
  y = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]], c("COUNTRY", "AGE", "RACE")),
      selected = "RACE",
      multiple = FALSE
    )
  ),
  row_facet = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]]),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  col_facet = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]]),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the multiple different long datasets
mod3 <- tm_g_bivariate(
  label = "Multiple different long datasets",
  x = data_extract_spec(
    dataname = "ADRS",
    filter = filter_spec(
      label = "Select endpoints:",
      vars = c("PARAMCD", "AVISIT"),
      choices = value_choices(data[["ADRS"]], c("PARAMCD", "AVISIT"), c("PARAM", "AVISIT")),
      selected = "OVRINV - END OF INDUCTION",
      multiple = TRUE
    ),
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AVALC", "AVAL")),
      selected = "AVALC",
      multiple = FALSE
    )
  ),
  y = data_extract_spec(
    dataname = "ADTTE",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADTTE"]], c("AVAL", "CNSR")),
      selected = "AVAL",
      multiple = FALSE,
      fixed = FALSE
    ),
    filter = filter_spec(
      label = "Select endpoint:",
      vars = c("PARAMCD"),
      choices = value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
      selected = "OS",
      multiple = FALSE
    )
  ),
  row_facet = data_extract_spec(
    dataname = "ADRS",
    filter = filter_spec(
      label = "Select endpoints:",
      vars = c("PARAMCD", "AVISIT"),
      choices = value_choices(data[["ADRS"]], c("PARAMCD", "AVISIT"), c("PARAM", "AVISIT")),
      selected = "OVRINV - SCREENING",
      multiple = TRUE
    ),
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADRS"]], c("SEX", "RACE", "COUNTRY", "ARM", "PARAMCD", "AVISIT")),
      selected = "SEX",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  col_facet = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]], c("SEX", "RACE")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  color_settings = TRUE,
  color = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("SEX", "RACE", "COUNTRY")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  fill = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("SEX", "RACE", "COUNTRY")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  size = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  plot_height = c(600, 200, 2000),
  ggtheme = "gray"
)

# configuration for the wide and long datasets
mod4 <- tm_g_bivariate(
  label = "Wide and long datasets",
  x = data_extract_spec(
    dataname = "ADRS",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data[["ADRS"]], "PARAMCD", "PARAM"),
        selected = levels(data[["ADRS"]]$PARAMCD)[1],
        multiple = FALSE,
        label = "Select response:"
      ),
      filter_spec(
        vars = "AVISIT",
        choices = levels(data[["ADRS"]]$AVISIT),
        selected = levels(data[["ADRS"]]$AVISIT)[1],
        multiple = FALSE,
        label = "Select visit:"
      )
    ),
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AVALC", "AVAL")),
      selected = "AVALC",
      multiple = FALSE,
      label = "Select variable:"
    )
  ),
  y = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("BMRKR1", "SEX", "AGE", "RACE", "COUNTRY")),
      selected = "BMRKR1",
      multiple = FALSE,
      label = "Select variable:",
      fixed = FALSE
    )
  ),
  row_facet = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("SEX", "RACE", "ARMCD", "PARAMCD")),
      selected = "SEX",
      multiple = FALSE,
      label = "Select variable:"
    )
  ),
  col_facet = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("SEX", "RACE", "ARMCD", "PARAMCD", "AVISIT")),
      selected = "ARMCD",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  )
)

# configuration for the wide and multiple long datasets
mod5 <- tm_g_bivariate(
  label = "Wide and multiple long datasets",
  x = data_extract_spec(
    dataname = "ADRS",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data[["ADRS"]], "PARAMCD", "PARAM"),
        selected = levels(data[["ADRS"]]$PARAMCD)[1],
        multiple = FALSE,
        label = "Select response:"
      ),
      filter_spec(
        vars = "AVISIT",
        choices = levels(data[["ADRS"]]$AVISIT),
        selected = levels(data[["ADRS"]]$AVISIT)[1],
        multiple = FALSE,
        label = "Select visit:"
      )
    ),
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AVALC", "AVAL")),
      selected = "AVALC",
      multiple = FALSE,
      label = "Select variable:"
    )
  ),
  y = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("BMRKR1", "SEX", "AGE", "RACE", "COUNTRY")),
      selected = "BMRKR1",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  row_facet = data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
        selected = levels(data[["ADLB"]]$PARAMCD)[1],
        multiple = FALSE,
        label = "Select measurement:"
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
      choices = "ARMCD",
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  col_facet = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("SEX", "AGE", "RACE", "COUNTRY")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  color_settings = TRUE,
  color = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("SEX", "RACE", "COUNTRY")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  fill = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("SEX", "RACE", "COUNTRY")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  size = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  plot_height = c(600, 200, 2000),
  ggtheme = "gray"
)

# Configuration for the same long datasets (same subset)
mod6 <- tm_g_bivariate(
  label = "Same long datasets (same subset)",
  x = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AVALC", "AVAL")),
      selected = "AVALC",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  y = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("SEX", "RACE", "COUNTRY", "ARMCD", "BMRKR1", "BMRKR2")),
      selected = "BMRKR1",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  row_facet = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AVISIT", "PARAMCD")),
      selected = "PARAMCD",
      multiple = FALSE,
      label = "Select variables:"
    )
  ),
  col_facet = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AVISIT", "PARAMCD")),
      selected = "AVISIT",
      multiple = FALSE,
      label = "Select variables:"
    )
  )
)

# Configuration for the same datasets (different subsets)
mod7 <- tm_g_bivariate(
  label = "Same datasets (different subsets)",
  x = data_extract_spec(
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
      choices = "AVAL",
      selected = "AVAL",
      multiple = FALSE,
      fixed = TRUE
    )
  ),
  y = data_extract_spec(
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
      choices = "AVAL",
      selected = "AVAL",
      multiple = FALSE,
      fixed = TRUE
    )
  ),
  use_density = FALSE,
  row_facet = data_extract_spec(
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
        label = "Select category:"
      )
    ),
    select = select_spec(
      choices = variable_choices(data[["ADLB"]], c("RACE", "SEX", "ARMCD", "ACTARMCD")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  col_facet = data_extract_spec(
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
        label = "Select category:"
      )
    ),
    select = select_spec(
      choices = variable_choices(data[["ADLB"]], c("RACE", "SEX", "ARMCD", "ACTARMCD")),
      selected = "ARMCD",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variables:"
    )
  ),
  color_settings = TRUE,
  color = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("SEX", "RACE", "COUNTRY")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  fill = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("SEX", "RACE", "COUNTRY")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  size = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  plot_height = c(600, 200, 2000),
  ggtheme = "gray"
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    # tm_g_bivariate ------
    modules(
      label = "Bivariate plot",
      mod1,
      mod2,
      mod3,
      mod4,
      mod5,
      mod6,
      mod7
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
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQACAMQLBouPwWkiCgozhRyWgBZwFaVAB0IdJiw79U1Vo0Uq1GrTr79aAM1YFfkMoBXVSUWtg0lCAHgBaLQNqAH1oqHY5KJCFBIUAd1pSY1oIdnTcBRBrBQUAQQARAGUAGVzErh50xERGRtaFAFIAPkGa2oUYQRjydgAVLDmGgFUsBQBeBUYiQQh+dihxCEF4RloCecWAUQa5gFkFRIW5ptu7uUqARjksiFrGrBNdpJbgYbq9AFNcaNOZzK7AzpgkI9Pq3OHQhotABCCOSSJiKMa2KGo3GtSmMzg7AAwgAJADi1I25nEfigBG0jHYBEOcBS+VMZTJEzp9NyCk+CgAfgpLGAAHJy3DC2qihTDCXS2VgAAKSpVCgWy3hMrl8Tlwp+vwAvr8AFZEUopADWcFYonKIUUeX4cDZwlIKQIvlEBBSDqdrvdwGg8A96TkAF1rCpiBA-LQJIJmKRaCQFH4iIwTGZRKUJCICrRfUEQuFrD5JXlSDAUhIUvRaAA3Fi0QxU8bUKD0ODUZlyprlyuFGvpet4cYAD2Z6RScEXpGYHJSolQcAuwvSsbMmzl-Ra+r+E3UIg5zJv+8Du-37ANQ5HY9PYCao8fCh7ZzDiIiCXhMEwEMYjoEGIzIAX29AiEGkHnGInoxMAwBns0F5gImiYfAaD4cp4X5YncWAANJYJ8oFgQWtCLiRCgAGJ1C0TRXJa4wEVegSbKu66buyT57geV61EesAntq560bURGkPev7bs+Yl0Qo76juO37KYpcFAXAIELuJ4HIdBoiwb2BlIVBqHpBhWGtHKeE8epClMROVwABpyRMUzULmTjSax7FXMqJm1BmjGaJsIUcVxV6uVsRD5CkbLQYp-EhGuG5biJL6HiEx7abJxlgQpSm3vlal0Zpn7aj+VX-lZCFiEZ4XqRBtkWZs+mtTZKHxiEDlgKVLkdXR7kxQo8rLC0LQTWB-mBZWsVsRxi0TFFTFxZxJm-LUSXEKk6VwJltYxDlQkqaJr4mZJ8AldhvkVZsCk7rdb7DlpX6NX+fUiKI7UGl1g2WYB-Wg+ZaFQCNY34Zt8m6Uxs3zYjkwBrQQXMrt6PbdNu0JYd1i-CmugkBmWY5nmfyFsWxRmKQ+Q6DOZhzmdkQQD4ABMCKtu2nb6bMg7ffVcpzMzVaznWHO0cuWWXYJeUfQV91FVJT1OWV166ZVj4qzVYF1dpf13gDhm+bUUMwb1LWIdbQ3oZho3PbhiaVAeYBkZR1FKjJ9KcXgDXeX7E4LHUcx1DRQdylgdTUoHPzo1NzKe97VHR0lS2Y9ja2hUTChJXxF1QFdyuqXdYEPdJjk4Ztr1hLpBuV7Vosm7r5tA5buhmTbzUQ-bveO7Dzvwx7r5gNSADyyzygsACaoejQHS9xwncpJ4RyPTbH8eB+jy1Y6tLHrXtYEHYXE3bKlp3nQJuXCc3hUxMVX6lfXutvU3FdfR+7dNebYGEUe7dXBvBQe3UYZw1duNLeVUUZzQWgaQ+ucT6hTxgxHap8C5HSICddkZ0VzZSVo-H+asX4azfq7D+TUv5VSfsA42v0O52zat3B2YDrIOygaPGBCM4GPgQWjZBOdj64wNPjHG2D9rcRJtYMmaZKbZkMDTAsRYSwYwCkfNm-g-AyAoIpagJAJAl3CJzHwABmPmbYOzdissLK8TDtR3FETovwej9BkA0sY0xsttbyxLmXUhn1yFQFfjJBogJfIZgCjIZkMTyCMAYepJxE5dYUEcI6MgXdtZgQAj1XQE8dR1DjncakDQl51AAGoAEkmg1LmBvdGHDbbUEEHybh9leFRLduPOUxTSnlMqbU+pjSwAfEKf0kpdQ7jDLqQ0jeWcdbwJ3mAKeVSsA1PlFUx4CgrjygaAoKezEFBbJWNSOYNSp6KlyX5VxzIjRnwmEshu70yGdSHpwyGQ8eFYR6XhPpo0qlsWpMMtiizk7bxKsClooLbnknuXneKMjEoTWLvfa61UW4SXVo9KhsJ97Cled-EJKS27MIAawoBHzQG2wHh0n5XSsIEucu7SZQLwUx0nvKJoWAIUCOIqs6pnKD6IrQRtCRmCCbSPPptBJcTNjyqSe81uf8KV-gyagLJpBqV0XyanIp0yykVPGc0z5rT2kDWhkyl2LLenagGTMoZXLHWzNNQK8gqyp5QnhZolawUZXPO4lfFKaUCF32IQ-G6qsq64pri7HpcraCxOLIq5NiTkmqp+g1dJewtWlAiLqvJLACme1dc6yoZ4RkLPdcAlpzU2kMsgTayErLAXlpNZWoF8yxkTLLUauZoz+XAJTl+dZmztm7KaNSLAVx9lbPpN3FBx9HmWhoX+Oh+sVVG3JTmyl9Ki2mVpf3cBTbBq-ITT6gF7KOI+S5Wvfe2pp6zwXpUrAbqu0dsHTWzeI6oW-RDr65dAb0GSuilI-OKLDoTWOmGjKRDFZRqxc-MJlCIla3XXeTd0bDYTFSTpfdp6clmuPebK1dlhq8K1tez2t7V572HW5f9M1EGiq0ag8RwDJFIqecTVF4xjpFh3GdXMEAJAFMeTBvB6iFalxITh7FJdwm1xep-Ru9Dt1HrBnS095Hh7QOo2y2jgGu0PqXs+ueWBF7jKWUjFZzJUZIOAcBiDEquNStc2FX+2a0mEYMkZHBE0Yn1QxeXUlExq6azrkStTbzwtgXrWRzplGVP2uM3e0zDGuUWdfTZyF9nNiObY-6zzGDwM8fRvh02ekqUWig5fcYZYABe0lQvBJjRFuNUXVO0PU1u+LWnzJfIgeeltfDAV1BXly9Ovs8seqEU59SLmKtgawaBxhu7fP-Vq2AQL4wnBEEDKYTMxhzoXAAGwAAZLuVG5td2713LtJQkBIBmeLtQSGYKwOrpMICpgppmZRuZ8x0w0azcwexvFid8REBsAgAAs1iBZ2MAg42o+GADq1YzBhM0EY6H7NYf+IQ3JpDmacUUPe38n1SaU3MjoKIUginIrppkOTvVJbtJft9Vbc1DbLXJadtTttDqjUVtF6UppBo7OCOmiILso59PdKvYmAAJB2uQwBPju2l367RpXddVd1voXcJB1ABeAbZ+iKb2fFsYAUqtPbu68+PfLxXF7W1u1V9UntVuZeCvp3ABX1AlfC69z70ZmvtfowRexsRp8Y8aU2wR-6tAyw6rq+pC+0GYu9bix1hLfOkuMpS5ekXnthWwrBThX9TGCsyRhXC4r+uVsbbVXu7bB7M9Bv47xEnQSFMoeUy7DDueN19cH3WovrC9Me-G+ymb0cu10a5ZNx9u917ZZnpZ6ztfJrMblIvpdYrONkvb1ts2O2ytreRefYN4xr5wcIbJgfyHQnD895h86+fcMu+0yerhEvIXMvNLCeFfTLTfLtEpY1JebnPfcqA-b8EzEROPEDNzM-HzFPS-LvXberXBfBeDF-eTN-WNSneNT-MfLDCfEguiRLGfQXEeMPGjMAkzbUMzVfd9cXKZQZTtBvJ3ObP9evM8TgztFAkrVvdSbjcVLzNvTA6rAA1qC3O-RKORCABRAHKmFREHdRBmKWHHSHFzfHExQncxAQAAViR1sSFgHEcWTyxxrFxz12xiMJh0iGJyILJ000iyoUTWFCVXpzTyZ0lRt00ztwd11DF1EOAT-yGwtTPWtVL092vW4KdV4JSLdT92oKYjdxDzn3+TVw1y1x12iKcPj3W3UnR2T3kJNy1QgHN271lWCIzVCImH1TfmrTGUT3rRyNDxALwm9w6MyNHQ0iD3dzG3yIGN9yKMT2W2kMTyNwATTyKCULomzwaxMmJQ0wGxiL7mL2bUSMiRVwm0b2r0Y33yEI5SryAxPwT28zFiwJqxwL2z7w8MxXZ28PQ2iw2NixJQL0G12PoKAMYJHxwmYMP3IgziXnAP9nXzAHYK7RyyszOIQIuKP2uNQIN3c3K2kOeNqEf1vn72IPeK6yoWxGiVZ1TQ0kCOZ2t2aO2NaM5y-G5y6On0bVnzGzJPtXSK4IiMl1rQqKyLl1GNyI5KxFZXV0iKj2KIFNmNPwqIWL-HgEOGzDgHgDIBWMaPcxCPpNqDaL4KHR5xAX-x6Ln05P6IjwaSGOYxNNFPFItLmClJmJuPKPlKqI7iWIz1wKzzXUoO-1+N-yNNiJkhEO7mGKKzEJbzmNW2lRdJ3XPweIUOAgaMLnvyvFgwJNeLCz+I+NSy-z1knxpX-z2NGwOMM0BWhLPCmwgNhMRN3ytzDNYwjI41uKxJvxkIwPuPkMAWTIvnwKE3CFE3EweSwGNCkyMUpLawLM6zIO61uU2P6z+J2IKWLISOALHhvVYI3xrO31y3gOWVlwc0bOc2dPQLoikLlLjLkJYSeLwKC2TRC0jTeK8JJM+J63Hx-xpLoPpXZNLNBKMxYIyzYKywRJ3KRIELrwPMKyPKWxPPbLPI8wkKzU7OvNPQ1JTN73kloBa0JM8IGxzJBLfKoI-JBmn2-IYIMz-ImyrO1DRL3P909UPOEWPIxMQrAnPJbI7P-k71Qp7NTNqAOyOzgBOzO3YCuxuwUDu3Eskqewmheze3jU+ygG+1wN+xUGpA0KB1UVB10NEA1hcJMIUHYF0selEEEHoHCF+B8HO0sMFnsRsMqPjKaD0p8QMqMo1lMvMrOiyHcMCSJOfJnJ8Jp19PzJoML1I0BP2LXMOPLwnkrybwb05TosFOhRBWPxYqjNbJjNPMvOQr80UN4owoUHRUfKzNw3wooO+Lz39M-NIt03IuVxis8kAq3PM1AusygJDOmwhNmy7Rm25mRP3ID1Im6qXybLKOyq2gQoys4vVWwJ4q9J7xzyvHxPDRwqfLwpfLDzzOw1Cv+OXIipLKivyOOP4M-UiIGvoo8l5NSLSvEOmqQq4rmoMhyVxPJgIOf18twuzM2pAO2uoNt0DIBLIqBOgWOvZXtNgPOvAvOMgv1JrWb2bNjLwzdLysBjQt7NULJnUvTEB2pm0PplMDCA1lct8HcX0S8Q8rMUsoEAAHYbKUc+w0ck9HLiaZYIhDLSaPEDEwgzKqa5Y1rSrFNyrMQxTbl-DNgGcgitS6TFyGT7cucoaWTwq2T6qsIzS2VuS0jrqMjE9hibSDj1aJSeDHTddZSOLXTHLdZ3w0KlrJCKSAbdTGS4bOjddujhTeiiRRbzTBjdbrT3bTSva1d7STaSizakacrHq9IPSbb0LNSUT3zqqSLj0q0RV5shUYVbrIyLzJrsTV1bzxhirEN1rvqAqIlOTadEkAjGcaSlUHb+5wjmTXbWSBcQbeF1azqeDIa+SrT699aorDbCjo9TbYL5iUa-xrbkzbb4LtTZbHb5b2jTqm7Xd-bbTw8fbdc9aV6DbA7JjI9pjh70rs6I7Zqo708Y61iXkfitjZb60U6viIKhqEr766Iw6JqWdc6RzeNY71jagPA0gKB09i45SVrCDPri6yqfrPbyS6cJbqSmi2cWi56G7Fal6izuAW7IrgSoGuTtaeTXUpcSjN7g8PaRbxTB7pSKjX64KZqO87wJ6Fq47p6ZaAykHoVF6Si3biGA67T17CG-auHV7vapih7Q6R7Dcx67weRyAJAixWBz6fTKqE7r6Ayvy6rW61bA7AV4Tg5mrRpOqoCLloDnUkqGymKYLD7za2Kpqj7marzUaLYGHv78Cn8I0i7Bah80MNHoHK7YHq74HlUdT66FbO7DS6CVb1GXZ26JcbqXUB1+SKiiGxjt6yHJT97RGLHw7kbLamp6GDQrda7EGgmF6DSlbjSt7+6d7g7fbe7ymsHSG17hGKHzG7qbGHK7G-wpG4AZG2B5H9o-riKp9wrgbMHoEonPZtGmq30YDV9DHOqTHECjGojmms7LGc62zKsJHHiiN0a+K3r+yRNywJNP6xyZMwH3H39PGCK5yr6FyVHarACRmqNKKNzdGJnJ42qLrkqoKzGX6xHMrMSaGL8tn-MCqp7gsBb2sIHS7czgqdqAbVGHnDq6myyXn6NICn0Pnob47H7wzmKWnVn371m7jI7EyHHXrmtWsSrIWhbIHqFYX-qWiEXvlHnUswTl5YTaL6zmNcXlnEa376JsSbGFSnr8rHHeyJoBKUhjsJBTsDUxKHspLHtntXtTAqcwBFLlLMa-sFBSgig+w6BsLdCLBrALBgRdWpaKdQhZMJofBhA+5bXAZFMVAWwbFbLUczB4hPWvXhQHXUJiXSIGb+wFABKl0BBPgD4BBuYI3+ALFo34do2zDo3zto2aacFVDRASgIBWA6h0ADh0BVdBBaBKgLBVd1BGAFdGBKgiBUBgc6iq6mdpXZXNhPhLtuZ42pZihmQW222fgwBrREwgA)
