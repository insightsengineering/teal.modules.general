# Using response plot

## `teal` application to use response plot with various datasets types

This vignette will guide you through the four parts to create a `teal`
application using various types of datasets using the response plot
module
[`tm_g_response()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_g_response.md):

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
[`tm_g_response()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_g_response.md)
using different combinations of data sets.

``` r

# configuration for the single wide dataset
mod1 <- tm_g_response(
  label = "Single wide dataset",
  response = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("BMRKR2", "ITTFL", "BEP01FL")),
      selected = "BMRKR2",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  x = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("SEX", "RACE", "COUNTRY", "ARMCD", "STRATA1")),
      selected = "ARMCD",
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the two wide datasets
mod2 <- tm_g_response(
  label = "Two wide datasets",
  response = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("BMRKR2", "ITTFL", "BEP01FL")),
      selected = "BMRKR2",
      multiple = FALSE
    )
  ),
  x = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = c("SEX", "COUNTRY", "RACE", "STRATA1", "ARMCD"),
      selected = "ARMCD",
      multiple = FALSE
    )
  )
)

# configuration for the multiple long datasets
mod3 <- tm_g_response(
  label = "Multiple long datasets",
  response = data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        label = "Select parameter:",
        vars = "PARAMCD",
        choices = levels(data[["ADLB"]]$PARAMCD),
        selected = levels(data[["ADLB"]]$PARAMCD)[1],
        multiple = FALSE
      ),
      filter_spec(
        label = "Select visit:",
        vars = "AVISIT",
        choices = levels(data[["ADLB"]]$AVISIT),
        selected = levels(data[["ADLB"]]$AVISIT)[1],
        multiple = FALSE
      )
    ),
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADLB"]], c("BMRKR2", "ITTFL", "BEP01FL")),
      selected = "BMRKR2",
      multiple = FALSE
    )
  ),
  x = data_extract_spec(
    dataname = "ADRS",
    filter = list(
      filter_spec(
        label = "Select parameter:",
        vars = "PARAMCD",
        choices = levels(data[["ADRS"]]$PARAMCD),
        selected = levels(data[["ADRS"]]$PARAMCD)[3],
        multiple = FALSE
      ),
      filter_spec(
        label = "Select visit:",
        vars = "AVISIT",
        choices = levels(data[["ADRS"]]$AVISIT),
        selected = levels(data[["ADRS"]]$AVISIT)[3],
        multiple = FALSE
      )
    ),
    select = select_spec(
      choices = c("AVALC", "ITTFL", "BEP01FL"),
      selected = "AVALC",
      multiple = FALSE,
      fixed = TRUE
    )
  ),
  row_facet = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = "SEX",
      selected = NULL,
      multiple = FALSE
    )
  ),
  col_facet = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("SEX", "COUNTRY")),
      selected = NULL,
      multiple = FALSE
    )
  )
)

# configuration for the wide and long dataset
mod4 <- tm_g_response(
  label = "Wide and long dataset",
  response = data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = levels(data[["ADLB"]]$PARAMCD),
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
      choices = variable_choices(data[["ADLB"]], c("BMRKR2", "ITTFL", "BEP01FL")),
      selected = "BMRKR2",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  x = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data[["ADSL"]], c("ARMCD", "BMRKR1", "BMRKR2", "BEP01FL")),
      selected = "BMRKR2",
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the same long datasets (same subsets)
mod5 <- tm_g_response(
  label = "Same long datasets (same subsets)",
  response = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("BMRKR2", "AVALC", "BEP01FL")),
      selected = "AVALC",
      multiple = FALSE,
      fixed = TRUE,
      label = "Select variable:"
    )
  ),
  x = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AVALC", "AGE", "SEX", "ARMCD", "STRATA1")),
      selected = "ARMCD",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  row_facet = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = "PARAMCD",
      selected = "PARAMCD",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  col_facet = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = "AVISIT",
      selected = "AVISIT",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  )
)

# configuration for the same long datasets (different subsets)
mod6 <- tm_g_response(
  label = "Same long datasets (different subsets)",
  response = data_extract_spec(
    dataname = "ADLB",
    filter = filter_spec(
      vars = "PARAMCD",
      choices = levels(data[["ADLB"]]$PARAMCD),
      selected = levels(data[["ADLB"]]$PARAMCD)[2],
      multiple = FALSE,
      label = "Select lab:"
    ),
    select = select_spec(
      choices = "BMRKR2",
      selected = "BMRKR2",
      multiple = FALSE,
      fixed = TRUE
    )
  ),
  x = data_extract_spec(
    dataname = "ADLB",
    filter = filter_spec(
      vars = "PARAMCD",
      choices = levels(data[["ADLB"]]$PARAMCD),
      selected = levels(data[["ADLB"]]$PARAMCD)[1],
      multiple = FALSE,
      label = "Select lab:"
    ),
    select = select_spec(
      choices = variable_choices(data[["ADLB"]], c("AVISIT", "PARAMCD", "BEP01FL")),
      selected = "AVISIT",
      multiple = FALSE,
      fixed = TRUE
    )
  ),
  row_facet = data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = levels(data[["ADLB"]]$PARAMCD),
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
      choices = variable_choices(data[["ADLB"]], c("SEX", "RACE", "ARMCD")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select Variable"
    )
  ),
  col_facet = data_extract_spec(
    dataname = "ADLB",
    filter = list(
      filter_spec(
        vars = "PARAMCD",
        choices = levels(data[["ADLB"]]$PARAMCD),
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
      choices = variable_choices(data[["ADLB"]], c("SEX", "RACE", "ARMCD")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  )
)

# initialize the app
app <- init(
  data = data,
  modules = modules(
    modules(
      label = "Response plot",
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
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQACAMQLBouPwWkiCgozhRyWgBZwFaVAB0IdJiw79U1Vo0Uq1GrTr79aAM1YFfkMoBXVSUWtg0lCAHgBaLQNqAH1oqHY5KJCFBIUAd1pSY1oIdnTcBRBrBQUAQQARAGUAGVzErh50xERGRtaFAFIAPkGa2oUYQRjydgAVLDmGgFUsBQBeBUYiQQh+dihxCEF4RloCecWAUQa5gFkFRIW5ptu7uUqARjksiFrGrBNdpJbgYbq9AFNcaNOZzK7AzpgkI9Pq3OHQhotABCCOSSJiKMa2KGo3GtSmMzg7AAwgAJADi1I25nEfigBG0jHYBEOcBS+VMZTJEzp9NyCk+CgAfgpLGAAHJy3DC2qihTDCXS2VgAAKSpVCgWy3hMrl8Tlwp+vwAvr8AFZEUopADWcFYonKIUUeX4cDZwlIKQIvlEBBSDqdrvdwGg8A96TkAF1rCpiBA-LQJIJmKRaCQFH4iIwTGZRKUJCICrRfUEQuFrD5JXlSDAUhIUvpRKgSOp2ONqFB6HBqMy5U1y5XCjX0vW8ONO92IOpmekUnAAB6kZgclJduAXYXpWNmTZy-otfV-CbqEQc5k3-eBvcHq8TBQDocj09gJrDx8KAA3FhaEHEREEvN9agIYxHQIMRmSAs5QL5aDYLET0YmAYAz2aC8wETRNKgPMAsTuLAAGksAAJiVbUAElYQAMTwyo5SxK4dQABk+Zi5R+ZVX2vP8OU8b9SIo6iIMgqZqFzJwTwURi6haJorgEyCC1oddRMU5TVMtcYPnGdcVxCNdN23J9UH3PtBKPWAFJw1opNqB8702NyrJsg0P2HUcf2E0hAOA5DwLnQSoJg854M2RCQPoEQgyiuD4xCLCnLwgiiL7H8rgADVouUsDqakrkKsBqQAeWWeUFgATXKuosDuakGnKpoFjqOY6k+PijIisJAp0s9mtalyJhkuTK02JSVLUg0M20zQZr0srBN+Wpfl+FNdBIDMsxzPM-kLYtijMUh8h0KczBnOAIgbAQqIRVt2wXHsqX7Qc-O-OZLqrac6zuyJwtqN6lwU1cNy3dkvJfN97Pgfzz3Gzz70C3drLhyDfK-bVf1vIK4tC8bIrQ0QEJChKUOS9D0nSsBkfwwjdBy8TKJovB6KYljtXYrieLw-iDU84aSLI9mScmANaHk5lZv09bDPUhQTM2SGLJhjHvLskJjyR3CUcCtGCa1rG3xx-z8f-ImqbC5W31Q6Lyc2YjVIKzm5Sqmr6vK4rSvazrut6j2GdGtqwH6jSReW7Umpa8P7Ym6XZZWuaDKvLbrB2tN9uzQwjoLIsSyl2SZcragSAkWsYnCSIIB8ABmZ62w7MRF17T7P38u5k-Lyvq95e6Qa2Nv3tMmJzOhndn1s+HdYc-XsXGjNZJkZk6FEUhZ8glfyEYU3t40i3vytu9UBYBy97tg1akQ53tR1Jq6njyXSad9e4AA4dUsw7CGcxLEcoCIABJH7FXjpHDSg0CY6REF-agP8oD0yJIApmoCn4QOAJ8QiN8S5TQUvLNaGlIFvl3jIA+uDj54yNgBWgZZSDXwGrfFg98zwADU6JNAYq-XQNN75wO-hhJBf8UFAMTMAuoHCuFzBIZBaOH94GIOQQAsREipEMTkFgnBTC8FlwIatA0G0JiyNRh5dGM8fJfVxmOGhlMwKS0dilCmSEqZJTJkI5RS8mbZTYuLSSIcGJzF4iHPm3FglCwGvIsSfiOaJ3JL3fRadFYZ2VqrAek9LIUJ1jEPW35ITL1oKvYsmwN5bwWoUveWSoHvisZbI259mDwCvjwu+-kwHPzGsPDSjiYrvk-oIumIiGiAlUe0iBcShIwJjgIhBHihkjLQWM1qmiG7aOqZNPRcsDEDVkbUMh+8LE6KoTYgmgE6FFEYdU1peT1FzB4T0-h-TZmDJwgskBkjOEaIma5Ia0ynlKPmVCNBHzpErLWVAjZKddJJOIZaROpjoGPiqZBB5zJiKSOUtScqgTgmsRIhxMJgsJlRNjmwzFktIXTWhapCZi0dJGiIcYpW84iD5BSGyOCQU1ZmShpkw5c8ckLzyQbLpiL3JithofCYxyAqnJtvY0Vb8nEn3ypLEl8plgtBaBMyliSFZviMZA4gqQOV3XHlADJmt+UTARo5f+zlRUIs8si82tST62JcQqiZqLYp2Opu4l59rMrM1dqqkOXtapYAahHXZEqdIaq1TqhJWyYVMoztYbaEBUx7UzHnXM+YTrF2uuYPY75+63VIA9fgAAWZur1R7g23jKgA6tWMwUBS0VwgFXCtUkwbLm5RPXlVrMZSttYvVBid9nrzoWUga+yXWQWuQ-DBnTvm8LJgogZaUhleJAUshosaflTK3c8ndOE93iIPZoqi4KNK6uZAy9dMrT5BXgIcbMcB4BkDCoY2lFTyHWo0su9hny7mKomD6vpii5kXtQe825R641-Jg4G0RwLEPAFveuh9mwn2ULddQuV5yGEWh2XC4UTrzGjoNFB+V-qnawf-penxYsJKxK5kEnmbECUCz6sS35-k2b+KTaXKFhD-1LRTTSyxXd3Vyr9b+5Jm1UnmstdPGj2SO1CtjiK+FRszEmyA5Bvhzj4qJQeUxxmWUWYjRfiEvxwc8XCY4zx-m4TY0kt8exilybU4yfnVpHShD06bQzVnLNu10y5sOgWouZ0wgLy7T2wGEQFDsFEAvUQgh6C11+D4AArHW1uXZ3pNsI2OJL5bUvkwy1lnLeW+0NoHek4dGntYCu04jYVCz9OnMM0i4zSren0bcYxtDwygU2eIi5xqZKWhYpCbxjzAmT03PJRB3D1L5qBak3hrAxoJkvo9eZuASmDXMqvGk9WU9JWHnnt13TvXKMGYlYu4b99RuWYm28kNOUMULcavSMqIc3aNTDgHYqQd+PC0E3kiHm2-Pbck8F1aR2KuyutopsjF2UksrZaarlrWNbtbNgPXJT2gV9f-AN0nUqPttNXQnWHa2V3gLXQaLbEnyl7eR7J76RGseerOzjtNKnxjGvZeyM1g6LVtbu1pinryqcvf629obG7343LA2quHpKdeI7E1S7nu3UdzXR3JwXd56PnbFwoTOEBs45oOvnOLp1TCJcRslgetd0u+D8H4GQFAgrZdy0DfLAgABsxX+0fSvC+qr3afdAz9-4QP+gyBhAa+HprpXwZqfl4u8dwqvFToA8UzSRT3vBUYKw3UTOHGmZKf8qzKjFlM883rmZAK4OjI71hu9Scjd6p20fDHr6an0Ft+LwSVGjOae6U33mMTdes+8xLQ3+DpOj53kFmODLQv29U7L9TCvOtK+Y5O4U07NgLo1yB+v7PmcDSg931vl70FP876zt-E2P-Xq0VEy338x31dUtxOX-A-GnyP1Vxp3VwXxRSXy+z4Xf3gz+1A2kXKgPXKlCT4xjVW0fFFhBW4U302RAJR33wO0ZTCzxyvG2AJ2lyJxuz5QQNqGL101L2v3LxnU3np0r0qQ12YVr0Zyf3uSX1-3PUvz7y-3XRJQkN-l73by-0ANwS5zRwI3AMxzvCgNFwNX-Sr0EJrzr2IPA3XVfxbz-zQLUTAyQzkIsMkPQwQxsJUJ0TUPNw0IFwgOtxI2gJoL0NgPFWdQ1zoz9TGxSlQLEVYzBxDj9hBzxTjk6QiSjj1wTW1U5yRxNw0jpRjkyOxnHyNjYT9V0Pt0uygiIBNUYILxJzPxtQeztXQzLyKR4LnSyPL2ryELr2wIgwZ2b1QwcLb33Q71kK73sIUKkKUI6UPRcPWQyPUKOXyNOR0LAD-XKQMNYKXRYSRluTEM3V6O3TGMcPERMNsJGL6IOIGKOMw2wRw1mPcPmM0In1oXoV8JKPWmp0COow60QN2Jr1OzCNpn6JY1s1yndjxViPB3sySLkRSM1TSIGjcIC1aN51yLAM8K0MJmx2WOU1eMzRUFKCKBAjoAAC9zoPcLBrALBgR8SWiB5zVlYfBhBekGSRAPRhRmT0J+drEwAsBmszAnAiBK1NsBBPgdVHpRT+AG5xTq1xSCtxTI9D8HdRASgIBWA6h0ADh0BgFBBaBKgLBgF1BGAv5GBKgiBUB80lxmj2BTBMxjAidPhOIqJpT-pihmR7THSfgwBrREwgA)
