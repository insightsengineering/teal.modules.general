# Using scatterplot

## `teal` application to use scatter plot with various datasets types

This vignette will guide you through the four parts to create a `teal`
application using various types of datasets using the scatter plot
module
[`tm_g_scatterplot()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_g_scatterplot.md):

1.  Load libraries
2.  Create data sets
3.  Create an `app` variable
4.  Run the app

### 1 - Load libraries

``` r

library(teal.modules.general) # used to create the app
library(dplyr) # used to modify data sets
library(ggpmisc)
library(ggExtra)
library(colourpicker)
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
[`tm_g_scatterplot()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_g_scatterplot.md)
using different combinations of data sets.

``` r

# configuration for the single wide datasets
mod1 <- tm_g_scatterplot(
  label = "Single wide dataset",
  x = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1", "BMRKR2")),
      selected = "AGE",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  y = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1", "BMRKR2")),
      selected = "BMRKR1",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  color_by = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices(data[["ADSL"]], c("RACE", "SEX")),
      selected = NULL,
      multiple = TRUE,
      fixed = FALSE
    )
  )
)

# configuration for the two wide datasets
mod2 <- tm_g_scatterplot(
  label = "Two wide datasets",
  x = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("BMRKR1", "BMRKR2")),
      selected = "BMRKR1",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  y = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "SEX")),
      selected = "AGE",
      multiple = FALSE,
      fixed = FALSE
    )
  ),
  color_by = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("COUNTRY", "AGE", "RACE")),
      selected = "COUNTRY",
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the different long datasets
mod3 <- tm_g_scatterplot(
  label = "Different long datasets",
  x = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADRS"]]),
      selected = "AVAL",
      multiple = FALSE,
      fixed = FALSE
    ),
    filter = filter_spec(
      label = "Select endpoint:",
      vars = c("PARAMCD", "AVISIT"),
      choices = value_choices(data[["ADRS"]], c("PARAMCD", "AVISIT"), c("PARAM", "AVISIT")),
      selected = "OVRINV - SCREENING",
      multiple = FALSE
    )
  ),
  y = data_extract_spec(
    dataname = "ADTTE",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADTTE"]]),
      selected = "AVAL",
      multiple = FALSE,
      fixed = FALSE
    ),
    filter = filter_spec(
      label = "Select parameters:",
      vars = c("PARAMCD"),
      choices = value_choices(data[["ADTTE"]], "PARAMCD", "PARAM"),
      selected = "OS",
      multiple = TRUE
    )
  ),
  color_by = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("AGE", "SEX")),
      selected = "AGE",
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the wide and long datasets
mod4 <- tm_g_scatterplot(
  label = "Wide and long datasets",
  x = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("SEX", "AGE", "BMRKR1", "COUNTRY")),
      selected = "AGE",
      multiple = FALSE,
      fixed = FALSE
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
      label = "Selected variable:",
      choices = "AVAL",
      selected = "AVAL",
      multiple = FALSE,
      fixed = TRUE
    )
  ),
  color_by = data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]], c("SEX", "AGE", "RACE", "COUNTRY")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the same long datasets (same subsets)
mod5 <- tm_g_scatterplot(
  label = "Same long datasets (same subsets)",
  x = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AVAL", "BMRKR1", "BMRKR2")),
      selected = "AVAL",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  y = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AVAL", "BMRKR1", "BMRKR2")),
      selected = "BMRKR1",
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  ),
  color_by = data_extract_spec(
    dataname = "ADRS",
    select = select_spec(
      choices = variable_choices(data[["ADRS"]], c("AGE", "SEX", "RACE")),
      selected = NULL,
      multiple = FALSE,
      fixed = FALSE,
      label = "Select variable:"
    )
  )
)

# configuration for the same long datasets (different subsets)
mod6 <- tm_g_scatterplot(
  label = "Same long datasets (different subsets)",
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
  color_by = data_extract_spec(
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
      choices = variable_choices(data[["ADLB"]], c("RACE", "SEX")),
      selected = "SEX",
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
      label = "Scatterplot",
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
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQACAMQLBouPwWkiCgozhRyWgBZwFaVAB0IdJiw79U1Vo0Uq1GrTr79aAM1YFfkMoBXVSUWtbZjZ2CQlUGFpRAjkohhiOeIBRAA9SZjSbDPt2YmoiQUZUWgIAaxki4NJQgB4AWi0DagB9Zqh2JpCFDoUAd1pSY1oIdn7cBRBrBQUAQQARAGUAGRHOrh5+xERGDZ2FAFIAPgvllYUYQRbydgAVLFf1gFUsBQBeBSMSoQfjsKDiCCCeCMWpvD7ZdavACyCk671em0RSLkCwAjHIiisNlhNnsutwMEcTsTNncNq9XtkyQdKSFjqdEYy6ettgAhZndVktdkbPmXG53FaPZ5wdgAYQAEgBxOX-cziPxQAjaRhlcFwHpjUyzSX3RVKkYKXEKAB+CksYAAcg7cKaVuaFFcrbb7WAAAout0Kd5fJl2h3tB2mglFAC+RQAVkQZj0GqxRHMQopRvw4JrhKQegRfCkekmU2nRMBoPAM-05ABdawqYgQPy0CRVQy0EgKPxERgmMyiGYSETjWi5oIhcKRCA+a2jUgwHoSHopQzkaoVUjsO7UKD0ODUNUOzaj8cTKf9cKBiArXJq-o9OD5Zja9eoOAEPf3+79GszABB0zm2O97jCY9v1INV1BED9RC-H8gwPI8T2AsBNig7UFAANxYWhDxERBwIg3RjGTAgxDVfCYSIg0CAo2oxEzFpgGAECtjAsAGwbBZkLAVYlWyF1fV5JEsAAaSwXFRIdcSpKwAAmB0CVdP8ILg6DPAwoSRLwINHmoUhaCcICFAAMVWbZNmydSyL7Whch0yzrNs6M7hxO5AgBZ9XwKLVC0Q79fwggDYHMzidlIlYtJwgE4qCpDQrI1Dj1PTDsJg2jCPoYiYvuRjKOogEcvoosmKousQnYqLuN4-i90E4S5LABTpNkvAxIk6SVLANSg0Slz5J6mSCqlAtTPHAErJsuyg3bZzNBmtyRI0hRCQ2+zdCICpGB6egfOnFoXzfQLPxC01wvgDLQIKxLYKyi7kPWlY0vQ30sPg7KCPo0QSIM17yOK0QaN+vKGMqlj+lqwSuIdBrdCarBVjlfSFjPbIAA1VK8oGhuWhRHS+bZtm2iCjJMsy1RDeagcWlzZvc9bNqKIpmx2tsOy7Eze37QcpjMUgxh0K8zBvOAImsHwlOZFc1w3UgtycIhd33Q90ow14RYna8Z0lyJAYfJ8QlOgKEOSq6QkA274aN+4HoSp7gpehz3oyr7oLw8H8vtiCiuY0HSp9yGQdYqBYbuni+KRkbFM6jG2tGvqBvxrLhqT+PxoeSbqZWubyfuBnCaZtaINZ7ajr8s6Lcu9brsiuHor9yDvse77npSiD3Ywz2cLKiGAcLlYA6qsG6Ihiqw5hjim-qmOBL01rbJx-q8YcgnbpaluJuMqbzNL4fHKWtVS48v9152vaDqr03-PfJK67C62Itt5vC8d1voM7lCNY+s8srewnr7I+o8SpANyiIKegdw6RztojAScoADyXxHTvAAJqtSXl1B0KM0a4yPpvDCyDUEYOzpTfep9VpH2LlQua58Vhs2sBzVs7ZOzMF5vefmQ4gj+D8DICgMEKgQAkMdfUUt5wCAAMxy1XOuAgm4ZAqzVn+Huvp1h8IEWQBQwjRESylvbR8vk7410fq7f8L8bq6XWCSe6gCnYdxdl3e4aiAFtwHiAoMYCg4QPKt42Bs8aQIwbJfTS6dCYgQAGrWXIbnaarkC4LScozVa0ZC7tmMjINUGStw-yBq4zKbcKCOGTGQIeQZaI+IEn6VYKMkRynWFgyJABJTYzTXiqVAVDHx+FqCCFDjAmenFbHR0ag6GpdSGlNNae0zpsd-S1NWEiaZbSOlr0IeEjKSDIlYGaY6SJqIFCbDlFgbI2RHR7KVLEveecEnM3Lp5SuJsTr33Ok4q2LQbbWIZOjU0n9Ep5Ldn-D2gCPFwHKUDbx49IEDKqgEziPzgmhIdps3S0TuJHwobcw+SST753ufcZFOSskAmJftd5+TgW90AagFgEUtz-WzpUtU1TFn1Maf1LpIMwZ9NhdDGqgTOT6URuMtlUycELLqZ0waqLfRINpDvHONz4m0wYVtO45QBw32eVAM2D9AUWM+a-ax78-n2K-rXcx3cqWfVBSHCFDkoXB2AXy6qbFBXNwQU1bBicV4EJld9DOPrDJxIPtQ3FKT6Es08tYdmEAWwkDYTzHsXCBw8LFuYEEOiSB6P1hInwAAWWRCsFFKyUTuFKBSADqk4zBQCzbosRs5SJGLEXqt5lt66WMblHD+5qAUUqBWhEF7j7XZydb4ye-ihlz2CWMzC2MsHb0Tu1MaEqSFoKwJg9ZAbtIROar8oGWL4k4vpskkuqTo0XyecYl5piDUrAbrbPkBUyVqjoKIFRDkyUPrIsyjCEylniqPiPbpPL+nQLhTO0UvI52+kA+y1qCHpVA1irKkQuFjxuojoKl90cAAkCGGlyGALiPiQZd5UxPeG1DOibVuK9vAcEVQ4DwDKVGIGyKVg-sHQ5IBPiokzLWSB4Ggd31wEw9QbDcC8O8Xw6sFpqyuMosDYTDDWH4Vw1kw2eTin2kkbIyJ49YbEm0YKX3bKyRJgAyDJtRhfa24OO-rx61w7qWqcnZ4yFYG0UxMVUQ30Cm-OYtDXQ2yNDz00ywKGNVl9NX7UOjq9tlrnFPpNRis1jmLVmOcW9ejhSvZgodWRCdYLIP8vdXVODAk-USp9b6PB6NfQbrITutOHmATE1JiF5VJnwsRovVGh5F9Y3MPjZzJNHCU19jTYLMIr9G36NBuwUQr9RCCHoLOIoPgACsxb5GKO3KrSt+XNgLZzU2g2CgVtrY21tltSXXkpY+fW41gWbEKoc17Jzz3vPcudTC8r0nBUjK9VE4L3Us4StXSnZTAXwcZaPaF-FdNv2RZR0fczdqXU2avfZ7yj370ubEV897IyvvxWy7+0D-3POus00E0Z8ygsYshx1VqMP-Xtb3RlVdCcQ29bC6jsitCMe-zc7a0dOOOPDfx3+eL2rb26qezll7pPhmfcy99qnxOadiYB347pDOPvVe9cuz6i6JWNa5xvWVXWyYC6o314XEFRd3Jdy4072OYW49lxtUbEAWGJu5lNvms3TDzZuotvNy3fB+H4fobR63NsG22wIAAbPtxWysK3qwl2ec7IjLsRGu3HhPgiwh3dTw9pXyXVddqNVY97eH0m0EyYOAEH6v0i7b7k3X9x-3wbFRykTpXuAQenQKzi2nE5EZH0PqVnKKPZZcupqTxvtOEeHwZ8jtHjNC5E1jtuqFfdkWUzxztfH+O3T08J5fE61-A+n7BgjCmhPKbQx1nREmNPQZ5C-uTN-JTUjXfK-ffMXMzL3dxKzUgU-P3OXMJLLAdS-ErHzQLdFbOeHQSDAxVcA93CLPFYMaLMuQlR5AnWvFXX9NLZvF-Vvdvd9ZIbvV3XvGQanAfFgATSVIDEfe-NA3pCfI3P-GfBfbgpDNlFDK-ALR-DfAAnTOfHfIzZHfA5fI-L2E-GXM-CLdvNglYQfQTVZbOPXMeTvH-dfIQ2Q3Td-ETKQ0wp-LTCwoA-TEAxQwXCAq-VQ-uGAuA0glmCnGCH7evR1NAhHTA2VEI3ApQ09NHQg1VPHdVeXXaLVRLCgonFAx9btZ9Wg00N9TvRg3LRybQ-vXQjgjKOfQw0TYw72XlIHGQuDUVSZefeopZCQvjGwyTOwmDYJLfBohQ5fPAqIvjDwoRQ8bwhAnvQotIv9EotFITcoh-Ww2o1-W-D-FfNTBY8wroxw14XovfSImjdwqAwrLwjQ+A+IxA7XZAp+VA2nMrSfSrew03XBVGJrTGVeVOW3L-V465J3A-AbX4ylfPArfuMdMAWLAPDmGYSYQiOgAALyFgjwsGsAsDJEhO736B1W2h8GEHASxJEAzFNFxJYnF01k+lLRz1VnIQEFxExQECUhpP4CkXpILXpJ23pPTzBIgCKFEGmAgFYFWHQDBHQHw0EFoAWAsHw3UEYEw0YAWCIFQE4R8S73YFMA7GMH8KtAAAYlJmTdYpg1RcQtSC0CQwBYwGwgA)
