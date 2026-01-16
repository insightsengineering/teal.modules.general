# Using cross table

## `teal` application to use cross table with various datasets types

This vignette will guide you through the four parts to create a `teal`
application using various types of datasets using the cross table module
[`tm_t_crosstable()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_t_crosstable.md):

1.  Load libraries
2.  Create data sets
3.  Create an `app` variable
4.  Run the app

### 1 - Load libraries

``` r

library(teal.modules.general) # used to create the app
library(dplyr) # used to modify data sets
library(rtables)
```

### 2 - Create data sets

Inside this app 2 datasets will be used

1.  `ADSL` A wide data set with subject data
2.  `ADLB` A long data set with lab measurements for each subject

``` r

data <- within(data, {
  ADSL <- teal.data::rADSL
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
[`tm_t_crosstable()`](https://insightsengineering.github.io/teal.modules.general/reference/tm_t_crosstable.md)
using different combinations of data sets.

``` r

# configuration for the single wide dataset
mod1 <- tm_t_crosstable(
  label = "Single wide dataset",
  x = data_extract_spec(
    "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]]),
      selected = names(data[["ADSL"]])[5],
      multiple = TRUE,
      fixed = FALSE,
      ordered = TRUE
    )
  ),
  y = data_extract_spec(
    "ADSL",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices(data[["ADSL"]]),
      selected = names(data[["ADSL"]])[6],
      multiple = FALSE,
      fixed = FALSE
    )
  )
)

# configuration for the same long datasets (different subsets)
mod2 <- tm_t_crosstable(
  label = "Same long datasets (different subsets)",
  x = data_extract_spec(
    dataname = "ADLB",
    filter = filter_spec(
      vars = "PARAMCD",
      choices = value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
      selected = levels(data[["ADLB"]]$PARAMCD)[1],
      multiple = FALSE
    ),
    select = select_spec(
      choices = variable_choices(data[["ADLB"]]),
      selected = "AVISIT",
      multiple = TRUE,
      fixed = FALSE,
      ordered = TRUE,
      label = "Select variable:"
    )
  ),
  y = data_extract_spec(
    dataname = "ADLB",
    filter = filter_spec(
      vars = "PARAMCD",
      choices = value_choices(data[["ADLB"]], "PARAMCD", "PARAM"),
      selected = levels(data[["ADLB"]]$PARAMCD)[1],
      multiple = FALSE
    ),
    select = select_spec(
      choices = variable_choices(data[["ADLB"]]),
      selected = "LOQFL",
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
      label = "Cross table",
      mod1,
      mod2
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
Shinylive](https://shinylive.io/r/app/#code=NobwRAdghgtgpmAXGKAHVA6ASmANGAYwHsIAXOMpMAGwEsAjAJykYE8AKcqajGIgEwCu1OAGcMAcwpxm1AJQACAMQLBouPwWkiCgozhRyWgBZwFaVAB0IdJiw79U1Vo0Uq1GrTr79aAM1YFfkMoBXVSUWtbZjZ2RlIoehFROWtghIUAHgBaLQNqAH10qHZUiGKs3IB3WlJjWgh2YtwFEGsFBQBBABEAZQAZSrzuDGLEREYegfau7v6AISGuHjGJnoWFAFIAPk2ZjphBBPJ2AGEACQBxU4UAXnNxPygCbUZ2Aih1AqrTRv2OhQXS5ZBQARgUAD8FJYwAA5GG4f4dIEKbZgyHQsAABQRSIUABUsABVACiGJh2Rh-zkNOsAF8ygArIgNAoAazgrFETRCihyQTgT2EpAKBF8ogIBWZrI5XOA0Hg3OKcgAutYVMQIH5aBJBMxSLQSAo-ERGCYzKIGhIRAoavwzMVwtYfOD+aQYAURXoiKJRAkknB2DNqIk4NQ7pjelabXaHSEnXgZgAPCPFApwJOkZgvAqiVBwAhBiAAzFTfq44sA9QiF4R6sFkV5gtFksdEP0MMRmG9MMNhQANxYtESIkQFdbumMLIIYgjg8Yw4DoqntBnSpCwGAMLLMJVKrkiMrJfrL089wVYh5CU32765bAe7kwAArCrDxPDtQDU4zPdCaT31bbUkzPBQADFOn6XoSUAktTXtfRND-YkSWpGYDxmQJ7jTDMs2eRt80Lf5b2mRMjzCXta3uE8CObPF207e5u0o0gByHEc4DHMiJwIFc1zndil146dL2KG8wB3B991gqsWNAi912vLcJLvXd92AAA2N88U-b8bXuCCoJgvFgNAwzoLQysyjKdVdBIbVdX1Q1ixNM06gtWAzGoEgJCCeM4AiBQmn8PwZAoVjREEehwhSZ0BAAJiWD0vUYH0-Q4lsFAY8MmLAXpPKyny-ISGKgt8PxQv0MgwiimLUm4hQU2wkJ00zbNaKI8jigvLsVIWccAW1L8ZAjIbyEYXNCMykt51EXqsU6LBOgAWVOboBpLYTV1ne5B2oQQ4GXETFKgcT1nmNSWhhBaltW9a8ExG6VphDDyI6GjQJEfswxOs65guqSABInrup9QW0t6FF02gfwjczUPI17jxYusWMmujIa2-jdsEkQju237lPOtSkdbD6kNLAA1ABJXpqfxDaAWh2HkIAkzaBAin4Zkjp4LCin-2MyHst6nsa1Y+dF1HKlEfQwCsOKqBWrwnMm06ktuoK3LiYajoxpG+59YmtXpoBWb5sWla1sZjosZ2tj9sOu3CdvfqpKu7FLbuhFHq9l6eYo8XPrgb7qBdvqAb3YGvbWsGIY-YUYf08DIIsxGZJo1HxfR9XW2dgSFw4-G1yvU6if+kmA-J3r+gAeQARTA+8A+Z5PufZzm4dToWJxF3Kxb7SWOK4yyOms6xbIaWphzoAAvMx3PMdBrAsIYp9ITKKmahJAJ8YR7b35JTcPy96NDHLMVOVLfS0DibahgRQRbhLR4UceIFEeoIFYTp0HYCxAaCFoC0AB6hGDfUYC0IgqADQkDmvcOgfp2CmB1MYVi9xQQAAZ4oABYWh2jqBGLBuCaRgDpCqIAA)
