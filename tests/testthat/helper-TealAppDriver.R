# Import non-exported TealAppDriver from `teal` package
TealAppDriver <- getFromNamespace("TealAppDriver", "teal") # nolint: object_name.

# Helper function
simple_teal_data <- function() {
  data <- within(teal.data::teal_data(), {
    require(nestcolor)
    iris <- iris
    mtcars <- mtcars
  })
  teal.data::datanames(data) <- c("iris", "mtcars")
  data
}

simple_cdisc_data <- function(datasets = c("ADSL", "ADRS", "ADTTE")) {
  datasets <- match.arg(datasets, several.ok = TRUE)
  data <- within(
    teal.data::teal_data(),
    {
      require(nestcolor)
      ADSL <- teal.modules.general::rADSL
      ADRS <- teal.modules.general::rADRS
      ADTTE <- teal.modules.general::rADTTE
    }
  )
  teal.data::datanames(data) <- datasets
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datasets]
  data
}

# local app drivers for module testing ----------------------------------------
# based on examples

app_driver_tm_missing_data <- function() {
  data <- within(simple_teal_data(), {
    require(nestcolor)

    add_nas <- function(x) {
      x[sample(seq_along(x), floor(length(x) * runif(1, .05, .17)))] <- NA
      x
    }

    iris[] <- lapply(iris, add_nas)
    mtcars[] <- lapply(mtcars, add_nas)
    mtcars[["cyl"]] <- as.factor(mtcars[["cyl"]])
    mtcars[["gear"]] <- as.factor(mtcars[["gear"]])
  })

  TealAppDriver$new(
    data = data,
    modules = tm_missing_data(
      label = "Missing data",
      plot_height = c(600, 400, 5000),
      plot_width = NULL,
      parent_dataname = "",
      ggtheme = "gray",
      ggplot2_args = list(
        "Combinations Hist" = teal.widgets::ggplot2_args(
          labs = list(subtitle = "Plot produced by Missing Data Module", caption = NULL)
        ),
        "Combinations Main" = teal.widgets::ggplot2_args(labs = list(title = NULL))
      ),
      pre_output = NULL,
      post_output = NULL
    ),
    timeout = 3000
  )
}
