# Import non-exported TealAppDriver from `teal` package
TealAppDriver <- getFromNamespace("TealAppDriver", "teal") # nolint: object_name.

# Data helper functions for reusable datasets ---------------------------------
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

app_driver_tm_a_pca <- function() {
  # Dataset only used once
  data <- within(teal.data::teal_data(), {
    require(nestcolor)

    USArrests <- USArrests # nolint: object_name.
  })
  teal.data::datanames(data) <- "USArrests"


  TealAppDriver$new(
    data = data,
    modules = tm_a_pca(
      dat = teal.transform::data_extract_spec(
        dataname = "USArrests",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices(
            data = data[["USArrests"]],
            c("Murder", "Assault", "UrbanPop", "Rape")
          ),
          selected = c("Murder", "Assault"),
          multiple = TRUE
        )
      )
    )
  )
}
