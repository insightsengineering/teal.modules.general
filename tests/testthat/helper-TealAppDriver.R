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

app_driver_tm_data_table <- function() {
  app <- TealAppDriver$new(
    data = simple_teal_data(),
    modules = teal::modules(
      tm_data_table(
        label = "Data Table",
        variables_selected = list(
          iris = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width", "Species")
        ),
        datasets_selected = c("iris", "mtcars"),
        dt_args = list(caption = "Table Caption"),
        dt_options = list(
          searching = FALSE, pageLength = 30, lengthMenu = c(5, 15, 30, 100),
          scrollX = TRUE
        ),
        server_rendering = FALSE,
        pre_output = NULL,
        post_output = NULL
      )
    ),
    timeout = 3000
  )
}
