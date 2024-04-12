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

app_driver_tm_file_viewer <- function() {
  TealAppDriver$new(
    data = simple_teal_data(),
    modules = tm_file_viewer(
      label = "File Viewer Module",
      input_path = list(
        folder = system.file("sample_files", package = "teal.modules.general"),
        png = system.file("sample_files/sample_file.png", package = "teal.modules.general"),
        txt = system.file("sample_files/sample_file.txt", package = "teal.modules.general"),
        url = "https://fda.gov/files/drugs/published/Portable-Document-Format-Specifications.pdf"
      )
    ),
    timeout = 3000
  )
}
