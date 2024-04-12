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

app_driver_tm_front_page <- function() {
  data <- simple_cdisc_data()
  data <- within(data, {
    attr(ADSL, "metadata") <- list("Author" = "NEST team", "data_source" = "synthetic data")
  })

  TealAppDriver$new(
    data = data,
    modules = tm_front_page(
      label = "Data Table",
      header_text = c(
        "Important information" = "It can go here.",
        "Other information" = "Can go here."
      ),
      tables = list("MTCARS" = head(mtcars, 5), "IRIS" = head(iris, 5)),
      additional_tags = HTML("Additional HTML or shiny tags go here"),
      footnotes = "This is a footnote",
      show_metadata = TRUE
    ),
    timeout = 3000
  )
}
