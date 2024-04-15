# Initialization function to create a new TealAppDriver object
#
# It handles the library loading of itself and necessary packages used without
# package prefixes.
# Related to https://github.com/rstudio/shinytest2/issues/381
init_teal_app_driver <- function(...) {
  shiny__shinyApp <- shiny::shinyApp # nolint: object_name.
  testthat::with_mocked_bindings(
    {
      TealAppDriver <- getFromNamespace("TealAppDriver", "teal") # nolint: object_name.
      TealAppDriver$new(...)
    },
    shinyApp = function(ui, server, ...) {
      # Load the package in the environment where the server function is defined
      # The pkgload::load_all() method is used on interactive and has a caveat
      # when one of the functions use `system.file` as it may return an empty
      # string
      load_package_call <- if (testthat::is_testing()) {
        bquote(
          library(.(testthat::testing_package()), character.only = TRUE)
        )
      } else {
        bquote(
          pkgload::load_all(
            .(normalizePath(file.path(testthat::test_path(), "..", ".."))),
            export_all = FALSE,
            attach_testthat = FALSE,
            warn_conflicts = FALSE
          )
        )
      }

      functionBody(server) <- bquote({
        .(load_package_call)
        # Packages in Depends that are used in modules' output
        library(ggmosaic)
        library(ggplot2)
        .(functionBody(server))
      })
      do.call(shiny__shinyApp, append(x = list(ui = ui, server = server), list(...)))
    },
    # shinyApp is being called without prefix, so it needs to be mocked in {teal}
    .package = "teal"
  )
}

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
