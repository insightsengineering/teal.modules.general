# Initialization function to create a new TealAppDriver object
#
# By manipulating the server function as below, we can hint {shinytest2} to load
# this package and its "Depends".
# Related to https://github.com/rstudio/shinytest2/issues/381
init_teal_app_driver <- function(...) {
  testthat::with_mocked_bindings(
    {
      TealAppDriver <- getFromNamespace("TealAppDriver", "teal") # nolint: object_name.
      dots <- list(...)
      if (!is.null(dots$load_timeout)) {
        dots$load_timeout <- 150 * 1000
      }
      if (!is.null(dots$timeout)) {
        dots$timeout <- 60 * 1000
      }
      TealAppDriver$new(dots)
    },
    shinyApp = function(ui, server, ...) {
      functionBody(server) <- bquote({
        # Hint to shinytest2 that this package should be available (via {globals})
        .hint_to_load_package <- add_facet_labels
        .(functionBody(server))
      })

      shiny::shinyApp(ui, server, ...)
    },
    # The relevant shinyApp call in `TealAppDriver` is being called without prefix,
    # hence why the package bindings that is changed is in {teal} and not {shiny}
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
  data
}

simple_cdisc_data <- function(datasets = c("ADSL", "ADRS", "ADTTE")) {
  datasets <- match.arg(datasets, several.ok = TRUE)

  data <- Reduce(
    x = datasets,
    function(u, x) eval_code(u, sprintf("%1$s <- teal.data::r%1$s", x)),
    init = within(teal.data::teal_data(), require(nestcolor))
  )

  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[datasets]
  data
}
