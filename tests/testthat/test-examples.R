# this test requires a `man` directory in the `tests/testthat` directory
# (presumably symlinked to the package root `man` directory to avoid duplication)
# this also requires `devtools::document()` to be run before running the tests

rd_files <- function() {
  man_path <- if (testthat::is_checking() && identical(Sys.getenv("R_COVR"), "true")) {
    testthat::test_path("man")
  } else if (testthat::is_checking()) {
    testthat::test_path("..", "..", "00_pkg_src", testthat::testing_package(), "man")
  } else {
    testthat::test_path("..", "..", "man")
  }

  testthat::skip_if_not(dir.exists(man_path), "Cannot find path to `man` directory.")

  list.files(
    man_path,
    pattern = "\\.[Rr]d$",
    full.names = TRUE
  )
}

suppress_warnings <- function(expr, pattern = "*", ...) {
  withCallingHandlers(
    expr,
    warning = function(w) {
      if (grepl(pattern, conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

with_mocked_app_bindings <- function(code) {
  shiny__shinyApp <- shiny::shinyApp # nolint object_name.
  # workaround of https://github.com/rstudio/shinytest2/issues/381
  # change to `print(shiny__shinyApp(...))` and remove allow warning once fixed
  mocked_shinyApp <- function(ui, server, ...) { # nolint object_linter.
    functionBody(server) <- bquote({
      .hint_to_load_package <- add_facet_labels # Hint to shinytest2 when looking for packages in globals
      .(functionBody(server))
    })
    mocked_runApp(do.call(shiny__shinyApp, append(x = list(ui = ui, server = server), list(...))))
  }

  mocked_runApp <- function(x, ...) { # nolint object_name_linter.
    args <- list(...)
    args[["launch.browser"]] <- FALSE # needed for RStudio

    app_driver <- tryCatch(
      shinytest2::AppDriver$new(
        x,
        shiny_args = args,
        timeout = 20 * 1000,
        load_timeout = 30 * 1000,
        check_names = FALSE, # explicit check below
        options = options() # https://github.com/rstudio/shinytest2/issues/377
      ),
      error = function(e) {
        e$app$stop() # Ensure the R instance is stopped
        stop(e)
      }
    )

    on.exit(app_driver$stop(), add = TRUE)
    app_driver$wait_for_idle()

    # Simple testing
    ## warning in the app does not invoke a warning in the test
    ## https://github.com/rstudio/shinytest2/issues/378
    app_logs <- subset(app_driver$get_logs(), location == "shiny")[["message"]]

    # Check if the teal app has content (indicator of a Shiny App fatal error)
    if (identical(trimws(app_driver$get_text("#teal-main_ui_container")), "")) {
      tryCatch(
        app_driver$wait_for_idle(duration = 2000), # wait 2 seconds for session to disconnect
        error = function(err) {
          stop(
            sprintf(
              "Teal Application is empty. An Error may have occured:\n%s",
              paste0(subset(app_driver$get_logs(), location == "shiny")[["message"]], collapse = "\n")
            )
          )
        }
      )
    }

    # allow `Warning in file(con, "r")` warning coming from pkgload::load_all()
    if (any(grepl("Warning in.*", app_logs) & !grepl("Warning in file\\(con, \"r\"\\)", app_logs))) {
      warning(
        sprintf(
          "Detected a warning in the application logs:\n%s",
          paste0(app_logs, collapse = "\n")
        )
      )
    }

    ## Throw an error instead of a warning (default `AppDriver$new(..., check_names = TRUE)` throws a warning)
    app_driver$expect_unique_names()

    err_el <- Filter(
      function(x) {
        allowed_errors <- getOption("test_examples.discard_error_regex", "")
        identical(allowed_errors, "") || !grepl(allowed_errors, x)
      },
      app_driver$get_html(".shiny-output-error")
    )

    ## shinytest2 captures app crash but teal continues on error inside the module
    ## we need to use a different way to check if there are errors
    if (!is.null(err_el) && length(err_el) > 0) {
      stop(sprintf("Module error is observed:\n%s", err_el))
    }

    ## validation errors from shinyvalidate - added by default to assure the examples are "clean"
    if (!is.null(err_el <- app_driver$get_html(".shiny-input-container.has-error:not(.shiny-output-error-validation)"))) { # nolint line_length_linter.
      stop(sprintf("shinyvalidate error is observed:\n%s", err_el))
    }
  }

  # support both `shinyApp(...)` as well as prefixed `shiny::shinyApp(...)` calls
  # mock `shinyApp` to `shiny::shinyApp` and `shiny::shinyApp` to custom function
  # same for `runApp(...)` and `shiny::runApp`
  # additionally mock `interactive()`
  testthat::with_mocked_bindings(
    testthat::with_mocked_bindings(
      code,
      shinyApp = shiny::shinyApp,
      runApp = shiny::runApp,
      interactive = function() TRUE
    ),
    shinyApp = mocked_shinyApp,
    runApp = mocked_runApp,
    .package = "shiny"
  )
}

strict_exceptions <- c(
  # https://github.com/r-lib/gtable/pull/94
  "tm_outliers.Rd",
  "tm_g_response.Rd",
  "tm_a_pca.Rd"
)

discard_validation_regex <- list(
  "tm_file_viewer.Rd" = "Please select a file\\.",
  "tm_g_distribution.Rd" = "Please select a test"
)

for (i in rd_files()) {
  testthat::test_that(
    paste0("example-", basename(i)),
    {
      testthat::skip_on_cran()
      testthat::skip("chromium")
      skip_if_too_deep(5)
      testthat::skip_if_not_installed("pkgload")
      if (basename(i) %in% strict_exceptions) {
        op <- options()
        withr::local_options(opts_partial_match_old)
        withr::defer(options(op))
      }
      # Allow for specific validation errors for individual examples
      withr::local_options(
        list(
          "test_examples.discard_error_regex" = discard_validation_regex[[basename(i)]]
        )
      )
      with_mocked_app_bindings(
        # suppress warnings coming from saving qenv https://github.com/insightsengineering/teal.code/issues/194
        suppress_warnings(
          testthat::expect_no_error(
            pkgload::run_example(i, run_donttest = TRUE, run_dontrun = FALSE, quiet = TRUE)
          ),
          "may not be available when loading"
        )
      )
    }
  )
}
