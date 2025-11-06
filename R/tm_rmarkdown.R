#' `teal` module: Rmarkdown render
#'
#' Module to render R Markdown files using the data provided in the `teal_data` object.
#'
#' The R Markdown file should be designed to accept parameters corresponding to the datasets.
#' See using `params` in R Markdown documentation:
#' [bookdown.org/yihui/rmarkdown/params-use.html](https://bookdown.org/yihui/rmarkdown/params-use.html)
#'
#' For example, if the `teal_data` object contains datasets named "mtcars" and "iris",
#' the R Markdown file can define parameters as follows:
#' ```yaml
#' ---
#' title: "R Markdown Report"
#' output: html_document
#' params:
#'   mtcars: NULL
#'   iris: NULL
#' ---
#' ````
#'
#' The libraries used in the R Markdown file must be available in
#' the Shiny app environment.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#'
#' @param rmd_file (`character`) Path to the R Markdown file to be rendered.
#' The file must be accessible from the Shiny app environment.
#' @param allow_download (`logical`) whether to allow downloading of the R Markdown file.
#' Defaults to `TRUE`.
#' @param extra_transform (`list`) of [teal::teal_transform_module()] that will be added in the module's UI.
#' This can be used to create interactive inputs that modify the parameters in R Markdown rendering.
#'
#' @inherit shared_params return
#'
#' @inheritSection teal::example_module Reporting
#'
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#'
#' # general data example
#' data <- teal_data()
#' data <- within(data, {
#'   CO2 <- CO2
#'   CO2[["primary_key"]] <- seq_len(nrow(CO2))
#' })
#' join_keys(data) <- join_keys(join_key("CO2", "CO2", "primary_key"))
#'
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_rmarkdown(
#'       label = "RMarkdown Module",
#'       rmd_file = system.file(file.path("sample_files", "test.Rmd"), package = "teal.modules.general")
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @examples
#' nrow_transform <- teal_transform_module(
#'   label = "N Rows selector",
#'   ui = function(id) {
#'     ns <- NS(id)
#'     tags$div(
#'       numericInput(ns("n_rows"), "Show n rows", value = 5, min = 0, max = 200, step = 5)
#'     )
#'   },
#'   server = function(id, data) {
#'     moduleServer(id, function(input, output, session) {
#'       reactive({
#'         req(data())
#'         within(data(),
#'           {
#'             rmd_data$n_rows <- n_rows_value
#'           },
#'           n_rows_value = input$n_rows
#'         )
#'       })
#'     })
#'   }
#' )
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_rmarkdown(
#'       label = "RMarkdown Module",
#'       rmd_file = system.file(file.path("sample_files", "test.Rmd"), package = "teal.modules.general"),
#'       allow_download = FALSE,
#'       extra_transform = list(nrow_transform)
#'     )
#'   )
#' ) |> shiny::runApp()
#' @export
#'
tm_rmarkdown <- function(label = "RMarkdown Module",
                         rmd_file,
                         datanames = "all",
                         allow_download = TRUE,
                         pre_output = NULL,
                         post_output = NULL,
                         transformators = list(),
                         extra_transform = list()) {
  message("Initializing tm_rmarkdown")

  # Start of assertions

  checkmate::assert_string(label)
  checkmate::assert_file(rmd_file, access = "r")
  checkmate::assert_flag(allow_download)

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)

  # End of assertions

  # Make UI args
  args <- as.list(environment())

  ans <- module(
    label = label,
    server = srv_rmarkdown,
    server_args = list(rmd_file = rmd_file, allow_download = allow_download, extra_transform = extra_transform),
    ui = ui_rmarkdown,
    ui_args = args,
    transformators = transformators,
    datanames = datanames
  )
  # attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the rmarkdown module
ui_rmarkdown <- function(id, rmd_file, allow_download, extra_transform, ...) {
  args <- list(...)
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      tags$div(
        tags$h4(
          "Rendered report from: ",
          tags$code(basename(rmd_file))
        ),
        if (allow_download) {
          downloadButton(
            ns("download_rmd"),
            sprintf("Download '%s'", basename(rmd_file)),
            class = "btn-primary btn-sm"
          )
        },
        ui_transform_teal_data(ns("extra_transform"), transformators = extra_transform)
      ),
      tags$hr(),
      uiOutput(ns("rmd_output"))
    ),
    encoding = NULL,
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

# Server function for the rmarkdown module
srv_rmarkdown <- function(id, data, rmd_file, allow_download, extra_transform) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    if (allow_download) {
      output$download_rmd <- downloadHandler(
        filename = function() basename(rmd_file),
        content = function(file) file.copy(rmd_file, file),
        contentType = "text/plain"
      )
    }

    pre_decorated_q_r <- reactive({
      data_q <- req(data())
      teal.reporter::teal_card(data_q) <- c(
        teal.reporter::teal_card(data_q),
        teal.reporter::teal_card("## Module's output(s)")
      )
      eval_code(
        data_q,
        sprintf(
          "rmd_data <- list(%s)",
          toString(sprintf("%1$s = %1$s", sapply(names(data_q), as.name)))
        )
      )
    })

    q_r <- data_with_output_decorated <- teal::srv_transform_teal_data(
      "extra_transform",
      data = pre_decorated_q_r,
      transformators = extra_transform
    )

    rendered_path_r <- reactive({
      datasets <- req(q_r()) # Ensure data is available
      temp_dir <- tempdir()
      temp_rmd <- tempfile(tmpdir = temp_dir, fileext = ".Rmd")
      temp_html <- tempfile(tmpdir = temp_dir, fileext = ".md")
      file.copy(rmd_file, temp_rmd) # Use a copy of the Rmd file to avoid modifying the original

      tryCatch(
        {
          rmarkdown::render(
            temp_rmd,
            output_format = rmarkdown::md_document(
              variant = "gfm",
              toc = TRUE,
              preserve_yaml = TRUE
            ),
            output_file = temp_html,
            params = datasets[["rmd_data"]],
            envir = new.env(parent = globalenv()),
            quiet = TRUE,
            runtime = "static"
          )
          temp_html
        },
        error = function(e) {
          warning("Error rendering RMD file: ", e$message) # verbose error in logs
          e
        }
      )
    })

    rendered_html_r <- reactive({
      output_path <- req(rendered_path_r())
      validate(
        need(inherits(output_path, "character"), "Error rendering RMD file. Please contact the app developer.")
      )
      htmltools::includeMarkdown(output_path)
    })

    output$rmd_output <- renderUI(rendered_html_r())

    reactive({
      out_data <- q_r()

      if (allow_download) {
        out_data <- eval_code(
          q_r(),
          paste(
            sep = "\n",
            sprintf("## R Markdown contents are generated from file, please download it from the module UI."),
            sprintf("# rmarkdown::render(%s, params = rmd_data)", shQuote(basename(rmd_file), type = "cmd"))
          )
        )
        out_data@verified <- FALSE # manual change verified status as code is being injected
      }

      teal.reporter::teal_card(out_data) <- c(
        teal.reporter::teal_card(out_data),
        rendered_html_r()
      )
      out_data
    })
  })
}
