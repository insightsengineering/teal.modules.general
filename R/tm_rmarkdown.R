#' `teal` module: R Markdown render
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' Module to render R Markdown files using the data provided in the
#' `teal_data` object.
#'
#' The R Markdown file should be designed to accept variables available
#' in the data names of the module.
#'
#' @details
#' For example, if the `teal_data` object contains datasets named `mtcars`
#'  and `iris`, the R Markdown file can use these as variables as they
#' will be available in the R Markdown environment.
#'
#' The libraries used in the R Markdown file must be available in the
#' deployed shiny app environment.
#'
#' When developing the R Markdown file, the working data can be simulated
#' on a code chunk, which in turn can look for the presence of `.raw_data`
#' object to determine if it is being run inside the `teal` module or not.
#'
#' Example R markdown file:
#'
#' ``````md
#' ---
#' title: "R Markdown Report"
#' output: html_document
#' ---
#'
#' ```{r eval=!exists(".raw_data")}
#' mtcars <- datasets::mtcars
#' iris <- datasets::iris
#' ```
#'
#' ```{r}
#' summary(mtcars) |> print()
#' summary(iris) |> print()
#' ```
#' ``````
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#'
#' @param rmd_content (`character`) Content of the R Markdown file to be rendered.
#' This can be the value of `readLines("path/to/file.Rmd")`.
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
#' })
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_rmarkdown(
#'       label = "RMarkdown Module",
#'       rmd_content = c(
#'         "---",
#'         "title: \"R Markdown Report\"",
#'         "output: html_document",
#'         "---",
#'         "",
#'         "```{r}",
#'         "summary(CO2) |> print()",
#'         "```"
#'       )
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
#'       numericInput(ns("n_rows"), "Show n rows", value = 40, min = 0, max = 200, step = 5)
#'     )
#'   },
#'   server = function(id, data) {
#'     moduleServer(id, function(input, output, session) {
#'       reactive({
#'         req(data())
#'         within(data(),
#'           {
#'             n_rows <- n_rows_value
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
#'       rmd_content = readLines(
#'         system.file(
#'           file.path("sample_files", "co2_example.Rmd"),
#'           package = "teal.modules.general"
#'         )
#'       ),
#'       allow_download = FALSE,
#'       extra_transform = list(nrow_transform)
#'     )
#'   )
#' ) |> shiny::runApp()
#' @export
tm_rmarkdown <- function(label = "RMarkdown Module",
                         rmd_content,
                         datanames = "all",
                         allow_download = TRUE,
                         pre_output = NULL,
                         post_output = NULL,
                         transformators = list(),
                         extra_transform = list()) {
  message("Initializing tm_rmarkdown")

  # Start of assertions

  checkmate::assert_string(label)
  checkmate::assert_character(rmd_content)
  checkmate::assert_flag(allow_download)

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)

  # End of assertions

  # Make UI args
  args <- as.list(environment())

  ans <- module(
    label = label,
    server = srv_rmarkdown,
    server_args = list(rmd_content = rmd_content, allow_download = allow_download, extra_transform = extra_transform),
    ui = ui_rmarkdown,
    ui_args = args,
    transformators = transformators,
    datanames = datanames
  )
  disable_src(ans)
}

# UI function for the rmarkdown module
ui_rmarkdown <- function(id, rmd_content, allow_download, extra_transform, ...) {
  args <- list(...)
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      tags$div(
        tags$h4("Rendered report from Rmd"),
        if (allow_download) {
          downloadButton(
            ns("download_rmd"),
            sprintf("Download R Markdown file"),
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
srv_rmarkdown <- function(id, data, rmd_content, allow_download, extra_transform) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    pre_decorated_q_r <- reactive({
      data_q <- req(data())
      teal.reporter::teal_card(data_q) <- c(
        teal.reporter::teal_card(data_q),
        teal.reporter::teal_card("## Module's output(s)")
      )
      data_q
    })

    q_r <- data_with_output_decorated <- teal::srv_transform_teal_data(
      "extra_transform",
      data = pre_decorated_q_r,
      transformators = extra_transform
    )

    if (allow_download) {
      output$download_rmd <- downloadHandler(
        filename = function() "teal_module.Rmd", # TODO: find a better name
        content = function(file) {
          # find the end of the YAML header or start of the file
          # and insert the contents of teal.code::get_code(q_r())
          yaml_end <- which(rmd_content == "---")[2]
          insert_pos <- if (!is.na(yaml_end)) yaml_end else 0
          note_lines <- c(
            "",
            "### Pre-processing data",
            "",
            "The following code chunk was automatically added by the teal markdown module.",
            "It shows how to generate the data used in this report.",
            "",
            "```{r}",
            teal.code::get_code(q_r()),
            "```",
            ""
          )
          rmd_content <- append(rmd_content, note_lines, after = insert_pos)
          writeLines(rmd_content, con = file)
        },
        contentType = "text/plain"
      )
    }

    clean_up_r <- shiny::reactiveVal(list())
    # Can only clean on sessionEnded as temporary files are needed for the reporter
    # during session
    onSessionEnded(function() {
      logger::log_debug("srv_rmarkdown: cleaning up temporary folders.")
      lapply(shiny::isolate(clean_up_r()), function(f) f())
    }, session)

    rendered_path_r <- reactive({
      datasets <- rlang::env_clone(as.environment(req(q_r()))) # Clone to use unlocked environment
      temp_dir <- tempfile(pattern = "rmd_")
      dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
      temp_rmd <- tempfile(pattern = "rmarkdown_module-", tmpdir = temp_dir, fileext = ".Rmd")
      # Schedule cleanup of temp files when reactive is re-executed
      shiny::isolate({
        old_clean_up <- clean_up_r()
        clean_up_r(c(old_clean_up, function() unlink(temp_dir, recursive = TRUE)))
      })
      writeLines(rmd_content, con = temp_rmd)

      tryCatch(
        {
          rmarkdown::render(
            temp_rmd,
            output_format = rmarkdown::md_document(
              variant = "markdown",
              standalone = TRUE,
              dev = "png"
            ),
            envir = datasets,
            quiet = TRUE,
            runtime = "static"
          )
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
      report_doc <- .markdown_internal(rendered_path_r(), rendered_html_r())
      teal.reporter::teal_card(out_data) <- c(
        teal.reporter::teal_card(out_data), report_doc
      )
      out_data
    })
  })
}

#' Create internal markdown object for use in reporter
#'
#' Creates an object of class `markdown_internal` that contains the
#' content of a markdown file.
#'
#' This package registers S3 methods for `toHTML` and `to_rmd` for this class to
#' facilitate rendering in `teal.reporter`.
#'
#' @param markdown_file (`character(1)`) path to markdown file.
#' @param rendered_html (`shiny.tag`) rendered HTML content.
#'
#' @return `markdown_internal` object
#'
#' @keywords internal
.markdown_internal <- function(markdown_file, rendered_html) {
  base_file <- basename(markdown_file)

  # Create new custom structure with contents and images in base64 as attribute
  structure(
    readLines(markdown_file),
    class = c("markdown_internal", "character"),
    parent_path = dirname(markdown_file),
    old_base_path = sprintf("%s_files/", tools::file_path_sans_ext(base_file)),
    cached_html = rendered_html
  )
}

#' @describeIn dot-markdown_internal Custom [tools::toHTML()] method for markdown_internal class that
#' uses a cached rendering of the module.
#' @inheritParams tools::toHTML
#' @param ... Arguments that will be passed to the next method.
#' @exportS3Method tools::toHTML
toHTML.markdown_internal <- function(x, ...) {
  cached_html <- attr(x, "cached_html", exact = TRUE)
  if (!is.null(cached_html)) {
    return(cached_html)
  }
  NextMethod(unclass(x), ...)
}

#' @describeIn dot-markdown_internal Custom [teal.reporter::to_rmd()] method for `markdown_internal`
#' object will be used to render the report.
#' @inheritParams teal.reporter::to_rmd
#' @param figures_dir (`character(1)`) directory where the R markdown auxiliary files will be saved.
#' @exportS3Method teal.reporter::to_rmd
to_rmd.markdown_internal <- function(block, figures_dir = "figures", ...) {
  old_base_path <- attr(block, "old_base_path", exact = TRUE)
  parent_path <- attr(block, "parent_path", exact = TRUE)
  new_base_path <- file.path(figures_dir, old_base_path)

  # Copy figures from old path to new location
  dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
  file.copy(file.path(parent_path, old_base_path), figures_dir, recursive = TRUE)

  # Change the image paths in the markdown content
  block <- gsub(pattern = old_base_path, replacement = new_base_path, x = block, fixed = TRUE)
  NextMethod(unclass(block), ...)
}
