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
        filename = function() basename(rmd_file),
        content = function(file) {
          lines <- readLines(rmd_file)

          # find the end of the YAML header or start of the file
          # and insert the contents of teal.code::get_code(q_r())
          yaml_end <- which(lines == "---")[2]
          insert_pos <- if (!is.na(yaml_end)) yaml_end else 0
          note_lines <- c(
            "",
            "```{r}",
            "# The following code chunk was automatically added by the teal markdown module",
            "# It shows how to generate the data used in this report",
            teal.code::get_code(q_r()),
            "```",
            ""
          )
          lines <- append(lines, note_lines, after = insert_pos)
          writeLines(lines, con = file)
        },
        contentType = "text/plain"
      )
    }

    temp_dir <- tempdir()
    temp_rmd <- tempfile(tmpdir = temp_dir, fileext = ".Rmd")
    file.copy(rmd_file, temp_rmd) # Use a copy of the Rmd file to avoid modifying the original

    rendered_path_r <- reactive({
      datasets <- req(q_r()) # Ensure data is available
      tryCatch(
        {
          rmarkdown::render(
            temp_rmd,
            output_format = rmarkdown::md_document(
              variant = "markdown",
              standalone = TRUE,
              dev = "png"
            ),
            envir = environment(datasets),
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

      if (allow_download) {
        out_data <- eval_code(
          q_r(),
          paste(
            sep = "\n",
            sprintf("## R Markdown contents are generated from file, please download it from the module UI."),
            sprintf("# rmarkdown::render(%s, params = params)", shQuote(basename(rmd_file), type = "cmd"))
          )
        )
        out_data@verified <- FALSE # manual change verified status as code is being injected
      }

      report_doc <- .markdown_internal(rendered_path_r(), temp_dir, rendered_html_r())
      teal.reporter::teal_card(out_data) <- c(
        teal.reporter::teal_card(out_data), report_doc
      )
      out_data
    })
  })
}

#' @exportS3Method tools::toHTML
toHTML.markdown_teal_internal <- function(block, ...) {
  cached_html <- attr(block, "cached_html", exact = TRUE)
  if (!is.null(cached_html)) {
    return(cached_html)
  }
  NextMethod(unclass(block), ...)
}

#' @method to_rmd markdown_internal
to_rmd.markdown_teal_internal <- function(block, figures_dir = "figures", include_chunk_output = TRUE, ...) {
  images_base64 <- attr(block, "images_base64", exact = TRUE)
  for (img_path in names(images_base64)) {
    img_data <- sub("^data:.*;base64,", "", images_base64[[img_path]])
    img_tag_pattern <- paste0("!\\[.*?\\]\\(", img_path, "\\)")
    dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)
    path <- file.path(
      figures_dir,
      sprintf(
        "markdown_img_%s.%s",
        substr(rlang::hash(img_data), 1, 6),
        sprintf("%s", tools::file_ext(img_path))
      )
    )
    writeBin(base64enc::base64decode(img_data), path)
    replacement_tag <- sprintf("![](%s)", path)
    block <- gsub(img_tag_pattern, replacement_tag, block, fixed = FALSE)
  }
  NextMethod(unclass(block), ...)
}

.markdown_internal <- function(markdown_file, temp_dir, rendered_html) {
  # Read the markdown file
  lines <- readLines(markdown_file)
  images_base64 <- list()

  # Extract images based on pattern ![](.*)
  img_pattern <- "!\\[.*?\\]\\((.*?)\\)"
  img_tags <- unlist(regmatches(lines, gregexpr(img_pattern, lines)))
  for (ix in seq_along(img_tags)) {
    img_tag <- img_tags[[ix]]
    img_path <- gsub("!\\[.*?\\]\\((.*?)\\)", "\\1", img_tag)
    full_img_path <- file.path(temp_dir, img_path)
    if (file.exists(full_img_path)) {
      img_data <- knitr::image_uri(full_img_path)
      images_base64[[img_path]] <- img_data
    }
  }

  # Create new custom structure with contents and images in base64 as attribute
  structure(
    lines,
    class = c("markdown_teal_internal", "character"),
    images_base64 = images_base64,
    cached_html = rendered_html
  )
}
