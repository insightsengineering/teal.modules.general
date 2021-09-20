#' File Viewer Teal Module
#'
#' The file viewer module provides a tool to upload and view static files.
#' Supported formats include text formats, PDF, PNG, JPEG and SVG.
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param input_path (`list`) A list of the input path to either specific files of accepted formats or a directory.
#'
#' @export
#'
#' @examples
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl")
#'   ),
#'   modules = root_modules(
#'     tm_file_viewer(input_path = list("./"))
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_file_viewer <- function(label = "File Viewer Module",
                           input_path = NULL) {
  stop_if_not(
    is_character_single(label),
    is.null(input_path) || sapply(input_path, function(x) file.exists(x))
  )

  if (!is.null(input_path) && !is.list(input_path)) {
    input_path <- list(input_path)
  } else if (!is.null(input_path) && utils::file_test("-d", input_path[[1]])) {
    files <- list.files(input_path[[1]])
    input_path <- as.list(paste0(input_path[[1]], files))
  }

  args <- as.list(environment())

  data_extract_list <- list(
    input_path = input_path
  )

  module(
    label = label,
    server = srv_viewer,
    server_args = list(input_path),
    ui = ui_viewer,
    ui_args = args,
    filters = NULL
  )
}

ui_viewer <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)

  standard_layout(
    output = div(
      verbatimTextOutput(ns("text")),
      uiOutput(ns("output"))
      ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      radioButtons(
        inputId = ns("file_name"),
        label = "Choose file to view:",
        choices = args$input_path,
        selected = args$input_path[[1]]
      )
    )
  )
}

srv_viewer <- function(input, output, session, datasets, input_path) {
  observeEvent(input$file_name, {
    data_path <- input$file_name

    req(data_path)

    test_path_text <- function(data_path) {
      out <- tryCatch({
        readLines(con = data_path)
        },
        error = function(cond) {
          return("error/warning")
        },
        warning = function(cond) {
          return("error/warning")
        }
      )
    }

    output_text <- test_path_text(data_path)

    if (output_text[1] != "error/warning") {
      output$text <- {
        renderText(paste0(output_text, collapse = "\n"))
      }

      output$output <- renderUI({""})

    } else if (gsub(".*\\.", "", data_path) %in% c("pdf", "png", "jpg", "jpg", "jpeg", "svg")) {
      suffix <- switch(gsub(".+\\.", "", data_path),
        "pdf" = ".pdf",
        "png" = ".png",
        "jpg" = ".jpg",
        "jpeg" = ".jpeg",
        "svg" = ".svg"
      )

      addResourcePath("www", system.file("www", package = "teal.modules.general"))

      file.copy(
        normalizePath(data_path, winslash = "/"),
        paste0(system.file("www", package = "teal.modules.general"), "/0", suffix),
        overwrite = T
      )

      output$output <- renderUI({
        tags$iframe(
          style = "height:600px; width:100%",
          src = paste0("www/0", suffix)
        )
      })

      output$text <- renderText("")

    } else {
      output$output <- renderText({
        "Please select a supported format."
      })
    }
  },
  ignoreNULL = FALSE
  )

  onStop(function() {
    cat("Session stopped\n")
    do.call(file.remove, list(list.files("inst/www", full.names = TRUE)))
    cat("Static files cleared")
    })
}
