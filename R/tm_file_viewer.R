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
  valid_url <- function(url_input, timeout = 2) {
    con <- url(url_input)
    check <- suppressWarnings(try(open.connection(con, open = "rt", timeout = timeout), silent = TRUE)[1])
    suppressWarnings(try(close.connection(con), silent = TRUE))
    ifelse(is.null(check), TRUE, FALSE)
  }

  stop_if_not(
    is_character_single(label),
    is.null(input_path) || sapply(input_path, function(x) file.exists(x)) || valid_url(input_path[[1]])
  )

  if (!is.null(input_path) && !is.list(input_path)) {
    input_path <- list(input_path)
  } else if (!is.null(input_path) && utils::file_test("-d", input_path[[1]])) {
    files <- list.files(input_path[[1]], include.dirs = FALSE)
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
  temp_dir <- tempdir()
  temp_dir_www <- paste0(temp_dir, "/www")
  if (!dir.exists(temp_dir_www)){
    dir.create(temp_dir_www)
  }
  addResourcePath("www", temp_dir_www)

  test_path_text <- function(file_path) {
    out <- tryCatch({
      readLines(con = file_path)
    },
    error = function(cond) {
      return("error/warning")
    },
    warning = function(cond) {
      return("error/warning")
    }
    )
  }

  handle_connection_type <- function(file_path) {
    file_extension <- tools::file_ext(file_path)
    file_class <- file(file_path)
    close(file_class)

    if (class(file_class)[1] == "url") {
      output_text <- test_path_text(file_path)
      list(file_path = file_path, output_text = output_text)
    } else {
      output_text <- test_path_text(file_path)
      file <- basename(file_path)
      temp_file <- basename(tempfile())

      if (output_text[1] == "error/warning" || file_extension == "svg") {
        new_path <-  paste0(temp_dir_www, "/", temp_file, ".", file_extension)
        file.copy(
          normalizePath(file_path, winslash = "/"),
          new_path
        )
        file_path <- paste0("www/", temp_file, ".", file_extension)
      }

      list(file_path = file_path, output_text = output_text)
    }
  }

  display_file <- function(file_path) {
    con_type <- handle_connection_type(file_path)
    file_extension <- tools::file_ext(file_path)

    if (con_type$output_text[1] != "error/warning" && file_extension != "svg") {
      output$text <- {
        renderText(paste0(con_type$output_text, collapse = "\n"))
      }
      output$output <- renderUI("")

      output
      } else if (file_extension %in% c("png", "apng", "jpg", "jpeg", "svg", "gif", "webp", "bmp")) {
        output$output <- renderUI({
          tags$img(
            src = con_type$file_path
          )
        })
        output$text <- renderText("")
        output
      } else if (file_extension %in% c("pdf")) {
        output$output <- renderUI({
          tags$embed(
            style = "height:600px; width:100%",
            src = con_type$file_path
          )
        })
        output$text <- renderText("")
        output
      } else {
        output$output <- renderText({
          "Please select a supported format."
        })
        output$text <- renderText("")
        output
      }
    }

  observeEvent(input$file_name, {
    file_path <- input$file_name
    req(file_path)

    output <- display_file(file_path)
  },
  ignoreNULL = FALSE
  )

  onStop(function() {
    cat("Session stopped\n")
    do.call(file.remove, list(list.files(temp_dir_www, full.names = TRUE)))
    cat("Static files cleared")
    })
}
