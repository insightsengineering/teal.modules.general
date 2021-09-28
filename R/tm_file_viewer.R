#' File Viewer Teal Module
#'
#' The file viewer module provides a tool to upload and view static files.
#' Supported formats include text formats, \code{PDF}, \code{PNG}, \code{APNG},
#' \code{JPEG}, \code{SVG}, \code{WEBP}, \code{GIF} and \code{BMP}.
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param input_path (`list`) A list of the input path to either specific files of accepted formats or a directory.
#'
#' @export
#'
#' @examples
#'
#' data <- data.frame(1)
#'
#' app <- init(
#'   data = teal_data(
#'     dataset("data", data)
#'   ),
#'   modules = root_modules(
#'     tm_file_viewer(input_path = list("."))
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_file_viewer <- function(label = "File Viewer Module",
                           input_path) {
  valid_url <- function(url_input, timeout = 2) {
    con <- try(url(url_input), silent = TRUE)
    check <- suppressWarnings(try(open.connection(con, open = "rt", timeout = timeout), silent = TRUE)[1])
    try(close.connection(con), silent = TRUE)
    ifelse(is.null(check), TRUE, FALSE)
  }

  stop_if_not(
    is_character_single(label),
    list(
      vapply(input_path, function(x) file.exists(x) || valid_url(x), logical(1)),
      "Non-existant file or url path, please provide valid paths."
    )
  )

  args <- as.list(environment())

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
      uiOutput(ns("output"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      shinyTree::shinyTree(
        ns("tree"),
        dragAndDrop = FALSE,
        sort = FALSE,
        wholerow = TRUE,
        theme = "proton",
        multiple = FALSE
      ),
      style = "overflow-y: none; overflow-x: auto;"
    )
  )
}

srv_viewer <- function(input, output, session, datasets, input_path) {
  temp_dir <- tempfile()
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }
  addResourcePath(basename(temp_dir), temp_dir)

  test_path_text <- function(file_path) {
    out <- tryCatch({
      readLines(con = file_path)
    },
    error = function(cond) {
      return(FALSE)
    },
    warning = function(cond) {
      return(FALSE)
    }
    )
  }

  handle_connection_type <- function(file_path) {
    file_extension <- tools::file_ext(file_path)
    file_class <- suppressWarnings(file(file_path))
    close(file_class)
    output_text <- test_path_text(file_path)

    if (class(file_class)[1] == "url") {
      list(file_path = file_path, output_text = output_text)
    } else {
      if (isFALSE(output_text[1]) || file_extension == "svg") {
        file.copy(
          normalizePath(file_path, winslash = "/"),
          temp_dir
        )
        file_path <- file.path(basename(temp_dir), basename(file_path))
      }

      list(file_path = file_path, output_text = output_text)
    }
  }

  display_file <- function(file_path) {
    con_type <- handle_connection_type(file_path)
    file_extension <- tools::file_ext(file_path)

    if (file_extension %in% c("png", "apng", "jpg", "jpeg", "svg", "gif", "webp", "bmp")) {
      tags$img(src = con_type$file_path, alt = "file does not exist")
    } else if (file_extension == "pdf") {
      tags$embed(
        style = "height:600px; width:100%",
        src = con_type$file_path
      )
    } else if (!isFALSE(con_type$output_text[1])) {
      tags$pre(paste0(con_type$output_text, collapse = "\n"))
    } else {
      tags$p("Please select a supported format.")
    }
  }

  tree_list <- function(file_or_dir) {
    nested_list <- lapply(file_or_dir, function(y) {
      isdir <- file.info(y)$isdir

      if (!isdir) {
        structure(y, sticon = "file")
      } else {
        files <- list.files(y, full.names = TRUE, include.dirs = TRUE)

        out <- lapply(files, function(x) tree_list(x))
        out <- unlist(out, recursive = F)
        if (!is_empty(files)) names(out) <- basename(files)
        out
      }
    })
    names(nested_list) <- file_or_dir
    nested_list
  }

  output$tree <- shinyTree::renderTree({
    if (all(vapply(input_path, file.exists, FUN.VALUE = logical(1)))) {
      tree_list(input_path)
    } else {
      input_path <- lapply(input_path, function(x) structure(x, sticon = "file"))
      names(input_path) <- input_path
      input_path
    }
  })

  observeEvent(
    eventExpr = input$tree,
    ignoreNULL = TRUE,
    handlerExpr = {
      if (!is_empty(shinyTree::get_selected(input$tree))) {
        obj <- shinyTree::get_selected(input$tree, format = "names")[[1]]
        file_path <- do.call("file.path", as.list(c(attr(obj, "ancestry"), obj[1])))
        req(file_path)

        output$output <- renderUI({
          validate(need(!isTRUE(file.info(file_path)$isdir), "Please select a single file."))
          display_file(file_path)
        })
      }
    }
  )

  onStop(function() {
    removeResourcePath(basename(temp_dir))
    unlink(temp_dir)
  })
}
