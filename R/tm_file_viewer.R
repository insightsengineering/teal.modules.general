#' File Viewer Teal Module
#'
#' The file viewer module provides a tool to view static files.
#' Supported formats include text formats, \code{PDF}, \code{PNG}, \code{APNG},
#' \code{JPEG}, \code{SVG}, \code{WEBP}, \code{GIF} and \code{BMP}.
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param input_path (`list`) `optional` A list of the input paths to either: specific files of accepted formats,
#'   a directory or a URL. The paths can be specified as absolute paths or relative to the running
#'   directory of the application. Will default to current working directory if not supplied.
#'
#' @export
#'
#' @examples
#' data <- data.frame(1)
#' app <- init(
#'   data = teal_data(
#'     dataset("data", data)
#'   ),
#'   modules = root_modules(
#'     tm_file_viewer(
#'       input_path = list(
#'         folder = system.file("sample_files", package = "teal.modules.general"),
#'         png = system.file("sample_files/sample_file.png", package = "teal.modules.general"),
#'         txt = system.file("sample_files/sample_file.txt", package = "teal.modules.general"),
#'         url = "https://www.fda.gov/files/drugs/published/Portable-Document-Format-Specifications.pdf"
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_file_viewer <- function(label = "File Viewer Module",
                           input_path = list("Current Working Directory" = ".")) {

  stop_if_not(
    is_character_single(label),
    is_character_list(input_path) || is_character_vector(input_path)
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

  test_path_text <- function(selected_path) {
    out <- tryCatch({
      readLines(con = normalizePath(selected_path, winslash = "/"))
    },
    error = function(cond) FALSE,
    warning = function(cond) {
     `if`(grepl("^incomplete final line found on", cond[[1]]), suppressWarnings(eval(cond[[2]])), FALSE)
    }
    )
  }

  handle_connection_type <- function(selected_path) {
    file_extension <- tools::file_ext(selected_path)
    file_class <- suppressWarnings(file(selected_path))
    close(file_class)
    output_text <- test_path_text(selected_path)

    if (class(file_class)[1] == "url") {
      list(selected_path = selected_path, output_text = output_text)
    } else {
      file.copy(normalizePath(selected_path, winslash = "/"), temp_dir)
      selected_path <- file.path(basename(temp_dir), basename(selected_path))
      list(selected_path = selected_path, output_text = output_text)
    }
  }

  display_file <- function(selected_path) {
    con_type <- handle_connection_type(selected_path)
    file_extension <- tools::file_ext(selected_path)

    if (file_extension %in% c("png", "apng", "jpg", "jpeg", "svg", "gif", "webp", "bmp")) {
      tags$img(src = con_type$selected_path, alt = "file does not exist")
    } else if (file_extension == "pdf") {
      tags$embed(
        style = "height:600px; width:100%",
        src = con_type$selected_path
      )
    } else if (!isFALSE(con_type$output_text[1])) {
      tags$pre(paste0(con_type$output_text, collapse = "\n"))
    } else {
      tags$p("Please select a supported format.")
    }
  }

  tree_list <- function(file_or_dir) {
    nested_list <- lapply(file_or_dir, function(path) {
      file_class <- suppressWarnings(file(path))
      close(file_class)
      if (class(file_class)[[1]] != "url") {
        isdir <- file.info(path)$isdir
        if (!isdir) {
          structure(path, ancestry = path, sticon = "file")
        } else {
          files <- list.files(path, full.names = TRUE, include.dirs = TRUE)
          out <- lapply(files, function(x) tree_list(x))
          out <- unlist(out, recursive = F)
          if (!is_empty(files)) names(out) <- basename(files)
          out
        }
      } else {
        structure(path, ancestry = path, sticon = "file")
      }
    })
    missing_labels <- if (is.null(names(nested_list))) seq_along(nested_list) else which(names(nested_list) == "")
    names(nested_list)[missing_labels] <- file_or_dir[missing_labels]
    nested_list
  }

  output$tree <- shinyTree::renderTree({

    valid_url <- function(url_input, timeout = 2) {
      con <- try(url(url_input), silent = TRUE)
      check <- suppressWarnings(try(open.connection(con, open = "rt", timeout = timeout), silent = TRUE)[1])
      try(close.connection(con), silent = TRUE)
      ifelse(is.null(check), TRUE, FALSE)
    }
    validate(
      need(
        vapply(input_path, function(x) file.exists(x) || valid_url(x), logical(1)),
        "Non-existant file or url path, please provide valid paths."
      )
    )

    tree_list(input_path)
  })

  observeEvent(
    eventExpr = input$tree,
    ignoreNULL = TRUE,
    handlerExpr = {
      if (!is_empty(shinyTree::get_selected(input$tree))) {
        obj <- shinyTree::get_selected(input$tree, format = "names")[[1]]
        repo <- attr(obj, "ancestry")
        repo_collapsed <- if (length(repo) > 1) paste0(repo, collapse = "/") else repo
        is_not_named <- file.exists(file.path(c(repo_collapsed, obj[1])))[1]

        if (is_not_named) {
          selected_path <- do.call("file.path", as.list(c(repo, obj[1])))
        } else {
          if (is_empty(repo)) {
            selected_path <- do.call("file.path", as.list(attr(input$tree[[obj[1]]], "ancestry")))
          } else {
            selected_path <- do.call("file.path", as.list(attr(input$tree[[repo]][[obj[1]]], "ancestry")))
          }
        }

        output$output <- renderUI({
          validate(
            need(!isTRUE(file.info(selected_path)$isdir) && !is_empty(selected_path), "Please select a single file.")
          )
          display_file(selected_path)
        })
      }
    }
  )

  onStop(function() {
    removeResourcePath(basename(temp_dir))
    unlink(temp_dir)
  })
}
