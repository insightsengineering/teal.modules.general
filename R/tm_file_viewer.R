#' `teal` module: File viewer
#'
#' The file viewer module provides a tool to view static files.
#' Supported formats include text formats, `PDF`, `PNG` `APNG`,
#' `JPEG` `SVG`, `WEBP`, `GIF` and `BMP`.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param input_path (`list`) of the input paths, optional. Each element can be:
#'
#' Paths can be specified as absolute paths or relative to the running directory of the application.
#' Default to the current working directory if not supplied.
#'
#' @inherit shared_params return
#'
#' @examples
#' data <- teal_data()
#' data <- within(data, {
#'   data <- data.frame(1)
#' })
#' datanames(data) <- c("data")
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_file_viewer(
#'       input_path = list(
#'         folder = system.file("sample_files", package = "teal.modules.general"),
#'         png = system.file("sample_files/sample_file.png", package = "teal.modules.general"),
#'         txt = system.file("sample_files/sample_file.txt", package = "teal.modules.general"),
#'         url = "https://fda.gov/files/drugs/published/Portable-Document-Format-Specifications.pdf"
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
#'
tm_file_viewer <- function(label = "File Viewer Module",
                           input_path = list("Current Working Directory" = ".")) {
  message("Initializing tm_file_viewer")

  # Normalize the parameters
  if (length(label) == 0 || identical(label, "")) label <- " "
  if (length(input_path) == 0 || identical(input_path, "")) input_path <- list()

  # Start of assertions
  checkmate::assert_string(label)

  checkmate::assert(
    checkmate::check_list(input_path, types = "character", min.len = 0),
    checkmate::check_character(input_path, min.len = 1)
  )
  if (length(input_path) > 0) {
    valid_url <- function(url_input, timeout = 2) {
      con <- try(url(url_input), silent = TRUE)
      check <- suppressWarnings(try(open.connection(con, open = "rt", timeout = timeout), silent = TRUE)[1])
      try(close.connection(con), silent = TRUE)
      is.null(check)
    }
    idx <- vapply(input_path, function(x) file.exists(x) || valid_url(x), logical(1))

    if (!all(idx)) {
      warning(
        paste0(
          "Non-existent file or url path. Please provide valid paths for:\n",
          paste0(input_path[!idx], collapse = "\n")
        )
      )
    }
    input_path <- input_path[idx]
  } else {
    warning(
      "No file or url paths were provided."
    )
  }
  # End of assertions

  # Make UI args
  args <- as.list(environment())

  ans <- module(
    label = label,
    server = srv_viewer,
    server_args = list(input_path = input_path),
    ui = ui_viewer,
    ui_args = args,
    datanames = NULL
  )
  attr(ans, "teal_bookmarkable") <- FALSE
  ans
}

# UI function for the file viewer module
ui_viewer <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)

  tagList(
    include_css_files("custom"),
    teal.widgets::standard_layout(
      output = tags$div(
        uiOutput(ns("output"))
      ),
      encoding = tags$div(
        class = "file_viewer_encoding",
        tags$label("Encodings", class = "text-primary"),
        shinyTree::shinyTree(
          ns("tree"),
          dragAndDrop = FALSE,
          sort = FALSE,
          wholerow = TRUE,
          theme = "proton",
          multiple = FALSE
        )
      )
    )
  )
}

# Server function for the file viewer module
srv_viewer <- function(id, input_path) {
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    temp_dir <- tempfile()
    if (!dir.exists(temp_dir)) {
      dir.create(temp_dir, recursive = TRUE)
    }
    addResourcePath(basename(temp_dir), temp_dir)

    test_path_text <- function(selected_path, type) {
      out <- tryCatch(
        expr = {
          if (type != "url") {
            selected_path <- normalizePath(selected_path, winslash = "/")
          }
          readLines(con = selected_path)
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

      output_text <- test_path_text(selected_path, type = class(file_class)[1])

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
          class = "embed_pdf",
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
            out <- unlist(out, recursive = FALSE)
            if (length(files) > 0) names(out) <- basename(files)
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
      if (length(input_path) > 0) {
        tree_list(input_path)
      } else {
        list("Empty Path" = NULL)
      }
    })

    output$output <- renderUI({
      validate(
        need(
          length(shinyTree::get_selected(input$tree)) > 0,
          "Please select a file."
        )
      )

      obj <- shinyTree::get_selected(input$tree, format = "names")[[1]]
      repo <- attr(obj, "ancestry")
      repo_collapsed <- if (length(repo) > 1) paste0(repo, collapse = "/") else repo
      is_not_named <- file.exists(file.path(c(repo_collapsed, obj[1])))[1]

      if (is_not_named) {
        selected_path <- do.call("file.path", as.list(c(repo, obj[1])))
      } else {
        if (length(repo) == 0) {
          selected_path <- do.call("file.path", as.list(attr(input$tree[[obj[1]]], "ancestry")))
        } else {
          selected_path <- do.call("file.path", as.list(attr(input$tree[[repo]][[obj[1]]], "ancestry")))
        }
      }

      validate(
        need(
          !isTRUE(file.info(selected_path)$isdir) && length(selected_path) > 0,
          "Please select a single file."
        )
      )
      display_file(selected_path)
    })

    onStop(function() {
      removeResourcePath(basename(temp_dir))
      unlink(temp_dir)
    })
  })
}
