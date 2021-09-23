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
                           input_path) {
  valid_url <- function(url_input, timeout = 2) {
    con <- url(url_input)
    check <- suppressWarnings(try(open.connection(con, open = "rt", timeout = timeout), silent = TRUE)[1])
    close.connection(con)
    ifelse(is.null(check), TRUE, FALSE)
  }

  stop_if_not(
    is_character_single(label),
    vapply(input_path, function(x) file.exists(x) || valid_url(x), logical(1))
  )

  if (!is.list(input_path)) {
    input_path <- list(input_path)
  } else if (all(vapply(input_path, function(x) utils::file_test("-d", x), logical(1)))) {
    input_path_list <- lapply(input_path, function(x) {
      files <- list.files(x, include.dirs = FALSE)
      as.list(paste0(x, files))
      })
    input_path <- unlist(input_path_list)
  }

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
      uiOutput(ns("output")),
      verbatimTextOutput(ns("debug"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      shinyTree::shinyTree(ns("tree"), dragAndDrop=FALSE, sort = FALSE, wholerow = TRUE, theme="proton"),
      radioButtons(
        inputId = ns("file_name"),
        label = "Choose file to view:",
        choices = args$input_path,
        selected = args$input_path[[1]]
      ),
      style = "overflow: auto;"
    )
  )
}

srv_viewer <- function(input, output, session, datasets, input_path) {

  # browser()


  output$tree <- renderTree({
    tree.list(input_path[[1]])
  })

  output$debug <- renderPrint({
    # shinyTrees will also be available as inputs so you can
    # monitor changes that occur to the tree as the user interacts
    # with it.
    if(length(get_selected(input$tree, format = "names"))>0)
    {
      obj <- get_selected(input$tree, format = "names")[[1]]
      res <- paste(c(attr(obj, "ancestry"), obj), collapse = "/")
      res
    }
  })

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
    file_class <- file(file_path)
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

  observeEvent(eventExpr = ({
    # get_selected(input$tree)
    input$file_name
  }),
  ignoreNULL = FALSE,
  handlerExpr = {
    # browser()
    file_path <- input$file_name

    req(file_path)

    output$output <- renderUI({
      display_file(file_path)
    })
  })

  onStop(function() {
    removeResourcePath(basename(temp_dir))
    unlink(temp_dir)
  })
}

# Helper function

tree.list <- function(file.or.dir) {
  isdir <- file.info(file.or.dir)$isdir
  if (!isdir) {
    out <- file.or.dir
  } else {
    files <- list.files(file.or.dir, full.names   = TRUE,
                        include.dirs = TRUE)
    out <- lapply(files, tree.list)
    names(out) <- basename(files)
  }
  out
}
