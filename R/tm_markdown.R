#' `teal` module: Rmarkdown page
#'
#' Render arbitrary Rmarkdown code. `data` provided to teal application are available in the
#' rendered document.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @inheritParams rmarkdown::render
#' @param text (`character`) arbitrary Rmd code
#'
#' @inherit shared_params return
#'
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#' data <- teal_data() |>
#'   within({
#'     iris <- iris
#'     mtcars <- mtcars
#'   })
#
#'
#' @export
#'
tm_rmarkdown <- function(label = "App Info",
                        text = character(0),
                        params = list(title = "Document", output = "html_output"),
                        datanames = "all") {
  message("Initializing tm_front_page")
  
  # Start of assertions
  checkmate::assert_string(label)
  checkmate::assert_character(text, min.len = 0, any.missing = FALSE)
  checkmate::assert_list(params)

  
  ans <- module(
    label = label,
    server = srv_rmarkdown,
    ui = ui_rmarkdown,
    server_args = list(text = text, params = params),
    datanames = datanames
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the front page module
ui_rmarkdown <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  uiOutput(ns("output"))
}

# Server function for the front page module
srv_rmarkdown <- function(id, data, text, params) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    file <- tempfile(fileext = ".Rmd")
    if (!file.exists(file)) {
      cat(text, file = file)
    }
    
    rmd_out <- reactive({
      rmarkdown::render(
        file, 
        envir = data(), 
        params = utils::modifyList(params, list(output = "html_document")) # html_document always as we renderUI below
      )
    })
    
    output$output <- renderUI({
      on.exit(unlink(rmd_out()))
      shiny::HTML(paste(readLines(rmd_out()), collpse = "\n"))
    })
  })
}
