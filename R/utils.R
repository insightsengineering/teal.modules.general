
# from teal.tern
whiteSmallWell <- function(...) {
  shiny::tags$div(class = "well well-sm", style = "background-color: white;", shiny::tags$div(style = "overflow-x: auto;",...))
}

as.global <- function(...) {

  dots <- substitute(list(...))[-1]
  names <- sapply(dots, deparse)

  args <- list(...)

  ge <- globalenv()

  Map(function(x, name) {
    ge[[name]] <- x
  }, args, names)

}

