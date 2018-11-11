
no_select_keyword <- "-- no selection --"

#' Choices function
#'
#'
#' @export
#'
#' @examples
#'
#' select_choices(LETTERS[1:5])
#'
#'
select_choices <- function(choices, selected = choices[1]) {

  stopifnot(is.atomic(choices))

  if (!is.null(choices) && no_select_keyword %in% choices)
    stop(paste(no_select_keyword, "is not a valid choice as it is used as a keyword"))

  if (length(setdiff(selected, choices)) > 0)
    choices <- c(setdiff(selected, choices), choices)

  structure(
    list(
      choices = unique(choices),
      selected = unique(selected)
    ),
    class = "select_choices"
  )
}

#' @export
is.select_choices <- function(x) is(x, "select_choices")


add_no_selected_choices <- function(x) {
  stopifnot(is.select_choices(x))

  x$choices <- c(no_select_keyword, x$choices)
  if (is.null(x$selected)) x$selected <- no_select_keyword

  x
}



stopifnot_select_choices <- function() {

}
