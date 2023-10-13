#' Landing Popup module
#'
#' @description This `teal` module creates a simple landing welcome popup for `teal` applications and
#' can be used in `teal::init(extra_server = )` parameter
#'
#' @param title tba
#' @param text tba
#' @param button tba
#' @param icon tba
#'
#' @export
tm_landing_popup <- function(title = NULL, text = NULL, button = NULL, icon = NULL) {
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_string(text, null.ok = TRUE)
  checkmate::assert_string(button, null.ok = TRUE)
  checkmate::assert_string(icon, null.ok = TRUE)

  shinyalert::shinyalert(
    title = title,
    text = text,
    type = "info",
    confirmButtonText = button
  )
}
