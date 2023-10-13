#' Landing Popup
#'
#' @description This function creates a simple landing welcome popup for `teal` applications and
#' can be used in a `teal::init(extra_server = )` parameter.
#'
#' @param title `character(1)` the text to be displayed as a title of the popup.
#' @param content `character(1)` the content of the popup. Passed to `...` of `shiny::modalDialog`. Can be a `character`
#' or a text input control (like `textInput`) or a list of `shiny` tags. See examples.
#' @param buttons `shiny` tag or a list of tags (`tagList`). Typically a `modalButton` or `actionButton`. See examples.
#'
#' @examples
#' app1 <- teal::init(
#'   data = teal.data::dataset("iris", iris),
#'   modules = teal::modules(
#'     teal.modules.general::tm_front_page("A")
#'   ),
#'   extra_server = teal.modules.general::landing_popup(
#'     title = "Welcome",
#'     content = "A place for the welcome message or a disclaimer statement.",
#'     buttons = modalButton("Proceed")
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app1$ui, app1$server)
#' }
#'
#' app2 <- teal::init(
#'   data = teal.data::dataset("iris", iris),
#'   modules = teal::modules(
#'     teal.modules.general::tm_front_page("A")
#'   ),
#'   extra_server = teal.modules.general::landing_popup(
#'     title = "Welcome",
#'     content = div(tags$b("A place for the welcome message or a disclaimer statement.", style = "color: red;")),
#'     buttons = tagList(modalButton("Proceed"), actionButton("close", "Read more", onclick = "window.open('http://google.com', '_blank')"))
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(app2$ui, app2$server)
#' }
#'
#' @export
landing_popup <- function(title = NULL, content = NULL, buttons = modalButton("Accept")) {
  checkmate::assert_string(title, null.ok = TRUE)
  checkmate::assert_multi_class(content, classes = c("character", "shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(buttons, classes = c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)

  showModal(
    modalDialog(
      title = title,
      content,
      footer = buttons
    )
  )
  # div(class="sweet-overlay", tabindex = "-1", style = "opacity: 1.17;display: block;backdrop-filter: blur(10px);")
}
