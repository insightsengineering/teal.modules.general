#' Landing Popup Module
#'
#' @description This module creates a simple landing welcome popup for `teal` applications.
#'
#' @details If you use this module with a default label `"Landing Popup"`, it will be used as a first module in
#' a `teal` application and there will not be a tab provided for this module in the application.
#'
#' @inheritParams teal::module
#' @param title `character(1)` the text to be displayed as a title of the popup.
#' @param content `character(1)` the content of the popup. Passed to `...` of `shiny::modalDialog`. Can be a `character`
#' or a text input control (like `textInput`) or a list of `shiny` tags. See examples.
#' @param buttons `shiny` tag or a list of tags (`tagList`). Typically a `modalButton` or `actionButton`. See examples.
#'
#' @return A `teal` module to be used in `teal` applications.
#'
#' @examples
#' app1 <- teal::init(
#'   data = teal.data::dataset("iris", iris),
#'   modules = teal::modules(
#'     teal.modules.general::tm_landing_popup(
#'       title = "Welcome",
#'       content = "A place for the welcome message or a disclaimer statement.",
#'       buttons = modalButton("Proceed")
#'     ),
#'     teal.modules.general::tm_front_page()
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app1$ui, app1$server)
#' }
#'
#' app2 <- teal::init(
#'   data = teal.data::dataset("iris", iris),
#'   modules = teal::modules(
#'     teal.modules.general::tm_landing_popup(
#'       title = "Welcome",
#'       content = div(tags$b("A place for the welcome message or a disclaimer statement.", style = "color: red;")),
#'       buttons = tagList(
#'         modalButton("Proceed"),
#'         actionButton("close", "Read more", onclick = "window.open('http://google.com', '_blank')")
#'       )
#'     ),
#'     teal.modules.general::tm_front_page()
#'   )
#' )
#'
#' if (interactive()) {
#'   shinyApp(app2$ui, app2$server)
#' }
#'
#' @export
tm_landing_popup <-
  function(
      label = "Landing Popup",
      title = NULL,
      content = NULL,
      buttons = modalButton("Accept")
    ) {
    checkmate::assert_string(label)
    checkmate::assert_string(title, null.ok = TRUE)
    checkmate::assert_multi_class(
      content,
      classes = c("character", "shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE
    )
    checkmate::assert_multi_class(buttons, classes = c("shiny.tag", "shiny.tag.list"), null.ok = TRUE)

    logger::log_info("Initializing tm_landing_popup")

    module(
      label = label,
      server = srv_landing_popup,
      ui = ui_landing_popup,
      ui_args = NULL,
      server_args = list(title = title, content = content, buttons = buttons),
      datanames = NULL
    )
  }

srv_landing_popup <- function(id, title, content, buttons) {
  moduleServer(id, function(input, output, session) {
    showModal(
      modalDialog(
        id = "landingpopup",
        title = title,
        content,
        footer = buttons
      )
    )
  })
}

ui_landing_popup <- function(id, ...) {
  NULL
}
