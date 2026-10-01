#' Change the selected values of teal.picks selectors
#' @param selectors A list of teal.picks selectors resulting from [teal.picks::picks_srv()]
#' @param ... Named arguments where the name corresponds to a selector and the value is the new selection.
#' @return `TRUE` if the values were set successfully, otherwise an error is thrown.
.change_selectors <- function(selectors, ...) {
  dots <- rlang::dots_list(..., .named = TRUE)
  for (name in names(dots)) {
    if (!name %in% names(selectors)) {
      stop(paste0("Selector '", name, "' not found in selectors."))
    }
    sel <- selectors[[name]]()
    sel$variables$selected <- dots[[name]]
    selectors[[name]](sel)
  }
  TRUE
}

#' Change the selected value of a shinywidgets::pickerInput in a teal app using shinytest2.
#' @param app_driver (`TealAppDriver`).
#' @param id (`character(1)`) `pickerInput` id.
#' @param value The value to set using `AppDriver$set_input`
.change_selectpicker <- function(app_driver, id, value, wait_ = TRUE) {
  if (is.null(value) || length(value) == 0 || identical(value, "")) { # De-select values needs to use shinytest2 API
    app_driver$set_input(id, "")
    value <- ""
  } else {
    json_parsed <- jsonlite::toJSON(value, auto_unbox = TRUE)
    app_driver$run_js(sprintf("$('select#%s').selectpicker('val', %s);", id, json_parsed))
  }
  if (wait_) {
    app_driver$wait_for_idle()
  }
  all.equal(app_driver$get_values()$input[[id]], value, tolerance = 1e-15)
}
