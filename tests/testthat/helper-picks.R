set_picks_slot_selected <- function(app_driver, id, value, slot_name = c("variables", "datasets", "values"), ...) {
  id <- app_driver$namespaces()$module(id)
  .set_picks_slot_selected(app_driver, id, value, slot_name, ...)
}

#' Set the selected value of a teal.picks slot in a teal app using shinytest2.
#'
#' It sets the value of picks of the currently active module.
#'
#' @param app_driver (`TealAppDriver`).
#' @param id (`character(1)`) `picks`` id.
#' @param slot_name (`character(1)`) The name of the slot. One of "variables", "datasets", or "values".
#' @param value The value to set using `AppDriver$set_input`
#' @param ... arguments passed to `AppDriver$set_input`.
#' @return `TRUE` if the value was set successfully, `FALSE` if the value was already set,
#' and a warning if the resolved value does not match the expected value.
.set_picks_slot_selected <- function(app_driver, id, value, slot_name = c("variables", "datasets", "values"), ...) {
  slot_name <- match.arg(slot_name)
  selected_open_id <- sprintf("%s-%s-selected_open", id, slot_name) # teal.picks implementation detail
  selected_id <- sprintf("%s-%s-selected", id, slot_name)

  if (isTRUE(app_driver$get_value(input = selected_open_id))) {
    stop("Cannot set picks slot while the picker input is open. Please close the picker input first.")
  }
  if (isTRUE(all.equal(app_driver$get_value(input = selected_id), value, tolerance = 1e-15))) {
    return(FALSE)
  }

  # Mock the opening of the picker input
  app_driver$set_input(selected_open_id, TRUE, allow_no_input_binding_ = TRUE)
  # Set the value

  tryCatch(
    .change_selectpicker(app_driver, selected_id, value),
    error = function(e) warning(e)
  )

  # Mock the closing of the picker input
  app_driver$set_input(selected_open_id, FALSE, allow_no_input_binding_ = TRUE)

  # Validate change in picks
  picks_export_id <- sprintf("%s-picks_resolved", id)
  resolved_value <- app_driver$get_values(export = picks_export_id)$export[[picks_export_id]][[slot_name]]$selected
  if (!isTRUE(all.equal(resolved_value, value, tolerance = 1e-15))) {
    warning("Setting picks slot did not result in expected resolved value.")
  }
  TRUE
}

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
  app_driver$run_js(sprintf("$('select#%s').selectpicker('val', %s);", id, jsonlite::toJSON(value, auto_unbox = TRUE)))
  if (wait_) {
    app_driver$wait_for_idle()
  }
  return(all.equal(app_driver$get_values()$input[[id]], value, tolerance = 1e-15))
}