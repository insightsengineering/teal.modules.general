#' add condition to rule function
#'
#' Adds a condition to a rule funciton to be used in an `InputValidator`.
#'
#' When building a `shinyvalidate::InputValidator`, some rules may only need to be checked
#' under specific conditions. Since an input value may only be validated by one validator object
#' (as per `shinyvalidate` documentation), some validation scenarios cannot be covered by
#' adding `condition`s to a whole validator.
#'
#' This function takes a function or formula that can be used as a validation rule
#' and incorporates `condition` into its body, upstream of the actual test.
#'
#' In cases where `condition` relies in input values, it is safer to wrap `condition`
#' in an `isTRUE` call so that missing values or NULLs do not crash evaluation.
#' For example, `input$id == "x"` will return `logical(0)` if input$id is NULL
#' and `NA` if input$id is NA, whereas `isTRUE(input$id == "x")` will reliably return `FALSE`.
#'
#' @section Expression length:
#' There is a yet unsolved bug that causes `crule` to fail if the condition is too long,
#' i.e. requires too many characters to specify. This is somehow tied to text wrapping in the console.
#' Efforts are being undertaken to remove this bug but for the time being it has to be avoided.
#' For example, if the application raises `Error in str2lang(s) : argument must be character`
#' and condition` is `input$id %in% c(element1, element2, element3, element, element5')`,
#' consider assigning `allowed <- c(element1, element2, element3, element, element5')`
#' and pass `condition = isTRUE(input$id %in% allowed)`.
#'
#' @param rule `function` or `formula` that specifies a validation rule and a failing message
#' @param condition `call` that specifies when to check `rule`, see `Details`
#' @param ... additional arguments passed to `rule`
#'
#' @return
#' Returns a rule function, ready to be placed into a `iv$add_rule` call.
#'
#' @seealso `[shinyvalidate::InputValidator]`
#'
#' @examples
#' \notrun{
#' library(shinyvalidate)
#'
#' iv <- InputValidator$new()
#' iv$add_rule("id", sv_required())
#' iv$add_rule("id", crule(sv_in_set(set), !is.null(set)))
#' }
#'
#' @export
#'
crule <- function(rule, condition, ...) {
  checkmate::assert(
    checkmate::check_class(rule, "function"),
    checkmate::check_class(rule, "formula")
  )
  checkmate::assert_class(substitute(condition), "call")

  if (inherits(rule, "formula")) {
    rule <- rlang::as_function(rule)
  }
  # add backstop for Error in str2lang(s) : argument must be a character string resulting from... line break in console???

  ex <- list(
    str2lang(sprintf("if (isFALSE(%s)) return(NULL)", deparse(substitute(condition)))),
    body(rule)
  )
  body(rule) <- as.call(c(as.name("{"), ex))
  rule
}
