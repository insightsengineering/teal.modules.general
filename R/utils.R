#' Shared parameters documentation
#'
#' Defines common arguments shared across multiple functions in the package
#' to avoid repetition by using `inheritParams`.
#'
#' @param plot_height (`numeric`) optional, specifies the plot height as a three-element vector of
#' `value`, `min`, and `max` intended for use with a slider UI element.
#' @param plot_width (`numeric`) optional, specifies the plot width as a three-element vector of
#' `value`, `min`, and `max` for a slider encoding the plot width.
#' @param rotate_xaxis_labels (`logical`) optional, whether to rotate plot X axis labels. Does not
#' rotate by default (`FALSE`).
#' @param ggtheme (`character`) optional, `ggplot2` theme to be used by default. Defaults to `"gray"`.
#' @param ggplot2_args (`ggplot2_args`) object created by [teal.widgets::ggplot2_args()]
#' with settings for the module plot.
#' The argument is merged with options variable `teal.ggplot2_args` and default module setup.
#'
#' For more details see the vignette: `vignette("custom-ggplot2-arguments", package = "teal.widgets")`
#' @param basic_table_args (`basic_table_args`) object created by [teal.widgets::basic_table_args()]
#' with settings for the module table.
#' The argument is merged with options variable `teal.basic_table_args` and default module setup.
#'
#' For more details see the vignette: `vignette("custom-basic-table-arguments", package = "teal.widgets")`
#' @param pre_output (`shiny.tag`) optional, text or UI element to be displayed before the module's output,
#' providing context or a title.
#'  with text placed before the output to put the output into context. For example a title.
#' @param post_output (`shiny.tag`) optional, text or UI element to be displayed after the module's output,
#' adding context or further instructions. Elements like `shiny::helpText()` are useful.
#' @param alpha (`integer(1)` or `integer(3)`) optional, specifies point opacity.
#' - When the length of `alpha` is one: the plot points will have a fixed opacity.
#' - When the length of `alpha` is three: the plot points opacity are dynamically adjusted based on
#' vector of `value`, `min`, and `max`.
#' @param size (`integer(1)` or `integer(3)`) optional, specifies point size.
#' - When the length of `size` is one: the plot point sizes will have a fixed size.
#' - When the length of `size` is three: the plot points size are dynamically adjusted based on
#' vector of `value`, `min`, and `max`.
#'
#' @return Object of class `teal_module` to be used in `teal` applications.
#'
#' @name shared_params
#' @keywords internal
NULL

#' Add labels for facets to a `ggplot2` object
#'
#' Enhances a `ggplot2` plot by adding labels that describe
#' the faceting variables along the x and y axes.
#'
#' @param p (`ggplot2`) object to which facet labels will be added.
#' @param xfacet_label (`character`) Label for the facet along the x-axis.
#' If `NULL`, no label is added. If a vector, labels are joined with " & ".
#' @param yfacet_label (`character`) Label for the facet along the y-axis.
#' Similar behavior to `xfacet_label`.
#'
#' @return Returns `grid` or `grob` object (to be drawn with `grid.draw`)
#'
#' @examples
#' library(ggplot2)
#' library(grid)
#'
#' p <- ggplot(mtcars) +
#'   aes(x = mpg, y = disp) +
#'   geom_point() +
#'   facet_grid(gear ~ cyl)
#'
#' xfacet_label <- "cylinders"
#' yfacet_label <- "gear"
#' res <- add_facet_labels(p, xfacet_label, yfacet_label)
#' grid.newpage()
#' grid.draw(res)
#'
#' grid.newpage()
#' grid.draw(add_facet_labels(p, xfacet_label = NULL, yfacet_label))
#' grid.newpage()
#' grid.draw(add_facet_labels(p, xfacet_label, yfacet_label = NULL))
#' grid.newpage()
#' grid.draw(add_facet_labels(p, xfacet_label = NULL, yfacet_label = NULL))
#'
#' @export
#'
add_facet_labels <- function(p, xfacet_label = NULL, yfacet_label = NULL) {
  checkmate::assert_class(p, classes = "ggplot")
  checkmate::assert_character(xfacet_label, null.ok = TRUE, min.len = 1)
  checkmate::assert_character(yfacet_label, null.ok = TRUE, min.len = 1)
  if (is.null(xfacet_label) && is.null(yfacet_label)) {
    return(ggplotGrob(p))
  }
  grid::grid.grabExpr({
    g <- ggplotGrob(p)

    # we are going to replace these, so we make sure they have nothing in them
    checkmate::assert_class(g$grobs[[grep("xlab-t", g$layout$name, fixed = TRUE)]], "zeroGrob")
    checkmate::assert_class(g$grobs[[grep("ylab-r", g$layout$name, fixed = TRUE)]], "zeroGrob")

    xaxis_label_grob <- g$grobs[[grep("xlab-b", g$layout$name, fixed = TRUE)]]
    xaxis_label_grob$children[[1]]$label <- paste(xfacet_label, collapse = " & ")
    yaxis_label_grob <- g$grobs[[grep("ylab-l", g$layout$name, fixed = TRUE)]]
    yaxis_label_grob$children[[1]]$label <- paste(yfacet_label, collapse = " & ")
    yaxis_label_grob$children[[1]]$rot <- 270

    top_height <- if (is.null(xfacet_label)) 0 else grid::unit(2, "line")
    right_width <- if (is.null(yfacet_label)) 0 else grid::unit(2, "line")

    grid::grid.newpage()
    grid::pushViewport(grid::plotViewport(margins = c(0, 0, top_height, right_width), name = "ggplot"))
    grid::grid.draw(g)
    grid::upViewport(1)

    # draw x facet
    if (!is.null(xfacet_label)) {
      grid::pushViewport(grid::viewport(
        x = 0, y = grid::unit(1, "npc") - top_height, width = grid::unit(1, "npc"),
        height = top_height, just = c("left", "bottom"), name = "topxaxis"
      ))
      grid::grid.draw(xaxis_label_grob)
      grid::upViewport(1)
    }

    # draw y facet
    if (!is.null(yfacet_label)) {
      grid::pushViewport(grid::viewport(
        x = grid::unit(1, "npc") - grid::unit(as.numeric(right_width) / 2, "line"), y = 0, width = right_width,
        height = grid::unit(1, "npc"), just = c("left", "bottom"), name = "rightyaxis"
      ))
      grid::grid.draw(yaxis_label_grob)
      grid::upViewport(1)
    }
  })
}

#' Call a function with a character vector for the `...` argument
#'
#' @param fun (`character`) Name of a function where the `...` argument shall be replaced by values from `str_args`.
#' @param str_args (`character`) A character vector that the function shall be executed with
#'
#' @return
#' Value of call to `fun` with arguments specified in `str_args`.
#'
#' @keywords internal
call_fun_dots <- function(fun, str_args) {
  do.call("call", c(list(fun), lapply(str_args, as.name)), quote = TRUE)
}

#' Generate a string for a variable including its label
#'
#' @param var_names (`character`) Name of variable to extract labels from.
#' @param dataset (`dataset`) Name of analysis dataset.
#' @param prefix,suffix (`character`) String to paste to the beginning/end of the variable name with label.
#' @param wrap_width (`numeric`) Number of characters to wrap original label to. Defaults to 80.
#'
#' @return (`character`) String with variable name and label.
#'
#' @keywords internal
#'
varname_w_label <- function(var_names,
                            dataset,
                            wrap_width = 80,
                            prefix = NULL,
                            suffix = NULL) {
  add_label <- function(var_names) {
    label <- vapply(
      dataset[var_names], function(x) {
        attr_label <- attr(x, "label")
        `if`(is.null(attr_label), "", attr_label)
      },
      character(1)
    )

    if (length(label) == 1 && !is.na(label) && !identical(label, "")) {
      paste0(prefix, label, " [", var_names, "]", suffix)
    } else {
      var_names
    }
  }

  if (length(var_names) < 1) {
    NULL
  } else if (length(var_names) == 1) {
    stringr::str_wrap(add_label(var_names), width = wrap_width)
  } else if (length(var_names) > 1) {
    stringr::str_wrap(vapply(var_names, add_label, character(1)), width = wrap_width)
  }
}

# see vignette("ggplot2-specs", package="ggplot2")
shape_names <- c(
  "circle", paste("circle", c("open", "filled", "cross", "plus", "small")), "bullet",
  "square", paste("square", c("open", "filled", "cross", "plus", "triangle")),
  "diamond", paste("diamond", c("open", "filled", "plus")),
  "triangle", paste("triangle", c("open", "filled", "square")),
  paste("triangle down", c("open", "filled")),
  "plus", "cross", "asterisk"
)

#' Get icons to represent variable types in dataset
#'
#' @param var_type (`character`) of R internal types (classes).
#' @return (`character`) vector of HTML icons corresponding to data type in each column.
#' @keywords internal
variable_type_icons <- function(var_type) {
  checkmate::assert_character(var_type, any.missing = FALSE)

  class_to_icon <- list(
    numeric = "arrow-up-1-9",
    integer = "arrow-up-1-9",
    logical = "pause",
    Date = "calendar",
    POSIXct = "calendar",
    POSIXlt = "calendar",
    factor = "chart-bar",
    character = "keyboard",
    primary_key = "key",
    unknown = "circle-question"
  )
  class_to_icon <- lapply(class_to_icon, function(icon_name) toString(icon(icon_name, lib = "font-awesome")))

  unname(vapply(
    var_type,
    FUN.VALUE = character(1),
    FUN = function(class) {
      if (class == "") {
        class
      } else if (is.null(class_to_icon[[class]])) {
        class_to_icon[["unknown"]]
      } else {
        class_to_icon[[class]]
      }
    }
  ))
}

#' Include `CSS` files from `/inst/css/` package directory to application header
#'
#' `system.file` should not be used to access files in other packages, it does
#' not work with `devtools`. Therefore, we redefine this method in each package
#' as needed. Thus, we do not export this method
#'
#' @param pattern (`character`) optional, regular expression to match the file names to be included.
#'
#' @return HTML code that includes `CSS` files.
#' @keywords internal
#'
include_css_files <- function(pattern = "*") {
  css_files <- list.files(
    system.file("css", package = "teal.modules.general", mustWork = TRUE),
    pattern = pattern, full.names = TRUE
  )
  if (length(css_files) == 0) {
    return(NULL)
  }
  singleton(tags$head(lapply(css_files, includeCSS)))
}

#' JavaScript condition to check if a specific tab is active
#'
#' @param id (`character(1)`) the id of the tab panel with tabs.
#' @param name (`character(1)`) the name of the tab.
#' @return JavaScript expression to be used in `shiny::conditionalPanel()` to determine
#' if the specified tab is active.
#' @keywords internal
#'
is_tab_active_js <- function(id, name) {
  # supporting the bs3 and higher version at the same time
  sprintf(
    "$(\"#%1$s > li.active\").text().trim() == '%2$s' || $(\"#%1$s > li a.active\").text().trim() == '%2$s'",
    id, name
  )
}

#' Assert single selection on `data_extract_spec` object
#' Helper to reduce code in assertions
#' @noRd
#'
assert_single_selection <- function(x,
                                    .var.name = checkmate::vname(x)) { # nolint: object_name.
  if (any(vapply(x, function(.x) .x$select$multiple, logical(1)))) {
    stop("'", .var.name, "' should not allow multiple selection")
  }
  invisible(TRUE)
}

#' Wrappers around `srv_transform_teal_data` that allows to decorate the data
#' @inheritParams teal::srv_transform_teal_data
#' @param expr (`expression` or `reactive`) to evaluate on the output of the decoration.
#' When an expression it must be inline code. See [within()]
#' Default is `NULL` which won't evaluate any appending code.
#' @param expr_is_reactive (`logical(1)`) whether `expr` is a reactive expression
#' that skips defusing the argument.
#' @details
#' `srv_decorate_teal_data` is a wrapper around `srv_transform_teal_data` that
#' allows to decorate the data with additional expressions.
#' When original `teal_data` object is in error state, it will show that error
#' first.
#'
#' @keywords internal
srv_decorate_teal_data <- function(id, data, decorators, expr, expr_is_reactive = FALSE) {
  checkmate::assert_class(data, classes = "reactive")
  checkmate::assert_list(decorators, "teal_transform_module")
  checkmate::assert_flag(expr_is_reactive)

  missing_expr <- missing(expr)
  if (!missing_expr && !expr_is_reactive) {
    expr <- dplyr::enexpr(expr) # Using dplyr re-export to avoid adding rlang to Imports
  }

  moduleServer(id, function(input, output, session) {
    decorated_output <- srv_transform_teal_data("inner", data = data, transformators = decorators)

    reactive({
      data_out <- try(data(), silent = TRUE)
      if (inherits(data_out, "qenv.error")) {
        data()
      } else {
        # ensure original errors are displayed and `eval_code` is never executed with NULL
        req(data(), decorated_output())
        if (missing_expr) {
          decorated_output()
        } else if (expr_is_reactive) {
          teal.code::eval_code(decorated_output(), expr())
        } else {
          teal.code::eval_code(decorated_output(), expr)
        }
      }
    })
  })
}

#' @rdname srv_decorate_teal_data
#' @details
#' `ui_decorate_teal_data` is a wrapper around `ui_transform_teal_data`.
#' @keywords internal
ui_decorate_teal_data <- function(id, decorators, ...) {
  teal::ui_transform_teal_data(NS(id, "inner"), transformators = decorators, ...)
}

#' Internal function to check if decorators is a valid object
#' @noRd
check_decorators <- function(x, names = NULL, null.ok = FALSE) { # nolint: object_name.
  checkmate::qassert(null.ok, "B1")

  check_message <- checkmate::check_list(
    x,
    null.ok = null.ok,
    names = "named"
  )

  if (!is.null(names)) {
    check_message <- if (isTRUE(check_message)) {
      out_message <- checkmate::check_names(names(x), subset.of = c("default", names))
      # see https://github.com/insightsengineering/teal.logger/issues/101
      if (isTRUE(out_message)) {
        out_message
      } else {
        gsub("\\{", "(", gsub("\\}", ")", out_message))
      }
    } else {
      check_message
    }
  }

  if (!isTRUE(check_message)) {
    return(check_message)
  }

  valid_elements <- vapply(
    x,
    checkmate::test_list,
    types = "teal_transform_module",
    null.ok = TRUE,
    FUN.VALUE = logical(1L)
  )

  if (all(valid_elements)) {
    return(TRUE)
  }

  "May only contain the type 'teal_transform_module' or a named list of 'teal_transform_module'."
}

#' Internal assertion on decorators
#' @noRd
assert_decorators <- checkmate::makeAssertionFunction(check_decorators)

#' Subset decorators based on the scope
#'
#' `default` is a protected decorator name that is always included in the output,
#' if it exists
#'
#' @param scope (`character`) a character vector of decorator names to include.
#' @param decorators (named `list`) of list decorators to subset.
#'
#' @return A flat list with all decorators to include.
#' It can be an empty list if none of the scope exists in `decorators` argument.
#' @keywords internal
select_decorators <- function(decorators, scope) {
  checkmate::assert_character(scope, null.ok = TRUE)
  scope <- intersect(union("default", scope), names(decorators))
  c(list(), unlist(decorators[scope], recursive = FALSE))
}

#' Convert flat list of `teal_transform_module` to named lists
#'
#' @param decorators (list of `teal_transform_modules`) to normalize.
#' @return A named list of lists with `teal_transform_module` objects.
#' @keywords internal
normalize_decorators <- function(decorators) {
  if (checkmate::test_list(decorators, "teal_transform_module", null.ok = TRUE)) {
    if (checkmate::test_names(names(decorators))) {
      lapply(decorators, list)
    } else {
      list(default = decorators)
    }
  } else {
    decorators
  }
}
