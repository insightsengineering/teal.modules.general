#' Shared Parameters
#'
#' @description Contains arguments that are shared between multiple functions
#'   in the package to avoid repetition using \code{inheritParams}.
#'
#' @param plot_height optional, (\code{numeric}) A vector of length three with \code{c(value, min and max)}
#'   for a slider encoding the plot height.
#' @param plot_width optional, (\code{numeric}) A vector of length three with \code{c(value, min and max)}
#'   for a slider encoding the plot width.
#' @param rotate_xaxis_labels optional, (\code{logical}) Whether to rotate plot X axis labels. Does not
#'   rotate by default (\code{FALSE}).
#' @param ggtheme optional, (\code{character}) \code{ggplot} Theme to be used by default.
#'   One of \code{c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test")}.
#'   Each theme can be chosen by the user during the session. Defaults to \code{gray}.
#' @param ggplot2_args (`ggplot2_args`) object created by [teal.devel::ggplot2_args()]
#'  with settings for the module plot.
#'  For more details see the help vignette:
#'  `vignette("Custom ggplot2_args arguments module", package = "teal.devel")`
#'  The argument is merged with options variable `teal.ggplot2_args` and default module setup.
#' @param basic_table_args (`basic_table_args`) object created by [teal.devel::basic_table_args()]
#'  with settings for the module table.
#'  For more details see the help vignette:
#'  `vignette("Custom basic_table arguments module", package = "teal.devel")`
#'  The argument is merged with options variable `teal.basic_table_args` and default module setup.
#'
#' @name shared_params
NULL

#' Add axis labels that show facetting variable
#'
#' Add axis labels that show facetting variable
#'
#' @param p ggplot2 object to add facet labels to
#' @param xfacet_label label of facet along x axis (nothing created if NULL),
#'   if vector, will be concatenated with " & "
#' @param yfacet_label label of facet along y axis (nothing created if NULL),
#'   if vector, will be concatenated with " & "
#'
#' @return grid grob object (to be drawn with \code{grid.draw})
#'
#' @export
#'
#' @examples
#' # we put donttest to avoid strictr error with seq along.with argument
#' \donttest{
#' library(ggplot2)
#' library(grid)
#'
#' p <- ggplot(mtcars) +
#'   aes(x = mpg, y = disp) +
#'   geom_point() +
#'   facet_grid(gear ~ cyl)
#' p
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
#' }
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

#' Call a function with a character vector for the \code{...} argument
#'
#' @param fun (\code{character}) Name of a function where the \code{...} argument
#'   shall be replaced by values from \code{str_args}.
#' @param str_args (\code{character}) A character vector that the function shall
#'  be executed with
#'
#' @return: call (i.e. expression) of the function provided by \code{fun}
#'  with arguments provided by \code{str_args}.
#'
#' @examples
#' \dontrun{
#' a <- 1
#' b <- 2
#' call_fun_dots("sum", c("a", "b"))
#' eval(call_fun_dots("sum", c("a", "b")))
#' }
call_fun_dots <- function(fun, str_args) {
  do.call("call", c(list(fun), lapply(str_args, as.name)), quote = TRUE)
}

#' Get variable name with label
#'
#' @param var_names (\code{character}) Name of variable to extract labels from.
#' @param dataset (\code{dataset}) Name of analysis dataset.
#' @param prefix (\code{character}) String to paste to the beginning of the
#'   variable name with label.
#' @param suffix (\code{character}) String to paste to the end of the variable
#'   name with label.
#' @param wrap_width (\code{numeric}) Number of characters to wrap original
#'   label to. Defaults to 80.
#'
#' @return (\code{character}) String with variable name and label.
#'
#' @examples
#' \dontrun{
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' varname_w_label("AGE", ADSL)
#' }
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

#' Extract html id for data_extract_ui
#' @description The data_extract_ui is located under extended html id.
#'   We could not use \code{ns("original id")} for reference, as it is extended with specific suffixes.
#' @param varname character original html id.
#'   This will be mostly retrieved with \code{ns("original id")} in ui or
#'   \code{session$ns("original id")} in server function.
#' @param dataname character \code{dataname} from data_extract input.
#'   This might be retrieved like \code{data_extract_spec(...)[[1]]$dataname}.
#' @param filter logical if the connected \code{extract_data_spec} is used with \code{filter} option.
extract_input <- function(varname, dataname, filter = FALSE) {
  if (filter) {
    paste0(varname, "-dataset_", dataname, "_singleextract-filter1-vals")
  } else {
    paste0(varname, "-dataset_", dataname, "_singleextract-select")
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
