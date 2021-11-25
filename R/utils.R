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
#'   \code{gg_themes} is defined internally as
#'   \code{c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test")}
#'   All themes can be chosen by the user. Defaults to \code{gray}.
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
#' aes(x = mpg, y = disp) +
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
#' @importFrom grid grid.newpage grid.draw pushViewport upViewport plotViewport viewport grid.grabExpr
add_facet_labels <- function(p, xfacet_label = NULL, yfacet_label = NULL) {
  stopifnot(
    is.null(xfacet_label) || is_character_vector(xfacet_label, min_length = 1),
    is.null(yfacet_label) || is_character_vector(yfacet_label, min_length = 1),
    is(p, "ggplot")
  )
  if (is.null(xfacet_label) && is.null(yfacet_label)) {
    return(ggplotGrob(p))
  }
  grid.grabExpr({
    g <- ggplotGrob(p)

    # we are going to replace these, so we make sure they have nothing in them
    stopifnot(is(g$grobs[[grep("xlab-t", g$layout$name, fixed = TRUE)]], "zeroGrob"))
    stopifnot(is(g$grobs[[grep("ylab-r", g$layout$name, fixed = TRUE)]], "zeroGrob"))

    xaxis_label_grob <- g$grobs[[grep("xlab-b", g$layout$name, fixed = TRUE)]]
    xaxis_label_grob$children[[1]]$label <- paste(xfacet_label, collapse = " & ")
    yaxis_label_grob <- g$grobs[[grep("ylab-l", g$layout$name, fixed = TRUE)]]
    yaxis_label_grob$children[[1]]$label <- paste(yfacet_label, collapse = " & ")
    yaxis_label_grob$children[[1]]$rot <- 270

    top_height <- if (is.null(xfacet_label)) 0 else unit(2, "line")
    right_width <- if (is.null(yfacet_label)) 0 else unit(2, "line")

    grid.newpage()
    pushViewport(plotViewport(margins = c(0, 0, top_height, right_width), name = "ggplot"))
    grid.draw(g)
    upViewport(1)

    # draw x facet
    if (!is.null(xfacet_label)) {
      pushViewport(viewport(
        x = 0, y = unit(1, "npc") - top_height, width = unit(1, "npc"),
        height = top_height, just = c("left", "bottom"), name = "topxaxis"
      ))
      grid.draw(xaxis_label_grob)
      upViewport(1)
    }

    # draw y facet
    if (!is.null(yfacet_label)) {
      pushViewport(viewport(
        x = unit(1, "npc") - unit(as.numeric(right_width) / 2, "line"), y = 0, width = right_width,
        height = unit(1, "npc"), just = c("left", "bottom"), name = "rightyaxis"
      ))
      grid.draw(yaxis_label_grob)
      upViewport(1)
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

#' Returns NULL or an argument coerced to a list
#'
#' @param obj object to be tested for NULL
#'
#' @return: NULL if obj is NULL, list(obj) otherwise
#'
#' @examples
#' \dontrun{
#' a <- NULL
#' b <- c(1, 2)
#' list_or_null(a) # returns NULL
#' l <- list_or_null(b) # return list(b)
#' print(l)
#' }
list_or_null <- function(obj) {
  if (is.null(obj)) {
    NULL
  } else {
    list(obj)
  }
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
#' @importFrom stringr str_wrap
varname_w_label <- function(var_names,
                            dataset,
                            wrap_width = 80,
                            prefix = NULL,
                            suffix = NULL) {

  add_label <- function(var_names) {

    label <- vapply(dataset[var_names], function(x) if_null(attr(x, "label"), ""), character(1))

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

#' Extract html id for data_extract_input
#' @description The data_extract_input is located under extended html id.
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

gg_themes <- c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test")

# see vignette("ggplot2-specs", package="ggplot2")
shape_names <- c(
  "circle", paste("circle", c("open", "filled", "cross", "plus", "small")), "bullet",
  "square", paste("square", c("open", "filled", "cross", "plus", "triangle")),
  "diamond", paste("diamond", c("open", "filled", "plus")),
  "triangle", paste("triangle", c("open", "filled", "square")),
  paste("triangle down", c("open", "filled")),
  "plus", "cross", "asterisk"
)

#' Transform \code{ggplot2_args} into \code{ggplot2} expression
#'
#' @description internal function to aggregate and reduce the \code{ggplot2_args}.
#' The \code{ggplot2_args} argument is part of every module which contains any \code{ggplot2} graphics.
#'
#' @param ggplot2_args (`list`) of the class \code{ggplot_args} or a list of list of the class \code{ggplot_args}.
#' @param plot_name (`character`) name of the plot. This is used when working with multi-plot modules.
#' @param chunk_plot_name (`symbol`) name of the main plot to which we will be adding labs and theme, chunks.
#' @param nest_ggplot2_args (`list`) of the class \code{ggplot_args} with nest default setup for theme and labs.
#' @param ggtheme (`character`) name of the \code{ggplot2} to be used, e.g. `"dark"`.
#'
#' @return (`expression`) the chunk_plot_name expanded with non empty labs/theme.
#' @export
get_expr_ggplot2_args <- function(ggplot2_args,
                                  plot_name = "default",
                                  chunk_plot_name = as.name("gg"),
                                  nest_ggplot2_args = NULL,
                                  ggtheme = NULL) {

  stop_if_not(is.null(nest_ggplot2_args) || inherits(nest_ggplot2_args, "ggplot_args"),
              is.name(chunk_plot_name),
              is.character(plot_name))

  is_ggplot_args <- inherits(ggplot2_args, "ggplot_args")

  ggplot2_args_f <- list()

  if (is_ggplot_args) {
    labs_args <- c(ggplot2_args$labs, nest_ggplot2_args$labs)
    labs_args <- if (is.null(labs_args)) NULL else labs_args[!duplicated(names(labs_args))]
  } else {
    # the order is important, as specific per plot labs have a priority
    labs_args <- c(ggplot2_args[[plot_name]]$labs, ggplot2_args[["default"]]$labs, nest_ggplot2_args$labs)
    labs_args <- if (is.null(labs_args)) NULL else labs_args[!duplicated(names(labs_args))]
  }

  if (is_ggplot_args) {
    theme_args <- c(ggplot2_args$theme, nest_ggplot2_args$theme)
    theme_args <- if (is.null(theme_args)) NULL else theme_args[!duplicated(names(theme_args))]
  } else {
    # the order is important, as specific per plot theme have a priority
    theme_args <- c(ggplot2_args[[plot_name]]$theme, ggplot2_args[["default"]]$theme, nest_ggplot2_args$theme)
    theme_args <- if (is.null(theme_args)) NULL else theme_args[!duplicated(names(theme_args))]
  }

  ggplot2_args_f <- list(labs = labs_args, theme = theme_args)

  labs_theme_expr <- chunk_plot_name

  if (length(labs_args) != 0) {
    labs_f <- as.call(c(list(quote(labs)), ggplot2_args_f$labs))
    labs_theme_expr <- bquote(.(labs_theme_expr) + .(labs_f))
  }

  if (length(ggtheme) != 0) {
    default_theme <- call(paste0("theme_", ggtheme))
    labs_theme_expr <- bquote(.(labs_theme_expr) + .(default_theme))
  }

  if (length(theme_args) != 0) {
    theme_f <- as.call(c(list(quote(theme)), ggplot2_args_f$theme))
    labs_theme_expr <- bquote(.(labs_theme_expr) + .(theme_f))
  }

  labs_theme_expr
}

#' Building a list of the class \code{ggplot_args}
#'
#' @description This function has to be used to build an input for a \code{ggplot2_args} argument.
#' The \code{ggplot2_args} argument is part of every module which contains any \code{ggplot2} graphics.
#' The input is validated to match its \code{ggplot2} equivalent.
#'
#' @param labs (named `list`) where all fields have to match \code{ggplot2::labs} arguments.
#' @param theme (named `list`) where all fields have to match \code{ggplot2::theme} arguments.
#'
#' @return (`list`) with the class \code{ggplot_args}.
#'
#' @export
ggplot_args <- function(labs = list(), theme = list()) {

  stop_if_not(
    list(is.list(labs), "labs has to be a list"),
    list(is.list(theme), "theme has to be a list"),
    list(!anyDuplicated(names(labs)), "labs argument has to have unique fields"),
    list(!anyDuplicated(names(theme)), "theme argument has to have unique fields")
  )

  ggplot2_theme <- formalArgs(ggplot2::theme)
  ggplot2_labs <- c(getFromNamespace(".all_aesthetics", "ggplot2"),
                    formalArgs(ggplot2::labs))

  stop_if_not(
    list((length(theme) == 0) || all(names(theme) %in% ggplot2_theme), "Please validate theme arguments names"),
    list((length(labs) == 0) || all(names(labs) %in% ggplot2_labs), "Please validate labs arguments names")
  )

  structure(list(labs = labs, theme = theme), class = "ggplot_args")
}

basic_table_args <- function(...) {

  table_args <- list(...)

  stop_if_not(
    list(is.list(table_args), "table_args has to be a list"),
    list(!anyDuplicated(names(table_args)), "table_args argument has to have unique fields")
  )

  basic_table_formals <- formalArgs(rtables::basic_table)

  stop_if_not(
    list((length(table_args) == 0) || all(names(table_args) %in% basic_table_formals),
         "Please validate table_args arguments names")
  )

  structure(table_args, class = "basic_table_args")
}

#' Additional validation for \code{ggplot2_args} argument
validate_ggplot2_args <- function(ggplot2_args, plot_names = NULL) {
  is_ggplot2_args <- inherits(ggplot2_args, "ggplot_args")
  is_nested_ggplot2_args <- is.list(ggplot2_args) && !is_ggplot2_args &&
    all(vapply(ggplot2_args, function(x) inherits(x, "ggplot_args"), logical(1)))

  stop_if_not(
    list(is_ggplot2_args || (is_nested_ggplot2_args && (all(names(ggplot2_args) %in% c("default", plot_names)))),
         paste0("Please use the ggplot2_args() function to generate input for ggplot2_args argument.\n",
                "ggplot2_args argument has to be ggplot_args class or named list of such objects.\n",
                "If it is a named list then each name has to be one of ",
                paste(c("default", plot_names), collapse = ", ")))
  )
}

#' Additional validation for \code{basic_table_args} argument
validate_basic_table_args <- function(basic_table_args, table_names = NULL) {
  is_basic_table_args <- inherits(basic_table_args, "basic_table_args")
  is_nested_basic_table_args <- is.list(basic_table_args) && !is_basic_table_args &&
    all(vapply(basic_table_args, function(x) inherits(x, "basic_table_args"), logical(1)))

  stop_if_not(
    list(is_basic_table_args || (is_nested_basic_table_args &&
                                   (all(names(basic_table_args) %in% c("default", plot_names)))),
         paste0("Please use the basic_table_args() function to generate input for basic_table_args argument.\n",
                "basic_table_args argument has to be basic_table_args class or named list of such objects.\n",
                "If it is a named list then each name has to be one of ",
                paste(c("default", table_names), collapse = ", ")))
  )
}

#' Transform \code{basic_table_args} into \code{basic_table} expression
#' @description function
#' @export
get_expr_table_args <- function(basic_table_args,
                                table_name = "default",
                                chunk_table_name = as.name("tt"),
                                nest_table_args = NULL) {
  stop_if_not(is.null(nest_ggplot2_args) || inherits(nest_ggplot2_args, "ggplot_args"),
              is.name(chunk_plot_name),
              is.character(plot_name))

  is_ggplot_args <- inherits(ggplot2_args, "ggplot_args")

  ggplot2_args_f <- list()

  if (is_ggplot_args) {
    labs_args <- c(ggplot2_args$labs, nest_ggplot2_args$labs)
    labs_args <- if (is.null(labs_args)) NULL else labs_args[!duplicated(names(labs_args))]
  } else {
    # the order is important, as specific per plot labs have a priority
    labs_args <- c(ggplot2_args[[plot_name]]$labs, ggplot2_args[["default"]]$labs, nest_ggplot2_args$labs)
    labs_args <- if (is.null(labs_args)) NULL else labs_args[!duplicated(names(labs_args))]
  }

  labs_theme_expr <- chunk_plot_name

  if (length(labs_args) != 0) {
    labs_f <- as.call(c(list(quote(labs)), ggplot2_args_f$labs))
    labs_theme_expr <- bquote(.(labs_theme_expr) + .(labs_f))
  }

  labs_theme_expr
}
