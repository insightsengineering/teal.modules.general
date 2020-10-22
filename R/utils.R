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

#' Stack Multiple Grobs
#'
#' Stack grobs as a new grob with 1 column and multiple rows layout.
#'
#' @param ... grobs.
#' @param grobs list of grobs.
#' @param padding unit of length 1, space between each grob.
#' @param vp a \code{\link{viewport}} object (or \code{NULL}).
#' @param name a character identifier for the grob.
#' @param gp A \code{\link{gpar}} object.
#'
#' @export
#'
#' @examples
#' library(grid)
#' g1 <- circleGrob(gp = gpar(col = "blue"))
#' g2 <- circleGrob(gp = gpar(col = "red"))
#' g3 <- textGrob("TEST TEXT")
#' grid.newpage()
#' grid.draw(stack_grobs(g1, g2, g3))
#'
#' showViewport()
#'
#' grid.newpage()
#' pushViewport(viewport(layout = grid.layout(1,2)))
#' vp1 <- viewport(layout.pos.row = 1, layout.pos.col = 2)
#' grid.draw(stack_grobs(g1, g2, g3, vp = vp1, name = "test"))
#'
#' showViewport()
#' grid.ls(grobs = TRUE, viewports = TRUE)
#'
#' @importFrom grid is.grob viewport grid.layout gList gTree unit.c
stack_grobs <- function(...,
                        grobs = list(...),
                        padding = unit(2, "line"),
                        vp = NULL,
                        gp = NULL,
                        name = NULL) {
  stopifnot(all(vapply(grobs, is.grob, logical(1))))

  if (length(grobs) == 1) {
    return(grobs[[1]])
  }

  n_layout <- 2 * length(grobs) - 1
  hts <- lapply(
    seq(1, n_layout),
    function(i) {
      if (i %% 2 != 0) {
        unit(1, "null")
      } else {
        padding
      }
    }
  )
  hts <- do.call("unit.c", hts)

  main_vp <- viewport(
    layout = grid.layout(nrow = n_layout, ncol = 1, heights = hts)
  )

  nested_grobs <- Map(function(g, i) {
    gTree(
      children = gList(g),
      vp = viewport(layout.pos.row = i, layout.pos.col = 1)
    )
  }, grobs, seq_along(grobs) * 2 - 1)

  grobs_mainvp <-   gTree(
    children = do.call("gList", nested_grobs),
    vp = main_vp
  )

  gTree(
    children = gList(grobs_mainvp),
    vp = vp,
    gp = gp,
    name = name
  )
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
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
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


gg_themes <- c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test")
