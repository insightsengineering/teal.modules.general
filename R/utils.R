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

list_or_null <- function(obj) {
  if (is.null(obj)) {
    NULL
  } else {
    list(obj)
  }
}

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
#' @importFrom ggplot2 ggplotGrob
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
        x = unit(1, "npc") - right_width, y = 0, width = right_width,
        height = unit(1, "npc"), just = c("left", "bottom"), name = "rightyaxis"
      ))
      grid.draw(yaxis_label_grob)
      upViewport(1)
    }
  })
}

#' Create simple HTML table
#'
#' Turns data.frame into simple non-styled HTML table
#' @param x two dimensional object (data.frame, matrix, array)
#' @param colnames \code{logical} whether colnames should be displayed
#' @param rownames \code{logical} whether rownames should be displayed
#' @return \code{shiny.tag} table object
#' @examples
#' teal.modules.general:::to_html_table(iris[1:5, ], rownames = FALSE)
to_html_table <- function(x, colnames = TRUE, rownames = TRUE) {

  tags$div(
    tags$table(
      if (colnames && !is.null(colnames(x))) {
        tags$thead(
          tags$tr(
            lapply(colnames(x), tags$th)
          )
        )
      },
      tags$tbody(
        lapply(seq_len(nrow(x)), function(i) {
          tags$tr(
            if (rownames && !is.null(rownames(x))) tags$td(rownames(x)[i], style = "font-weight: bold;"),
            lapply(x[i, ], tags$td)
          )
        })
      ))
  )
}
