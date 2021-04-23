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

# see vignette("ggplot2-specs", package="ggplot2")
shape_names <- c(
  "circle", paste("circle", c("open", "filled", "cross", "plus", "small")), "bullet",
  "square", paste("square", c("open", "filled", "cross", "plus", "triangle")),
  "diamond", paste("diamond", c("open", "filled", "plus")),
  "triangle", paste("triangle", c("open", "filled", "square")),
  paste("triangle down", c("open", "filled")),
  "plus", "cross", "asterisk"
)

#' returns a data frame that stores the trend line statistics of a polynomial line fitted to scatterplot
#'
#' @param data_df (\code{data.frame}) the data where the x-axis and y-axis is host
#' @param x_var (\code{character}) the name of the column in `data_df` to be used as the x-axis
#' @param y_var (\code{character}) the name of the column in `data_df` to be used as the y-axis
#' @param smoothing_degree (\code{integer}) the degree of the polynomial line to be fitted
#' @param group_by_var optional, (\code{character}) the name of the column in `data_df` that is used for grouping.
#'  This parameter is only used for determining whether a row in the output will have multiple groups or only one group.
#' @param facet_by_var optional, (\code{character}) a vector of the name(s) of the column(s) in `data_df`
#'  to be used for filtering. Each row of the output will consists of trend line statistics for a single combination of
#'  all the possible combinations of the matrix created by all of the unique values of columns in this parameter.
#'
#' @examples
#' \dontrun{
#' trend_line_stats(mpg, "displ", "hwy", 2)
#' trend_line_stats(mpg, "displ", "hwy", 2, group_by_var = "class", facet_by_var = "manufacturer")
#' }
#' @export
#' @importFrom purrr pmap_dfr
trend_line_stats <- function(data_df,
                             x_var,
                             y_var,
                             smoothing_degree,
                             group_by_var = NULL,
                             facet_by_var = NULL) {

  stopifnot(inherits(data_df, "data.frame"))
  stopifnot(is_character_single(x_var) && is_character_single(y_var))
  stopifnot(all(c(x_var, y_var) %in% names(data_df)))
  stopifnot(is.numeric(data_df[[x_var]]) && is.numeric(data_df[[y_var]]))
  stopifnot(is_numeric_single(smoothing_degree) && trunc(smoothing_degree) == smoothing_degree)
  stopifnot(is.null(group_by_var) || (is_character_single(group_by_var) && group_by_var %in% names(data_df)))
  if (!is_empty(facet_by_var)) {
    stopifnot(is_character_vector(facet_by_var))
    stopifnot(length(unique(facet_by_var)) == length(facet_by_var))
    stopifnot(all(facet_by_var %in% names(data_df)))
    for (var in facet_by_var) {
      if (!any(class(data_df[[var]]) %in% c("character", "factor", "Date", "integer"))) {
        stop("facet_by_var columns may only be of these types c('character', 'factor', 'Date', 'integer')")
      }
    }
  }

  trend_line_stats_one_row <- function(facet_by_var = NULL, ...) {
    if (!is_empty(facet_by_var)) {
      filter_by_val <- tibble::tibble(...)
      for (i in seq_along(facet_by_var)) {
        data_df <- data_df %>% dplyr::filter(.data[[facet_by_var[i]]] %in% filter_by_val[[i]])
      }
    }
    data_df <- data_df %>% dplyr::select(x_var, y_var, group_by_var)
    ANL_sub_no_na <- na.omit(data_df) # nolint
    model <- try(
      lm(ANL_sub_no_na[[y_var]] ~ poly(ANL_sub_no_na[[x_var]], smoothing_degree), ANL_sub_no_na),
      silent = TRUE)
    output <- if (!inherits(model, "try-error")) {
      r_2 <- paste(
        "adj-R^2:",
        ifelse(
          is.nan(summary(model)$adj.r.squared),
          "not enough points",
          round(summary(model)$adj.r.squared, 8)
        )
      )
      form <- sprintf(
        "%s = %#.4f %s %#.4f * %s%s",
        y_var,
        coef(model)[1],
        ifelse(coef(model)[2] < 0, "-", "+"),
        abs(coef(model)[2]),
        x_var,
        paste(
          vapply(
            X = seq_len(smoothing_degree)[-1],
            FUN = function(deg) {
              sprintf(
                " %s %#.4f*%s^%s",
                ifelse(coef(model)[deg + 1] < 0, "-", "+"),
                abs(coef(model)[deg + 1]),
                x_var,
                deg
              )
            },
            FUN.VALUE = character(1)),
          collapse = ""
        )
      )
      data.frame(
        form = form,
        r_2 = r_2,
        msg = ifelse(
          !is.null(group_by_var) && length(unique(ANL_sub_no_na[[group_by_var]])) > 1,
          "note: stats are from combined selected color groups",
          NA_character_),
        warn_na = ifelse(
          nrow(data_df) - nrow(ANL_sub_no_na) > 0,
          paste(nrow(data_df) - nrow(ANL_sub_no_na), "row(s) with NA removed"),
          NA_character_),
        failed_fit_msg = NA_character_,
        stringsAsFactors = FALSE)
    } else {
      data.frame(
        form = NA_character_,
        r_2 = NA_character_,
        msg = NA_character_,
        warn_na = NA_character_,
        failed_fit_msg = paste("not enough unique x values to fit line with degree", smoothing_degree),
        stringsAsFactors = FALSE
      )
    }
    if (!is_empty(facet_by_var)) {
      for (i in seq_along(facet_by_var)) {
        output[[facet_by_var[[i]]]] <- filter_by_val[[i]]
      }
    }
    output
  }
  if (!is_empty(facet_by_var)) {
    col_combination <- lapply(facet_by_var, function(col_name) unique(data_df[[col_name]]))
    names(col_combination) <- facet_by_var
    combination_matrix <- expand.grid(col_combination)
    purrr::pmap_dfr(
      combination_matrix,
      function(...) {
        trend_line_stats_one_row(facet_by_var = names(combination_matrix), ...)
      }
    )
  } else {
    trend_line_stats_one_row()
  }
}
