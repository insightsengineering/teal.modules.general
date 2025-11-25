#' Mosaic Rectangles Layer for ggplot2
#'
#' Adds a mosaic-style rectangles layer to a ggplot, visualizing the joint distribution of categorical variables.
#' Each rectangle's size reflects the proportion of observations for combinations of `x` and `fill`.
#'
#' @param mapping Set of aesthetic mappings created by `aes()`. Must specify `x` and `fill`.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data. Defaults to `"rects"`.
#' @param position Position adjustment. Defaults to `"identity"`.
#' @param ... Other arguments passed to `layer()`.
#' @param na.rm Logical. Should missing values be removed?
#' @param show.legend Logical. Should this layer be included in the legends?
#' @param inherit.aes Logical. If `FALSE`, overrides default aesthetics.
#'
#' @return A ggplot2 layer that adds mosaic rectangles to the plot.
#'
#' @examples
#' df <- data.frame(RACE = c("Black", "White", "Black", "Asian"), SEX = c("M", "M", "F", "F"))
#' library(ggplot2)
#' ggplot(df, aes(x = RACE, fill = SEX)) + geom_rects()
#' @export
geom_mosaic <- function(mapping = NULL, data = NULL,
                       stat = "mosaic", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = TRUE,
                       inherit.aes = TRUE) {
  aes_x <- list(rlang::quo_get_expr(mapping$x))
  var_x <- sprintf("x__%s", as.character(aes_x))
  aes_fill <- rlang::quo_text(mapping$fill)
  var_fill <- sprintf("x__fill__%s", aes_fill)

  mapping[[var_x]] <- mapping$x
  mapping$x <- structure(1L, class = "mosaic")
  # mapping[[var_fill]] <- mapping$fill

  layer <- ggplot2::layer(
    geom = GeomMosaic,
    stat = "mosaic",
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = FALSE,
    params = list(na.rm = na.rm, ...)
  )
  #list(layer, .scale_x_mosaic())
  layer
}

GeomMosaic <- ggplot2::ggproto(
  "GeomMosaic", ggplot2::GeomRect,
  default_aes = ggplot2::aes(
    colour = NA, linewidth = 0.5, linetype = 1, alpha = 1, fill = "grey30"
  ),
  draw_panel = function(data, panel_params, coord) {
    if (all(is.na(data$colour))) data$colour <- scales::alpha(data$fill, data$alpha)
    ggplot2::GeomRect$draw_panel(data, panel_params, coord)
  },
  required_aes = c("xmin", "xmax", "ymin", "ymax")
)

#' Calculate Rectangle Coordinates for Mosaic Plot
#'
#' Computes the coordinates for rectangles in a mosaic plot based on combinations of `x` and `fill` variables.
#' For each unique `x` and `fill`, calculates the proportional widths and heights, stacking rectangles within each `x` group.
#'
#' @param data A data frame containing at least `x` and `fill` columns.
#'
#' @return A data frame with columns: `x`, `fill`, `xmin`, `xmax`, `ymin`, `ymax`, representing the position and size of each rectangle.
#'
#' @details
#' - Counts occurrences of each `x`/`fill` combination.
#' - Calculates proportions within each `x` group.
#' - Determines horizontal (`xmin`, `xmax`) and vertical (`ymin`, `ymax`) boundaries for each rectangle.
#' - Adds small padding to each boundary for visual separation.
.calculate_coordinates <- function(data) {
  # Example: compute rectangles from x and y
  result <- data |>
    # Count combinations of X and Y
    dplyr::count(x, fill) |>
    # Compute total for each X group
    dplyr::mutate(
      .by = x,
      x_total = sum(n),
      prop = n / x_total
    ) |>
    # Change order from biggest group to smaller
    dplyr::arrange(dplyr::desc(x_total), x, fill) |>
    # Compute total sample size to turn counts into widths
    dplyr::mutate(
      N_total = dplyr::n(),
      x_width = x_total / N_total
    ) |>
    # Convert counts to x widths
    dplyr::mutate(
      .by = x,
      x_width_last = dplyr::if_else(dplyr::row_number() == dplyr::n(), x_width, 0)
    ) |>
    # Compute x-min/x-max for each group
    dplyr::mutate(
      xmin = cumsum(dplyr::lag(x_width_last, default = 0)),
      xmax = xmin + x_width
    ) |>
    # Compute y-min/y-max for stacked proportions
    dplyr::mutate(
      .by = x,
      ymin = c(0, head(cumsum(prop), -1)),
      ymax = cumsum(prop)
    ) |>
    dplyr::mutate(
      xmin = xmin / max(xmax),
      xmax = xmax / max(xmax),
      xmin = xmin + 0.005,
      xmax = xmax - 0.005,
      ymin = ymin + 0.005,
      ymax = ymax - 0.005
    ) |>
    dplyr::select(x, fill, xmin, xmax, ymin, ymax)
  result
}

StatMosaic <- ggplot2::ggproto(
  "StatMosaic", ggplot2::Stat,

  required_aes = c("x", "fill"),

  compute_group = function(data, scales) {
    data
  },
  compute_panel = function(data, scales) {
    data$x <- data[, grepl("x__", colnames(data))]
    result <- .calculate_coordinates(data)

    scale_x <- ggplot2::ScaleContinuous
    scale_x[["breaks"]] <- result |>
      dplyr::distinct(x, xmin, xmax) |>
      dplyr::mutate(mid = (xmin + xmax) / 2) |>
      dplyr::pull(mid)

    scale_x[["labels"]] <- result |>
      dplyr::distinct(x) |>
      dplyr::pull(x)

    result$x <- list(scale = scale_x)
    result$group <- 1
    result$PANEL <- unique(data$PANEL)
    result
  }
)

#' Helper function for determining scales
#'
#' Used internally to determine class of variable x
#' @param x variable
#' @return character string "productlist"
#' @importFrom ggplot2 scale_type
#' @export
scale_type.mosaic <- function(x) {
  #  cat("checking for type productlist\n")
  #browser()
  "mosaic"
}


#' Determining scales for mosaics
#'
#' @param name set to pseudo waiver function `product_names` by default.
#' @param ... other arguments passed to `continuous_scale()`.
#' @inheritParams ggplot2::continuous_scale
#' @export
scale_x_mosaic <- function(breaks = function() function(x) unique(x),
                           minor_breaks = NULL,
                           labels = function() function(x) unique(x),
                           na.value = NA_real_,
                           position = "bottom",
                           ...) {
  ggplot2::continuous_scale(
    aesthetics = c(
      "x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final",
      "xlower", "xmiddle", "xupper"
    ), palette = identity, breaks = breaks, minor_breaks = minor_breaks,
    labels = labels, na.value = na.value, position = position,
    super = ScaleContinuousProduct,, guide = ggplot2::waiver(), ...
  )
}

#' @rdname dot-scale_x_mosaic
ScaleContinuousProduct <- ggplot2::ggproto(
  "ScaleContinuousProduct", ggplot2::ScaleContinuousPosition,
  train =function(self, x) {
    if (is.list(x)) {
      x <- x[[1]]
      if ("Scale" %in% class(x)) {
        # re-assign the scale values now that we have the information - but only if necessary
        if (is.function(self$breaks)) self$breaks <- x$breaks
        if (is.function(self$labels)) self$labels <- x$labels
        return(NULL)
      }
    }
    if (self$is.discrete(x)) {
      self$range$train(x=c(0,1))
      return(NULL)
    }
    self$range$train(x)
  },
  map = function(self, x, limits = self$get_limits()) {
    if (self$is.discrete(x)) return(x)
    if (is.list(x)) return(0) # need a number
    scaled <- as.numeric(self$oob(x, limits))
    ifelse(!is.na(scaled), scaled, self$na.value)
  },
  dimension = function(self, expand = c(0, 0)) {
    c(-0.05,1.05)
  },
  make_title = function(..., self) {
    title <- ggplot2::ggproto_parent(ggplot2::ScaleContinuousPosition, self)$make_title(...)
    if (isTRUE(title %in% self$aesthetics)) {
      title <- self$product_name
    }
    else title
  },
  is.discrete = function(self, x) is.factor(x) || is.character(x) || is.logical(x)
)


