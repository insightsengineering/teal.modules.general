# minimal implementation of ggplot2 mosaic after ggmosaic was archived in CRAN
#
# This was heavily inspired by github.com/haleyjeppson/ggmosaic package but
# simplified to only support 2 categorical variables

#' Mosaic Rectangles Layer for ggplot2
#'
#' Adds a mosaic-style rectangles layer to a ggplot, visualizing the
#' joint distribution of categorical variables.
#' Each rectangle's size reflects the proportion of observations for
#' combinations of `x` and `fill`.
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
#' ggplot(df) +
#'   geom_mosaic(aes(x = RACE, fill = SEX))
#' @export
geom_mosaic <- function(mapping = NULL, data = NULL,
                        stat = "mosaic", position = "identity",
                        ...,
                        na.rm = FALSE, # nolint: object_name_linter.
                        show.legend = TRUE, # nolint: object_name_linter.
                        inherit.aes = TRUE) { # nolint: object_name_linter.

  aes_x <- mapping$x
  if (!is.null(aes_x)) {
    aes_x <- list(rlang::quo_get_expr(mapping$x))
    var_x <- paste0("x__", as.character(aes_x))
    mapping[[var_x]] <- mapping$x
  }

  aes_fill <- mapping$fill
  if (!is.null(aes_fill)) {
    aes_fill <- rlang::quo_text(mapping$fill)
  }

  mapping$x <- structure(1L)

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
  list(layer, .scale_x_mosaic())
}

#' @keywords internal
GeomMosaic <- ggplot2::ggproto( # nolint: object_name_linter.
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

#' @keywords internal
StatMosaic <- ggplot2::ggproto( # nolint: object_name_linter.
  "StatMosaic", ggplot2::Stat,
  required_aes = c("x", "fill"),
  compute_group = function(data, scales) data,
  compute_panel = function(data, scales) {
    data$x <- data[, grepl("x__", colnames(data))]
    result <- .calculate_coordinates(data)

    breaks <- breaks <- unique(with(result, (xmin + xmax) / 2))
    labels <- unique(result$x)
    result$x <- list(list2env(list(breaks = breaks[breaks != 0], labels = labels[breaks != 0])))

    result$group <- 1
    result$PANEL <- unique(data$PANEL)
    result
  }
)

#' Determining scales for mosaics
#'
#' @param name set to pseudo waiver function `product_names` by default.
#' @param ... other arguments passed to `continuous_scale()`.
#' @inheritParams ggplot2::continuous_scale
#' @keywords internal
.scale_x_mosaic <- function(breaks = function(x) unique(x),
                            minor_breaks = NULL,
                            labels = function(x) unique(x),
                            na.value = NA_real_, # nolint: object_name_linter.
                            position = "bottom",
                            ...) {
  ggplot2::continuous_scale(
    aesthetics = c(
      "x", "xmin", "xmax", "xend", "xintercept", "xmin_final", "xmax_final",
      "xlower", "xmiddle", "xupper"
    ),
    palette = identity,
    breaks = breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    na.value = na.value,
    position = position,
    super = ScaleContinuousMosaic, ,
    guide = ggplot2::waiver(),
    ...
  )
}

#' @keywords internal
ScaleContinuousMosaic <- ggplot2::ggproto( # nolint: object_name_linter.
  "ScaleContinuousMosaic", ggplot2::ScaleContinuousPosition,
  train = function(self, x) {
    if (length(x) == 0) {
      return()
    }
    if (is.list(x)) {
      scale_x <- x[[1]]
      # re-assign the scale values now that we have the information - but only if necessary
      if (is.function(self$breaks)) self$breaks <- scale_x$breaks
      if (is.function(self$labels)) self$labels <- as.vector(scale_x$labels)
      return(NULL)
    }
    if (is_discrete(x)) {
      self$range$train(x = c(0, 1))
      return(NULL)
    }
    self$range$train(x, call = self$call)
  },
  map = function(self, x, limits = self$get_limits()) {
    if (is_discrete(x)) {
      return(x)
    }
    if (is.list(x)) {
      return(0)
    } # need a number
    scaled <- as.numeric(self$oob(x, limits))
    ifelse(!is.na(scaled), scaled, self$na.value)
  },
  dimension = function(self, expand = c(0, 0)) c(-0.05, 1.05)
)

#' @noRd
is_discrete <- function(x) is.factor(x) || is.character(x) || is.logical(x)

#' @describeIn geom_mosaic
#' Computes the coordinates for rectangles in a mosaic plot based
#' on combinations of `x` and `fill` variables.
#' For each unique `x` and `fill`, calculates the proportional
#' widths and heights, stacking rectangles within each `x` group.
#'
#' ### Value
#'
#' A data frame with columns: `x`, `fill`, `xmin`, `xmax`, `ymin`, `ymax`,
#' representing the position and size of each rectangle.
#'
#' @keywords internal
.calculate_coordinates <- function(data) {
  # Example: compute rectangles from x and y
  result <- data |>
    # Count combinations of X and Y
    dplyr::count(x, fill, .drop = FALSE) |>
    # Compute total for each X group
    dplyr::mutate(
      .by = x,
      x_total = sum(n),
      prop = n / x_total,
      prop = dplyr::if_else(is.nan(prop), 0, prop)
    ) |>
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
      xmin = dplyr::if_else(n == 0, 0, xmin + 0.005),
      xmax = dplyr::if_else(n == 0, 0, xmax - 0.005),
      ymin = dplyr::if_else(n == 0, 0, ymin + 0.005),
      ymax = dplyr::if_else(n == 0, 0, ymax - 0.005)
    ) |>
    dplyr::select(x, fill, xmin, xmax, ymin, ymax, .n = n)
  result
}
