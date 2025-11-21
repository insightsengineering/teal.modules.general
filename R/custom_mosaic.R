#' Minimal mosaic plot
#'
#' Provides a minimal mosaic plot implementation using ggplot2.
#' @param data_name Name of the data frame to use.
#' @param x_var Name of the variable to use on the x-axis.
#' @param y_var Name of the variable to use for fill colors.
#' @param reduce_plot_call Function that takes multiple ggplot2 layers and combines them into a single plot call.
#' @return An expression that creates a mosaic plot when evaluated.
#' @keywords internal
.create_mosaic_layers <- function(data_name, x_var, y_var, reduce_plot_call) {
  data_call <- substitute(
    mosaic_data <- data_name |>
      # Count combinations of X and Y
      dplyr::count(x_var, y_var) |>
      # Compute total for each X group
      dplyr::group_by(x_var) |>
      dplyr::mutate(
        x_total = sum(n),
        prop = n / x_total
      ) |>
      dplyr::ungroup() |>
      # Compute total sample size to turn counts into widths
      dplyr::mutate(N_total = sum(x_total)) |>
      # Convert counts to x widths
      dplyr::group_by(x_var) |>
      dplyr::mutate(
        x_width = x_total / unique(N_total)
      ) |>
      # Compute x-min/x-max for each group
      dplyr::group_by(x_var) |>
      dplyr::mutate(
        x_width_last = dplyr::if_else(dplyr::row_number() == dplyr::n(), x_width, 0)
      ) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        xmin = cumsum(dplyr::lag(x_width_last, default = 0)),
        xmax = xmin + x_width
      ) |>
      # Compute y-min/y-max for stacked proportions
      dplyr::group_by(x_var) |>
      dplyr::arrange(x_var, y_var) |>
      dplyr::mutate(
        ymin = c(0, head(cumsum(prop), -1)),
        ymax = cumsum(prop)
      ) |>
      dplyr::ungroup(),
    env = list(x_var = as.name(x_var), y_var = as.name(y_var), data_name = as.name(data_name))
  )

  layer_rect <- substitute(
    ggplot2::geom_rect(
      ggplot2::aes(
        xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = y_var
      ),
      data = mosaic_data,
      color = "white"
    ),
    env = list(y_var = as.name(y_var))
  )

  layer_scale_x <- substitute(
    ggplot2::scale_x_continuous(
      breaks = mosaic_data |>
        dplyr::distinct(x_var, xmin, xmax) |>
        dplyr::rowwise() |>
        dplyr::mutate(mid = (xmin + xmax) / 2) |>
        dplyr::pull(mid),
      labels = mosaic_data |>
        dplyr::distinct(x_var) |>
        dplyr::pull(x_var),
      expand = c(0, 0)
    ),
    env = list(x_var = as.name(x_var))
  )

  bquote(
    local({
      .(data_call)

      list(
        .(layer_rect),
        .(layer_scale_x),
        ggplot2::scale_y_continuous(expand = c(0, 0), labels = scales::percent_format(scale = 100))
      )
    })
  )
}
