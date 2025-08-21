# todo: to teal widgets?

colour_picker_ui <- function(id) {
  ns <- NS(id)
  bslib::accordion(
    uiOutput(ns("module"), title = "Event colors:", container = bslib::accordion_panel),
    open = FALSE
  )
}

colour_picker_srv <- function(id, x, default_colors) {
  moduleServer(id, function(input, output, session) {
    default_colors_adjusted <- reactive({
      req(x())
      .color_palette_discrete(
        levels = unique(x()),
        color = default_colors
      )
    })

    color_values <- reactiveVal()
    observeEvent(default_colors_adjusted(), {
      if (!identical(default_colors_adjusted(), color_values())) {
        color_values(default_colors_adjusted())
      }
    })

    output$module <- renderUI({
      tagList(
        lapply(
          names(color_values()),
          function(level) {
            div(
              colourpicker::colourInput(
                inputId = session$ns(.name_to_id(level)),
                label = level,
                value = color_values()[level]
              )
            )
          }
        )
      )
    })

    color_input_values <- reactiveVal()
    observe({
      req(color_values())
      new_input_values <- sapply(names(color_values()), function(level) {
        c(input[[.name_to_id(level)]], color_values()[[level]])[1]
      })
      if (!identical(new_input_values, isolate(color_input_values()))) {
        isolate(color_input_values(new_input_values))
      }
    })

    color_input_values
  })
}



#' Color palette discrete
#'
#' To specify custom discrete colors to `plotly` or `ggplot` elements one needs to specify a vector named by
#' levels of variable used for coloring. This function allows to specify only some or none of the colors/levels
#' as the rest will be filled automatically.
#' @param levels (`character`) values of possible variable levels
#' @param color (`named character`) valid color names (see [colors()]) or hex-colors named by `levels`.
#' @return `character` with hex colors named by `levels`.
.color_palette_discrete <- function(levels, color) {
  p <- color[names(color) %in% levels]

  if (length(p) > 0) {
    p_rgb_num <- grDevices::col2rgb(p)
    p_hex <- grDevices::rgb(p_rgb_num[1, ] / 255, p_rgb_num[2, ] / 255, p_rgb_num[3, ] / 255)
    p <- stats::setNames(p_hex, names(p))
  }

  missing_levels <- setdiff(levels, names(p))
  N <- length(levels)
  n <- length(p)
  m <- N - n

  if (m > 0 && n > 0) {
    all_colors <- colorspace::qualitative_hcl(N)

    if (n == 1) {
      current_color_hsv <- grDevices::rgb2hsv(grDevices::col2rgb(p))
      all_colors_hsv <- grDevices::rgb2hsv(grDevices::col2rgb(all_colors))

      distances <- numeric(length(all_colors))
      for (i in seq_along(all_colors)) {
        h_diff <- min(
          abs(current_color_hsv[1] - all_colors_hsv[1, i]),
          1 - abs(current_color_hsv[1] - all_colors_hsv[1, i])
        )
        s_diff <- abs(current_color_hsv[2] - all_colors_hsv[2, i])
        v_diff <- abs(current_color_hsv[3] - all_colors_hsv[3, i])
        distances[i] <- sqrt(h_diff^2 + s_diff^2 + v_diff^2)
      }

      idx <- order(distances, decreasing = TRUE)[seq_len(m)]
      missing_colors <- all_colors[idx]
    } else {
      remaining_colors <- all_colors[seq_len(m)]
      missing_colors <- remaining_colors
    }

    p <- c(p, stats::setNames(missing_colors, missing_levels))
  } else if (m > 0) {
    missing_colors <- colorspace::qualitative_hcl(m)
    p <- stats::setNames(missing_colors, missing_levels)
  }

  result <- p[match(levels, names(p))]
  stats::setNames(result, levels)
}


.shape_palette_discrete <- function(levels, symbol) {
  if (length(symbol) == 0) {
    s <- rep("circle-open", length(levels))
    s <- stats::setNames(s, levels)
  } else {
    s <- stats::setNames(symbol[levels], levels)
    s[is.na(s)] <- "circle-open"
  }
  s
}
