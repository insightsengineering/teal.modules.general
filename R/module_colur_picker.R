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
  p_rgb_num <- grDevices::col2rgb(p)
  p_hex <- grDevices::rgb(p_rgb_num[1, ] / 255, p_rgb_num[2, ] / 255, p_rgb_num[3, ] / 255)
  p <- stats::setNames(p_hex, names(p))
  missing_levels <- setdiff(levels, names(p))
  N <- length(levels)
  n <- length(p)
  m <- N - n
  if (m > 0 && n > 0) {
    current_space <- grDevices::rgb2hsv(grDevices::col2rgb(p))
    optimal_color_space <- colorspace::qualitative_hcl(N)
    color_distances <- stats::dist(t(cbind(current_space, grDevices::rgb2hsv(grDevices::col2rgb(optimal_color_space)))))
    optimal_to_current_dist <- as.matrix(color_distances)[seq_len(n), -seq_len(n)]
    furthest_neighbours_idx <- order(apply(optimal_to_current_dist, 2, min), decreasing = TRUE)
    missing_colors <- optimal_color_space[furthest_neighbours_idx][seq_len(m)]
    p <- c(p, stats::setNames(missing_colors, missing_levels))
  } else if (length(missing_levels)) {
    colorspace::qualitative_hcl(N)
  } else {
    p
  }
  p[names(p) %in% levels]
}

.shape_palette_discrete <- function(levels, symbol) {
  s <- stats::setNames(symbol[levels], levels)
  s[is.na(s)] <- "circle-open"
  s
}
