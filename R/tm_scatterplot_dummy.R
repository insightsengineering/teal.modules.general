#' Create a simple scatterplot
#'
#' Create a plot with the \code{\link{plot}[base]} function
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param dataname name of dataset used to generate plot
#' @param x (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   x variable
#' @param y (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   y variable
#' @param color_by (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Defines the color encoding. If
#'   \code{NULL} then no color encoding option will be displayed. Note
#'   \code{_none_} is a keyword and means that no color encoding should be used.
#' @param plot_height if scalar then the plot will have a fixed height. If a
#'   slider should be presented to adjust the plot height dynamically then it
#'   can be a vector of length three with \code{c(value, min and max)}.
#' @param alpha if scalar then the plot points will have a fixed opacity. If a
#'   slider should be presented to adjust the plot point opacity dynamically
#'   then it can be a vector of length three with vlaue, min and max.
#' @param size if scalar then the plot points sizes will have a fixed opacity.
#'   If a slider should be presented to adjust the plot point sizes dynamically
#'   then it can be a vector of length three with vlaue, min and max.
#'
#' @noRd
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ASL <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     code = "ASL <- cadsl",
#'     check = FALSE),
#'   root_modules(
#'     tm_scatterplot_dummy(
#'       "Scatterplot Choices",
#'       dataname = "ASL",
#'       x = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = c("AGE", "BMRKR1", "BMRKR2"),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "x variable"
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = c("AGE", "BMRKR1", "BMRKR2"),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "y variable"
#'         )
#'       ),
#'       color_by = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = c("RACE"),
#'           selected = "RACE", # todo: how to select nothing
#'           multiple = TRUE, #todo: 0 or 1
#'           fixed = FALSE,
#'           label = "color by"
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#'   shinyApp(app$ui, app$server)
#' }
tm_scatterplot_dummy <- function(label,
                           dataname,
                           x,
                           y,
                           color_by = NULL,
                           plot_height = c(600, 200, 2000),
                           alpha = c(1, 0, 1),
                           size = c(4, 1, 12),
                           pre_output = NULL,
                           post_output = NULL) {
  if (!is.class.list("data_extract_spec")(x)) {
    x <- list(x)
  }
  if (!is.class.list("data_extract_spec")(y)) {
    y <- list(y)
  }
  if (!is.class.list("data_extract_spec")(color_by)) {
    color_by <- list_or_null(color_by)
  }

  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  stopifnot(is.class.list("data_extract_spec")(x))
  stopifnot(is.class.list("data_extract_spec")(y))
  stopifnot(is.class.list("data_extract_spec")(color_by))
  stopifnot(is.numeric.vector(plot_height) && (length(plot_height) == 3 || length(plot_height) == 1))
  stopifnot(`if`(length(plot_height) == 3, plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3], TRUE))
  stopifnot(is.numeric.vector(alpha) && (length(alpha) == 3 || length(alpha) == 1))
  stopifnot(`if`(length(alpha) == 3, alpha[1] >= alpha[2] && alpha[1] <= alpha[3], TRUE))
  stopifnot(is.numeric.vector(size) && (length(size) == 3 || length(size) == 1))
  stopifnot(`if`(length(size) == 3, size[1] >= size[2] && size[1] <= size[3], TRUE))

  args <- as.list(environment())

  module(
    label = label,
    server = function(input, output, session, datasets, ...) return(NULL),
    server_args = list(dataname = dataname),
    ui = ui_scatterplot_dummy,
    ui_args = args,
    filters = dataname
  )
}

ui_scatterplot_dummy <- function(id,
                           label,
                           dataname,
                           x,
                           y,
                           color_by,
                           plot_height,
                           alpha,
                           size,
                           pre_output,
                           post_output) {
  if (plot_height < 200 || plot_height > 2000) {
    stop("plot_height must be between 200 and 2000")
  }

  ns <- NS(id)

  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(dataname)),
      data_extract_input(
        id = ns("x"),
        label = "x variable",
        data_extract_spec = x
      ),
      data_extract_input(
        id = ns("y"),
        label = "y variable",
        data_extract_spec = y
      ),
      data_extract_input(
        id = ns("color_by"),
        label = "color by",
        data_extract_spec = color_by
      ),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", plot_height, ticks = FALSE),
      optionalSliderInputValMinMax(ns("alpha"), "opacity", alpha, ticks = FALSE),
      optionalSliderInputValMinMax(ns("size"), "point size", size, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
}
