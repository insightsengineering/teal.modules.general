#' @include utils.R
NULL

#' Create a simple scatterplot
#'
#' Create a plot with the \code{\link{plot}[base]} function
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
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
#' @param rotate_xaxis_labels (\code{logical}) Wheater to rotate plot X axis labels
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#'
#' @export
#' @examples
#' # Scatterplot of variables from ADSL dataset
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- radsl(cached = TRUE)",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_scatterplot(
#'       label = "Scatterplot Choices",
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL, c("AGE", "BMRKR1", "BMRKR2")),
#'           selected = "AGE",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'            choices = variable_choices(ADSL, c("AGE", "BMRKR1", "BMRKR2")),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#'   shinyApp(app$ui, app$server)
#' }
tm_g_scatterplot <- function(label,
                           x,
                           y,
                           color_by = NULL,
                           plot_height = c(600, 200, 2000),
                           alpha = c(1, 0, 1),
                           size = c(4, 1, 12),
                           rotate_xaxis_labels = FALSE,
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
  stopifnot(is.class.list("data_extract_spec")(x))
  stopifnot(is.class.list("data_extract_spec")(y))
  stopifnot(is.class.list("data_extract_spec")(color_by) | is.null(color_by))
  stopifnot(is.numeric.vector(plot_height) && (length(plot_height) == 3 || length(plot_height) == 1))
  stopifnot(`if`(length(plot_height) == 3, plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3], TRUE))
  stopifnot(is.numeric.vector(alpha) && (length(alpha) == 3 || length(alpha) == 1))
  stopifnot(`if`(length(alpha) == 3, alpha[1] >= alpha[2] && alpha[1] <= alpha[3], TRUE))
  stopifnot(is.numeric.vector(size) && (length(size) == 3 || length(size) == 1))
  stopifnot(`if`(length(size) == 3, size[1] >= size[2] && size[1] <= size[3], TRUE))
  stopifnot(is.logical.single(rotate_xaxis_labels))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_scatterplot,
    ui = ui_g_scatterplot,
    ui_args = args,
    server_args = list(x = x, y = y, color_by = color_by),
    filters = "all"
  )
}

ui_g_scatterplot <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)

  if (args$plot_height < 200 || args$plot_height > 2000) {
    stop("plot_height must be between 200 and 2000")
  }

  standard_layout(
    output = white_small_well(
      plot_height_output(id = ns("myplot"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("x", "y", "color_by")]),
      data_extract_input(
        id = ns("x"),
        label = "X variable",
        data_extract_spec = args$x
      ),
      data_extract_input(
        id = ns("y"),
        label = "Y variable",
        data_extract_spec = args$y
      ),
      if (!is.null(args$color_by)) {
        data_extract_input(
          id = ns("color_by"),
          label = "Color by variable",
          data_extract_spec = args$color_by
        )
      },
      plot_height_input(id = ns("myplot"), value = args$plot_height),
      panel_group(
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("alpha"), "Opacity:", args$alpha, ticks = FALSE),
          optionalSliderInputValMinMax(ns("size"), "Points size:", args$size, ticks = FALSE),
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = args$rotate_xaxis_labels)
        )
      )
    ),
    forms = actionButton(ns("show_rcode"), "Show R code", width = "100%"),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

#' @importFrom magrittr %>%
#' @importFrom methods substituteDirect
srv_g_scatterplot <- function(input, output, session, datasets, x, y, color_by) {
  init_chunks(session)

  # Insert the plot into a plot_height module from teal.devel
  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  merged_data <- if (is.null(color_by)) {
    data_merge_module(
      datasets = datasets,
      data_extract = list(x, y),
      input_id = c("x", "y")
    )
  } else {
    data_merge_module(
      datasets = datasets,
      data_extract = list(x, y, color_by),
      input_id = c("x", "y", "color_by")
    )
  }



  output$plot <- renderPlot({
    ANL <- merged_data()$data() # nolint
    validate_has_data(ANL, 10)
    chunks_reset()
    x_var <- unname(merged_data()$columns_source$x)
    y_var <- unname(merged_data()$columns_source$y)
    color_by_var <- unname(merged_data()$columns_source$color_by)
    alpha <- input$alpha
    size <- input$size # nolint
    rotate_xaxis_labels <- input$rotate_xaxis_labels

    validate(need(alpha, "Need opacity alpha."))
    validate(need(length(x_var) == 1, "There must be exactly one x var."))
    validate(need(length(y_var) == 1, "There must be exactly one y var."))
    if (!is.null(color_by_var)) {
      validate(need(length(color_by_var) <= 1, "There must be at most 1 coloring variable."))
    }

    plot_call <- quote(ANL %>% ggplot())
    plot_call <- if (is.null(color_by_var) || is.character.empty(color_by_var)) {
      bquote(
        .(plot_call) + aes(x = .(as.name(x_var)), y = .(as.name(y_var)))
      )
    } else {
      bquote(
       .(plot_call) + aes(x = .(as.name(x_var)), y = .(as.name(y_var)), color = .(as.name(color_by_var)))
      )
    }


    plot_call <- bquote(
      .(plot_call) +
      geom_point(alpha = .(alpha), size = .(size)) +
      ylab(.(paste0(attr(ANL[[y_var]], "label"), " [", y_var, "]"))) +
      xlab(.(paste0(attr(ANL[[x_var]], "label"), " [", x_var, "]")))
    )


    if (rotate_xaxis_labels) {
      plot_call <- bquote(
        .(plot_call) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    }

    chunks_push(plot_call)
    chunks_safe_eval()
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "R Code for a scatterplot matrix",
      rcode = get_rcode(
        datasets = datasets,
        merge_expression = merged_data()$expr,
        title = "Scatterplot matrix"
      )
    )
  })
}
