#' Create a simple scatterplot
#'
#' Create a plot with the \code{\link{ggplot2}[geom_point]} function
#' @md
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#' @param x (`data_extract_spec` or `list` of multiple `data_extract_spec`) Variable
#'   names selected to plot along the x-axis by default.
#' @param y (`data_extract_spec` or `list` of multiple `data_extract_spec`) Variable
#'   names selected to plot along the y-axis by default.
#' @param color_by optional (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Defines the color encoding. If `NULL` then no color encoding option will be displayed.
#'   Note `_none_` is a keyword and means that no color encoding should be used.
#' @param alpha optional, (`numeric`) If scalar then the plot points will have a fixed opacity. If a
#'   slider should be presented to adjust the plot point opacity dynamically then it can be a vector of
#'   length three with `c(value, min, max)`.
#' @param size optional, (`numeric`) If scalar then the plot point sizes will have a fixed size
#'   If a slider should be presented to adjust the plot point sizes dynamically then it can be a
#'   vector of length three with `c(value, min, max)`.
#' @param rug_plot optional, (`logical`) should a rug plot be displayed on both axis of the scatter plot.
#'
#' @note For more examples, please see the vignette "Using scatterplot" via
#'   `vignette("using-scatterplot", package = "teal.modules.general")`.
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
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
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
#' shinyApp(app$ui, app$server)
#' }
tm_g_scatterplot <- function(label,
                             x,
                             y,
                             color_by = NULL,
                             plot_height = c(600, 200, 2000),
                             plot_width = NULL,
                             alpha = c(1, 0, 1),
                             size = c(2, 1, 8),
                             rotate_xaxis_labels = FALSE,
                             ggtheme = gg_themes,
                             pre_output = NULL,
                             post_output = NULL,
                             rug_plot = FALSE) {
  if (!is_class_list("data_extract_spec")(x)) {
    x <- list(x)
  }
  if (!is_class_list("data_extract_spec")(y)) {
    y <- list(y)
  }
  if (!is_class_list("data_extract_spec")(color_by)) {
    color_by <- list_or_null(color_by)
  }

  ggtheme <- match.arg(ggtheme)

  stop_if_not(
    is_character_single(label),
    is_class_list("data_extract_spec")(x),
    is_class_list("data_extract_spec")(y),
    is_class_list("data_extract_spec")(color_by) || is.null(color_by),
    is_numeric_vector(alpha) && (length(alpha) == 3 || length(alpha) == 1),
    all(c(alpha >= 0, alpha <= 1)),
    `if`(length(alpha) == 3, alpha[1] >= alpha[2] && alpha[1] <= alpha[3], TRUE),
    is_numeric_vector(size) && (length(size) == 3 || length(size) == 1),
    `if`(length(size) == 3, size[1] >= size[2] && size[1] <= size[3], TRUE),
    is_logical_single(rotate_xaxis_labels),
    is_logical_single(rug_plot),
    all(size >= 0),
    is_character_single(ggtheme)
    )

  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())

  data_extract_list <- list(
    x = x,
    y = y,
    color_by = color_by
  )

  module(
    label = label,
    server = srv_g_scatterplot,
    ui = ui_g_scatterplot,
    ui_args = args,
    server_args = c(data_extract_list, list(plot_height = plot_height, plot_width = plot_width)),
    filters = get_extract_datanames(data_extract_list)
  )
}

ui_g_scatterplot <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  is_single_dataset_value <- is_single_dataset(args$x, args$y, args$color_by)

  standard_layout(
    output = white_small_well(
      plot_with_settings_ui(id = ns("myplot"), height = args$plot_height, width = args$plot_width)
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("x", "y", "color_by")]),
      data_extract_input(
        id = ns("x"),
        label = "X variable",
        data_extract_spec = args$x,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("y"),
        label = "Y variable",
        data_extract_spec = args$y,
        is_single_dataset = is_single_dataset_value
      ),
      if (!is.null(args$color_by)) {
        data_extract_input(
          id = ns("color_by"),
          label = "Color by variable",
          data_extract_spec = args$color_by,
          is_single_dataset = is_single_dataset_value
        )
      },
      panel_group(
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("alpha"), "Opacity:", args$alpha, ticks = FALSE),
          optionalSliderInputValMinMax(ns("size"), "Points size:", args$size, ticks = FALSE),
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = args$rotate_xaxis_labels),
          checkboxInput(ns("rug_plot"), "Include rug plot", value = FALSE),
          optionalSelectInput(
            inputId = ns("ggtheme"),
            label = "Theme (by ggplot):",
            choices = gg_themes,
            selected = args$ggtheme,
            multiple = FALSE
          )
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

#' @importFrom magrittr %>%
srv_g_scatterplot <- function(input, output, session, datasets, x, y, color_by, plot_height, plot_width) {
  init_chunks()

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

  plot_r <- reactive({
    chunks_reset()
    chunks_push_data_merge(merged_data())

    ANL <- chunks_get_var("ANL") # nolint
    validate_has_data(ANL, 10)

    x_var <- as.vector(merged_data()$columns_source$x)
    y_var <- as.vector(merged_data()$columns_source$y)
    color_by_var <- as.vector(merged_data()$columns_source$color_by)
    alpha <- input$alpha # nolint
    size <- input$size # nolint
    rotate_xaxis_labels <- input$rotate_xaxis_labels
    ggtheme <- input$ggtheme
    rug_plot <- input$rug_plot

    validate(need(!is.null(ggtheme), "Please select a theme."))
    validate(need(length(x_var) == 1, "There must be exactly one x var."))
    validate(need(length(y_var) == 1, "There must be exactly one y var."))
    if (!is.null(color_by_var)) {
      validate(need(length(color_by_var) <= 1, "There must be at most 1 coloring variable."))
    }

    validate_has_data(ANL[, c(x_var, y_var)], 10, complete = TRUE, allow_inf = FALSE)

    plot_call <- quote(ANL %>% ggplot())
    plot_call <- if (is.null(color_by_var) || is_character_empty(color_by_var)) {
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
        ylab(.(varname_w_label(y_var, ANL))) +
        xlab(.(varname_w_label(x_var, ANL))) +
        .(call(paste0("theme_", ggtheme)))
    )

    # add color label if existing
    if (!is.null(color_by_var) && !is_character_empty(color_by_var)) {
      plot_call <- bquote(
        .(plot_call) +
        labs(color = .(varname_w_label(color_by_var, ANL))) +
        theme(legend.position = "bottom"))
    }

    if (rotate_xaxis_labels) {
      plot_call <- bquote(
        .(plot_call) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      )
    }

    if (rug_plot) {
      plot_call <- bquote(
        .(plot_call) +
          geom_rug()
      )
    }

    plot_call <- bquote(p <- .(plot_call))
    chunks_push(plot_call)

    #explicitly calling print on the plot inside the chunk evaluates
    #the ggplot call and therefore catches errors
    plot_print_call <- quote(print(p))
    chunks_push(plot_print_call)
    chunks_safe_eval()
  })

  # Insert the plot into a plot_with_settings module from teal.devel
  callModule(
    plot_with_settings_srv,
    id = "myplot",
    plot_r = plot_r,
    height = plot_height,
    width = plot_width
  )

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(x, y, color_by)),
    modal_title = "R Code for a scatterplot",
    code_header = "Scatterplot"
  )
}
