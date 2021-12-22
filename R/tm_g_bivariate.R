#' Univariate and bivariate visualizations
#' @md
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#' @param x (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Variable names selected to plot along the x-axis by default. Variable can be numeric, factor or character.
#'   No empty selections are allowed!
#' @param y (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Variable names selected to plot along the y-axis by default. Variable can be numeric, factor or character.
#' @param use_density optional, (`logical`) value for whether density (`TRUE`) is plotted or
#'   frequency (`FALSE`). Defaults to frequency (`FALSE`).
#' @param row_facet optional, (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Variables for row facetting.
#' @param col_facet optional, (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Variables for col facetting.
#' @param facet optional, (`logical`) to specify whether the facet encodings ui elements are toggled
#'   on and shown to the user by default. Defaults to `TRUE` if either `row_facet` or `column_facet`
#'   are supplied.
#' @param color_settings (`logical`) Whether coloring, filling and size should be applied
#' and UI tool offered to the user.
#' @param color optional, (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Variables selected for the outline color inside the coloring settings.
#'   It will be applied when `color_settings` is set to `TRUE`.
#' @param fill optional, (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Variables selected for the fill color inside the coloring settings.
#'   It will be applied when `color_settings` is set to `TRUE`.
#' @param size optional, (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Variables selected for the size of `geom_point` plots inside the coloring settings.
#'   It will be applied when `color_settings` is set to `TRUE`.
#' @param free_x_scales optional, (`logical`) Whether X scaling shall be changeable.
#'   Does not allow scaling to be changed by default (`FALSE`).
#' @param free_y_scales optional, (`logical`) Whether Y scaling shall be changeable.
#'   Does not allow scaling to be changed by default (`FALSE`).
#' @param swap_axes optional, (`logical`) Whether to swap X and Y axes. Defaults to `FALSE`.
#'
#' @details
#' This is a general module to visualize 1 & 2 dimensional data.
#'
#' @note
#' For more examples, please see the vignette "Using bivariate plot" via
#'   `vignette("using-bivariate-plot", package = "teal.modules.general")`.
#'
#' @importFrom methods is
#' @export
#'
#' @examples
#' # Bivariate plot of selected variable (AGE) against selected (SEX)
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_bivariate(
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL),
#'           selected = "AGE",
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL),
#'           selected = "SEX",
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
tm_g_bivariate <- function(label = "Bivariate Plots",
                           x,
                           y,
                           row_facet = NULL,
                           col_facet = NULL,
                           facet = !is.null(row_facet) || !is.null(col_facet),
                           color = NULL,
                           fill = NULL,
                           size = NULL,
                           use_density = FALSE,
                           color_settings = FALSE,
                           free_x_scales = FALSE,
                           free_y_scales = FALSE,
                           plot_height = c(600, 200, 2000),
                           plot_width = NULL,
                           rotate_xaxis_labels = FALSE,
                           swap_axes = FALSE,
                           ggtheme = gg_themes,
                           ggplot2_args = teal.devel::ggplot2_args(),
                           pre_output = NULL,
                           post_output = NULL) {
  logger::log_info("Initializing tm_g_bivariate")
  ggtheme <- match.arg(ggtheme)

  stop_if_not(
    is_character_single(label),
    is_class_list("data_extract_spec")(x) || is(x, "data_extract_spec"),
    is_class_list("data_extract_spec")(y) || is(y, "data_extract_spec"),
    is.null(row_facet) || is_class_list("data_extract_spec")(row_facet) || is(row_facet, "data_extract_spec"),
    is.null(col_facet) || is_class_list("data_extract_spec")(col_facet) || is(col_facet, "data_extract_spec"),
    is.null(color) || is_class_list("data_extract_spec")(color) || is(color, "data_extract_spec"),
    is.null(fill) || is_class_list("data_extract_spec")(fill) || is(fill, "data_extract_spec"),
    is.null(size) || is_class_list("data_extract_spec")(size) || is(size, "data_extract_spec"),
    is_logical_single(use_density),
    is_logical_single(color_settings),
    is_logical_single(free_x_scales),
    is_logical_single(free_y_scales),
    is_logical_single(rotate_xaxis_labels),
    is_logical_single(swap_axes),
    is_character_single(ggtheme),
    list(
      (is(x, "data_extract_spec") && !isTRUE(x$select$multiple)) ||
        (is_class_list("data_extract_spec")(x) && all(vapply(x, function(xx) !isTRUE(xx$select$multiple), logical(1)))),
      "x variable should not allow multiple selection"
    ),
    list(
      (is(y, "data_extract_spec") && !isTRUE(y$select$multiple)) ||
        (is_class_list("data_extract_spec")(y) && all(vapply(y, function(yy) !isTRUE(yy$select$multiple), logical(1)))),
      "y variable should not allow multiple selection"
    ),
    list(
      is.null(color) ||
        ((is(color, "data_extract_spec") && !isTRUE(color$select$multiple)) ||
          (is_class_list("data_extract_spec")(color) &&
            all(vapply(color, function(z) !isTRUE(z$select$multiple), logical(1))))),
      "color variable should not allow multiple selection"
    ),
    list(
      is.null(fill) ||
        ((is(fill, "data_extract_spec") && !isTRUE(fill$select$multiple)) ||
          (is_class_list("data_extract_spec")(fill) &&
            all(vapply(fill, function(z) !isTRUE(z$select$multiple), logical(1))))),
      "fill variable should not allow multiple selection"
    ),
    list(
      is.null(size) ||
        ((is(size, "data_extract_spec") && !isTRUE(size$select$multiple)) ||
          (is_class_list("data_extract_spec")(size) &&
            all(vapply(size, function(z) !isTRUE(z$select$multiple), logical(1))))),
      "size variable should not allow multiple selection"
    )
  )

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(plot_width[1],
                            lower = plot_width[2], upper = plot_width[3], null.ok = TRUE,
                            .var.name = "plot_width")

  checkmate::assert_class(ggplot2_args, "ggplot2_args")

  if (color_settings) {
    if (is.null(color)) {
      color <- `if`(inherits(x, "list"), x, list(x))
      color[[1]]$select <- select_spec(choices = color[[1]]$select$choices, selected = NULL)
    }
    if (is.null(fill)) {
      fill <- `if`(inherits(x, "list"), x, list(x))
      fill[[1]]$select <- select_spec(choices = fill[[1]]$select$choices, selected = NULL)
    }
    if (is.null(size)) {
      size <- `if`(inherits(x, "list"), x, list(x))
      size[[1]]$select <- select_spec(choices = size[[1]]$select$choices, selected = NULL)
    }
  } else {
    stop_if_not(list(
      is.null(color) && is.null(fill) && is.null(size),
      "'color_settings' argument needs to be set to TRUE if 'color', 'fill', and/or 'size' is/are supplied."
    ))
  }

  args <- as.list(environment())

  data_extract_list <- list(
    x = x,
    y = y,
    row_facet = row_facet,
    col_facet = col_facet,
    color_settings = color_settings,
    color = color,
    fill = fill,
    size = size
  )

  module(
    label = label,
    server = srv_g_bivariate,
    ui = ui_g_bivariate,
    ui_args = args,
    server_args = c(
      data_extract_list,
      list(plot_height = plot_height, plot_width = plot_width, ggplot2_args = ggplot2_args)
    ),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @importFrom shinyWidgets radioGroupButtons switchInput
ui_g_bivariate <- function(id, ...) {
  args <- list(...)
  is_single_dataset_value <- is_single_dataset(
    args$x, args$y, args$row_facet, args$col_facet, args$color, args$fill, args$size
  )

  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      tags$div(plot_with_settings_ui(id = ns("myplot")))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("x", "y", "row_facet", "col_facet", "color", "fill", "size")]),
      data_extract_ui(
        id = ns("x"),
        label = "X variable",
        data_extract_spec = args$x,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("y"),
        label = "Y variable",
        data_extract_spec = args$y,
        is_single_dataset = is_single_dataset_value
      ),
      conditionalPanel(
        condition =
          "$(\"button[data-id*='-x-dataset'][data-id$='-select']\").text() == '- Nothing selected - ' ||
          $(\"button[data-id*='-y-dataset'][data-id$='-select']\").text() == '- Nothing selected - ' ",
        radioGroupButtons(
          inputId = ns("use_density"),
          label = NULL,
          choices = c("frequency", "density"),
          selected = ifelse(args$use_density, "density", "frequency"),
          justified = TRUE
        )
      ),
      if (!is.null(args$row_facet) || !is.null(args$col_facet)) {
        div(
          class = "data-extract-box",
          tags$label("Facetting"),
          switchInput(inputId = ns("facetting"), value = args$facet, size = "mini"),
          conditionalPanel(
            condition = paste0("input['", ns("facetting"), "']"),
            div(
              if (!is.null(args$row_facet)) {
                data_extract_ui(
                  id = ns("row_facet"),
                  label = "Row facetting variable",
                  data_extract_spec = args$row_facet,
                  is_single_dataset = is_single_dataset_value
                )
              },
              if (!is.null(args$col_facet)) {
                data_extract_ui(
                  id = ns("col_facet"),
                  label = "Column facetting variable",
                  data_extract_spec = args$col_facet,
                  is_single_dataset = is_single_dataset_value
                )
              },
              checkboxInput(ns("free_x_scales"), "free x scales", value = args$free_x_scales),
              checkboxInput(ns("free_y_scales"), "free y scales", value = args$free_y_scales)
            )
          )
        )
      },
      if (args$color_settings) {
        # Put a grey border around the coloring settings
        div(
          class = "data-extract-box",
          tags$label("Color settings"),
          switchInput(inputId = ns("coloring"), value = TRUE, size = "mini"),
          conditionalPanel(
            condition = paste0("input['", ns("coloring"), "']"),
            div(
              data_extract_ui(
                id = ns("color"),
                label = "Outline color by variable",
                data_extract_spec = args$color,
                is_single_dataset = is_single_dataset_value
              ),
              data_extract_ui(
                id = ns("fill"),
                label = "Fill color by variable",
                data_extract_spec = args$fill,
                is_single_dataset = is_single_dataset_value
              ),
              div(
                id = ns("size_settings"),
                data_extract_ui(
                  id = ns("size"),
                  label = "Size of points by variable (only if x and y are numeric)",
                  data_extract_spec = args$size,
                  is_single_dataset = is_single_dataset_value
                )
              )
            )
          )
        )
      },
      panel_group(
        panel_item(
          title = "Plot settings",
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = args$rotate_xaxis_labels),
          checkboxInput(ns("swap_axes"), "Swap axes", value = args$swap_axes),
          optionalSelectInput(
            inputId = ns("ggtheme"),
            label = "Theme (by ggplot):",
            choices = gg_themes,
            selected = args$ggtheme,
            multiple = FALSE
          ),
          sliderInput(
            ns("alpha"), "Opacity Scatterplot:",
            min = 0, max = 1,
            step = .05, value = .5, ticks = FALSE
          ),
          sliderInput(
            ns("fixed_size"), "Scatterplot point size:",
            min = 1, max = 8,
            step = 1, value = 2, ticks = FALSE
          ),
          checkboxInput(ns("add_lines"), "Add lines"),
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}


srv_g_bivariate <- function(input,
                            output,
                            session,
                            datasets,
                            x,
                            y,
                            row_facet,
                            col_facet,
                            color_settings = FALSE,
                            color,
                            fill,
                            size,
                            plot_height,
                            plot_width,
                            ggplot2_args) {
  init_chunks()
  data_extract <- stats::setNames(
    list(x, y),
    c("x", "y")
  )

  if (!is.null(row_facet)) {
    data_extract <- append(
      data_extract,
      stats::setNames(
        list(row_facet),
        c("row_facet")
      )
    )
  }

  if (!is.null(col_facet)) {
    data_extract <- append(
      data_extract,
      stats::setNames(
        list(col_facet),
        c("col_facet")
      )
    )
  }

  if (color_settings) {
    data_extract <- append(
      data_extract,
      stats::setNames(
        list(color, fill, size),
        c("color", "fill", "size")
      )
    )
  }

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = data_extract
  )

  plot_r <- reactive({
    chunks_reset()
    chunks_push_data_merge(merged_data())

    ANL <- chunks_get_var("ANL") # nolint
    validate_has_data(ANL, 3)

    x_name <- if_null(as.vector(merged_data()$columns_source$x), character(0))
    y_name <- if_null(as.vector(merged_data()$columns_source$y), character(0))

    validate(
      need(
        !is_character_empty(x_name) || !is_character_empty(y_name),
        "x-variable and y-variable aren't correctly specified. At least one should be valid."
      )
    )

    row_facet_name <- as.vector(merged_data()$columns_source$row_facet)
    col_facet_name <- as.vector(merged_data()$columns_source$col_facet)
    color_name <- as.vector(merged_data()$columns_source$color)
    fill_name <- as.vector(merged_data()$columns_source$fill)
    size_name <- as.vector(merged_data()$columns_source$size)

    use_density <- input$use_density == "density"
    free_x_scales <- input$free_x_scales
    free_y_scales <- input$free_y_scales
    ggtheme <- input$ggtheme
    rotate_xaxis_labels <- input$rotate_xaxis_labels
    swap_axes <- input$swap_axes

    is_scatterplot <- all(vapply(ANL[c(x_name, y_name)], is.numeric, logical(1))) && !is_empty(x_name)

    if (is_scatterplot) {
      shinyjs::show("alpha")
      alpha <- input$alpha # nolint
      shinyjs::show("add_lines")

      if (color_settings && input$coloring) {
        shinyjs::hide("fixed_size")
        shinyjs::show("size_settings")
        size <- NULL
      } else {
        shinyjs::show("fixed_size")
        size <- input$fixed_size
      }
    } else {
      shinyjs::hide("add_lines")
      updateCheckboxInput(session, "add_lines", value = FALSE)
      shinyjs::hide("alpha")
      shinyjs::hide("fixed_size")
      shinyjs::hide("size_settings")
      alpha <- 1
      size <- NULL
    }


    validate_has_data(ANL[, c(x_name, y_name)], 3, complete = TRUE, allow_inf = FALSE)
    validate(need(!is.null(ggtheme), "Please select a theme."))

    cl <- bivariate_plot_call(
      data_name = "ANL",
      x = x_name,
      y = y_name,
      x_class = ifelse(!is_character_empty(x_name), class(ANL[[x_name]]), "NULL"),
      y_class = ifelse(!is_character_empty(y_name), class(ANL[[y_name]]), "NULL"),
      x_label = varname_w_label(x_name, ANL),
      y_label = varname_w_label(y_name, ANL),
      freq = !use_density,
      theme = ggtheme,
      rotate_xaxis_labels = rotate_xaxis_labels,
      swap_axes = swap_axes,
      alpha = alpha,
      size = size,
      ggplot2_args = ggplot2_args
    )

    facetting <- (if_null(input$facetting, FALSE) && (!is.null(row_facet_name) || !is.null(col_facet_name)))

    if (facetting) {
      facet_cl <- facet_ggplot_call(row_facet_name, col_facet_name, free_x_scales, free_y_scales)

      if (!is.null(facet_cl)) {
        cl <- call("+", cl, facet_cl)
      }
    }

    if (input$add_lines) {
      cl <- call("+", cl, quote(geom_line(size = 1)))
    }

    coloring_cl <- NULL
    if (color_settings) {
      if (input$coloring) {
        coloring_cl <- coloring_ggplot_call(
          colour = color_name,
          fill = fill_name,
          size = size_name,
          is_point = any(grepl("geom_point", cl %>% deparse()))
        )
        legend_lbls <- substitute(
          expr = labs(color = color_name, fill = fill_name, size = size_name),
          env = list(
            color_name = varname_w_label(color_name, ANL),
            fill_name = varname_w_label(fill_name, ANL),
            size_name = varname_w_label(size_name, ANL)
          )
        )
      }
      if (!is.null(coloring_cl)) {
        cl <- call("+", call("+", cl, coloring_cl), legend_lbls)
      }
    }

    chunks_push(substitute(expr = p <- cl, env = list(cl = cl)))

    # Add labels to facets
    nulled_row_facet_name <- varname_w_label(row_facet_name, ANL)
    nulled_col_facet_name <- varname_w_label(col_facet_name, ANL)

    if ((is.null(nulled_row_facet_name) && is.null(nulled_col_facet_name)) || !facetting) {
      chunks_push(quote(print(p)))
      chunks_safe_eval()
    } else {
      chunks_push(substitute(
        expr = {
          # Add facetting labels
          # optional: grid.newpage() #nolintr
          g <- add_facet_labels(p, xfacet_label = nulled_col_facet_name, yfacet_label = nulled_row_facet_name)
          grid::grid.draw(g)
        },
        env = list(nulled_col_facet_name = nulled_col_facet_name, nulled_row_facet_name = nulled_row_facet_name)
      ))
      chunks_safe_eval()
      chunks_get_var("g")
    }
  })

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
    datanames = get_extract_datanames(list(x, y, row_facet, col_facet, color, fill, size)),
    modal_title = "R Code for a Bivariate plot"
  )
}


#' Get Substituted ggplot call
#'
#' @noRd
#'
#' @examples
#'
#' bivariate_plot_call("ANL", "BAGE", "RACE", "numeric", "factor")
#' bivariate_plot_call("ANL", "BAGE", character(0), "numeric", "NULL")
bivariate_plot_call <- function(data_name,
                                x = character(0),
                                y = character(0),
                                x_class = "NULL",
                                y_class = "NULL",
                                x_label = NULL,
                                y_label = NULL,
                                freq = TRUE,
                                theme = "gray",
                                rotate_xaxis_labels = FALSE,
                                swap_axes = FALSE,
                                alpha = double(0),
                                size = 2,
                                ggplot2_args = teal.devel::ggplot2_args()) {
  supported_types <- c("NULL", "numeric", "integer", "factor", "character", "logical")
  validate(need(x_class %in% supported_types, paste0("Data type '", x_class, "' is not supported.")))
  validate(need(y_class %in% supported_types, paste0("Data type '", y_class, "' is not supported.")))


  if (is_character_empty(x)) {
    x <- x_label <- "-"
  } else {
    x <- if (is.call(x)) x else as.name(x)
  }
  if (is_character_empty(y)) {
    y <- y_label <- "-"
  } else {
    y <- if (is.call(y)) y else as.name(y)
  }

  cl <- bivariate_ggplot_call(
    x_class = x_class,
    y_class = y_class,
    freq = freq,
    theme = theme,
    rotate_xaxis_labels = rotate_xaxis_labels,
    swap_axes = swap_axes,
    alpha = alpha,
    size = size,
    ggplot2_args = ggplot2_args,
    x = x,
    y = y,
    xlab = x_label,
    ylab = y_label,
    data_name = data_name
  )
}

substitute_q <- function(x, env) {
  stopifnot(is.language(x))
  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}


#' Create ggplot part of plot call
#'
#' Due to the type of the x and y variable the plot type is chosen
#'
#' @noRd
#'
#' @examples
#' bivariate_ggplot_call("numeric", "NULL")
#' bivariate_ggplot_call("numeric", "NULL", freq = FALSE)
#'
#' bivariate_ggplot_call("NULL", "numeric")
#' bivariate_ggplot_call("NULL", "numeric", freq = FALSE)
#'
#' bivariate_ggplot_call("NULL", "factor")
#' bivariate_ggplot_call("NULL", "factor", freq = FALSE)
#'
#' bivariate_ggplot_call("factor", "NULL")
#' bivariate_ggplot_call("factor", "NULL", freq = FALSE)
#'
#' bivariate_ggplot_call("numeric", "numeric")
#' bivariate_ggplot_call("numeric", "factor")
#' bivariate_ggplot_call("factor", "numeric")
#' bivariate_ggplot_call("factor", "factor")
bivariate_ggplot_call <- function(x_class = c("NULL", "numeric", "integer", "factor", "character", "logical"),
                                  y_class = c("NULL", "numeric", "integer", "factor", "character", "logical"),
                                  freq = TRUE,
                                  theme = "gray",
                                  rotate_xaxis_labels = FALSE,
                                  swap_axes = FALSE,
                                  size = double(0),
                                  alpha = double(0),
                                  x = NULL,
                                  y = NULL,
                                  xlab = "-",
                                  ylab = "-",
                                  data_name = "ANL",
                                  ggplot2_args = teal.devel::ggplot2_args()) {
  x_class <- match.arg(x_class)
  y_class <- match.arg(y_class)

  if (x_class %in% c("character", "logical")) {
    x_class <- "factor"
  }
  if (x_class %in% c("integer")) {
    x_class <- "numeric"
  }
  if (y_class %in% c("character", "logical")) {
    y_class <- "factor"
  }
  if (y_class %in% c("integer")) {
    y_class <- "numeric"
  }

  if (all(c(x_class, y_class) == "NULL")) {
    stop("either x or y is required")
  }

  reduce_plot_call <- function(...) {
    args <- Filter(Negate(is.null), list(...))
    utils.nest::calls_combine_by("+", args)
  }

  plot_call <- substitute(ggplot(data_name), env = list(data_name = as.name(data_name)))

  # Single data plots
  if (x_class == "numeric" && y_class == "NULL") {
    plot_call <- reduce_plot_call(plot_call, substitute(aes(x = xval), env = list(xval = x)))

    if (freq) {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(geom_histogram(bins = 30)),
        quote(ylab("Frequency"))
      )
    } else {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(geom_histogram(bins = 30, aes(y = ..density..))),
        quote(geom_density(aes(y = ..density..))),
        quote(ylab("Density"))
      )
    }
  } else if (x_class == "NULL" && y_class == "numeric") {
    plot_call <- reduce_plot_call(plot_call, substitute(aes(x = yval), env = list(yval = y)))

    if (freq) {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(geom_histogram(bins = 30)),
        quote(ylab("Frequency"))
      )
    } else {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(geom_histogram(bins = 30, aes(y = ..density..))),
        quote(geom_density(aes(y = ..density..))),
        quote(ylab("Density"))
      )
    }
  } else if (x_class == "factor" && y_class == "NULL") {
    plot_call <- reduce_plot_call(plot_call, substitute(aes(x = xval), env = list(xval = x)))

    if (freq) {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(geom_bar()),
        quote(ylab("Frequency"))
      )
    } else {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(geom_bar(aes(y = ..prop.., group = 1))),
        quote(ylab("Proportion"))
      )
    }
  } else if (x_class == "NULL" && y_class == "factor") {
    plot_call <- reduce_plot_call(plot_call, substitute(aes(x = yval), env = list(yval = y)))

    if (freq) {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(geom_bar()),
        quote(ylab("Frequency"))
      )
    } else {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(geom_bar(aes(y = ..prop.., group = 1))),
        quote(ylab("Proportion"))
      )
    }
    # Numeric Plots
  } else if (x_class == "numeric" && y_class == "numeric") {
    plot_call <- reduce_plot_call(
      plot_call,
      substitute(aes(x = xval, y = yval), env = list(xval = x, yval = y)),
      # pch = 21 for consistent coloring behaviour b/w all geoms (outline and fill properties)
      `if`(
        !is.null(size),
        substitute(
          geom_point(alpha = alphaval, size = sizeval, pch = 21),
          env = list(alphaval = alpha, sizeval = size)
        ),
        substitute(
          geom_point(alpha = alphaval, pch = 21),
          env = list(alphaval = alpha)
        ),
      )
    )
  } else if ((x_class == "numeric" && y_class == "factor") || (x_class == "factor" && y_class == "numeric")) {
    plot_call <- reduce_plot_call(
      plot_call,
      substitute(aes(x = xval, y = yval), env = list(xval = x, yval = y)),
      quote(geom_boxplot())
    )
    # Factor and character plots
  } else if (x_class == "factor" && y_class == "factor") {
    plot_call <- reduce_plot_call(
      plot_call,
      substitute(
        geom_mosaic(aes(x = product(xval), fill = yval), na.rm = TRUE),
        env = list(xval = x, yval = y)
      )
    )
  } else {
    stop("x y type combination not allowed")
  }

  labs_base <- if (is.null(x_class)) {
    list(x = substitute(ylab, list(ylab = ylab)))
  } else if (is.null(y_class)) {
    list(x = substitute(xlab, list(xlab = xlab)))
  } else {
    list(
      x = substitute(xlab, list(xlab = xlab)),
      y = substitute(ylab, list(ylab = ylab))
    )
  }

  dev_ggplot2_args <- ggplot2_args(labs = labs_base)

  if (rotate_xaxis_labels) {
    dev_ggplot2_args$theme <- list(axis.text.x = quote(element_text(angle = 45, hjust = 1)))
  }

  all_ggplot2_args <- resolve_ggplot2_args(
    user_plot = ggplot2_args,
    module_plot = dev_ggplot2_args
  )

  parsed_ggplot2_args <- parse_ggplot2_args(all_ggplot2_args, ggtheme = theme)

  plot_call <- reduce_plot_call(
    plot_call,
    parsed_ggplot2_args$labs,
    parsed_ggplot2_args$ggtheme,
    parsed_ggplot2_args$theme
  )

  if (swap_axes) {
    plot_call <- reduce_plot_call(plot_call, quote(coord_flip()))
  }

  return(plot_call)
}


#' Create facet call
#'
#' @noRd
#'
#' @examples
#'
#' facet_ggplot_call(LETTERS[1:3])
#' facet_ggplot_call(NULL, LETTERS[23:26])
#' facet_ggplot_call(LETTERS[1:3], LETTERS[23:26])
facet_ggplot_call <- function(row_facet = character(0),
                              col_facet = character(0),
                              free_x_scales = FALSE,
                              free_y_scales = FALSE) {
  scales <- if (free_x_scales && free_y_scales) {
    "free"
  } else if (free_x_scales) {
    "free_x"
  } else if (free_y_scales) {
    "free_y"
  } else {
    "fixed"
  }

  if (is_character_empty(row_facet) && is_character_empty(col_facet)) {
    NULL
  } else if (!is_character_empty(row_facet) && !is_character_empty(col_facet)) {
    call(
      "facet_grid",
      rows = call_fun_dots("vars", row_facet),
      cols = call_fun_dots("vars", col_facet),
      scales = scales
    )
  } else if (is_character_empty(row_facet) && !is_character_empty(col_facet)) {
    call("facet_grid", cols = call_fun_dots("vars", col_facet), scales = scales)
  } else if (!is_character_empty(row_facet) && is_character_empty(col_facet)) {
    call("facet_grid", rows = call_fun_dots("vars", row_facet), scales = scales)
  }
}

coloring_ggplot_call <- function(colour,
                                 fill,
                                 size,
                                 is_point = FALSE) {
  if (!is_character_empty(colour) && !is_character_empty(fill) &&
    is_point && !is_character_empty(size)) {
    substitute(
      expr = aes(colour = colour_name, fill = fill_name, size = size_name),
      env = list(colour_name = as.name(colour), fill_name = as.name(fill), size_name = as.name(size))
    )
  } else if (is_character_empty(colour) && !is_character_empty(fill) &&
    is_point && is_character_empty(size)) {
    substitute(expr = aes(fill = fill_name), env = list(fill_name = as.name(fill)))
  } else if (!is_character_empty(colour) && !is_character_empty(fill) &&
    (!is_point || is_character_empty(size))) {
    substitute(
      expr = aes(colour = colour_name, fill = fill_name),
      env = list(colour_name = as.name(colour), fill_name = as.name(fill))
    )
  } else if (!is_character_empty(colour) && is_character_empty(fill) &&
    (!is_point || is_character_empty(size))) {
    substitute(expr = aes(colour = colour_name), env = list(colour_name = as.name(colour)))
  } else if (is_character_empty(colour) && !is_character_empty(fill) &&
    (!is_point || is_character_empty(size))) {
    substitute(expr = aes(fill = fill_name), env = list(fill_name = as.name(fill)))
  } else if (is_character_empty(colour) && is_character_empty(fill) &&
    is_point && !is_character_empty(size)) {
    substitute(expr = aes(size = size_name), env = list(size_name = as.name(size)))
  } else if (!is_character_empty(colour) && is_character_empty(fill) &&
    is_point && !is_character_empty(size)) {
    substitute(
      expr = aes(colour = colour_name, size = size_name),
      env = list(colour_name = as.name(colour), size_name = as.name(size))
    )
  } else if (is_character_empty(colour) && !is_character_empty(fill) &&
    is_point && !is_character_empty(size)) {
    substitute(
      expr = aes(colour = colour_name, fill = fill_name, size = size_name),
      env = list(colour_name = as.name(fill), fill_name = as.name(fill), size_name = as.name(size))
    )
  } else {
    NULL
  }
}
