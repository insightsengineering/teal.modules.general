#' @include utils.R
NULL

#' Univariate and bivariate visualizations.
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param label (\code{character}) Label of the module
#' @param x (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable name selected to plot along the x-axis by default. Variable can be numeric, factor or character.
#'   No empty selections are allowed!
#' @param y (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable name selected to plot along the y-axis by default. Variable can be numeric, factor or character.
#' @param use_density (\code{logical}) value for whether density (\code{TRUE}) is plotted or frequency (\code{FALSE})
#' @param row_facet (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable for row facetting
#' @param col_facet (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable for col facetting
#' @param color_settings (\code{logical}) Whether coloring, filling and size should be chosen
#'   by the user
#' @param color optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable selection for the coloring inside the coloring settings
#' @param fill optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable selection for the filling inside the coloring settings
#' @param size optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable selection for the size of \code{geom_point} plots inside the coloring settings
#' @param free_x_scales (\code{logical}) If X scaling shall be changeable
#' @param free_y_scales (\code{logical}) If Y scaling shall be changeable
#' @param plot_height (\code{numeric}) \code{c(value, min and max)} of plot height slider
#' @param rotate_xaxis_labels (\code{logical}) Whether to rotate plot X axis labels
#' @param swap_axes (\code{logical}) Whether to swap X an Y axes
#' @param ggtheme (\code{character}) ggplot theme to be used by default. All themes can be chosen by the user.
#'
#'
#'
#' @details
#' This is a general module to visualize 1 & 2 dimensional data.
#'
#' @importFrom methods is
#' @export
#'
#'
#' @examples
#' # Bivariate plot of selected variable (AGE) against selected (SEX)
#' library(teal.modules.general)
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- radsl(cached = TRUE)",
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
                           color = NULL,
                           fill = NULL,
                           size = NULL,
                           use_density = FALSE,
                           color_settings = FALSE,
                           free_x_scales = FALSE,
                           free_y_scales = FALSE,
                           plot_height = c(600, 200, 2000),
                           rotate_xaxis_labels = FALSE,
                           swap_axes = FALSE,
                           ggtheme = c(
                             "grey", "gray", "bw", "linedraw", "light", "dark", "minimal",
                             "classic", "void", "test"
                           ),
                           pre_output = NULL,
                           post_output = NULL) {
  stopifnot(is_character_single(label))
  stopifnot(is_class_list("data_extract_spec")(x) || is(x, "data_extract_spec"))
  stopifnot(is_class_list("data_extract_spec")(y) || is(y, "data_extract_spec"))
  stopifnot(is.null(row_facet) || is_class_list("data_extract_spec")(row_facet) || is(row_facet, "data_extract_spec"))
  stopifnot(is.null(col_facet) || is_class_list("data_extract_spec")(col_facet) || is(col_facet, "data_extract_spec"))
  stopifnot(is.null(color) || is_class_list("data_extract_spec")(color) || is(color, "data_extract_spec"))
  stopifnot(is.null(fill) || is_class_list("data_extract_spec")(fill) || is(fill, "data_extract_spec"))
  stopifnot(is.null(size) || is_class_list("data_extract_spec")(size) || is(size, "data_extract_spec"))
  if (is_class_list("data_extract_spec")(x)) {
    stop_if_not(list(
      all(vapply(x, function(xx) !isTRUE(xx$select$multiple), logical(1))),
      "x variable should not allow multiple selection"
    ))
  } else if (is(x, "data_extract_spec")) {
    stop_if_not(list(
      !isTRUE(x$select$multiple),
      "x variable should not allow multiple selection"
    ))
  }
  if (is_class_list("data_extract_spec")(y)) {
    stop_if_not(list(
      all(vapply(y, function(x) !isTRUE(x$select$multiple), logical(1))),
      "y variable should not allow multiple selection"
    ))
  } else if (is(y, "data_extract_spec")) {
    stop_if_not(list(
      !isTRUE(y$select$multiple),
      "y variable should not allow multiple selection"
    ))
  }
  stopifnot(is_logical_single(use_density))
  stopifnot(is_logical_single(color_settings))
  stopifnot(is_logical_single(free_x_scales))
  stopifnot(is_logical_single(free_y_scales))
  stopifnot(is_numeric_vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])
  stopifnot(is_logical_single(rotate_xaxis_labels))
  stopifnot(is_logical_single(swap_axes))
  ggtheme <- match.arg(ggtheme)
  stopifnot(is_character_single(ggtheme))

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
  }

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_bivariate,
    ui = ui_g_bivariate,
    ui_args = args,
    server_args = list(
      x = x,
      y = y,
      row_facet = row_facet,
      col_facet = col_facet,
      color_settings = color_settings,
      color = color,
      fill = fill,
      size = size
    ),
    filters = "all"
  )
}


#' @importFrom shinyWidgets radioGroupButtons switchInput
ui_g_bivariate <- function(id, ...) {
  args <- list(...)

  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      tags$div(plot_height_output(id = ns("myplot")))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("x", "y", "row_facet", "col_facet", "color", "fill", "size")]),
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
          switchInput(inputId = ns("facetting"), value = TRUE, size = "mini"),
          conditionalPanel(
            condition = paste0("input['", ns("facetting"), "']"),
            div(
              if (!is.null(args$row_facet)) {
                data_extract_input(
                  id = ns("row_facet"),
                  label = "Row facetting variable",
                  data_extract_spec = args$row_facet
                )
              },
              if (!is.null(args$col_facet)) {
                data_extract_input(
                  id = ns("col_facet"),
                  label = "Column facetting variable",
                  data_extract_spec = args$col_facet
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
              data_extract_input(
                id = ns("color"),
                label = "Color by variable",
                data_extract_spec = args$color
              ),
              data_extract_input(
                id = ns("fill"),
                label = "Fill color by variable",
                data_extract_spec = args$fill
              ),
              data_extract_input(
                id = ns("size"),
                label = "Size of points by variable (only if x and y are numeric)",
                data_extract_spec = args$size
              )
            )
          )
        )
      },
      plot_height_input(id = ns("myplot"), value = args$plot_height),
      panel_group(
        panel_item(
          title = "Plot settings",
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = args$rotate_xaxis_labels),
          checkboxInput(ns("swap_axes"), "Swap axes", value = args$swap_axes),
          optionalSelectInput(
            inputId = ns("ggtheme"),
            label = "Theme (by ggplot):",
            choices = c("grey", "gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test"),
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
#' @importFrom methods is
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
                            size) {
  init_chunks()
  data_extract <- setNames(
    list(x, y),
    c("x", "y")
  )

  if (!is.null(row_facet)) {
    data_extract <- append(
      data_extract,
      setNames(
        list(row_facet),
        c("row_facet")
      )
    )
  }

  if (!is.null(col_facet)) {
    data_extract <- append(
      data_extract,
      setNames(
        list(col_facet),
        c("col_facet")
      )
    )
  }

  if (color_settings) {
    data_extract <- append(
      data_extract,
      setNames(
        list(color, fill, size),
        c("color", "fill", "size")
      )
    )
  }

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = data_extract,
    input_id = names(data_extract)
  )

  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  output$plot <- renderPlot({
    ANL <- merged_data()$data() # nolint
    validate_has_data(ANL, 3)
    chunks_reset()

    x_name <- as.vector(merged_data()$columns_source$x)
    y_name <- as.vector(merged_data()$columns_source$y)
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

    validate(
      need(
        !is_character_empty(x_name) || !is_empty(y_name),
        "x-variable and y-variable isn't correcly specified. At least one should be valid."
      )
    )

    cl <- bivariate_plot_call(
      data_name = "ANL",
      x = x_name,
      y = y_name,
      x_class = ifelse(!is_character_empty(x_name), class(ANL[[x_name]]), "NULL"),
      y_class = ifelse(!is_character_empty(y_name), class(ANL[[y_name]]), "NULL"),
      x_label = attr(ANL[[x_name]], "label"),
      y_label = attr(ANL[[y_name]], "label"),
      freq = !use_density,
      theme = as.call(parse(text = paste0("theme_", ggtheme))),
      rotate_xaxis_labels = rotate_xaxis_labels,
      swap_axes = swap_axes
    )

    facetting <- (if_null(input$facetting, FALSE) && (!is.null(row_facet_name) || !is.null(col_facet_name)))

    if (facetting) {
      facet_cl <- facet_ggplot_call(row_facet_name, col_facet_name, free_x_scales, free_y_scales)

      if (!is.null(facet_cl)) {
        cl <- call("+", cl, facet_cl)
      }
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
      }
      if (!is.null(coloring_cl)) {
        cl <- call("+", cl, coloring_cl)
      }
    }

    nulled_row_facet_name <- if (identical(row_facet_name, character(0))) NULL else row_facet_name
    nulled_col_facet_name <- if (identical(col_facet_name, character(0))) NULL else col_facet_name
    if (is.null(nulled_row_facet_name) && is.null(nulled_col_facet_name)) {
      chunks_push(expression = cl, id = "plotCall")
    } else {
      chunks_push(bquote({
        p <- .(cl)

        # Add facetting labels
        # optional: grid.newpage() #nolintr
        grid::grid.draw(
          add_facet_labels(p, xfacet_label = .(nulled_col_facet_name), yfacet_label = .(nulled_row_facet_name))
        )
      }))
    }

    chunks_safe_eval()
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    merge_expression = merged_data()$expr,
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
                                theme = quote(theme_gray()),
                                rotate_xaxis_labels = FALSE,
                                swap_axes = FALSE) {
  cl <- bivariate_ggplot_call(
    x_class = x_class,
    y_class = y_class,
    freq = freq,
    theme = theme,
    rotate_xaxis_labels = rotate_xaxis_labels,
    swap_axes = swap_axes
  )

  if (is_character_empty(x)) {
    x <- x_label <- "-"
  } else {
    x <- if (is.call(x)) x else as.name(x)
    x_label <- ifelse(
      is.null(x_label),
      paste0("[", deparse(x), "]"),
      paste0(x_label, " [", deparse(x), "]")
    )
  }
  if (is_character_empty(y)) {
    y <- y_label <- "-"
  } else {
    y <- if (is.call(y)) y else as.name(y)
    y_label <- ifelse(
      is.null(y_label),
      paste0("[", deparse(y), "]"),
      paste0(y_label, " [", deparse(y), "]")
    )
  }

  cl_plot <- substitute_q(
    cl,
    list(
      .ggplotcall = bquote(ggplot(.(as.name(data_name)))),
      .x = x,
      .y = y,
      .xlab = x_label,
      .ylab = y_label
    )
  )

  cl_plot
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
                                  theme = quote(theme_grey()),
                                  rotate_xaxis_labels = FALSE,
                                  swap_axes = FALSE) {
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

  reduce_plot_call_internal <- function(x, y) {
    call("+", x, y)
  }
  reduce_plot_call <- function(...) {
    args <- list(...)
    Reduce(reduce_plot_call_internal, args)
  }

  plot_call <- reduce_plot_call(
    quote(.ggplotcall),
    theme
  )

  # Single data plots
  if (x_class == "numeric" && y_class == "NULL") {
    plot_call <- reduce_plot_call(plot_call, quote(aes(x = .x)))

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

    plot_call <- reduce_plot_call(plot_call, quote(xlab(.xlab)))
  } else if (x_class == "NULL" && y_class == "numeric") {
    plot_call <- reduce_plot_call(plot_call, quote(aes(x = .y)))

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

    plot_call <- reduce_plot_call(plot_call, quote(xlab(.ylab)))
  } else if (x_class == "factor" && y_class == "NULL") {
    plot_call <- reduce_plot_call(plot_call, quote(aes(x = .x)))

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

    plot_call <- reduce_plot_call(plot_call, quote(xlab(.xlab)))
  } else if (x_class == "NULL" && y_class == "factor") {
    plot_call <- reduce_plot_call(plot_call, quote(aes(x = .y)))

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

    plot_call <- reduce_plot_call(plot_call, quote(xlab(.ylab)))

    # Numeric Plots
  } else if (x_class == "numeric" && y_class == "numeric") {
    plot_call <- reduce_plot_call(
      plot_call,
      quote(aes(x = .x, y = .y)),
      quote(geom_point()),
      quote(ylab(.ylab)),
      quote(xlab(.xlab))
    )
  } else if (x_class == "numeric" && y_class == "factor") {
    plot_call <- reduce_plot_call(
      plot_call,
      quote(aes(x = .y, y = .x)),
      quote(geom_boxplot()),
      quote(ylab(.xlab)),
      quote(xlab(.ylab))
    )

    # perform coord flip on default
    # when user decides to flip additionally then do nothing (i.e. flip twice)
    if (swap_axes) {
      swap_axes <- FALSE
    } else {
      plot_call <- reduce_plot_call(plot_call, quote(coord_flip()))
    }

  } else if (x_class == "factor" && y_class == "numeric") {
    plot_call <- reduce_plot_call(
      plot_call,
      quote(aes(x = .x, y = .y)),
      quote(geom_boxplot()),
      quote(ylab(.ylab)),
      quote(xlab(.xlab))
    )

    # Factor and character plots
  } else if (x_class == "factor" && y_class == "factor") {
    plot_call <- reduce_plot_call(
      plot_call,
      quote(geom_mosaic(aes(x = product(.x), fill = .y), na.rm = TRUE)),
      quote(ylab(.ylab)),
      quote(xlab(.xlab))
    )
  } else {
    stop("x y type combination not allowed")
  }

  if (swap_axes) {
    plot_call <- reduce_plot_call(plot_call, quote(coord_flip()))
  }

  if (rotate_xaxis_labels) {
    plot_call <- reduce_plot_call(plot_call, quote(theme(axis.text.x = element_text(angle = 45, hjust = 1))))
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
    call("facet_grid",
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
    bquote(aes(
      colour = .(as.name(colour)),
      fill = .(as.name(fill)),
      size = .(as.name(size))
    ))
  } else if (!is_character_empty(colour) && !is_character_empty(fill) &&
    (!is_point || is_character_empty(size))) {
    bquote(aes(
      colour = .(as.name(colour)),
      fill = .(as.name(fill))
    ))
  } else if (!is_character_empty(colour) && is_character_empty(fill) &&
    (!is_point || is_character_empty(size))) {
    bquote(aes(
      colour = .(as.name(colour))
    ))
  } else if (is_character_empty(colour) && !is_character_empty(fill) &&
    (!is_point || is_character_empty(size))) {
    bquote(aes(
      fill = .(as.name(fill))
    ))
  } else if (is_character_empty(colour) && is_character_empty(fill) &&
    is_point && !is_character_empty(size)) {
    bquote(aes(
      size = .(as.name(size))
    ))
  } else if (!is_character_empty(colour) && is_character_empty(fill) &&
    is_point && !is_character_empty(size)) {
    bquote(aes(
      colour = .(as.name(colour)),
      size = .(as.name(size))
    ))
  } else if (is_character_empty(colour) && !is_character_empty(fill) &&
    is_point && !is_character_empty(size)) {
    bquote(aes(
      fill = .(as.name(fill)),
      size = .(as.name(size))
    ))
  } else {
    NULL
  }
}
