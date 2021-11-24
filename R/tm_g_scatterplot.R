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
#' @param size_by optional (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Defines the point size encoding. If `NULL` then no size encoding option will be displayed.
#' @param row_facet optional, (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Which data columns to use for faceting rows.
#' @param col_facet optional, (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Which data to use for faceting columns.
#' @param alpha optional, (`numeric`) If scalar then the plot points will have a fixed opacity. If a
#'   slider should be presented to adjust the plot point opacity dynamically then it can be a vector of
#'   length three with `c(value, min, max)`.
#' @param size optional, (`numeric`) If scalar then the plot point sizes will have a fixed size
#'   If a slider should be presented to adjust the plot point sizes dynamically then it can be a
#'   vector of length three with `c(value, min, max)`.
#' @param shape optional, (`character`) A character vector with the English names of the
#'   shape, e.g. `c("triangle", "square", "circle")`. It defaults to `shape_names`. This is a complete list from
#'   `vignette("ggplot2-specs", package="ggplot2")`.
#' @param max_deg optional, (`integer`) The maximum degree for the polynomial trend line. Must not be less than 1.
#' @param table_dec optional, (`integer`) Number of decimal places used to round numeric values in the table.
#'
#'
#' @note For more examples, please see the vignette "Using scatterplot" via
#'   `vignette("using-scatterplot", package = "teal.modules.general")`.
#'
#' @export
#' @examples
#' # Scatterplot of variables from ADSL dataset
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
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
#'           choices = variable_choices(ADSL, c("AGE", "BMRKR1", "BMRKR2")),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       color_by = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL, c("AGE", "BMRKR1", "BMRKR2", "RACE", "REGION1")),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       size_by = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL, c("AGE", "BMRKR1", "BMRKR2", "RACE", "REGION1")),
#'           selected = "AGE",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       row_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL, c("AGE", "BMRKR1", "BMRKR2", "RACE", "REGION1")),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL, c("AGE", "BMRKR1", "BMRKR2", "RACE", "REGION1")),
#'           selected = NULL,
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
tm_g_scatterplot <- function(label = "Scatterplot",
                             x,
                             y,
                             color_by = NULL,
                             size_by = NULL,
                             row_facet = NULL,
                             col_facet = NULL,
                             plot_height = c(600, 200, 2000),
                             plot_width = NULL,
                             alpha = c(1, 0, 1),
                             shape = shape_names,
                             size = c(5, 1, 15),
                             max_deg = 5L,
                             rotate_xaxis_labels = FALSE,
                             ggtheme = gg_themes,
                             pre_output = NULL,
                             post_output = NULL,
                             table_dec = 4) {
  logger::log_info("Initializing tm_g_scatterplot")
  if (!is_class_list("data_extract_spec")(x)) {
    x <- list(x)
  }
  if (!is_class_list("data_extract_spec")(y)) {
    y <- list(y)
  }
  if (!is_class_list("data_extract_spec")(color_by)) {
    color_by <- list_or_null(color_by)
  }
  if (!is_class_list("data_extract_spec")(size_by)) {
    size_by <- list_or_null(size_by)
  }
  if (!is_class_list("data_extract_spec")(row_facet)) {
    row_facet <- list_or_null(row_facet)
  }
  if (!is_class_list("data_extract_spec")(col_facet)) {
    col_facet <- list_or_null(col_facet)
  }

  ggtheme <- match.arg(ggtheme)

  stop_if_not(
    is_character_single(label),
    is_class_list("data_extract_spec")(x),
    is_class_list("data_extract_spec")(y),
    list(is_character_vector(shape) && length(shape) > 0, "`shape` must be a character vector of length 1 or more"),
    is.null(size_by) || is_class_list("data_extract_spec")(size_by),
    is.null(color_by) || is_class_list("data_extract_spec")(color_by),
    is.null(row_facet) || is_class_list("data_extract_spec")(row_facet),
    is.null(col_facet) || is_class_list("data_extract_spec")(col_facet),
    is_character_single(ggtheme),
    list(is_numeric_single(max_deg), "`max_deg` must be an integer vector of length of 1"),
    list(
      max_deg < Inf && max_deg == as.integer(max_deg) && max_deg >= 1,
      "`max_deg` must be a finite whole number greater than zero"),
    is_numeric_single(table_dec)
  )

  check_slider_input(alpha, allow_null = FALSE, allow_single = TRUE)
  check_slider_input(size, allow_null = FALSE, allow_single = TRUE)
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())

  data_extract_list <- list(
    x = x,
    y = y,
    color_by = color_by,
    size_by = size_by,
    row_facet = row_facet,
    col_facet = col_facet
  )

  module(
    label = label,
    server = srv_g_scatterplot,
    ui = ui_g_scatterplot,
    ui_args = args,
    server_args = c(
      data_extract_list,
      list(plot_height = plot_height, plot_width = plot_width, table_dec = table_dec)),
    filters = get_extract_datanames(data_extract_list)
  )
}

ui_g_scatterplot <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  is_single_dataset_value <- is_single_dataset(
    args$x, args$y, args$color_by, args$size_by, args$row_facet, args$col_facet)

  standard_layout(
    output = white_small_well(
      plot_with_settings_ui(id = ns("scatter_plot")),
      tags$h1("Selected points:", style = "text-align:center; font-weight: bold; font-size:150%;"),
      get_dt_rows(ns("data_table"), ns("data_table_rows")),
      DT::dataTableOutput(ns("data_table"), width = "100%")
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("x", "y", "color_by", "size_by", "row_facet", "col_facet")]),
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
      if (!is.null(args$color_by)) {
        data_extract_ui(
          id = ns("color_by"),
          label = "Color by variable",
          data_extract_spec = args$color_by,
          is_single_dataset = is_single_dataset_value
        )
      },
      if (!is.null(args$size_by)) {
        data_extract_ui(
          id = ns("size_by"),
          label = "Size by variable",
          data_extract_spec = args$size_by,
          is_single_dataset = is_single_dataset_value
        )
      },
      if (!is.null(args$row_facet)) {
        data_extract_ui(
          id = ns("row_facet"),
          label = "Row facetting",
          data_extract_spec = args$row_facet,
          is_single_dataset = is_single_dataset_value
        )
      },
      if (!is.null(args$col_facet)) {
        data_extract_ui(
          id = ns("col_facet"),
          label = "Column facetting",
          data_extract_spec = args$col_facet,
          is_single_dataset = is_single_dataset_value
        )
      },
      panel_group(
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("alpha"), "Opacity:", args$alpha, ticks = FALSE),
          optionalSelectInput(
            inputId = ns("shape"),
            label = "Points shape:",
            choices = args$shape,
            selected = args$shape[1],
            multiple = FALSE
          ),
          colourpicker::colourInput(ns("color"), "Points color:", "black"),
          optionalSliderInputValMinMax(ns("size"), "Points size:", args$size, ticks = FALSE, step = .1),
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = args$rotate_xaxis_labels),
          checkboxInput(ns("add_density"), "Add marginal density", value = FALSE),
          checkboxInput(ns("rug_plot"), "Include rug plot", value = FALSE),
          checkboxInput(ns("show_count"), "Show N (number of observations)", value = FALSE),
          shinyjs::hidden(helpText(id = ns("line_msg"), "Trendline needs numeric X and Y variables")),
          optionalSelectInput(ns("smoothing_degree"), "Smoothing degree", seq_len(args$max_deg)),
          shinyjs::hidden(optionalSelectInput(ns("color_sub"), label = "", multiple = TRUE)),
          optionalSliderInputValMinMax(ns("ci"), "Confidence", c(.95, .8, .99), ticks = FALSE),
          shinyjs::hidden(checkboxInput(ns("show_form"), "Show formula", value = TRUE)),
          shinyjs::hidden(checkboxInput(ns("show_r2"), "Show adj-R Squared", value = TRUE)),
          uiOutput(ns("num_na_removed")),
          div(
            id = ns("label_pos"),
            div(style = "display: inline-block; width: 10%", helpText("Left")),
            div(
              style = "display: inline-block; width: 70%",
              optionalSliderInput(
                ns("pos"), "Stats Position", min = 0, max = 1, value = 1, ticks = FALSE, step = .01)
              ),
            div(style = "display: inline-block; width: 10%", helpText("Right"))
          ),
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

srv_g_scatterplot <- function(input,
                              output,
                              session,
                              datasets,
                              x,
                              y,
                              color_by,
                              size_by,
                              row_facet,
                              col_facet,
                              plot_height,
                              plot_width,
                              table_dec) {
  init_chunks()

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract  = Reduce(
      f = append,
      init = list(x, y),
      x = list(color_by, size_by, row_facet, col_facet)
    ),
    input_id = c(
      "x", "y",
      if_not_null(color_by, "color_by"),
      if_not_null(size_by, "size_by"),
      if_not_null(row_facet, "row_facet"),
      if_not_null(col_facet, "col_facet")),
    merge_function = "dplyr::inner_join"
  )

  trend_line_is_applicable <- reactive({
    ANL <- merged_data()$data() # nolint
    x_var <- as.vector(merged_data()$columns_source$x)
    y_var <- as.vector(merged_data()$columns_source$y)
    !is_empty(x_var) && !is_empty(y_var) && is.numeric(ANL[[x_var]]) && is.numeric(ANL[[y_var]])
  })

  add_trend_line <- reactive({
    smoothing_degree <- as.integer(input$smoothing_degree)
    trend_line_is_applicable() && !is_empty(smoothing_degree)
  })

  if (!is.null(color_by)) {
    observeEvent(merged_data()$columns_source$color_by, {
      color_by_var <- as.vector(merged_data()$columns_source$color_by)
      if (!is_empty(color_by_var)) {
        shinyjs::hide("color")
      } else {
        shinyjs::show("color")
      }
    })
  }

  output$num_na_removed <- renderUI({
    if (add_trend_line()) {
      ANL <- merged_data()$data() # nolint
      x_var <- as.vector(merged_data()$columns_source$x)
      y_var <- as.vector(merged_data()$columns_source$y)
      if ((num_total_na <- nrow(ANL) - nrow(stats::na.omit(ANL[, c(x_var, y_var)]))) > 0) {
        shiny::tags$div(paste(num_total_na, "row(s) with missing values were removed"), shiny::tags$hr())
      }
    }
  })

  plot_r <- reactive({
    chunks_reset()
    chunks_push_data_merge(merged_data())

    ANL <- merged_data()$data() # nolint
    validate_has_data(ANL, 10)

    x_var <- as.vector(merged_data()$columns_source$x)
    y_var <- as.vector(merged_data()$columns_source$y)
    color_by_var <- as.vector(merged_data()$columns_source$color_by)
    size_by_var <- as.vector(merged_data()$columns_source$size_by)
    row_facet_name <- as.vector(if_empty(merged_data()$columns_source$row_facet, character(0)))
    col_facet_name <- as.vector(if_empty(merged_data()$columns_source$col_facet, character(0)))
    alpha <- input$alpha # nolint
    size <- input$size # nolint
    rotate_xaxis_labels <- input$rotate_xaxis_labels # nolint
    add_density <- input$add_density
    ggtheme <- input$ggtheme
    rug_plot <- input$rug_plot
    color <- input$color # nolint
    shape <- if_empty_string(if_null(input$shape, "circle"), "circle") # nolint
    smoothing_degree <- as.integer(input$smoothing_degree)
    ci <- input$ci # nolint

    validate(need(!is.null(ggtheme), "Please select a theme."))
    validate(need(length(x_var) == 1, "There must be exactly one x var."))
    validate(need(length(y_var) == 1, "There must be exactly one y var."))
    validate(need(is.null(color_by_var) || length(color_by_var) <= 1, "There must be 1 or no color variable."))
    validate(need(is.null(size_by_var) || length(size_by_var) <= 1, "There must be 1 or no size variable."))
    validate(need(length(row_facet_name) <= 1, "There must be 1 or no row facetting variable."))
    validate(need(length(col_facet_name) <= 1, "There must be 1 or no column facetting variable."))
    validate(need(
      is_empty(row_facet_name) || any(class(ANL[[row_facet_name]]) %in% c("character", "factor", "Date", "integer")),
      "`Row facetting` variable must be of class `character`, `factor`, `Date`, or `integer`"))
    validate(need(
      is_empty(col_facet_name) || any(class(ANL[[col_facet_name]]) %in% c("character", "factor", "Date", "integer")),
      "`Column facetting` variable must be of class `character`, `factor`, `Date`, or `integer`"))
    if (add_density && !is_empty(color_by_var)) {
      validate(need(
        !is.numeric(ANL[[color_by_var]]),
        "Marginal plots cannot be produced when the points are colored by numeric variables.
        \n Uncheck the 'Add marginal density' checkbox to display the plot."
      ))
      validate(need(
        !(inherits(ANL[[color_by_var]], "Date") ||
          inherits(ANL[[color_by_var]], "POSIXct") ||
          inherits(ANL[[color_by_var]], "POSIXlt")),
        "Marginal plots cannot be produced when the points are colored by Date or POSIX variables.
        \n Uncheck the 'Add marginal density' checkbox to display the plot."
      ))
    }

    validate_has_data(ANL[, c(x_var, y_var)], 10, complete = TRUE, allow_inf = FALSE)

    facet_cl <- facet_ggplot_call(row_facet_name, col_facet_name)
    if (!is.null(facet_cl)) {
      validate(need(
        !add_density,
        "Marginal density is not supported when faceting is used. Please uncheck `Add marginal density`
        or remove facetting."))
    }

    point_sizes <- if (!is_empty(size_by_var)) {
      validate(need(is.numeric(ANL[[size_by_var]]), "Variable to size by must be numeric"))
      substitute(
        expr = size * ANL[[size_by_var]] / max(ANL[[size_by_var]], na.rm = TRUE),
        env = list(size = size, size_by_var = size_by_var)
      )
    } else {
      size
    }

    pre_pro_anl <- if (input$show_count) {
      paste0(
        "ANL %>% dplyr::group_by(",
        paste(
          c(if (!is_empty(color_by_var) && inherits(ANL[[color_by_var]], c("factor", "character"))) color_by_var,
            row_facet_name,
            col_facet_name),
          collapse = ", "),
        ") %>% dplyr::mutate(n = dplyr::n()) %>% dplyr::ungroup()"
      )
    } else {
      "ANL"
    }

    plot_call <- substitute(expr = pre_pro_anl %>% ggplot(), env = list(pre_pro_anl = str2lang(pre_pro_anl)))

    plot_call <- if (is_empty(color_by_var)) {
      substitute(
        expr = plot_call +
          aes(x = x_name, y = y_name) +
          geom_point(alpha = alpha_value, size = point_sizes, shape = shape_value, color = color_value),
        env = list(
          plot_call = plot_call,
          x_name = as.name(x_var),
          y_name = as.name(y_var),
          alpha_value = alpha,
          point_sizes = point_sizes,
          shape_value = shape,
          color_value = color
        )
      )
    } else {
      substitute(
        expr = plot_call +
          aes(x = x_name, y = y_name, color = color_by_var_name) +
          geom_point(alpha = alpha_value, size = point_sizes, shape = shape_value) +
          labs(color = color_labs),
        env = list(
          plot_call = plot_call,
          x_name = as.name(x_var),
          y_name = as.name(y_var),
          color_by_var_name = as.name(color_by_var),
          alpha_value = alpha,
          point_sizes = point_sizes,
          shape_value = shape,
          color_labs = varname_w_label(color_by_var, ANL)
        )
      )
    }

    plot_call <- substitute(
      expr = plot_call +
        ylab(y_label) +
        xlab(x_label) +
        ggtheme_call +
        theme(
          legend.position = "bottom",
          axis.text.x = axis_text_x
        ),
      env = list(
        plot_call = plot_call,
        y_label = varname_w_label(y_var, ANL),
        x_label = varname_w_label(x_var, ANL),
        ggtheme_call = call(paste0("theme_", ggtheme)),
        axis_text_x = if (rotate_xaxis_labels) quote(element_text(angle = 45, hjust = 1))
      )
    )

    if (rug_plot) plot_call <- substitute(expr = plot_call + geom_rug(), env = list(plot_call = plot_call))

    plot_label_generator <- function(rhs_formula = quote(y ~ 1),
                                     show_form = input$show_form,
                                     show_r2 = input$show_r2,
                                     show_count = input$show_count,
                                     pos = input$pos) {
      stopifnot(sum(show_form, show_r2, show_count) >= 1)
      aes_label <- paste0(
        "aes(",
        if (show_count) "n = n, ",
        "label = ",
        if (sum(show_form, show_r2, show_count) > 1) "paste(",
        paste(
          c(if (show_form) "stat(eq.label)", if (show_r2) "stat(adj.rr.label)", if (show_count) "paste('N ~`=`~', n)"),
          collapse = ", "
        ),
        if (sum(show_form, show_r2, show_count) > 1) ", sep = '*\", \"*'))" else ")"
      )
      label_geom <- substitute(
        expr = ggpmisc::stat_poly_eq(
          mapping = aes_label,
          formula = rhs_formula,
          parse = TRUE,
          label.x = pos
        ),
        env = list(
          rhs_formula = rhs_formula,
          pos = pos,
          aes_label = str2lang(aes_label)
        )
      )
      substitute(
        expr = plot_call + label_geom,
        env = list(
          plot_call = plot_call,
          label_geom = label_geom)
      )
    }

    if (trend_line_is_applicable()) {
      shinyjs::hide("line_msg")
      shinyjs::show("smoothing_degree")
      if (!add_trend_line()) {
        shinyjs::hide("ci")
        shinyjs::hide("color_sub")
        shinyjs::hide("show_form")
        shinyjs::hide("show_r2")
        if (input$show_count) {
          plot_call <- plot_label_generator(show_form = FALSE, show_r2 = FALSE)
          shinyjs::show("label_pos")
        } else {
          shinyjs::hide("label_pos")
        }
      } else {
        shinyjs::show("ci")
        shinyjs::show("show_form")
        shinyjs::show("show_r2")
        if (nrow(ANL) - nrow(stats::na.omit(ANL[, c(x_var, y_var)])) > 0) {
          chunks_push(substitute(
            expr = ANL <- dplyr::filter(ANL, !is.na(x_var) & !is.na(y_var)), # nolint
            env = list(x_var = as.name(x_var), y_var = as.name(y_var))
          ))
        }
        rhs_formula <- substitute(
          expr = y ~ poly(x, smoothing_degree, raw = TRUE),
          env = list(smoothing_degree = smoothing_degree)
        )
        if (input$show_form || input$show_r2 || input$show_count) {
          plot_call <- plot_label_generator(rhs_formula = rhs_formula)
          shinyjs::show("label_pos")
        } else {
          shinyjs::hide("label_pos")
        }
        plot_call <- substitute(
          expr = plot_call + geom_smooth(formula = rhs_formula, se = TRUE, level = ci, method = "lm"),
          env = list(plot_call = plot_call, rhs_formula = rhs_formula, ci = ci)
        )
      }
    } else {
      shinyjs::hide("smoothing_degree")
      shinyjs::hide("ci")
      shinyjs::hide("color_sub")
      shinyjs::hide("show_form")
      shinyjs::hide("show_r2")
      if (input$show_count) {
        plot_call <- plot_label_generator(show_form = FALSE, show_r2 = FALSE)
        shinyjs::show("label_pos")
      } else {
        shinyjs::hide("label_pos")
      }
      shinyjs::show("line_msg")
    }

    if (!is.null(facet_cl)) {
      plot_call <- substitute(expr = plot_call + facet_cl, env = list(plot_call = plot_call, facet_cl = facet_cl))
    }

    if (add_density) {
      plot_call <- substitute(
        expr = ggExtra::ggMarginal(
          plot_call,
          type = "density",
          groupColour = group_colour
        ),
        env = list(plot_call = plot_call, group_colour = if (!is_empty(color_by_var)) TRUE else FALSE)
      )
    }

    plot_call <- substitute(expr = p <- plot_call, env = list(plot_call = plot_call))
    chunks_push(plot_call)

    #explicitly calling print on the plot inside the chunk evaluates
    #the ggplot call and therefore catches errors
    plot_print_call <- quote(print(p))
    chunks_push(plot_print_call)
    chunks_safe_eval()
    chunks_get_var(var = "p")
  })

  # Insert the plot into a plot_with_settings module from teal.devel
  brush <- callModule(
    plot_with_settings_srv,
    id = "scatter_plot",
    plot_r = plot_r,
    height = plot_height,
    width = plot_width,
    brushing = TRUE
  )

  output$data_table <- DT::renderDataTable({
    # if not dependent on plot_r() it tries to print a table before chunks are populated with the correct ANL in plot_r
    # which ends up in a bunch of warnings that ANL is not in chunks
    plot_r()
    plot_brush <- brush$brush()

    if (!is.null(plot_brush)) {
      validate(need(!input$add_density, "Brushing feature is currently not supported when plot has marginal density"))
    }

    merged_data <- isolate(chunks_get_var("ANL"))

    brushed_df <- clean_brushedPoints(merged_data, plot_brush)
    numeric_cols <- names(brushed_df)[vapply(brushed_df, function(x) is.numeric(x), FUN.VALUE = logical(1))]

    DT::formatRound(
      DT::datatable(brushed_df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = input$data_table_rows)),
      numeric_cols,
      table_dec)
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(x, y, color_by, size_by, row_facet, col_facet)),
    modal_title = "R Code for a scatterplot",
    code_header = "Scatterplot"
  )
}
