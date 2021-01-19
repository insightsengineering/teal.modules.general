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
#' @importFrom shinyjs show hide hidden
#' @importFrom stats coef lm
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
                             size_by = NULL,
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

  ggtheme <- match.arg(ggtheme)

  stop_if_not(
    is_character_single(label),
    is_class_list("data_extract_spec")(x),
    is_class_list("data_extract_spec")(y),
    list(is_character_vector(shape) && length(shape) > 0, "`shape` must be a character vector of length 1 or more"),
    is.null(size_by) || is_class_list("data_extract_spec")(size_by),
    is.null(color_by) || is_class_list("data_extract_spec")(color_by),
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
    size_by = size_by
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

#' @importFrom colourpicker colourInput
ui_g_scatterplot <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  is_single_dataset_value <- is_single_dataset(args$x, args$y, args$color_by, args$size_by)

  standard_layout(
    output = white_small_well(
      plot_with_settings_ui(id = ns("scatter_plot")),
      tags$h1("Selected points:", style = "text-align:center; font-weight: bold; font-size:150%;"),
      DT::dataTableOutput(ns("data_table"), width = "100%")
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("x", "y", "color_by", "size_by")]),
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
      if (!is.null(args$size_by)) {
        data_extract_input(
          id = ns("size_by"),
          label = "Size by variable",
          data_extract_spec = args$size_by,
          is_single_dataset = is_single_dataset_value
        )
      },
      panel_group(
        panel_item(
          title = "Add trend line",
          shinyjs::hidden(helpText(id = ns("line_msg"), "first select numeric X and Y variables")),
          optionalSelectInput(ns("smoothing_degree"), "Smoothing degree", seq_len(args$max_deg)),
          optionalSliderInputValMinMax(ns("ci"), "Confidence", c(.95, .8, .99), ticks = FALSE),
          shinyjs::hidden(optionalSelectInput(ns("color_sub"), label = "", multiple = TRUE)),
          shinyjs::hidden(checkboxInput(ns("show_form"), "Show formula", value = TRUE)),
          shinyjs::hidden(checkboxInput(ns("show_r2"), "Show R Squared", value = TRUE)),
          shinyjs::hidden(checkboxInput(ns("show_warn"), "", value = TRUE)),
          div(
            id = ns("label_pos"),
            div(style = "display: inline-block; width: 10%", helpText("Left")),
            div(
              style = "display: inline-block; width: 70%",
              optionalSliderInput(ns("pos"), "Stats Position", min = 0, max = 1, value = 1, ticks = FALSE, step = .1)),
            div(style = "display: inline-block; width: 10%", helpText("Right"))
          )
        )
      ),
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
#' @importFrom ggExtra ggMarginal
#' @importFrom shinyjs hide show
srv_g_scatterplot <- function(input,
                              output,
                              session,
                              datasets,
                              x,
                              y,
                              color_by,
                              size_by,
                              plot_height,
                              plot_width,
                              table_dec) {
  init_chunks()

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = append(append(list(x, y), color_by), size_by),
    input_id = c("x", "y", if_not_null(color_by, "color_by"), if_not_null(size_by, "size_by"))
  )

  observe({
    color_by_var <- as.vector(merged_data()$columns_source$color_by)
    if (!is_empty(color_by_var)) {
      shinyjs::hide("color")
    } else {
      shinyjs::show("color")
    }
  })

  if (!is.null(color_by)) {
    # The reason why a reactive is used instead of observer is because there is a bug in shiny 1.4 that automatically
    # unhides selectInput/optionalSelectInput whenever you call their update functions.
    # This behavior creates a bug whenever some validation step fails and observer would update the selectInput
    # widget anyways causing it to unhide.
    # The observer should replace the reactive whenever we upgrade to shiny 1.5, where the bug is fixed.
    update_color_sub <- reactive({
      ANL <- merged_data()$data() # nolint
      color_by_var <- as.vector(merged_data()$columns_source$color_by)
      choices <- value_choices(ANL, color_by_var)
      updateOptionalSelectInput(
        session = session,
        inputId = "color_sub",
        label = color_by_var,
        choices = choices,
        selected = if_empty(isolate(input$color_sub)[isolate(input$color_sub) %in% choices], NULL))
      NULL
    })
  }

  plot_r_line <- reactiveValues(
    ANL_no_NA = NULL,
    form = NULL,
    r_2 = NULL,
    warn_NA = NULL,
    degree_too_high = FALSE
  )

  observe({ # nolint
    ANL <- merged_data()$data() # nolint
    color_sub <- input$color_sub
    color_by_var <- as.vector(merged_data()$columns_source$color_by)
    x_var <- as.vector(merged_data()$columns_source$x)
    y_var <- as.vector(merged_data()$columns_source$y)
    req(ANL)
    req(!is_empty(x_var) && is.numeric(ANL[[x_var]]))
    req(!is_empty(y_var) && is.numeric(ANL[[y_var]]))
    if (
      !is_empty(color_sub) &&
      !is_empty(color_by_var) &&
      (is.character(ANL[[color_by_var]]) || is.factor(ANL[[color_by_var]]))) {
      ANL <- ANL[ANL[[color_by_var]] %in% color_sub, ] # nolint
    }
    ANL_no_NA <- ANL[!is.na(ANL[[x_var]]) & !is.na(ANL[[y_var]]), ] # nolint
    if (nrow(ANL_no_NA) < nrow(ANL)) {
      warn_NA <- paste(nrow(ANL) - nrow(ANL_no_NA), "row(s) with NA got removed") # nolint
      updateCheckboxInput(session, "show_warn", label = warn_NA)
      plot_r_line$warn_NA <- warn_NA # nolint
    } else {
      plot_r_line$warn_NA <- NULL # nolint
    }
    plot_r_line$ANL_no_NA <- ANL_no_NA[, c(x_var, y_var)] # nolint
  })

  observe({
    df <- plot_r_line$ANL_no_NA
    smoothing_degree <- as.integer(input$smoothing_degree)
    req(df)
    req(smoothing_degree)
    m <- try(lm(df[[2]] ~ poly(df[[1]], smoothing_degree), df), silent = TRUE)
    plot_r_line$degree_too_high <- inherits(m, "try-error")
    req(!inherits(m, "try-error"))
    plot_r_line$r_2 <- paste("R^2:", round(summary(m)$r.squared, 8))
    plot_r_line$form <- sprintf(
      "%s = %#.4f %s %#.4f * %s%s",
      names(df)[2],
      coef(m)[1],
      ifelse(coef(m)[2] < 0, "-", "+"),
      abs(coef(m)[2]),
      names(df)[1],
      paste(
        vapply(
          X = seq_len(smoothing_degree)[-1],
          FUN = function(deg) {
            sprintf(
              "%s%s %#.4f*%s^%s",
              ifelse(deg %%  5 == 0, "\n", " "),
              ifelse(coef(m)[deg + 1] < 0, "-", "+"),
              abs(coef(m)[deg + 1]),
              names(df)[1],
              deg
            )
          },
          FUN.VALUE = character(1)),
        collapse = ""
      )
    )
  })

  plot_r <- reactive({
    chunks_reset()
    chunks_push_data_merge(merged_data())

    ANL <- chunks_get_var("ANL") # nolint
    validate_has_data(ANL, 10)

    x_var <- as.vector(merged_data()$columns_source$x)
    y_var <- as.vector(merged_data()$columns_source$y)
    color_by_var <- as.vector(merged_data()$columns_source$color_by)
    size_by_var <- as.vector(merged_data()$columns_source$size_by)
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
    color_sub <- input$color_sub
    show_form <- input$show_form
    show_r2 <- input$show_r2
    show_warn <- input$show_warn
    pos <- input$pos # nolint

    validate(need(!is.null(ggtheme), "Please select a theme."))
    validate(need(length(x_var) == 1, "There must be exactly one x var."))
    validate(need(length(y_var) == 1, "There must be exactly one y var."))
    validate(need(is.null(color_by_var) || length(color_by_var) <= 1, "There must be 1 or no color variable."))
    validate(need(is.null(size_by_var) || length(size_by_var) <= 1, "There must be 1 or no size variable."))

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

    point_sizes <- if (!is_empty(size_by_var)) {
      chunks_validate_custom(bquote(is.numeric(ANL[[.(size_by_var)]])), msg = "Variable to size by must be numeric")
      bquote(.(size) * ANL[[.(size_by_var)]] / max(ANL[[.(size_by_var)]], na.rm = TRUE))
    } else {
      size
    }

    plot_call <- quote(ANL %>% ggplot())
    plot_call <- if (is_empty(color_by_var)) {
      bquote(
        .(plot_call) +
          aes(x = .(as.name(x_var)), y = .(as.name(y_var))) +
          geom_point(alpha = .(alpha), size = .(point_sizes), shape = .(shape), color = .(color))
      )
    } else {
      bquote(
        .(plot_call) +
          aes(x = .(as.name(x_var)), y = .(as.name(y_var)), color = .(as.name(color_by_var))) +
          geom_point(alpha = .(alpha), size = .(point_sizes), shape = .(shape)) +
          labs(color = .(varname_w_label(color_by_var, ANL)))
      )
    }

    plot_call <- bquote(
      .(plot_call) +
        ylab(.(varname_w_label(y_var, ANL))) +
        xlab(.(varname_w_label(x_var, ANL))) +
        .(call(paste0("theme_", ggtheme))) +
        theme(
          legend.position = "bottom",
          axis.text.x = .(if (rotate_xaxis_labels) quote(element_text(angle = 45, hjust = 1))))
    )

    if (rug_plot) {
      plot_call <- bquote(
        .(plot_call) +
          geom_rug()
      )
    }

    if (is.numeric(ANL[[x_var]]) && is.numeric(ANL[[y_var]])) {
      shinyjs::hide("line_msg")
      shinyjs::show("smoothing_degree")
      if (is_empty(smoothing_degree)) {
        shinyjs::hide("ci")
        shinyjs::hide("color_sub")
        shinyjs::hide("show_form")
        shinyjs::hide("show_r2")
        shinyjs::hide("label_pos")
        shinyjs::hide("show_warn")
      } else {
        shinyjs::show("ci")
        shinyjs::show("show_form")
        shinyjs::show("show_r2")
        if (show_form || show_r2 || (!is.null(plot_r_line$warn_NA) && show_warn)) {
          shinyjs::show("label_pos")
        } else {
          shinyjs::hide("label_pos")
        }
        msg <- if (!is_empty(color_by_var) && (is.character(ANL[[color_by_var]]) || is.factor(ANL[[color_by_var]]))) {
          isolate(update_color_sub())
          shinyjs::show("color_sub")
          if (length(value_choices(ANL, color_by_var)) > 1) {
            if (!is.null(color_sub)) {
              validate(need(color_sub %in% value_choices(ANL, color_by_var), "processing..."))
              chunks_push(bquote(ANL <- dplyr::filter(ANL, .(as.name(color_by_var)) %in% .(color_sub)))) # nolint
              if (length(color_sub) > 1) "stats from combined selected color groups"
            } else {
              "stats from entire dataset, i.e. disregarding color groups"
            }
          }
        } else {
          shinyjs::hide("color_sub")
          NULL
        }
        validate(need(
          !plot_r_line$degree_too_high,
          paste(
            "Not enough unique values in X variable for the selected smoothing degree.",
            "\nNumber of unique x values:",
            length(unique(plot_r_line$ANL_no_NA[[x_var]])),
            "\nNumber of rows in data:",
            nrow(plot_r_line$ANL_no_NA),
            "\nPlease try lower smoothing degrees."))
        )
        if (!is.null(plot_r_line$warn_NA)) {
          shinyjs::show("show_warn")
          chunks_push(bquote(ANL <- dplyr::filter(ANL, !is.na(.(as.name(x_var))) & !is.na(.(as.name(y_var)))))) # nolint
        } else {
          shinyjs::hide("show_warn")
        }
        label <- paste0(
          if (show_form) paste0(plot_r_line$form, "\n"),
          if (show_r2) paste0(plot_r_line$r_2, "\n"),
          if ((show_form || show_r2) && !is.null(msg)) paste0(msg, "\n"),
          if (show_warn) plot_r_line$warn_NA
        )
        plot_call <- bquote(
          .(plot_call) +
            geom_smooth(formula = y ~ poly(x, .(smoothing_degree)), se = TRUE, level = .(ci), method = "lm") +
            annotate(
              "text",
              x = min(ANL[[.(x_var)]], na.rm = TRUE) * (.(1 - pos)) + max(ANL[[.(x_var)]], na.rm = TRUE) * .(pos),
              hjust = .(if (pos > .5) 1 else if (pos == .5) .5 else 0),
              y = Inf,
              vjust = 1,
              label =  .(label),
              size = 5
            )
        )
      }
    } else {
      shinyjs::hide("smoothing_degree")
      shinyjs::hide("ci")
      shinyjs::hide("color_sub")
      shinyjs::hide("show_form")
      shinyjs::hide("show_r2")
      shinyjs::hide("label_pos")
      shinyjs::hide("show_warn")
      shinyjs::show("line_msg")
    }

    if (add_density) {
      plot_call <- bquote(
        ggExtra::ggMarginal(
          .(plot_call),
          type = "density",
          groupColour = .(if (!is_empty(color_by_var)) TRUE else FALSE)
        )
      )
    }

    plot_call <- bquote(p <- .(plot_call))
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

    merged_data <- isolate(chunks_get_var("ANL"))

    df <- brushedPoints(merged_data, plot_brush)
    numeric_cols <- names(df)[vapply(df, function(x) is.numeric(x), FUN.VALUE = logical(1))]

    DT::formatRound(
      DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE)),
      numeric_cols,
      table_dec)
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(x, y, color_by, size_by)),
    modal_title = "R Code for a scatterplot",
    code_header = "Scatterplot"
  )
}
