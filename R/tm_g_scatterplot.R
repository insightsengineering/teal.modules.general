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
#' @param size_by optional (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Defines the point size encoding. If `NULL` then no size encoding option will be displayed.
#' @param alpha optional, (`numeric`) If scalar then the plot points will have a fixed opacity. If a
#'   slider should be presented to adjust the plot point opacity dynamically then it can be a vector of
#'   length three with `c(value, min, max)`.
#' @param size optional, (`numeric`) If scalar then the plot point sizes will have a fixed size
#'   If a slider should be presented to adjust the plot point sizes dynamically then it can be a
#'   vector of length three with `c(value, min, max)`.
#'
#' @importFrom shinyjs show hide hidden
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
                             size = c(5, 0, 15),
                             rotate_xaxis_labels = FALSE,
                             ggtheme = gg_themes,
                             pre_output = NULL,
                             post_output = NULL) {
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
    is.null(size_by) || is_class_list("data_extract_spec")(size_by),
    is.null(color_by) || is_class_list("data_extract_spec")(color_by),
    is_character_single(ggtheme)
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
    server_args = c(data_extract_list, list(plot_height = plot_height, plot_width = plot_width)),
    filters = get_extract_datanames(data_extract_list)
  )
}

ui_g_scatterplot <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  is_single_dataset_value <- is_single_dataset(args$x, args$y, args$color_by, args$size_by)

  standard_layout(
    output = white_small_well(
      plot_with_settings_ui(id = ns("myplot"), height = args$plot_height, width = args$plot_width)
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
          optionalSelectInput(ns("formula"), "Formula", c("Linear" = 1, "2nd Deg Polynomial" = 2)),
          optionalSliderInputValMinMax(ns("ci"), "Confidence", c(.95, .8, .99), ticks = FALSE),
          shinyjs::hidden(optionalSelectInput(ns("color_sub"), label = "", multiple = TRUE)),
          shinyjs::hidden(checkboxInput(ns("show_form"), "Show formula", value = TRUE)),
          shinyjs::hidden(checkboxInput(ns("show_r2"), "Show R square", value = TRUE)),
          shinyjs::hidden(optionalSliderInput(
            ns("pos"), "left <- stat position -> right", min = 0, max = 1, value = 1, ticks = FALSE, step = .1)),
        )
      ),
      panel_group(
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("alpha"), "Opacity:", args$alpha, ticks = FALSE),
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
srv_g_scatterplot <- function(input, output, session, datasets, x, y, color_by, size_by, plot_height, plot_width) {
  init_chunks()

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = append(append(list(x, y), color_by), size_by),
    input_id = c("x", "y", if_not_null(color_by, "color_by"), if_not_null(size_by, "size_by"))
  )

  if (!is.null(color_by)) {
    cur_color <- reactiveValues(var = "", choices = "")
    observe({
      selected <- if_empty(isolate(input$color_sub)[isolate(input$color_sub) %in% cur_color$choices], NULL)
      updateOptionalSelectInput(
        session = session,
        inputId = "color_sub",
        label = cur_color$var,
        choices = cur_color$choices,
        selected = selected)
    })
  }

  plot_r_line <- reactive({
    ANL <- merged_data()$data() # nolint
    formula <- input$formula
    color_sub <- input$color_sub
    color_by_var <- as.vector(merged_data()$columns_source$color_by)
    x_var <- as.vector(merged_data()$columns_source$x)
    y_var <- as.vector(merged_data()$columns_source$y)

    if (is.numeric(ANL[[x_var]]) && is.numeric(ANL[[y_var]]) && !is.null(formula)) {
      if (!is_empty(color_sub) && !is_empty(color_by_var) && !is.numeric(ANL[[color_by_var]])) {
        ANL <- ANL[ANL[[color_by_var]] %in% color_sub, ] # nolint
      }
      if (formula == 1) {
        m <- lm(ANL[[y_var]] ~ ANL[[x_var]], ANL)
        form <- paste0(y_var, " = ", round(coef(m)[1], 4), " + ", round(coef(m)[2], 4), "*", x_var)
        r_2 <- paste("R^2:", round(summary(m)$r.squared, 8))
      } else if (formula == 2) {
        validate(need(
          length(unique(ANL[[x_var]])) > 2, "number of unique values in x variable is less than or equal to 2"))
        m <- lm(ANL[[y_var]] ~ poly(ANL[[x_var]], 2), ANL)
        form <- paste0(
          y_var, " = ",
          round(coef(m)[1], 4), " + ",
          round(coef(m)[2], 4),
          "*", x_var, " + ",
          round(coef(m)[3], 4),
          "*", x_var, "^2")
        r_2 <- paste("R^2:", round(summary(m)$r.squared, 8))
      }
      list(form = form, r_2 = r_2)
    }
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
    rotate_xaxis_labels <- input$rotate_xaxis_labels
    add_density <- input$add_density
    ggtheme <- input$ggtheme
    rug_plot <- input$rug_plot
    formula <- input$formula
    ci <- input$ci # nolint
    color_sub <- input$color_sub
    show_form <- input$show_form
    show_r2 <- input$show_r2
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
      bquote(.(size) * ANL[[.(size_by_var)]] / max(ANL[[.(size_by_var)]]))
    } else {
      size
    }

    plot_call <- quote(ANL %>% ggplot())
    plot_call <- bquote(
      .(plot_call) + aes(
        x = .(as.name(x_var)),
        y = .(as.name(y_var)),
        color = .(if (!is_empty(color_by_var)) as.name(color_by_var)))
    )

    plot_call <- bquote(
      .(plot_call) +
        geom_point(alpha = .(alpha), size = .(point_sizes)) +
        ylab(.(varname_w_label(y_var, ANL))) +
        xlab(.(varname_w_label(x_var, ANL))) +
        .(call(paste0("theme_", ggtheme)))
    )

    # add color label if existing
    if (!is_empty(color_by_var)) {
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


    if (is.numeric(ANL[[x_var]]) && is.numeric(ANL[[y_var]])) {
      shinyjs::hide("line_msg")
      shinyjs::show("formula")
      if (is.null(formula)) {
        shinyjs::hide("ci")
        shinyjs::hide("color_sub")
        shinyjs::hide("show_form")
        shinyjs::hide("show_r2")
        shinyjs::hide("pos")
      } else {
        shinyjs::show("ci")
        shinyjs::show("show_form")
        shinyjs::show("show_r2")
        if (show_form || show_r2) shinyjs::show("pos") else shinyjs::hide("pos")
        msg <- if (!is_empty(color_by_var) && !is.numeric(ANL[[color_by_var]])) {
          cur_color$var <- color_by_var
          cur_color$choices <- opts <- value_choices(ANL, color_by_var)
          shinyjs::show("color_sub")
          if (length(opts) > 1) {
            if (!is.null(color_sub)) {
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
        line <- if (formula == 1) {
          bquote(geom_smooth(formula = y ~ x, se = TRUE, level = .(ci), method = "lm"))
        } else if (formula == 2) {
          bquote(geom_smooth(formula = y ~ poly(x, 2), se = TRUE, level = .(ci), method = "lm"))
        }
        label <- paste0(
          if (show_form) paste0(plot_r_line()$form, "\n"),
          if (show_r2) paste0(plot_r_line()$r_2, "\n"),
          if (show_form || show_r2) msg
        )
        plot_call <- bquote(
          .(plot_call) +
            .(line) +
            annotate(
              "text",
              x = min(ANL[[.(x_var)]]) + (max(ANL[[.(x_var)]]) - min(ANL[[.(x_var)]])) * .(pos),
              hjust = .(if (pos > .5) 1 else if (pos == .5) .5 else 0),
              y = max(ANL[[.(y_var)]]),
              vjust = 1,
              label =  .(label),
              size = 5
            )
        )
      }
    } else {
      shinyjs::hide("formula")
      shinyjs::hide("ci")
      shinyjs::hide("color_sub")
      shinyjs::hide("show_form")
      shinyjs::hide("show_r2")
      shinyjs::hide("pos")
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
    datanames = get_extract_datanames(list(x, y, color_by, size_by)),
    modal_title = "R Code for a scatterplot",
    code_header = "Scatterplot"
  )
}
