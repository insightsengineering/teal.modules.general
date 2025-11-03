#' @export
tm_g_scatterplot.picks <- function(label = "Scatterplot",
                                   x = teal.transform::picks(
                                     teal.transform::datasets(),
                                     teal.transform::variables(is.numeric),
                                     teal.transform::values()
                                   ),
                                   y = teal.transform::picks(
                                     teal.transform::datasets(),
                                     teal.transform::variables(is.numeric, selected = 2L),
                                     teal.transform::values()
                                   ),
                                   color_by = teal.transform::picks(
                                     teal.transform::datasets(),
                                     teal.transform::variables(
                                       choices = teal.transform::is_categorical(min.len = 2, max.len = 10),
                                       selected = NULL,
                                       multiple = TRUE
                                     )
                                   ),
                                   size_by = teal.transform::picks(
                                     teal.transform::datasets(),
                                     teal.transform::variables(
                                       choices = is.numeric,
                                       selected = NULL,
                                       multiple = TRUE
                                     )
                                   ),
                                   row_facet = teal.transform::picks(
                                     teal.transform::datasets(),
                                     teal.transform::variables(
                                       choices = teal.transform::is_categorical(min.len = 2, max.len = 10),
                                       selected = NULL
                                     ),
                                     teal.transform::values()
                                   ),
                                   col_facet = teal.transform::picks(
                                     teal.transform::datasets(),
                                     teal.transform::variables(
                                       choices = teal.transform::is_categorical(min.len = 2, max.len = 10),
                                       selected = NULL
                                     ),
                                     teal.transform::values()
                                   ),
                                   plot_height = c(600, 200, 2000),
                                   plot_width = NULL,
                                   alpha = c(1, 0, 1),
                                   shape = shape_names,
                                   size = c(5, 1, 15),
                                   max_deg = 5L,
                                   rotate_xaxis_labels = FALSE,
                                   ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
                                   pre_output = NULL,
                                   post_output = NULL,
                                   table_dec = 4,
                                   ggplot2_args = teal.widgets::ggplot2_args(),
                                   transformators = list(),
                                   decorators = list()) {
  message("Initializing tm_g_scatterplot")

  # Start of assertions
  checkmate::assert_string(label)
  checkmate::assert_class(x, "picks")
  checkmate::assert_class(y, "picks")
  checkmate::assert_class(color_by, "picks", null.ok = TRUE)
  checkmate::assert_class(size_by, "picks", null.ok = TRUE)

  checkmate::assert_class(row_facet, "picks", null.ok = TRUE)
  if (isTRUE(attr(row_facet$variables, "multiple"))) {
    warning("`row_facet` accepts only a single variable selection. Forcing `teal.transform::variables(multiple) to FALSE`")
    attr(row_facet$variables, "multiple") <- FALSE
  }

  checkmate::assert_class(col_facet, "picks", null.ok = TRUE)
  if (isTRUE(attr(col_facet$variables, "multiple"))) {
    warning("`col_facet` accepts only a single variable selection. Forcing `teal.transform::variables(multiple) to FALSE`")
    attr(col_facet$variables, "multiple") <- FALSE
  }

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  if (length(alpha) == 1) {
    checkmate::assert_numeric(alpha, any.missing = FALSE, finite = TRUE)
  } else {
    checkmate::assert_numeric(alpha, len = 3, any.missing = FALSE, finite = TRUE)
    checkmate::assert_numeric(alpha[1], lower = alpha[2], upper = alpha[3], .var.name = "alpha")
  }

  checkmate::assert_character(shape)

  if (length(size) == 1) {
    checkmate::assert_numeric(size, any.missing = FALSE, finite = TRUE)
  } else {
    checkmate::assert_numeric(size, len = 3, any.missing = FALSE, finite = TRUE)
    checkmate::assert_numeric(size[1], lower = size[2], upper = size[3], .var.name = "size")
  }

  checkmate::assert_int(max_deg, lower = 1L)
  checkmate::assert_flag(rotate_xaxis_labels)
  ggtheme <- match.arg(ggtheme)

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)

  checkmate::assert_scalar(table_dec)
  checkmate::assert_class(ggplot2_args, "ggplot2_args")

  assert_decorators(decorators, "plot")

  # End of assertions

  # Make UI args
  args <- as.list(environment())
  ans <- module(
    label = label,
    server = srv_g_scatterplot.picks,
    ui = ui_g_scatterplot.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_scatterplot.picks))],
    server_args = args[names(args) %in% names(formals(srv_g_scatterplot.picks))],
    transformators = transformators,
    datanames = .picks_datanames(list(x, y, color_by, size_by, row_facet, col_facet))
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the scatterplot module
ui_g_scatterplot.picks <- function(id,
                                   x,
                                   y,
                                   color_by,
                                   size_by,
                                   row_facet,
                                   col_facet,
                                   alpha,
                                   shape,
                                   color,
                                   size,
                                   rotate_xaxis_labels,
                                   max_deg,
                                   ggtheme,
                                   pre_output,
                                   post_output,
                                   decorators) {
  ns <- NS(id)
  tagList(
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        teal.widgets::plot_with_settings_ui(id = ns("scatter_plot")),
        tags$br(),
        tags$h1(tags$strong("Selected points:"), style = "font-size: 150%;"),
        teal.widgets::get_dt_rows(ns("data_table"), ns("data_table_rows")),
        DT::dataTableOutput(ns("data_table"), width = "100%")
      ),
      encoding = tags$div(
        tags$label("Encodings", class = "text-primary"),
        tags$div(
          tags$strong("X variable"),
          teal.transform::picks_ui(id = ns("x"), picks = x),
          checkboxInput(ns("log_x"), "Use log transformation", value = FALSE),
          conditionalPanel(
            condition = paste0("input['", ns("log_x"), "'] == true"),
            radioButtons(
              ns("log_x_base"),
              label = NULL,
              inline = TRUE,
              choices = c("Natural" = "log", "Base 10" = "log10", "Base 2" = "log2")
            )
          )
        ),
        tags$div(
          tags$strong("Y variable"),
          teal.transform::picks_ui(id = ns("y"), picks = y),
          checkboxInput(ns("log_y"), "Use log transformation", value = FALSE),
          conditionalPanel(
            condition = paste0("input['", ns("log_y"), "'] == true"),
            radioButtons(
              ns("log_y_base"),
              label = NULL,
              inline = TRUE,
              choices = c("Natural" = "log", "Base 10" = "log10", "Base 2" = "log2")
            )
          )
        ),
        if (!is.null(color_by)) {
          tags$div(
            tags$strong("Color by:"),
            teal.transform::picks_ui(id = ns("color_by"), picks = color_by)
          )
        },
        if (!is.null(size_by)) {
          tags$div(
            tags$strong("Size by:"),
            teal.transform::picks_ui(id = ns("size_by"), picks = size_by)
          )
        },
        if (!is.null(row_facet)) {
          tags$div(
            tags$strong("Row facetting"),
            teal.transform::picks_ui(id = ns("row_facet"), picks = row_facet)
          )
        },
        if (!is.null(col_facet)) {
          tags$div(
            tags$strong("Column facetting"),
            teal.transform::picks_ui(id = ns("col_facet"), picks = col_facet)
          )
        },
        ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "plot")),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = "Plot settings",
            teal.widgets::optionalSliderInputValMinMax(ns("alpha"), "Opacity:", alpha, ticks = FALSE),
            teal.widgets::optionalSelectInput(
              inputId = ns("shape"),
              label = "Points shape:",
              choices = shape,
              selected = shape[1],
              multiple = FALSE
            ),
            colourpicker::colourInput(ns("color"), "Points color:", "black"),
            teal.widgets::optionalSliderInputValMinMax(ns("size"), "Points size:", size, ticks = FALSE, step = .1),
            checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = rotate_xaxis_labels),
            checkboxInput(ns("add_density"), "Add marginal density", value = FALSE),
            checkboxInput(ns("rug_plot"), "Include rug plot", value = FALSE),
            checkboxInput(ns("show_count"), "Show N (number of observations)", value = FALSE),
            shinyjs::hidden(helpText(id = ns("line_msg"), "Trendline needs numeric X and Y variables")),
            teal.widgets::optionalSelectInput(ns("smoothing_degree"), "Smoothing degree", seq_len(max_deg)),
            shinyjs::hidden(teal.widgets::optionalSelectInput(ns("color_sub"), label = "", multiple = TRUE)),
            teal.widgets::optionalSliderInputValMinMax(ns("ci"), "Confidence", c(.95, .8, .99), ticks = FALSE),
            shinyjs::hidden(checkboxInput(ns("show_form"), "Show formula", value = TRUE)),
            shinyjs::hidden(checkboxInput(ns("show_r2"), "Show adj-R Squared", value = TRUE)),
            uiOutput(ns("num_na_removed")),
            tags$div(
              id = ns("label_pos"),
              tags$div(tags$strong("Stats position")),
              tags$div(style = "display: inline-block; width: 70%;", helpText("Left")),
              tags$div(
                style = "display: inline-block; width: 70%;",
                teal.widgets::optionalSliderInput(
                  ns("pos"),
                  label = NULL,
                  min = 0, max = 1, value = .99, ticks = FALSE, step = .01
                )
              ),
              tags$div(style = "display: inline-block; width: 10%;", helpText("Right"))
            ),
            teal.widgets::optionalSliderInput(
              ns("label_size"), "Stats font size",
              min = 3, max = 10, value = 5, ticks = FALSE, step = .1
            ),
            if (!is.null(row_facet) || !is.null(col_facet)) {
              checkboxInput(ns("free_scales"), "Free scales", value = FALSE)
            },
            selectInput(
              inputId = ns("ggtheme"),
              label = "Theme (by ggplot):",
              choices = ggplot_themes,
              selected = ggtheme,
              multiple = FALSE
            )
          )
        )
      ),
      pre_output = pre_output,
      post_output = post_output
    )
  )
}

# Server function for the scatterplot module
srv_g_scatterplot.picks <- function(id,
                                    data,
                                    x,
                                    y,
                                    color_by,
                                    size_by,
                                    row_facet,
                                    col_facet,
                                    plot_height,
                                    plot_width,
                                    table_dec,
                                    ggplot2_args,
                                    decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")


    selectors <- teal.transform::picks_srv(
      picks = list(x = x, y = y, color_by = color_by, size_by = size_by, row_facet = row_facet, col_facet = col_facet),
      data = data
    )


    validated_q <- reactive({
      validate_input(
        inputId = "x-variables-selected",
        condition = length(selectors$x()$variables$selected) == 1,
        message = "Please select exactly one x var."
      )
      validate_input(
        inputId = "y-variables-selected",
        condition = length(selectors$y()$variables$selected) == 1,
        message = "Please select exactly one y var."
      )
      validate_input(
        inputId = c("x-variables-selected", "y-variables-selected"),
        condition = !any(selectors$x()$variables$selected %in% selectors$y()$variables$selected),
        message = "X and Y variables must be different."
      )
      validate_input(
        inputId = "row_facet-variables-selected",
        condition = is.null(row_facet) || length(selectors$row_facet()$variables$selected) < 2,
        message = "Only single Row Facetting variable is allowed."
      )
      validate_input(
        inputId = "col_facet-variables-selected",
        condition = is.null(col_facet) || length(selectors$col_facet()$variables$selected) < 2,
        message = "Only single Column Facetting variable is allowed."
      )
      validate_input(
        inputId = c("row_facet-variables-selected", "col_facet-variables-selected"),
        condition = is.null(row_facet) || !is.null(col_facet) ||
          !any(selectors$row_facet()$variables$selected %in% selectors$col_facet()$variables$selected),
        message = "Row and Column Facetting variables must be different."
      )
      validate_input(
        "add_density",
        condition = !(is.null(input$add_density) &&
          (length(selectors$row_facet()$variables$selected) || length(selectors$col_facet()$variables$selected))
        ),
        message = "Cannot add marginal density when Row or Column facetting has been selected"
      )
      obj <- req(data())
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Scatter Plot"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      teal.code::eval_code(obj, 'library("ggplot2");library("dplyr");')
    })

    merged <- teal.transform::merge_srv("merge", data = validated_q, selectors = selectors, output_name = "anl")

    trend_line_is_applicable <- reactive({
      anl <- merged$data()[["anl"]]
      x_var <- merged$variables()$x
      y_var <- merged$variables()$y
      length(x_var) > 0 && length(y_var) > 0 && is.numeric(anl[[x_var]]) && is.numeric(anl[[y_var]])
    })

    add_trend_line <- reactive({
      smoothing_degree <- as.integer(input$smoothing_degree)
      trend_line_is_applicable() && length(smoothing_degree) > 0
    })

    observeEvent(
      eventExpr = selectors$color_by(),
      handlerExpr = {
        color_by_var <- merged$variables()$color_by
        if (length(color_by_var) > 0) {
          shinyjs::hide("color")
        } else {
          shinyjs::show("color")
        }
      }
    )

    output$num_na_removed <- renderUI({
      if (add_trend_line()) {
        anl <- merged$data()[["anl"]]
        x_var <- merged$variables()$x
        y_var <- merged$variables()$y
        if ((num_total_na <- nrow(anl) - nrow(stats::na.omit(anl[, c(x_var, y_var)]))) > 0) {
          tags$div(paste(num_total_na, "row(s) with missing values were removed"), tags$hr())
        }
      }
    })

    observeEvent(
      eventExpr = list(selectors$row_facet(), selectors$col_facet()),
      handlerExpr = {
        if (
          length(merged$variables()$row_facet) == 0 &&
            length(merged$variables()$col_facet) == 0
        ) {
          shinyjs::hide("free_scales")
        } else {
          shinyjs::show("free_scales")
        }
      }
    )

    output_q <- reactive({
      req(merged$data())
      anl <- merged$data()[["anl"]]
      x_var <- merged$variables()$x
      y_var <- merged$variables()$y
      color_by_var <- merged$variables()$color_by
      size_by_var <- merged$variables()$size_by
      row_facet_var <- merged$variables()$row_facet
      col_facet_var <- merged$variables()$col_facet
      alpha <- input$alpha
      size <- input$size
      rotate_xaxis_labels <- input$rotate_xaxis_labels
      add_density <- input$add_density
      ggtheme <- input$ggtheme
      rug_plot <- input$rug_plot
      color <- input$color
      shape <- `if`(is.null(input$shape) || identical(input$shape, ""), "circle", input$shape)
      smoothing_degree <- as.integer(input$smoothing_degree)
      ci <- input$ci

      log_x <- input$log_x
      log_y <- input$log_y

      validate_input(
        inputId = "row_facet-variables-selected",
        condition = length(col_facet_var) == 0 ||
          inherits(anl[[row_facet_var]], c("character", "factor", "Date", "integer")),
        message = "`Row facetting` variable must be of class `character`, `factor`, `Date`, or `integer`"
      )
      validate_input(
        inputId = "col_facet-variables-selected",
        condition = length(col_facet_var) == 0 ||
          inherits(anl[[col_facet_var]], c("character", "factor", "Date", "integer")),
        message = "`Column facetting` variable must be of class `character`, `factor`, `Date`, or `integer`"
      )


      if (add_density && length(color_by_var) > 0) {
        validate_input(
          inputId = "col_facet-variables-selected",
          condition = !is.numeric(anl[[color_by_var]]),
          message = paste0(
            "Marginal plots cannot be produced when the points are colored by numeric variables.",
            "\nUncheck the 'Add marginal density' checkbox to display the plot."
          )
        )
        validate_input(
          "color_by-variables-selected",
          condition = !(
            inherits(anl[[color_by_var]], "Date") ||
              inherits(anl[[color_by_var]], "POSIXct") ||
              inherits(anl[[color_by_var]], "POSIXlt")
          ),
          message = paste0(
            "Marginal plots cannot be produced when the points are colored by Date or POSIX variables.",
            "\n Uncheck the 'Add marginal density' checkbox to display the plot."
          )
        )
      }

      teal::validate_has_data(anl[, c(x_var, y_var)], 1, complete = TRUE, allow_inf = FALSE)

      if (log_x) {
        validate_input(
          "x-variables-selected",
          condition = is.numeric(anl[[x_var]]) && all(anl[[x_var]] > 0 | is.na(anl[[x_var]])),
          nessage = "X variable can only be log transformed if variable is numeric and all values are positive."
        )
      }
      if (log_y) {
        validate_input(
          "y-variables-selected",
          condition = is.numeric(anl[[y_var]]) && all(anl[[y_var]] > 0 | is.na(anl[[y_var]])),
          message = "Y variable can only be log transformed if variable is numeric and all values are positive."
        )
      }

      point_sizes <- if (length(size_by_var) > 0) {
        validate(need(is.numeric(anl[[size_by_var]]), "Variable to size by must be numeric"))
        substitute(
          expr = size * anl[[size_by_var]] / max(anl[[size_by_var]], na.rm = TRUE),
          env = list(size = size, size_by_var = size_by_var)
        )
      } else {
        size
      }

      plot_q <- merged$data()

      if (log_x) {
        log_x_fn <- input$log_x_base
        plot_q <- teal.code::eval_code(
          object = plot_q,
          code = substitute(
            expr = anl[, log_x_var] <- log_x_fn(anl[, x_var]),
            env = list(
              x_var = x_var,
              log_x_fn = as.name(log_x_fn),
              log_x_var = paste0(log_x_fn, "_", x_var)
            )
          )
        )
      }

      if (log_y) {
        log_y_fn <- input$log_y_base
        plot_q <- teal.code::eval_code(
          object = plot_q,
          code = substitute(
            expr = anl[, log_y_var] <- log_y_fn(anl[, y_var]),
            env = list(
              y_var = y_var,
              log_y_fn = as.name(log_y_fn),
              log_y_var = paste0(log_y_fn, "_", y_var)
            )
          )
        )
      }

      group_anl_call <- if (input$show_count) {
        paste0(
          "anl %>% dplyr::group_by(",
          paste(
            c(
              if (length(color_by_var) > 0 && inherits(anl[[color_by_var]], c("factor", "character"))) color_by_var,
              row_facet_var,
              col_facet_var
            ),
            collapse = ", "
          ),
          ") %>% dplyr::mutate(n = dplyr::n()) %>% dplyr::ungroup()"
        )
      } else {
        "anl"
      }

      plot_call <- substitute(
        expr = group_anl_call %>% ggplot2::ggplot(),
        env = list(group_anl_call = str2lang(group_anl_call))
      )

      plot_call <- if (length(color_by_var) == 0) {
        substitute(
          expr = plot_call +
            ggplot2::aes(x = x_name, y = y_name) +
            ggplot2::geom_point(alpha = alpha_value, size = point_sizes, shape = shape_value, color = color_value),
          env = list(
            plot_call = plot_call,
            x_name = if (log_x) as.name(paste0(log_x_fn, "_", x_var)) else as.name(x_var),
            y_name = if (log_y) as.name(paste0(log_y_fn, "_", y_var)) else as.name(y_var),
            alpha_value = alpha,
            point_sizes = point_sizes,
            shape_value = shape,
            color_value = color
          )
        )
      } else {
        substitute(
          expr = plot_call +
            ggplot2::aes(x = x_name, y = y_name, color = color_by_var_name) +
            ggplot2::geom_point(alpha = alpha_value, size = point_sizes, shape = shape_value),
          env = list(
            plot_call = plot_call,
            x_name = if (log_x) as.name(paste0(log_x_fn, "_", x_var)) else as.name(x_var),
            y_name = if (log_y) as.name(paste0(log_y_fn, "_", y_var)) else as.name(y_var),
            color_by_var_name = as.name(color_by_var),
            alpha_value = alpha,
            point_sizes = point_sizes,
            shape_value = shape
          )
        )
      }

      if (rug_plot) plot_call <- substitute(expr = plot_call + geom_rug(), env = list(plot_call = plot_call))

      plot_label_generator <- function(rhs_formula = quote(y ~ 1),
                                       show_form = input$show_form,
                                       show_r2 = input$show_r2,
                                       show_count = input$show_count,
                                       pos = input$pos,
                                       label_size = input$label_size) {
        stopifnot(sum(show_form, show_r2, show_count) >= 1)
        aes_label <- paste0(
          "aes(",
          if (show_count) "n = n, ",
          "label = ",
          if (sum(show_form, show_r2, show_count) > 1) "paste(",
          paste(
            c(
              if (show_form) "stat(eq.label)",
              if (show_r2) "stat(adj.rr.label)",
              if (show_count) "paste('N ~`=`~', n)"
            ),
            collapse = ", "
          ),
          if (sum(show_form, show_r2, show_count) > 1) ", sep = '*\", \"*'))" else ")"
        )
        label_geom <- substitute(
          expr = ggpmisc::stat_poly_eq(
            mapping = aes_label,
            formula = rhs_formula,
            parse = TRUE,
            label.x = pos,
            size = label_size
          ),
          env = list(
            rhs_formula = rhs_formula,
            pos = pos,
            aes_label = str2lang(aes_label),
            label_size = label_size
          )
        )
        substitute(
          expr = plot_call + label_geom,
          env = list(
            plot_call = plot_call,
            label_geom = label_geom
          )
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
            shinyjs::show("label_size")
          } else {
            shinyjs::hide("label_pos")
            shinyjs::hide("label_size")
          }
        } else {
          shinyjs::show("ci")
          shinyjs::show("show_form")
          shinyjs::show("show_r2")
          if (nrow(anl) - nrow(stats::na.omit(anl[, c(x_var, y_var)])) > 0) {
            plot_q <- teal.code::eval_code(
              plot_q,
              substitute(
                expr = anl <- dplyr::filter(anl, !is.na(x_var) & !is.na(y_var)),
                env = list(x_var = as.name(x_var), y_var = as.name(y_var))
              )
            )
          }
          rhs_formula <- substitute(
            expr = y ~ poly(x, smoothing_degree, raw = TRUE),
            env = list(smoothing_degree = smoothing_degree)
          )
          if (input$show_form || input$show_r2 || input$show_count) {
            plot_call <- plot_label_generator(rhs_formula = rhs_formula)
            shinyjs::show("label_pos")
            shinyjs::show("label_size")
          } else {
            shinyjs::hide("label_pos")
            shinyjs::hide("label_size")
          }
          plot_call <- substitute(
            expr = plot_call + ggplot2::geom_smooth(formula = rhs_formula, se = TRUE, level = ci, method = "lm"),
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
          shinyjs::show("label_size")
        } else {
          shinyjs::hide("label_pos")
          shinyjs::hide("label_size")
        }
        shinyjs::show("line_msg")
      }

      if (length(row_facet_var) || length(col_facet_var)) {
        facet_cl <- facet_ggplot_call(
          row_facet_var,
          col_facet_var,
          free_x_scales = isTRUE(input$free_scales),
          free_y_scales = isTRUE(input$free_scales)
        )
        plot_call <- substitute(expr = plot_call + facet_cl, env = list(plot_call = plot_call, facet_cl = facet_cl))
      }

      y_label <- varname_w_label(
        y_var,
        anl,
        prefix = if (log_y) paste(log_y_fn, "(") else NULL,
        suffix = if (log_y) ")" else NULL
      )
      x_label <- varname_w_label(
        x_var,
        anl,
        prefix = if (log_x) paste(log_x_fn, "(") else NULL,
        suffix = if (log_x) ")" else NULL
      )

      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        labs = list(y = y_label, x = x_label),
        theme = list(legend.position = "bottom")
      )

      if (rotate_xaxis_labels) {
        dev_ggplot2_args$theme[["axis.text.x"]] <- quote(ggplot2::element_text(angle = 45, hjust = 1))
      }

      all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args,
        module_plot = dev_ggplot2_args
      )

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(all_ggplot2_args, ggtheme = ggtheme)


      if (add_density) {
        plot_call <- substitute(
          expr = ggExtra::ggMarginal(
            plot_call + labs + ggthemes + themes,
            type = "density",
            groupColour = group_colour
          ),
          env = list(
            plot_call = plot_call,
            group_colour = if (length(color_by_var) > 0) TRUE else FALSE,
            labs = parsed_ggplot2_args$labs,
            ggthemes = parsed_ggplot2_args$ggtheme,
            themes = parsed_ggplot2_args$theme
          )
        )
      } else {
        plot_call <- substitute(
          expr = plot_call +
            labs +
            ggthemes +
            themes,
          env = list(
            plot_call = plot_call,
            labs = parsed_ggplot2_args$labs,
            ggthemes = parsed_ggplot2_args$ggtheme,
            themes = parsed_ggplot2_args$theme
          )
        )
      }

      plot_call <- substitute(expr = plot <- plot_call, env = list(plot_call = plot_call))

      teal.reporter::teal_card(plot_q) <- c(teal.reporter::teal_card(plot_q), "## Plot")
      teal.code::eval_code(plot_q, plot_call)
    })

    decorated_output_plot_q <- srv_decorate_teal_data(
      id = "decorator",
      data = output_q,
      decorators = select_decorators(decorators, "plot"),
      expr = quote(plot)
    )

    plot_r <- reactive(req(decorated_output_plot_q())[["plot"]])

    # Insert the plot into a plot_with_settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "scatter_plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
      brushing = TRUE,
      click = TRUE
    )

    output$data_table <- DT::renderDataTable({
      plot_brush <- pws$brush()

      if (!is.null(plot_brush)) {
        validate(need(!input$add_density, "Brushing feature is currently not supported when plot has marginal density"))
      }

      merged_data <- isolate(output_q()[["anl"]])

      brushed_df <- teal.widgets::clean_brushedPoints(merged_data, plot_brush)
      numeric_cols <- names(brushed_df)[
        vapply(brushed_df, function(x) is.numeric(x) && !is.integer(x), FUN.VALUE = logical(1))
      ]

      if (length(numeric_cols) > 0) {
        DT::formatRound(
          DT::datatable(brushed_df,
            rownames = FALSE,
            options = list(scrollX = TRUE, pageLength = input$data_table_rows)
          ),
          numeric_cols,
          table_dec
        )
      } else {
        DT::datatable(brushed_df, rownames = FALSE, options = list(scrollX = TRUE, pageLength = input$data_table_rows))
      }
    })

    set_chunk_dims(pws, decorated_output_plot_q)
  })
}
