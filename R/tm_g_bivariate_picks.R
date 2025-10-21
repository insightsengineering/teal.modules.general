#' @export
tm_g_bivariate.picks <- function(label = "Bivariate Plots",
                                 x = picks(
                                   datasets(),
                                   variables(
                                     choices = tidyselect::where(is.numeric) |
                                       teal.transform::is_categorical(min.len = 2, max.len = 10),
                                     selected = 1
                                   ),
                                   values(selected = tidyselect::everything(), multiple = TRUE)
                                 ),
                                 y = picks(
                                   datasets(),
                                   variables(
                                     choices = tidyselect::where(is.numeric) |
                                       teal.transform::is_categorical(min.len = 2, max.len = 10),
                                     selected = 2
                                   ),
                                   values(selected = tidyselect::everything(), multiple = TRUE)
                                 ),
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
                                 ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
                                 ggplot2_args = teal.widgets::ggplot2_args(),
                                 pre_output = NULL,
                                 post_output = NULL,
                                 transformators = list(),
                                 decorators = list()) {
  message("Initializing tm_g_bivariate")

  # Start of assertions
  checkmate::assert_class(x, "picks")
  checkmate::assert_class(y, "picks")
  if (isTRUE(attr(x$variables, "multiple"))) {
    warning("`x`-axis doesn't accept multiple variables. Changing automatically.")
    attr(x$variables, "multiple") <- FALSE
  }
  if (isTRUE(attr(y$variables, "multiple"))) {
    warning("`y`-axis doesn't accept multiple variables. Changing automatically.")
    attr(x$variables, "multiple") <- FALSE
  }
  checkmate::assert_class(col_facet, "picks", null.ok = TRUE)
  checkmate::assert_class(row_facet, "picks", null.ok = TRUE)
  checkmate::assert_class(color, "picks", null.ok = TRUE)
  checkmate::assert_class(size, "picks", null.ok = TRUE)
  checkmate::assert_string(label)
  checkmate::assert_flag(use_density)

  # Determines color, fill & size if they are not explicitly set
  checkmate::assert_flag(color_settings)
  if (color_settings) {
    if (is.null(color)) {
      color <- x
      color$selected <- NULL
    }
    if (is.null(fill)) {
      fill <- x
      fill$selected <- NULL
    }
    if (is.null(size)) {
      size <- x
      size$selected <- NULL
    }
  } else {
    if (!is.null(c(color, fill, size))) {
      stop("'color_settings' argument needs to be set to TRUE if 'color', 'fill', and/or 'size' is/are supplied.")
    }
  }

  checkmate::assert_flag(free_x_scales)
  checkmate::assert_flag(free_y_scales)

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  checkmate::assert_flag(rotate_xaxis_labels)
  checkmate::assert_flag(swap_axes)

  ggtheme <- match.arg(ggtheme)
  checkmate::assert_class(ggplot2_args, "ggplot2_args")

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)

  assert_decorators(decorators, "plot")
  # End of assertions

  # Make UI args
  args <- as.list(environment())

  ans <- module(
    label = label,
    server = srv_g_bivariate.picks,
    ui = ui_g_bivariate.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_bivariate.picks))],
    server_args = args[names(args) %in% names(formals(srv_g_bivariate.picks))],
    transformators = transformators,
    datanames = {
      datanames <- teal.transform::datanames(list(x, y, row_facet, col_facet, color, fill, size))
      if (length(datanames)) datanames else "all"
    }
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the bivariate module
ui_g_bivariate.picks <- function(id,
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
                                 rotate_xaxis_labels = FALSE,
                                 swap_axes = FALSE,
                                 ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
                                 ggplot2_args = teal.widgets::ggplot2_args(),
                                 pre_output = NULL,
                                 post_output = NULL,
                                 decorators = list()) {
  ns <- NS(id)
  teal::standard_layout2(
    output = bslib::card(
      teal.widgets::plot_with_settings_ui(id = ns("myplot")),
      full_screen = TRUE
    ),
    encoding = shiny::tagList(
      teal::teal_nav_item(
        label = tags$strong("X variable"),
        teal.transform::picks_ui(id = ns("x"), spec = x)
      ),
      teal::teal_nav_item(
        label = tags$strong("Y variable"),
        teal.transform::picks_ui(id = ns("y"), spec = y)
      ),
      conditionalPanel(
        condition =
          "$(\"button[data-id*='-x-dataset'][data-id$='-select']\").text() == '- Nothing selected - ' ||
            $(\"button[data-id*='-y-dataset'][data-id$='-select']\").text() == '- Nothing selected - ' ",
        teal::teal_nav_item(
          label = NULL,
          shinyWidgets::radioGroupButtons(
            inputId = ns("use_density"),
            label = NULL,
            choices = c("frequency", "density"),
            selected = ifelse(use_density, "density", "frequency"),
            justified = TRUE
          )
        )
      ),
      if (!is.null(row_facet)) {
        teal::teal_nav_item(
          tags$div(
            tags$strong("Row facetting variable"),
            teal.transform::picks_ui(id = ns("row_facet"), spec = row_facet),
            checkboxInput(ns("free_x_scales"), "free x scales", value = free_x_scales)
          )
        )
      },
      if (!is.null(col_facet)) {
        teal::teal_nav_item(
          tags$div(
            tags$strong("Column facetting variable"),
            teal.transform::picks_ui(id = ns("col_facet"), spec = col_facet),
            checkboxInput(ns("free_y_scales"), "free y scales", value = free_y_scales)
          )
        )
      },
      if (color_settings) {
        # Put a grey border around the coloring settings
        teal::teal_nav_item(
          label = tags$strong("Color settings"),
          tags$div(
            bslib::input_switch(id = ns("coloring"), label = "Color settings", value = TRUE),
            conditionalPanel(
              condition = paste0("input['", ns("coloring"), "']"),
              tags$div(
                teal.transform::picks_ui(id = ns("color"), spec = color), # label = "Outline color by variable"
                teal.transform::picks_ui(id = ns("fill"), spec = fill), # label = "Outline color by variable"
                tags$div(
                  id = ns("size_settings"),
                  teal.transform::picks_ui(id = ns("size"), spec = size) # label = "Size of points by variable (only if x and y are numeric)"
                )
              )
            )
          )
        )
      },
      teal::teal_nav_item(
        label = NULL,
        teal:::.teal_navbar_menu(
          id = ns("plot_settings"),
          label = "Plot settings",
          icon = "gear",
          tags$div(
            checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = rotate_xaxis_labels),
            checkboxInput(ns("swap_axes"), "Swap axes", value = swap_axes),
            selectInput(
              inputId = ns("ggtheme"),
              label = "Theme (by ggplot):",
              choices = ggplot_themes,
              selected = ggtheme,
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
            checkboxInput(ns("add_lines"), "Add lines")
          )
        )
      ),
      teal::teal_nav_item(
        ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "plot"))
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

# Server function for the bivariate module
srv_g_bivariate.picks <- function(id,
                                  data,
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
                                  ggplot2_args,
                                  decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    ns <- session$ns
    selectors <- teal.transform::picks_srv(
      spec = list(
        x = x,
        y = y,
        row_facet = row_facet,
        col_facet = col_facet,
        color = color,
        fill = fill,
        size = size
      ),
      data = data
    )

    validated_q <- reactive({
      validate_input(
        inputId = c("x-variables-selected", "y-variables-selected"),
        condition = length(selectors$x()$variables$selected) && length(selectors$y()$variables$selected),
        message = "Please select at least one of x-variable or y-variable"
      )
      if (!is.null(col_facet) && !is.null(row_facet)) {
        validate_input(
          inputId = c("row_facet-variables-selected", "col_facet-variables-selected"),
          condition = is.null(selectors$row_facet()$variables$selected) ||
            is.null(selectors$col_facet()$variables$selected) ||
            !identical(selectors$row_facet()$variables$selected, selectors$col_facet()$variables$selected),
          message = "Row and column facetting variables must be different."
        )
      }

      obj <- req(data())
      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card("# Bivariate Plot"),
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's code")
      )
      teal.code::eval_code(obj, 'library("ggplot2");library("dplyr")')
    })

    merged <- teal.transform::merge_srv("merge", data = validated_q, selectors = selectors, output_name = "anl")

    output_q <- reactive(label = "make bivariateplot", {
      req(merged$data())
      logger::log_debug("Plotting bivariate")
      anl <- merged$data()[["anl"]]
      teal::validate_has_data(anl, 3)


      x_name <- merged$variables()$x
      y_name <- merged$variables()$y
      row_facet_name <- merged$variables()$row_facet
      col_facet_name <- merged$variables()$col_facet
      color_name <- merged$variables()$color
      fill_name <- merged$variables()$fill
      size_name <- merged$variables()$size

      use_density <- input$use_density == "density"
      free_x_scales <- input$free_x_scales
      free_y_scales <- input$free_y_scales
      ggtheme <- input$ggtheme
      rotate_xaxis_labels <- input$rotate_xaxis_labels
      swap_axes <- input$swap_axes


      supported_types <- c("NULL", "numeric", "integer", "factor", "character", "logical", "ordered")
      x_class <- class(anl[[x_name]])[1]
      validate_input(
        "x-variables-selected",
        condition = x_class %in% supported_types,
        message = paste0("Data type '", x_class, "' is not supported.")
      )
      y_class <- class(anl[[y_name]])[[1]]
      validate_input(
        "x-variables-selected",
        condition = y_class %in% supported_types,
        message = paste0("Data type '", y_class, "' is not supported.")
      )

      is_scatterplot <- all(vapply(anl[c(x_name, y_name)], is.numeric, logical(1))) &&
        length(x_name) > 0 && length(y_name) > 0

      if (is_scatterplot) {
        shinyjs::show("alpha")
        alpha <- input$alpha
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
        updateCheckboxInput(session, "add_lines", value = restoreInput(ns("add_lines"), FALSE))
        shinyjs::hide("alpha")
        shinyjs::hide("fixed_size")
        shinyjs::hide("size_settings")
        alpha <- 1
        size <- NULL
      }

      teal::validate_has_data(anl[, c(x_name, y_name), drop = FALSE], 3, complete = TRUE, allow_inf = FALSE)



      cl <- bivariate_plot_call(
        data_name = "anl",
        x = x_name,
        y = y_name,
        x_class = ifelse(length(x_name), class(anl[[x_name]]), "NULL"),
        y_class = ifelse(length(y_name), class(anl[[y_name]]), "NULL"),
        x_label = varname_w_label(x_name, anl),
        y_label = varname_w_label(y_name, anl),
        freq = !use_density,
        theme = ggtheme,
        rotate_xaxis_labels = rotate_xaxis_labels,
        swap_axes = swap_axes,
        alpha = alpha,
        size = size,
        ggplot2_args = ggplot2_args
      )

      if (!is.null(row_facet_name) || !is.null(col_facet_name)) {
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

      obj <- merged$data()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Plot")
      teal.code::eval_code(obj, substitute(expr = plot <- cl, env = list(cl = cl)))
    })

    decorated_output_q_facets <- srv_decorate_teal_data(
      "decorator",
      data = output_q,
      decorators = select_decorators(decorators, "plot"),
      expr = reactive({
        anl <- merged$data()[["anl"]]
        row_facet_name <- merged$variables()$row_facet
        col_facet_name <- merged$variables()$col_facet

        # Add labels to facets
        nulled_row_facet_name <- varname_w_label(row_facet_name, anl)
        nulled_col_facet_name <- varname_w_label(col_facet_name, anl)
        facetting <- (isTRUE(input$facetting) && (!is.null(row_facet_name) || !is.null(col_facet_name)))
        without_facet <- (is.null(nulled_row_facet_name) && is.null(nulled_col_facet_name)) || !facetting

        print_call <- if (without_facet) {
          quote(plot)
        } else {
          substitute(
            expr = {
              teal.modules.general::add_facet_labels(
                plot,
                xfacet_label = nulled_col_facet_name,
                yfacet_label = nulled_row_facet_name
              )
            },
            env = list(nulled_col_facet_name = nulled_col_facet_name, nulled_row_facet_name = nulled_row_facet_name)
          )
        }
        print_call
      })
    )

    plot_r <- reactive(req(decorated_output_q_facets())[["plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    set_chunk_dims(pws, decorated_output_q_facets)
  })
}
