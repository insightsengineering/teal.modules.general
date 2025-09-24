#' `teal` module: Univariate and bivariate visualizations
#'
#' Module enables the creation of univariate and bivariate plots,
#' facilitating the exploration of data distributions and relationships between two variables.
#'
#' This is a general module to visualize 1 & 2 dimensional data.
#'
#' @note
#' For more examples, please see the vignette "Using bivariate plot" via
#' `vignette("using-bivariate-plot", package = "teal.modules.general")`.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param x (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Variable names selected to plot along the x-axis by default.
#' Can be numeric, factor or character.
#' No empty selections are allowed.
#' @param y (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Variable names selected to plot along the y-axis by default.
#' Can be numeric, factor or character.
#' @param use_density (`logical`) optional, indicates whether to plot density (`TRUE`) or frequency (`FALSE`).
#' Defaults to frequency (`FALSE`).
#' @param row_facet (`data_extract_spec` or `list` of multiple `data_extract_spec`) optional,
#' specification of the data variable(s) to use for faceting rows.
#' @param col_facet (`data_extract_spec` or `list` of multiple `data_extract_spec`) optional,
#' specification of the data variable(s) to use for faceting columns.
#' @param facet (`logical`) optional, specifies whether the facet encodings `ui` elements are toggled
#' on and shown to the user by default. Defaults to `TRUE` if either `row_facet` or `column_facet`
#' are supplied.
#' @param color_settings (`logical`) Whether coloring, filling and size should be applied
#' and `UI` tool offered to the user.
#' @param color (`data_extract_spec` or `list` of multiple `data_extract_spec`) optional,
#' specification of the data variable(s) selected for the outline color inside the coloring settings.
#' It will be applied when `color_settings` is set to `TRUE`.
#' @param fill (`data_extract_spec` or `list` of multiple `data_extract_spec`) optional,
#' specification of the data variable(s) selected for the fill color inside the coloring settings.
#' It will be applied when `color_settings` is set to `TRUE`.
#' @param size (`data_extract_spec` or `list` of multiple `data_extract_spec`) optional,
#' specification of the data variable(s) selected for the size of `geom_point` plots inside the coloring settings.
#' It will be applied when `color_settings` is set to `TRUE`.
#' @param free_x_scales (`logical`) optional, whether X scaling shall be changeable.
#' Does not allow scaling to be changed by default (`FALSE`).
#' @param free_y_scales (`logical`) optional, whether Y scaling shall be changeable.
#' Does not allow scaling to be changed by default (`FALSE`).
#' @param swap_axes (`logical`) optional, whether to swap X and Y axes. Defaults to `FALSE`.
#'
#' @inherit shared_params return
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `plot` (`ggplot`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_g_bivariate(
#'    ..., # arguments for module
#'    decorators = list(
#'      plot = teal_transform_module(...) # applied to the `plot` output
#'    )
#' )
#' ```
#'
#' For additional details and examples of decorators, refer to the vignette
#' `vignette("decorate-module-output", package = "teal.modules.general")`.
#'
#' To learn more please refer to the vignette
#' `vignette("transform-module-output", package = "teal")` or the [`teal::teal_transform_module()`] documentation.
#'
#' @inheritSection teal::example_module Reporting
#'
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#' # general data example
#' data <- teal_data()
#' data <- within(data, {
#'   require(nestcolor)
#'   CO2 <- data.frame(CO2)
#' })
#'
#' app <- init(
#'   data = data,
#'   modules = tm_g_bivariate(
#'     x = data_extract_spec(
#'       dataname = "CO2",
#'       select = select_spec(
#'         label = "Select variable:",
#'         choices = variable_choices(data[["CO2"]]),
#'         selected = "conc",
#'         fixed = FALSE
#'       )
#'     ),
#'     y = data_extract_spec(
#'       dataname = "CO2",
#'       select = select_spec(
#'         label = "Select variable:",
#'         choices = variable_choices(data[["CO2"]]),
#'         selected = "uptake",
#'         multiple = FALSE,
#'         fixed = FALSE
#'       )
#'     ),
#'     row_facet = data_extract_spec(
#'       dataname = "CO2",
#'       select = select_spec(
#'         label = "Select variable:",
#'         choices = variable_choices(data[["CO2"]]),
#'         selected = "Type",
#'         fixed = FALSE
#'       )
#'     ),
#'     col_facet = data_extract_spec(
#'       dataname = "CO2",
#'       select = select_spec(
#'         label = "Select variable:",
#'         choices = variable_choices(data[["CO2"]]),
#'         selected = "Treatment",
#'         fixed = FALSE
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#' # CDISC data example
#' data <- teal_data()
#' data <- within(data, {
#'   require(nestcolor)
#'   ADSL <- teal.data::rADSL
#' })
#' join_keys(data) <- default_cdisc_join_keys[names(data)]
#'
#' app <- init(
#'   data = data,
#'   modules = tm_g_bivariate(
#'     x = data_extract_spec(
#'       dataname = "ADSL",
#'       select = select_spec(
#'         label = "Select variable:",
#'         choices = variable_choices(data[["ADSL"]]),
#'         selected = "AGE",
#'         fixed = FALSE
#'       )
#'     ),
#'     y = data_extract_spec(
#'       dataname = "ADSL",
#'       select = select_spec(
#'         label = "Select variable:",
#'         choices = variable_choices(data[["ADSL"]]),
#'         selected = "SEX",
#'         multiple = FALSE,
#'         fixed = FALSE
#'       )
#'     ),
#'     row_facet = data_extract_spec(
#'       dataname = "ADSL",
#'       select = select_spec(
#'         label = "Select variable:",
#'         choices = variable_choices(data[["ADSL"]]),
#'         selected = "ARM",
#'         fixed = FALSE
#'       )
#'     ),
#'     col_facet = data_extract_spec(
#'       dataname = "ADSL",
#'       select = select_spec(
#'         label = "Select variable:",
#'         choices = variable_choices(data[["ADSL"]]),
#'         selected = "COUNTRY",
#'         fixed = FALSE
#'       )
#'     )
#'   )
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
#' }
#'
#' @export
#'
tm_g_bivariate <- function(label = "Bivariate Plots",
                           x = picks(
                             datasets(),
                             variables(
                               choices = tidyselect::where(is.numeric) |
                                 teal.transform::is_categorical(min.len = 2, max.len = 10),
                               selected = 1
                             )
                           ),
                           y = picks(
                             datasets(),
                             variables(
                               choices = tidyselect::where(is.numeric) |
                                 teal.transform::is_categorical(min.len = 2, max.len = 10),
                               selected = 2
                             )
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
  UseMethod("tm_g_bivariate", x)
}

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
        teal.transform::module_input_ui(id = ns("x"), spec = x)
      ),
      teal::teal_nav_item(
        label = tags$strong("Y variable"),
        teal.transform::module_input_ui(id = ns("y"), spec = y)
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
            teal.transform::module_input_ui(id = ns("row_facet"), spec = row_facet),
            checkboxInput(ns("free_x_scales"), "free x scales", value = free_x_scales)
          )
        )
      },
      if (!is.null(col_facet)) {
        teal::teal_nav_item(
          tags$div(
            tags$strong("Column facetting variable"),
            teal.transform::module_input_ui(id = ns("col_facet"), spec = col_facet),
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
                teal.transform::module_input_ui(id = ns("color"), spec = color), # label = "Outline color by variable"
                teal.transform::module_input_ui(id = ns("fill"), spec = fill), # label = "Outline color by variable"
                tags$div(
                  id = ns("size_settings"),
                  teal.transform::module_input_ui(id = ns("size"), spec = size) # label = "Size of points by variable (only if x and y are numeric)"
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
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
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
    selectors <- teal.transform::module_input_srv(
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

    anl_merged_q <- reactive({
      validate_input(
        inputId = c("x-variables-selected", "y-variables-selected"),
        condition = length(selectors$x()$variables$selected) && length(selectors$y()$variables$selected),
        message = "Please select at least one of x-variable or y-variable"
      )
      if (!is.null(col_facet) && !is.null(row_facet)) {
        validate_input(
          inputId = c("row_facet-variables-selected", "col_facet-variables-selected"),
          condition = length(selectors$row_facet()$variables$selected) &&
            length(selectors$col_facet()$variables$selected) &&
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
      obj %>%
        teal.code::eval_code('library("ggplot2");library("dplyr")') %>%
        teal.transform::qenv_merge_selectors(selectors = selectors)
    })

    output_q <- reactive(label = "make bivariateplot", {
      req(anl_merged_q())
      logger::log_debug("Plotting bivariate")
      merged <- anl_merged_q()[["merged"]]
      teal::validate_has_data(merged, 3)

      x_name <- map_merged(selectors)$x$variables
      y_name <- map_merged(selectors)$y$variables
      row_facet_name <- map_merged(selectors)$row_facet$variables
      col_facet_name <- map_merged(selectors)$col_facet$variables
      color_name <- map_merged(selectors)$color$variables
      fill_name <- map_merged(selectors)$fill$variables
      size_name <- map_merged(selectors)$size$variables

      use_density <- input$use_density == "density"
      free_x_scales <- input$free_x_scales
      free_y_scales <- input$free_y_scales
      ggtheme <- input$ggtheme
      rotate_xaxis_labels <- input$rotate_xaxis_labels
      swap_axes <- input$swap_axes

      is_scatterplot <- all(vapply(merged[c(x_name, y_name)], is.numeric, logical(1))) &&
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

      teal::validate_has_data(merged[, c(x_name, y_name), drop = FALSE], 3, complete = TRUE, allow_inf = FALSE)

      cl <- bivariate_plot_call(
        data_name = "merged",
        x = x_name,
        y = y_name,
        x_class = ifelse(length(x_name), class(merged[[x_name]]), "NULL"),
        y_class = ifelse(length(y_name), class(merged[[y_name]]), "NULL"),
        x_label = varname_w_label(x_name, merged),
        y_label = varname_w_label(y_name, merged),
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

      obj <- anl_merged_q()
      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Plot")
      teal.code::eval_code(obj, substitute(expr = plot <- cl, env = list(cl = cl)))
    })

    decorated_output_q_facets <- srv_decorate_teal_data(
      "decorator",
      data = output_q,
      decorators = select_decorators(decorators, "plot"),
      expr = reactive({
        merged <- anl_merged_q()[["merged"]]
        row_facet_name <- map_merged(selectors)$row_facet$variables
        col_facet_name <- map_merged(selectors)$col_facet$variables

        # Add labels to facets
        nulled_row_facet_name <- varname_w_label(row_facet_name, merged)
        nulled_col_facet_name <- varname_w_label(col_facet_name, merged)
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

    decorated_output_dims_q <- set_chunk_dims(pws, decorated_output_q_facets)

    # Render R code.

    source_code_r <- reactive(teal.code::get_code(req(decorated_output_dims_q())))

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = "Bivariate Plot"
    )
    decorated_output_dims_q
  })
}

# Get Substituted ggplot call
bivariate_plot_call <- function(data_name,
                                x = NULL,
                                y = NULL,
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
                                ggplot2_args = teal.widgets::ggplot2_args()) {
  supported_types <- c("NULL", "numeric", "integer", "factor", "character", "logical", "ordered")
  validate(need(x_class %in% supported_types, paste0("Data type '", x_class, "' is not supported.")))
  validate(need(y_class %in% supported_types, paste0("Data type '", y_class, "' is not supported.")))


  if (is.null(x)) {
    x <- x_label <- "-"
  } else {
    x <- if (is.call(x)) x else as.name(x)
  }
  if (is.null(y)) {
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

# Create ggplot part of plot call
# Due to the type of the x and y variable the plot type is chosen
bivariate_ggplot_call <- function(x_class,
                                  y_class,
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
                                  ggplot2_args = teal.widgets::ggplot2_args()) {
  x_class <- switch(x_class,
    "character" = ,
    "ordered" = ,
    "logical" = ,
    "factor" = "factor",
    "integer" = ,
    "numeric" = "numeric",
    "NULL" = "NULL",
    stop("unsupported x_class: ", x_class)
  )
  y_class <- switch(y_class,
    "character" = ,
    "ordered" = ,
    "logical" = ,
    "factor" = "factor",
    "integer" = ,
    "numeric" = "numeric",
    "NULL" = "NULL",
    stop("unsupported y_class: ", y_class)
  )

  if (all(c(x_class, y_class) == "NULL")) {
    stop("either x or y is required")
  }

  reduce_plot_call <- function(...) {
    args <- Filter(Negate(is.null), list(...))
    Reduce(function(x, y) call("+", x, y), args)
  }

  plot_call <- substitute(ggplot2::ggplot(data_name), env = list(data_name = as.name(data_name)))

  # Single data plots
  if (x_class == "numeric" && y_class == "NULL") {
    plot_call <- reduce_plot_call(plot_call, substitute(ggplot2::aes(x = xval), env = list(xval = x)))

    if (freq) {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(ggplot2::geom_histogram(bins = 30)),
        quote(ggplot2::ylab("Frequency"))
      )
    } else {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(ggplot2::geom_histogram(bins = 30, ggplot2::aes(y = ggplot2::after_stat(density)))),
        quote(ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(density)))),
        quote(ggplot2::ylab("Density"))
      )
    }
  } else if (x_class == "NULL" && y_class == "numeric") {
    plot_call <- reduce_plot_call(plot_call, substitute(ggplot2::aes(x = yval), env = list(yval = y)))

    if (freq) {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(ggplot2::geom_histogram(bins = 30)),
        quote(ggplot2::ylab("Frequency"))
      )
    } else {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(ggplot2::geom_histogram(bins = 30, ggplot2::aes(y = ggplot2::after_stat(density)))),
        quote(ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(density)))),
        quote(ggplot2::ylab("Density"))
      )
    }
  } else if (x_class == "factor" && y_class == "NULL") {
    plot_call <- reduce_plot_call(plot_call, substitute(ggplot2::aes(x = xval), env = list(xval = x)))

    if (freq) {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(ggplot2::geom_bar()),
        quote(ggplot2::ylab("Frequency"))
      )
    } else {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(ggplot2::geom_bar(ggplot2::aes(y = ggplot2::after_stat(prop), group = 1))),
        quote(ggplot2::ylab("Fraction"))
      )
    }
  } else if (x_class == "NULL" && y_class == "factor") {
    plot_call <- reduce_plot_call(plot_call, substitute(ggplot2::aes(x = yval), env = list(yval = y)))

    if (freq) {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(ggplot2::geom_bar()),
        quote(ggplot2::ylab("Frequency"))
      )
    } else {
      plot_call <- reduce_plot_call(
        plot_call,
        quote(ggplot2::geom_bar(ggplot2::aes(y = ggplot2::after_stat(prop), group = 1))),
        quote(ggplot2::ylab("Fraction"))
      )
    }
    # Numeric Plots
  } else if (x_class == "numeric" && y_class == "numeric") {
    plot_call <- reduce_plot_call(
      plot_call,
      substitute(ggplot2::aes(x = xval, y = yval), env = list(xval = x, yval = y)),
      # pch = 21 for consistent coloring behaviour b/w all geoms (outline and fill properties)
      `if`(
        !is.null(size),
        substitute(
          ggplot2::geom_point(alpha = alphaval, size = sizeval, pch = 21),
          env = list(alphaval = alpha, sizeval = size)
        ),
        substitute(
          ggplot2::geom_point(alpha = alphaval, pch = 21),
          env = list(alphaval = alpha)
        )
      )
    )
  } else if ((x_class == "numeric" && y_class == "factor") || (x_class == "factor" && y_class == "numeric")) {
    plot_call <- reduce_plot_call(
      plot_call,
      substitute(ggplot2::aes(x = xval, y = yval), env = list(xval = x, yval = y)),
      quote(ggplot2::geom_boxplot())
    )
    # Factor and character plots
  } else if (x_class == "factor" && y_class == "factor") {
    plot_call <- reduce_plot_call(
      plot_call,
      substitute(
        ggmosaic::geom_mosaic(aes(x = ggmosaic::product(xval), fill = yval), na.rm = TRUE),
        env = list(xval = x, yval = y)
      )
    )
  } else {
    stop("x y type combination not allowed")
  }

  labs_base <- if (x_class == "NULL") {
    list(x = substitute(ylab, list(ylab = ylab)))
  } else if (y_class == "NULL") {
    list(x = substitute(xlab, list(xlab = xlab)))
  } else {
    list(
      x = substitute(xlab, list(xlab = xlab)),
      y = substitute(ylab, list(ylab = ylab))
    )
  }

  dev_ggplot2_args <- teal.widgets::ggplot2_args(labs = labs_base)

  if (rotate_xaxis_labels) {
    dev_ggplot2_args$theme <- list(axis.text.x = quote(ggplot2::element_text(angle = 45, hjust = 1)))
  }

  all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
    user_plot = ggplot2_args,
    module_plot = dev_ggplot2_args
  )

  parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(all_ggplot2_args, ggtheme = theme)

  plot_call <- reduce_plot_call(
    plot_call,
    parsed_ggplot2_args$labs,
    parsed_ggplot2_args$ggtheme,
    parsed_ggplot2_args$theme
  )

  if (swap_axes) {
    plot_call <- reduce_plot_call(plot_call, quote(coord_flip()))
  }

  plot_call
}

# Create facet call
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

  if (identical(row_facet, character(0)) && identical(col_facet, character(0))) {
    NULL
  } else if (!identical(row_facet, character(0)) && !identical(col_facet, character(0))) {
    call(
      "facet_grid",
      rows = call_fun_dots("vars", row_facet),
      cols = call_fun_dots("vars", col_facet),
      scales = scales
    )
  } else if (identical(row_facet, character(0)) && !identical(col_facet, character(0))) {
    call("facet_grid", cols = call_fun_dots("vars", col_facet), scales = scales)
  } else if (!identical(row_facet, character(0)) && identical(col_facet, character(0))) {
    call("facet_grid", rows = call_fun_dots("vars", row_facet), scales = scales)
  }
}

coloring_ggplot_call <- function(colour,
                                 fill,
                                 size,
                                 is_point = FALSE) {
  if (
    !identical(colour, character(0)) &&
      !identical(fill, character(0)) &&
      is_point &&
      !identical(size, character(0))
  ) {
    substitute(
      expr = ggplot2::aes(colour = colour_name, fill = fill_name, size = size_name),
      env = list(colour_name = as.name(colour), fill_name = as.name(fill), size_name = as.name(size))
    )
  } else if (
    identical(colour, character(0)) &&
      !identical(fill, character(0)) &&
      is_point &&
      identical(size, character(0))
  ) {
    substitute(expr = ggplot2::aes(fill = fill_name), env = list(fill_name = as.name(fill)))
  } else if (
    !identical(colour, character(0)) &&
      !identical(fill, character(0)) &&
      (!is_point || identical(size, character(0)))
  ) {
    substitute(
      expr = ggplot2::aes(colour = colour_name, fill = fill_name),
      env = list(colour_name = as.name(colour), fill_name = as.name(fill))
    )
  } else if (
    !identical(colour, character(0)) &&
      identical(fill, character(0)) &&
      (!is_point || identical(size, character(0)))
  ) {
    substitute(expr = ggplot2::aes(colour = colour_name), env = list(colour_name = as.name(colour)))
  } else if (
    identical(colour, character(0)) &&
      !identical(fill, character(0)) &&
      (!is_point || identical(size, character(0)))
  ) {
    substitute(expr = ggplot2::aes(fill = fill_name), env = list(fill_name = as.name(fill)))
  } else if (
    identical(colour, character(0)) &&
      identical(fill, character(0)) &&
      is_point &&
      !identical(size, character(0))
  ) {
    substitute(expr = ggplot2::aes(size = size_name), env = list(size_name = as.name(size)))
  } else if (
    !identical(colour, character(0)) &&
      identical(fill, character(0)) &&
      is_point &&
      !identical(size, character(0))
  ) {
    substitute(
      expr = ggplot2::aes(colour = colour_name, size = size_name),
      env = list(colour_name = as.name(colour), size_name = as.name(size))
    )
  } else if (
    identical(colour, character(0)) &&
      !identical(fill, character(0)) &&
      is_point &&
      !identical(size, character(0))
  ) {
    substitute(
      expr = ggplot2::aes(colour = colour_name, fill = fill_name, size = size_name),
      env = list(colour_name = as.name(fill), fill_name = as.name(fill), size_name = as.name(size))
    )
  } else {
    NULL
  }
}
