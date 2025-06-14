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
                           ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
                           ggplot2_args = teal.widgets::ggplot2_args(),
                           pre_output = NULL,
                           post_output = NULL,
                           transformators = list(),
                           decorators = list()) {
  message("Initializing tm_g_bivariate")

  # Normalize the parameters
  if (inherits(x, "data_extract_spec")) x <- list(x)
  if (inherits(y, "data_extract_spec")) y <- list(y)
  if (inherits(row_facet, "data_extract_spec")) row_facet <- list(row_facet)
  if (inherits(col_facet, "data_extract_spec")) col_facet <- list(col_facet)
  if (inherits(color, "data_extract_spec")) color <- list(color)
  if (inherits(fill, "data_extract_spec")) fill <- list(fill)
  if (inherits(size, "data_extract_spec")) size <- list(size)

  # Start of assertions
  checkmate::assert_string(label)

  checkmate::assert_list(x, types = "data_extract_spec")
  assert_single_selection(x)

  checkmate::assert_list(y, types = "data_extract_spec")
  assert_single_selection(y)

  checkmate::assert_list(row_facet, types = "data_extract_spec", null.ok = TRUE)
  assert_single_selection(row_facet)

  checkmate::assert_list(col_facet, types = "data_extract_spec", null.ok = TRUE)
  assert_single_selection(col_facet)

  checkmate::assert_flag(facet)

  checkmate::assert_list(color, types = "data_extract_spec", null.ok = TRUE)
  assert_single_selection(color)

  checkmate::assert_list(fill, types = "data_extract_spec", null.ok = TRUE)
  assert_single_selection(fill)

  checkmate::assert_list(size, types = "data_extract_spec", null.ok = TRUE)
  assert_single_selection(size)

  checkmate::assert_flag(use_density)

  # Determines color, fill & size if they are not explicitly set
  checkmate::assert_flag(color_settings)
  if (color_settings) {
    if (is.null(color)) {
      color <- x
      color[[1]]$select <- teal.transform::select_spec(choices = color[[1]]$select$choices, selected = NULL)
    }
    if (is.null(fill)) {
      fill <- x
      fill[[1]]$select <- teal.transform::select_spec(choices = fill[[1]]$select$choices, selected = NULL)
    }
    if (is.null(size)) {
      size <- x
      size[[1]]$select <- teal.transform::select_spec(choices = size[[1]]$select$choices, selected = NULL)
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

  ans <- module(
    label = label,
    server = srv_g_bivariate,
    ui = ui_g_bivariate,
    ui_args = args,
    server_args = c(
      data_extract_list,
      list(plot_height = plot_height, plot_width = plot_width, ggplot2_args = ggplot2_args, decorators = decorators)
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the bivariate module
ui_g_bivariate <- function(id, ...) {
  args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    args$x, args$y, args$row_facet, args$col_facet, args$color, args$fill, args$size
  )

  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      tags$div(teal.widgets::plot_with_settings_ui(id = ns("myplot")))
    ),
    encoding = tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(args[c("x", "y", "row_facet", "col_facet", "color", "fill", "size")]),
      teal.transform::data_extract_ui(
        id = ns("x"),
        label = "X variable",
        data_extract_spec = args$x,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("y"),
        label = "Y variable",
        data_extract_spec = args$y,
        is_single_dataset = is_single_dataset_value
      ),
      conditionalPanel(
        condition =
          "$(\"button[data-id*='-x-dataset'][data-id$='-select']\").text() == '- Nothing selected - ' ||
          $(\"button[data-id*='-y-dataset'][data-id$='-select']\").text() == '- Nothing selected - ' ",
        shinyWidgets::radioGroupButtons(
          inputId = ns("use_density"),
          label = NULL,
          choices = c("frequency", "density"),
          selected = ifelse(args$use_density, "density", "frequency"),
          justified = TRUE
        )
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(args$decorators, "plot")),
      if (!is.null(args$row_facet) || !is.null(args$col_facet)) {
        tags$div(
          class = "data-extract-box",
          tags$br(),
          bslib::input_switch(
            id = ns("facetting"),
            label = "Facetting",
            value = args$facet
          ),
          conditionalPanel(
            condition = paste0("input['", ns("facetting"), "']"),
            tags$div(
              if (!is.null(args$row_facet)) {
                teal.transform::data_extract_ui(
                  id = ns("row_facet"),
                  label = "Row facetting variable",
                  data_extract_spec = args$row_facet,
                  is_single_dataset = is_single_dataset_value
                )
              },
              if (!is.null(args$col_facet)) {
                teal.transform::data_extract_ui(
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
        tags$div(
          class = "data-extract-box",
          tags$label("Color settings"),
          bslib::input_switch(
            id = ns("coloring"),
            label = "Color settings",
            value = TRUE
          ),
          conditionalPanel(
            condition = paste0("input['", ns("coloring"), "']"),
            tags$div(
              teal.transform::data_extract_ui(
                id = ns("color"),
                label = "Outline color by variable",
                data_extract_spec = args$color,
                is_single_dataset = is_single_dataset_value
              ),
              teal.transform::data_extract_ui(
                id = ns("fill"),
                label = "Fill color by variable",
                data_extract_spec = args$fill,
                is_single_dataset = is_single_dataset_value
              ),
              tags$div(
                id = ns("size_settings"),
                teal.transform::data_extract_ui(
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
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Plot settings",
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = args$rotate_xaxis_labels),
          checkboxInput(ns("swap_axes"), "Swap axes", value = args$swap_axes),
          selectInput(
            inputId = ns("ggtheme"),
            label = "Theme (by ggplot):",
            choices = ggplot_themes,
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
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    ),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

# Server function for the bivariate module
srv_g_bivariate <- function(id,
                            data,
                            reporter,
                            filter_panel_api,
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
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    ns <- session$ns

    data_extract <- list(
      x = x, y = y, row_facet = row_facet, col_facet = col_facet,
      color = color, fill = fill, size = size
    )

    rule_var <- function(other) {
      function(value) {
        othervalue <- selector_list()[[other]]()$select
        if (length(value) == 0L && length(othervalue) == 0L) {
          "Please select at least one of x-variable or y-variable"
        }
      }
    }
    rule_diff <- function(other) {
      function(value) {
        othervalue <- selector_list()[[other]]()[["select"]]
        if (!is.null(othervalue)) {
          if (identical(value, othervalue)) {
            "Row and column facetting variables must be different."
          }
        }
      }
    }

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = data_extract,
      datasets = data,
      select_validation_rule = list(
        x = rule_var("y"),
        y = rule_var("x"),
        row_facet = shinyvalidate::compose_rules(
          shinyvalidate::sv_optional(),
          rule_diff("col_facet")
        ),
        col_facet = shinyvalidate::compose_rules(
          shinyvalidate::sv_optional(),
          rule_diff("row_facet")
        )
      )
    )

    iv_r <- reactive({
      iv_facet <- shinyvalidate::InputValidator$new()
      iv_child <- teal.transform::compose_and_enable_validators(iv_facet, selector_list,
        validator_names = c("row_facet", "col_facet")
      )
      iv_child$condition(~ isTRUE(input$facetting))

      iv <- shinyvalidate::InputValidator$new()
      iv$add_validator(iv_child)
      teal.transform::compose_and_enable_validators(iv, selector_list, validator_names = c("x", "y"))
    })

    anl_merged_input <- teal.transform::merge_expression_srv(
      selector_list = selector_list,
      datasets = data
    )
    qenv <- reactive(
      teal.code::eval_code(data(), 'library("ggplot2");library("dplyr");library("teal.modules.general")') # nolint quotes
    )

    anl_merged_q <- reactive({
      req(anl_merged_input())
      qenv() %>%
        teal.code::eval_code(as.expression(anl_merged_input()$expr))
    })

    merged <- list(
      anl_input_r = anl_merged_input,
      anl_q_r = anl_merged_q
    )

    output_q <- reactive({
      teal::validate_inputs(iv_r())

      ANL <- merged$anl_q_r()[["ANL"]]
      teal::validate_has_data(ANL, 3)

      x_col_vec <- as.vector(merged$anl_input_r()$columns_source$x)
      x_name <- `if`(is.null(x_col_vec), character(0), x_col_vec)
      y_col_vec <- as.vector(merged$anl_input_r()$columns_source$y)
      y_name <- `if`(is.null(y_col_vec), character(0), y_col_vec)

      row_facet_name <- as.vector(merged$anl_input_r()$columns_source$row_facet)
      col_facet_name <- as.vector(merged$anl_input_r()$columns_source$col_facet)
      color_name <- if ("color" %in% names(merged$anl_input_r()$columns_source)) {
        as.vector(merged$anl_input_r()$columns_source$color)
      } else {
        character(0)
      }
      fill_name <- if ("fill" %in% names(merged$anl_input_r()$columns_source)) {
        as.vector(merged$anl_input_r()$columns_source$fill)
      } else {
        character(0)
      }
      size_name <- if ("size" %in% names(merged$anl_input_r()$columns_source)) {
        as.vector(merged$anl_input_r()$columns_source$size)
      } else {
        character(0)
      }

      use_density <- input$use_density == "density"
      free_x_scales <- input$free_x_scales
      free_y_scales <- input$free_y_scales
      ggtheme <- input$ggtheme
      rotate_xaxis_labels <- input$rotate_xaxis_labels
      swap_axes <- input$swap_axes

      is_scatterplot <- all(vapply(ANL[c(x_name, y_name)], is.numeric, logical(1))) &&
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

      teal::validate_has_data(ANL[, c(x_name, y_name), drop = FALSE], 3, complete = TRUE, allow_inf = FALSE)

      cl <- bivariate_plot_call(
        data_name = "ANL",
        x = x_name,
        y = y_name,
        x_class = ifelse(!identical(x_name, character(0)), class(ANL[[x_name]]), "NULL"),
        y_class = ifelse(!identical(y_name, character(0)), class(ANL[[y_name]]), "NULL"),
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

      facetting <- (isTRUE(input$facetting) && (!is.null(row_facet_name) || !is.null(col_facet_name)))

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

      teal.code::eval_code(merged$anl_q_r(), substitute(expr = plot <- cl, env = list(cl = cl)))
    })

    decorated_output_q_facets <- srv_decorate_teal_data(
      "decorator",
      data = output_q,
      decorators = select_decorators(decorators, "plot"),
      expr = reactive({
        ANL <- merged$anl_q_r()[["ANL"]]
        row_facet_name <- as.vector(merged$anl_input_r()$columns_source$row_facet)
        col_facet_name <- as.vector(merged$anl_input_r()$columns_source$col_facet)

        # Add labels to facets
        nulled_row_facet_name <- varname_w_label(row_facet_name, ANL)
        nulled_col_facet_name <- varname_w_label(col_facet_name, ANL)
        facetting <- (isTRUE(input$facetting) && (!is.null(row_facet_name) || !is.null(col_facet_name)))
        without_facet <- (is.null(nulled_row_facet_name) && is.null(nulled_col_facet_name)) || !facetting

        print_call <- if (without_facet) {
          quote(plot)
        } else {
          substitute(
            expr = {
              # Add facetting labels
              # optional: grid.newpage() # nolint: commented_code.
              # Prefixed with teal.modules.general as its usage will appear in "Show R code"
              plot <- teal.modules.general::add_facet_labels(
                plot,
                xfacet_label = nulled_col_facet_name,
                yfacet_label = nulled_row_facet_name
              )
              grid::grid.newpage()
              grid::grid.draw(plot)
            },
            env = list(nulled_col_facet_name = nulled_col_facet_name, nulled_row_facet_name = nulled_row_facet_name)
          )
        }
        print_call
      }),
      expr_is_reactive = TRUE
    )

    plot_r <- reactive(req(decorated_output_q_facets())[["plot"]])

    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    # Render R code.

    source_code_r <- reactive(teal.code::get_code(req(decorated_output_q_facets())))

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = "Bivariate Plot"
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Bivariate Plot",
          label = label,
          with_filter = with_filter,
          filter_panel_api = filter_panel_api
        )
        card$append_text("Plot", "header3")
        card$append_plot(plot_r(), dim = pws$dim())
        if (!comment == "") {
          card$append_text("Comment", "header3")
          card$append_text(comment)
        }
        card$append_src(source_code_r())
        card
      }
      teal.reporter::simple_reporter_srv("simple_reporter", reporter = reporter, card_fun = card_fun)
    }
    ###
  })
}

# Get Substituted ggplot call
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
                                ggplot2_args = teal.widgets::ggplot2_args()) {
  supported_types <- c("NULL", "numeric", "integer", "factor", "character", "logical", "ordered")
  validate(need(x_class %in% supported_types, paste0("Data type '", x_class, "' is not supported.")))
  validate(need(y_class %in% supported_types, paste0("Data type '", y_class, "' is not supported.")))


  if (identical(x, character(0))) {
    x <- x_label <- "-"
  } else {
    x <- if (is.call(x)) x else as.name(x)
  }
  if (identical(y, character(0))) {
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
