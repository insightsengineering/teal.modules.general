#' `teal` module: Scatterplot
#'
#' Generates a customizable scatterplot using `ggplot2`.
#' This module allows users to select variables for the x and y axes,
#' color and size encodings, faceting options, and more. It supports log transformations,
#' trend line additions, and dynamic adjustments of point opacity and size through UI controls.
#'
#' @note For more examples, please see the vignette "Using scatterplot" via
#'   `vignette("using-scatterplot", package = "teal.modules.general")`.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param x (`data_extract_spec` or `list` of multiple `data_extract_spec`) Specifies
#' variable names selected to plot along the x-axis by default.
#' @param y (`data_extract_spec` or `list` of multiple `data_extract_spec`) Specifies
#' variable names selected to plot along the y-axis by default.
#' @param color_by (`data_extract_spec` or `list` of multiple `data_extract_spec`) optional,
#' defines the color encoding. If `NULL` then no color encoding option will be displayed.
#' @param size_by (`data_extract_spec` or `list` of multiple `data_extract_spec`) optional,
#' defines the point size encoding. If `NULL` then no size encoding option will be displayed.
#' @param row_facet (`data_extract_spec` or `list` of multiple `data_extract_spec`) optional,
#' specifies the variable(s) for faceting rows.
#' @param col_facet (`data_extract_spec` or `list` of multiple `data_extract_spec`) optional,
#' specifies the variable(s) for faceting columns.
#' @param shape (`character`) optional, character vector with the names of the
#' shape, e.g. `c("triangle", "square", "circle")`. It defaults to `shape_names`. This is a complete list from
#' `vignette("ggplot2-specs", package="ggplot2")`.
#' @param max_deg (`integer`) optional, maximum degree for the polynomial trend line. Must not be less than 1.
#' @param table_dec (`integer`) optional, number of decimal places used to round numeric values in the table.
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
#' tm_g_scatterplot(
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
# nolint start: line_length_linter.
#' @examples
# nolint end: line_length_linter.
#' # general data example
#' data <- teal_data()
#' data <- within(data, {
#'   require(nestcolor)
#'   CO2 <- CO2
#' })
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_scatterplot(
#'       label = "Scatterplot Choices",
#'       x = data_extract_spec(
#'         dataname = "CO2",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["CO2"]], c("conc", "uptake")),
#'           selected = "conc",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "CO2",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["CO2"]], c("conc", "uptake")),
#'           selected = "uptake",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       color_by = data_extract_spec(
#'         dataname = "CO2",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(
#'             data[["CO2"]],
#'             c("Plant", "Type", "Treatment", "conc", "uptake")
#'           ),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       size_by = data_extract_spec(
#'         dataname = "CO2",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["CO2"]], c("conc", "uptake")),
#'           selected = "uptake",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       row_facet = data_extract_spec(
#'         dataname = "CO2",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["CO2"]], c("Plant", "Type", "Treatment")),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "CO2",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["CO2"]], c("Plant", "Type", "Treatment")),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
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
# nolint start: line_length_linter.
#' @examples
# nolint end: line_length_linter.
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
#'   modules = modules(
#'     tm_g_scatterplot(
#'       label = "Scatterplot Choices",
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1", "BMRKR2")),
#'           selected = "AGE",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1", "BMRKR2")),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       color_by = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(
#'             data[["ADSL"]],
#'             c("AGE", "BMRKR1", "BMRKR2", "RACE", "REGION1")
#'           ),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       size_by = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["ADSL"]], c("AGE", "BMRKR1")),
#'           selected = "AGE",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       row_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["ADSL"]], c("BMRKR2", "RACE", "REGION1")),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["ADSL"]], c("BMRKR2", "RACE", "REGION1")),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
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
                             ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
                             pre_output = NULL,
                             post_output = NULL,
                             table_dec = 4,
                             ggplot2_args = teal.widgets::ggplot2_args(),
                             transformators = list(),
                             decorators = list()) {
  message("Initializing tm_g_scatterplot")

  # Normalize the parameters
  if (inherits(x, "data_extract_spec")) x <- list(x)
  if (inherits(y, "data_extract_spec")) y <- list(y)
  if (inherits(color_by, "data_extract_spec")) color_by <- list(color_by)
  if (inherits(size_by, "data_extract_spec")) size_by <- list(size_by)
  if (inherits(row_facet, "data_extract_spec")) row_facet <- list(row_facet)
  if (inherits(col_facet, "data_extract_spec")) col_facet <- list(col_facet)
  if (is.double(max_deg)) max_deg <- as.integer(max_deg)

  # Start of assertions
  checkmate::assert_string(label)
  checkmate::assert_list(x, types = "data_extract_spec")
  checkmate::assert_list(y, types = "data_extract_spec")
  checkmate::assert_list(color_by, types = "data_extract_spec", null.ok = TRUE)
  checkmate::assert_list(size_by, types = "data_extract_spec", null.ok = TRUE)

  checkmate::assert_list(row_facet, types = "data_extract_spec", null.ok = TRUE)
  assert_single_selection(row_facet)

  checkmate::assert_list(col_facet, types = "data_extract_spec", null.ok = TRUE)
  assert_single_selection(col_facet)

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

  data_extract_list <- list(
    x = x,
    y = y,
    color_by = color_by,
    size_by = size_by,
    row_facet = row_facet,
    col_facet = col_facet
  )

  ans <- module(
    label = label,
    server = srv_g_scatterplot,
    ui = ui_g_scatterplot,
    ui_args = args,
    server_args = c(
      data_extract_list,
      list(
        plot_height = plot_height,
        plot_width = plot_width,
        table_dec = table_dec,
        ggplot2_args = ggplot2_args,
        decorators = decorators
      )
    ),
    transformators = transformators,
    datanames = teal.transform::get_extract_datanames(data_extract_list)
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the scatterplot module
ui_g_scatterplot <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  is_single_dataset_value <- teal.transform::is_single_dataset(
    args$x, args$y, args$color_by, args$size_by, args$row_facet, args$col_facet
  )

  tagList(
    include_css_files("custom"),
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        teal.widgets::plot_with_settings_ui(id = ns("scatter_plot")),
        tags$h1(tags$strong("Selected points:"), class = "text-center font-150p"),
        teal.widgets::get_dt_rows(ns("data_table"), ns("data_table_rows")),
        DT::dataTableOutput(ns("data_table"), width = "100%")
      ),
      encoding = tags$div(
        ### Reporter
        teal.reporter::simple_reporter_ui(ns("simple_reporter")),
        ###
        tags$label("Encodings", class = "text-primary"),
        teal.transform::datanames_input(args[c("x", "y", "color_by", "size_by", "row_facet", "col_facet")]),
        teal.transform::data_extract_ui(
          id = ns("x"),
          label = "X variable",
          data_extract_spec = args$x,
          is_single_dataset = is_single_dataset_value
        ),
        checkboxInput(ns("log_x"), "Use log transformation", value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("log_x"), "'] == true"),
          radioButtons(
            ns("log_x_base"),
            label = NULL,
            inline = TRUE,
            choices = c("Natural" = "log", "Base 10" = "log10", "Base 2" = "log2")
          )
        ),
        teal.transform::data_extract_ui(
          id = ns("y"),
          label = "Y variable",
          data_extract_spec = args$y,
          is_single_dataset = is_single_dataset_value
        ),
        checkboxInput(ns("log_y"), "Use log transformation", value = FALSE),
        conditionalPanel(
          condition = paste0("input['", ns("log_y"), "'] == true"),
          radioButtons(
            ns("log_y_base"),
            label = NULL,
            inline = TRUE,
            choices = c("Natural" = "log", "Base 10" = "log10", "Base 2" = "log2")
          )
        ),
        if (!is.null(args$color_by)) {
          teal.transform::data_extract_ui(
            id = ns("color_by"),
            label = "Color by variable",
            data_extract_spec = args$color_by,
            is_single_dataset = is_single_dataset_value
          )
        },
        if (!is.null(args$size_by)) {
          teal.transform::data_extract_ui(
            id = ns("size_by"),
            label = "Size by variable",
            data_extract_spec = args$size_by,
            is_single_dataset = is_single_dataset_value
          )
        },
        if (!is.null(args$row_facet)) {
          teal.transform::data_extract_ui(
            id = ns("row_facet"),
            label = "Row facetting",
            data_extract_spec = args$row_facet,
            is_single_dataset = is_single_dataset_value
          )
        },
        if (!is.null(args$col_facet)) {
          teal.transform::data_extract_ui(
            id = ns("col_facet"),
            label = "Column facetting",
            data_extract_spec = args$col_facet,
            is_single_dataset = is_single_dataset_value
          )
        },
        ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(args$decorators, "plot")),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = "Plot settings",
            teal.widgets::optionalSliderInputValMinMax(ns("alpha"), "Opacity:", args$alpha, ticks = FALSE),
            teal.widgets::optionalSelectInput(
              inputId = ns("shape"),
              label = "Points shape:",
              choices = args$shape,
              selected = args$shape[1],
              multiple = FALSE
            ),
            colourpicker::colourInput(ns("color"), "Points color:", "black"),
            teal.widgets::optionalSliderInputValMinMax(ns("size"), "Points size:", args$size, ticks = FALSE, step = .1),
            checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = args$rotate_xaxis_labels),
            checkboxInput(ns("add_density"), "Add marginal density", value = FALSE),
            checkboxInput(ns("rug_plot"), "Include rug plot", value = FALSE),
            checkboxInput(ns("show_count"), "Show N (number of observations)", value = FALSE),
            shinyjs::hidden(helpText(id = ns("line_msg"), "Trendline needs numeric X and Y variables")),
            teal.widgets::optionalSelectInput(ns("smoothing_degree"), "Smoothing degree", seq_len(args$max_deg)),
            shinyjs::hidden(teal.widgets::optionalSelectInput(ns("color_sub"), label = "", multiple = TRUE)),
            teal.widgets::optionalSliderInputValMinMax(ns("ci"), "Confidence", c(.95, .8, .99), ticks = FALSE),
            shinyjs::hidden(checkboxInput(ns("show_form"), "Show formula", value = TRUE)),
            shinyjs::hidden(checkboxInput(ns("show_r2"), "Show adj-R Squared", value = TRUE)),
            uiOutput(ns("num_na_removed")),
            tags$div(
              id = ns("label_pos"),
              tags$div(tags$strong("Stats position")),
              tags$div(class = "inline-block w-10", helpText("Left")),
              tags$div(
                class = "inline-block w-70",
                teal.widgets::optionalSliderInput(
                  ns("pos"),
                  label = NULL,
                  min = 0, max = 1, value = .99, ticks = FALSE, step = .01
                )
              ),
              tags$div(class = "inline-block w-10", helpText("Right"))
            ),
            teal.widgets::optionalSliderInput(
              ns("label_size"), "Stats font size",
              min = 3, max = 10, value = 5, ticks = FALSE, step = .1
            ),
            if (!is.null(args$row_facet) || !is.null(args$col_facet)) {
              checkboxInput(ns("free_scales"), "Free scales", value = FALSE)
            },
            selectInput(
              inputId = ns("ggtheme"),
              label = "Theme (by ggplot):",
              choices = ggplot_themes,
              selected = args$ggtheme,
              multiple = FALSE
            )
          )
        )
      ),
      forms = tagList(
        teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
      ),
      pre_output = args$pre_output,
      post_output = args$post_output
    )
  )
}

# Server function for the scatterplot module
srv_g_scatterplot <- function(id,
                              data,
                              reporter,
                              filter_panel_api,
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
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    data_extract <- list(
      x = x,
      y = y,
      color_by = color_by,
      size_by = size_by,
      row_facet = row_facet,
      col_facet = col_facet
    )

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
        x = ~ if (length(.) != 1) "Please select exactly one x var.",
        y = ~ if (length(.) != 1) "Please select exactly one y var.",
        color_by = ~ if (length(.) > 1) "There cannot be more than 1 color variable.",
        size_by = ~ if (length(.) > 1) "There cannot be more than 1 size variable.",
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
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })
    iv_facet <- shinyvalidate::InputValidator$new()
    iv_facet$add_rule("add_density", ~ if (
      isTRUE(.) &&
        (
          length(selector_list()$row_facet()$select) > 0L ||
            length(selector_list()$col_facet()$select) > 0L
        )
    ) {
      "Cannot add marginal density when Row or Column facetting has been selected"
    })
    iv_facet$enable()

    anl_merged_input <- teal.transform::merge_expression_srv(
      selector_list = selector_list,
      datasets = data,
      merge_function = "dplyr::inner_join"
    )
    qenv <- reactive(
      teal.code::eval_code(data(), 'library("ggplot2");library("dplyr")') # nolint quotes
    )

    anl_merged_q <- reactive({
      req(anl_merged_input())
      qenv() %>%
        teal.code::eval_code(as.expression(anl_merged_input()$expr)) %>%
        teal.code::eval_code(quote(ANL)) # used to display table when running show-r-code code
    })

    merged <- list(
      anl_input_r = anl_merged_input,
      anl_q_r = anl_merged_q
    )

    trend_line_is_applicable <- reactive({
      ANL <- merged$anl_q_r()[["ANL"]]
      x_var <- as.vector(merged$anl_input_r()$columns_source$x)
      y_var <- as.vector(merged$anl_input_r()$columns_source$y)
      length(x_var) > 0 && length(y_var) > 0 && is.numeric(ANL[[x_var]]) && is.numeric(ANL[[y_var]])
    })

    add_trend_line <- reactive({
      smoothing_degree <- as.integer(input$smoothing_degree)
      trend_line_is_applicable() && length(smoothing_degree) > 0
    })

    if (!is.null(color_by)) {
      observeEvent(
        eventExpr = merged$anl_input_r()$columns_source$color_by,
        handlerExpr = {
          color_by_var <- as.vector(merged$anl_input_r()$columns_source$color_by)
          if (length(color_by_var) > 0) {
            shinyjs::hide("color")
          } else {
            shinyjs::show("color")
          }
        }
      )
    }

    output$num_na_removed <- renderUI({
      if (add_trend_line()) {
        ANL <- merged$anl_q_r()[["ANL"]]
        x_var <- as.vector(merged$anl_input_r()$columns_source$x)
        y_var <- as.vector(merged$anl_input_r()$columns_source$y)
        if ((num_total_na <- nrow(ANL) - nrow(stats::na.omit(ANL[, c(x_var, y_var)]))) > 0) {
          tags$div(paste(num_total_na, "row(s) with missing values were removed"), tags$hr())
        }
      }
    })

    observeEvent(
      eventExpr = merged$anl_input_r()$columns_source[c("col_facet", "row_facet")],
      handlerExpr = {
        if (
          length(merged$anl_input_r()$columns_source$col_facet) == 0 &&
            length(merged$anl_input_r()$columns_source$row_facet) == 0
        ) {
          shinyjs::hide("free_scales")
        } else {
          shinyjs::show("free_scales")
        }
      }
    )

    output_q <- reactive({
      teal::validate_inputs(iv_r(), iv_facet)

      ANL <- merged$anl_q_r()[["ANL"]]

      x_var <- as.vector(merged$anl_input_r()$columns_source$x)
      y_var <- as.vector(merged$anl_input_r()$columns_source$y)
      color_by_var <- as.vector(merged$anl_input_r()$columns_source$color_by)
      size_by_var <- as.vector(merged$anl_input_r()$columns_source$size_by)
      row_facet_name <- if (length(merged$anl_input_r()$columns_source$row_facet) == 0) {
        character(0)
      } else {
        as.vector(merged$anl_input_r()$columns_source$row_facet)
      }
      col_facet_name <- if (length(merged$anl_input_r()$columns_source$col_facet) == 0) {
        character(0)
      } else {
        as.vector(merged$anl_input_r()$columns_source$col_facet)
      }
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

      validate(need(
        length(row_facet_name) == 0 || inherits(ANL[[row_facet_name]], c("character", "factor", "Date", "integer")),
        "`Row facetting` variable must be of class `character`, `factor`, `Date`, or `integer`"
      ))
      validate(need(
        length(col_facet_name) == 0 || inherits(ANL[[col_facet_name]], c("character", "factor", "Date", "integer")),
        "`Column facetting` variable must be of class `character`, `factor`, `Date`, or `integer`"
      ))

      if (add_density && length(color_by_var) > 0) {
        validate(need(
          !is.numeric(ANL[[color_by_var]]),
          "Marginal plots cannot be produced when the points are colored by numeric variables.
        \n Uncheck the 'Add marginal density' checkbox to display the plot."
        ))
        validate(need(
          !(
            inherits(ANL[[color_by_var]], "Date") ||
              inherits(ANL[[color_by_var]], "POSIXct") ||
              inherits(ANL[[color_by_var]], "POSIXlt")
          ),
          "Marginal plots cannot be produced when the points are colored by Date or POSIX variables.
        \n Uncheck the 'Add marginal density' checkbox to display the plot."
        ))
      }

      teal::validate_has_data(ANL[, c(x_var, y_var)], 1, complete = TRUE, allow_inf = FALSE)

      if (log_x) {
        validate(
          need(
            is.numeric(ANL[[x_var]]) && all(
              ANL[[x_var]] > 0 | is.na(ANL[[x_var]])
            ),
            "X variable can only be log transformed if variable is numeric and all values are positive."
          )
        )
      }
      if (log_y) {
        validate(
          need(
            is.numeric(ANL[[y_var]]) && all(
              ANL[[y_var]] > 0 | is.na(ANL[[y_var]])
            ),
            "Y variable can only be log transformed if variable is numeric and all values are positive."
          )
        )
      }

      facet_cl <- facet_ggplot_call(
        row_facet_name,
        col_facet_name,
        free_x_scales = isTRUE(input$free_scales),
        free_y_scales = isTRUE(input$free_scales)
      )

      point_sizes <- if (length(size_by_var) > 0) {
        validate(need(is.numeric(ANL[[size_by_var]]), "Variable to size by must be numeric"))
        substitute(
          expr = size * ANL[[size_by_var]] / max(ANL[[size_by_var]], na.rm = TRUE),
          env = list(size = size, size_by_var = size_by_var)
        )
      } else {
        size
      }

      plot_q <- merged$anl_q_r()

      if (log_x) {
        log_x_fn <- input$log_x_base
        plot_q <- teal.code::eval_code(
          object = plot_q,
          code = substitute(
            expr = ANL[, log_x_var] <- log_x_fn(ANL[, x_var]),
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
            expr = ANL[, log_y_var] <- log_y_fn(ANL[, y_var]),
            env = list(
              y_var = y_var,
              log_y_fn = as.name(log_y_fn),
              log_y_var = paste0(log_y_fn, "_", y_var)
            )
          )
        )
      }

      pre_pro_anl <- if (input$show_count) {
        paste0(
          "ANL %>% dplyr::group_by(",
          paste(
            c(
              if (length(color_by_var) > 0 && inherits(ANL[[color_by_var]], c("factor", "character"))) color_by_var,
              row_facet_name,
              col_facet_name
            ),
            collapse = ", "
          ),
          ") %>% dplyr::mutate(n = dplyr::n()) %>% dplyr::ungroup()"
        )
      } else {
        "ANL"
      }

      plot_call <- substitute(expr = pre_pro_anl %>% ggplot2::ggplot(), env = list(pre_pro_anl = str2lang(pre_pro_anl)))

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
          if (nrow(ANL) - nrow(stats::na.omit(ANL[, c(x_var, y_var)])) > 0) {
            plot_q <- teal.code::eval_code(
              plot_q,
              substitute(
                expr = ANL <- dplyr::filter(ANL, !is.na(x_var) & !is.na(y_var)),
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

      if (!is.null(facet_cl)) {
        plot_call <- substitute(expr = plot_call + facet_cl, env = list(plot_call = plot_call, facet_cl = facet_cl))
      }

      y_label <- varname_w_label(
        y_var,
        ANL,
        prefix = if (log_y) paste(log_y_fn, "(") else NULL,
        suffix = if (log_y) ")" else NULL
      )
      x_label <- varname_w_label(
        x_var,
        ANL,
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

      teal.code::eval_code(plot_q, plot_call)
    })

    decorated_output_plot_q <- srv_decorate_teal_data(
      id = "decorator",
      data = output_q,
      decorators = select_decorators(decorators, "plot"),
      expr = print(plot)
    )

    plot_r <- reactive(req(decorated_output_plot_q())[["plot"]])

    # Insert the plot into a plot_with_settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "scatter_plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
      brushing = TRUE
    )

    output$data_table <- DT::renderDataTable({
      plot_brush <- pws$brush()

      if (!is.null(plot_brush)) {
        validate(need(!input$add_density, "Brushing feature is currently not supported when plot has marginal density"))
      }

      merged_data <- isolate(output_q()[["ANL"]])

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

    # Render R code.
    source_code_r <- reactive(teal.code::get_code(req(decorated_output_plot_q())))

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = "R Code for scatterplot"
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Scatter Plot",
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
