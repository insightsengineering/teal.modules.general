#' `teal` module: Response plot
#'
#' Generates a response plot for a given `response` and `x` variables.
#' This module allows users customize and add annotations to the plot depending
#' on the module's arguments.
#' It supports showing the counts grouped by other variable facets (by row / column),
#' swapping the coordinates, show count annotations and displaying the response plot
#' as frequency or density.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param response (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Which variable to use as the response.
#' You can define one fixed column by setting `fixed = TRUE` inside the `select_spec`.
#'
#' The `data_extract_spec` must not allow multiple selection in this case.
#' @param x (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Specifies which variable to use on the X-axis of the response plot.
#' Allow the user to select multiple columns from the `data` allowed in teal.
#'
#' The `data_extract_spec` must not allow multiple selection in this case.
#' @param row_facet (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' optional specification of the data variable(s) to use for faceting rows.
#' @param col_facet (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' optional specification of the data variable(s) to use for faceting columns.
#' @param coord_flip (`logical(1)`)
#' Indicates whether to flip coordinates between `x` and `response`.
#' The default value is `FALSE` and it will show the `x` variable on the x-axis
#' and the `response` variable on the y-axis.
#' @param count_labels (`logical(1)`)
#' Indicates whether to show count labels.
#' Defaults to `TRUE`.
#' @param freq (`logical(1)`)
#' Indicates whether to display frequency (`TRUE`) or density (`FALSE`).
#' Defaults to density (`FALSE`).
#'
#' @inherit shared_params return
#'
#' @note For more examples, please see the vignette "Using response plot" via
#' `vignette("using-response-plot", package = "teal.modules.general")`.
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
#' tm_g_response(
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
#' @examplesShinylive
#' library(teal.modules.general)
#' interactive <- function() TRUE
#' {{ next_example }}
#' @examples
#' # general data example
#' data <- teal_data()
#' data <- within(data, {
#'   require(nestcolor)
#'   mtcars <- mtcars
#'   for (v in c("cyl", "vs", "am", "gear")) {
#'     mtcars[[v]] <- as.factor(mtcars[[v]])
#'   }
#' })
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_response(
#'       label = "Response Plots",
#'       response = data_extract_spec(
#'         dataname = "mtcars",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["mtcars"]], c("cyl", "gear")),
#'           selected = "cyl",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "mtcars",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["mtcars"]], c("vs", "am")),
#'           selected = "vs",
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
#'   modules = modules(
#'     tm_g_response(
#'       label = "Response Plots",
#'       response = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["ADSL"]], c("BMRKR2", "COUNTRY")),
#'           selected = "BMRKR2",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["ADSL"]], c("SEX", "RACE")),
#'           selected = "RACE",
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
tm_g_response <- function(label = "Response Plot",
                          response,
                          x,
                          row_facet = NULL,
                          col_facet = NULL,
                          coord_flip = FALSE,
                          count_labels = TRUE,
                          rotate_xaxis_labels = FALSE,
                          freq = FALSE,
                          plot_height = c(600, 400, 5000),
                          plot_width = NULL,
                          ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
                          ggplot2_args = teal.widgets::ggplot2_args(),
                          pre_output = NULL,
                          post_output = NULL,
                          transformators = list(),
                          decorators = list()) {
  message("Initializing tm_g_response")

  # Normalize the parameters
  if (inherits(response, "data_extract_spec")) response <- list(response)
  if (inherits(x, "data_extract_spec")) x <- list(x)
  if (inherits(row_facet, "data_extract_spec")) row_facet <- list(row_facet)
  if (inherits(col_facet, "data_extract_spec")) col_facet <- list(col_facet)

  # Start of assertions
  checkmate::assert_string(label)

  checkmate::assert_list(response, types = "data_extract_spec")
  if (!all(vapply(response, function(x) !("" %in% x$select$choices), logical(1)))) {
    stop("'response' should not allow empty values")
  }
  assert_single_selection(response)

  checkmate::assert_list(x, types = "data_extract_spec")
  if (!all(vapply(x, function(x) !("" %in% x$select$choices), logical(1)))) {
    stop("'x' should not allow empty values")
  }
  assert_single_selection(x)

  checkmate::assert_list(row_facet, types = "data_extract_spec", null.ok = TRUE)
  checkmate::assert_list(col_facet, types = "data_extract_spec", null.ok = TRUE)
  checkmate::assert_flag(coord_flip)
  checkmate::assert_flag(count_labels)
  checkmate::assert_flag(rotate_xaxis_labels)
  checkmate::assert_flag(freq)

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  ggtheme <- match.arg(ggtheme)
  checkmate::assert_class(ggplot2_args, "ggplot2_args")

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)

  assert_decorators(decorators, "plot")
  # End of assertions

  # Make UI args
  args <- as.list(environment())

  data_extract_list <- list(
    response = response,
    x = x,
    row_facet = row_facet,
    col_facet = col_facet
  )

  ans <- module(
    label = label,
    server = srv_g_response,
    ui = ui_g_response,
    ui_args = args,
    server_args = c(
      data_extract_list,
      list(
        plot_height = plot_height,
        plot_width = plot_width,
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

# UI function for the response module
ui_g_response <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(args$response, args$x, args$row_facet, args$col_facet)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      teal.widgets::plot_with_settings_ui(id = ns("myplot"))
    ),
    encoding = tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(args[c("response", "x", "row_facet", "col_facet")]),
      teal.transform::data_extract_ui(
        id = ns("response"),
        label = "Response variable",
        data_extract_spec = args$response,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("x"),
        label = "X variable",
        data_extract_spec = args$x,
        is_single_dataset = is_single_dataset_value
      ),
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
      shinyWidgets::radioGroupButtons(
        inputId = ns("freq"),
        label = NULL,
        choices = c("frequency", "density"),
        selected = ifelse(args$freq, "frequency", "density"),
        justified = TRUE
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(args$decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Plot settings",
          checkboxInput(ns("count_labels"), "Add count labels", value = args$count_labels),
          checkboxInput(ns("coord_flip"), "Swap axes", value = args$coord_flip),
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = args$rotate_xaxis_labels),
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
}

# Server function for the response module
srv_g_response <- function(id,
                           data,
                           reporter,
                           filter_panel_api,
                           response,
                           x,
                           row_facet,
                           col_facet,
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

    data_extract <- list(response = response, x = x, row_facet = row_facet, col_facet = col_facet)

    rule_diff <- function(other) {
      function(value) {
        if (other %in% names(selector_list())) {
          othervalue <- selector_list()[[other]]()[["select"]]
          if (!is.null(othervalue)) {
            if (identical(value, othervalue)) {
              "Row and column facetting variables must be different."
            }
          }
        }
      }
    }

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = data_extract,
      datasets = data,
      select_validation_rule = list(
        response = shinyvalidate::sv_required("Please define a column for the response variable"),
        x = shinyvalidate::sv_required("Please define a column for X variable"),
        row_facet = shinyvalidate::compose_rules(
          shinyvalidate::sv_optional(),
          ~ if (length(.) > 1) "There must be 1 or no row facetting variable.",
          rule_diff("col_facet")
        ),
        col_facet = shinyvalidate::compose_rules(
          shinyvalidate::sv_optional(),
          ~ if (length(.) > 1) "There must be 1 or no column facetting variable.",
          rule_diff("row_facet")
        )
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      iv$add_rule("ggtheme", shinyvalidate::sv_required("Please select a theme"))
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_merged_input <- teal.transform::merge_expression_srv(
      selector_list = selector_list,
      datasets = data
    )

    qenv <- reactive(
      teal.code::eval_code(data(), 'library("ggplot2");library("dplyr")') # nolint quotes
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

      qenv <- merged$anl_q_r()
      ANL <- qenv[["ANL"]]
      resp_var <- as.vector(merged$anl_input_r()$columns_source$response)
      x <- as.vector(merged$anl_input_r()$columns_source$x)

      validate(need(is.factor(ANL[[resp_var]]), "Please select a factor variable as the response."))
      validate(need(is.factor(ANL[[x]]), "Please select a factor variable as the X-Variable."))
      teal::validate_has_data(ANL, 10)
      teal::validate_has_data(ANL[, c(resp_var, x)], 10, complete = TRUE, allow_inf = FALSE)

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

      freq <- input$freq == "frequency"
      swap_axes <- input$coord_flip
      counts <- input$count_labels
      rotate_xaxis_labels <- input$rotate_xaxis_labels
      ggtheme <- input$ggtheme

      arg_position <- if (freq) "stack" else "fill"

      rowf <- if (length(row_facet_name) != 0) as.name(row_facet_name)
      colf <- if (length(col_facet_name) != 0) as.name(col_facet_name)
      resp_cl <- as.name(resp_var)
      x_cl <- as.name(x)

      if (swap_axes) {
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = ANL[[x]] <- with(ANL, forcats::fct_rev(x_cl)),
            env = list(x = x, x_cl = x_cl)
          )
        )
      }

      qenv <- teal.code::eval_code(
        qenv,
        substitute(
          expr = ANL[[resp_var]] <- factor(ANL[[resp_var]]),
          env = list(resp_var = resp_var)
        )
      ) %>%
        # rowf and colf will be a NULL if not set by a user
        teal.code::eval_code(
          substitute(
            expr = ANL2 <- ANL %>%
              dplyr::group_by_at(dplyr::vars(x_cl, resp_cl, rowf, colf)) %>%
              dplyr::summarise(ns = dplyr::n()) %>%
              dplyr::group_by_at(dplyr::vars(x_cl, rowf, colf)) %>%
              dplyr::mutate(sums = sum(ns), percent = round(ns / sums * 100, 1)),
            env = list(x_cl = x_cl, resp_cl = resp_cl, rowf = rowf, colf = colf)
          )
        ) %>%
        teal.code::eval_code(
          substitute(
            expr = ANL3 <- ANL %>%
              dplyr::group_by_at(dplyr::vars(x_cl, rowf, colf)) %>%
              dplyr::summarise(ns = dplyr::n()),
            env = list(x_cl = x_cl, rowf = rowf, colf = colf)
          )
        )

      plot_call <- substitute(
        expr = ggplot2::ggplot(ANL2, ggplot2::aes(x = x_cl, y = ns)) +
          ggplot2::geom_bar(ggplot2::aes(fill = resp_cl), stat = "identity", position = arg_position),
        env = list(
          x_cl = x_cl,
          resp_cl = resp_cl,
          arg_position = arg_position
        )
      )

      if (!freq) {
        plot_call <- substitute(
          plot_call + ggplot2::expand_limits(y = c(0, 1.1)),
          env = list(plot_call = plot_call)
        )
      }

      if (counts) {
        plot_call <- substitute(
          expr = plot_call +
            ggplot2::geom_text(
              data = ANL2,
              ggplot2::aes(label = ns, x = x_cl, y = ns, group = resp_cl),
              col = "white",
              vjust = "middle",
              hjust = "middle",
              position = position_anl2_value
            ) +
            ggplot2::geom_text(
              data = ANL3, ggplot2::aes(label = ns, x = x_cl, y = anl3_y),
              hjust = hjust_value,
              vjust = vjust_value,
              position = position_anl3_value
            ),
          env = list(
            plot_call = plot_call,
            x_cl = x_cl,
            resp_cl = resp_cl,
            hjust_value = if (swap_axes) "left" else "middle",
            vjust_value = if (swap_axes) "middle" else -1,
            position_anl2_value = if (!freq) quote(position_fill(0.5)) else quote(position_stack(0.5)), # nolint: line_length.
            anl3_y = if (!freq) 1.1 else as.name("ns"),
            position_anl3_value = if (!freq) "fill" else "stack"
          )
        )
      }

      if (swap_axes) {
        plot_call <- substitute(plot_call + coord_flip(), env = list(plot_call = plot_call))
      }

      facet_cl <- facet_ggplot_call(row_facet_name, col_facet_name)

      if (!is.null(facet_cl)) {
        plot_call <- substitute(expr = plot_call + facet_cl, env = list(plot_call = plot_call, facet_cl = facet_cl))
      }

      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        labs = list(
          x = varname_w_label(x, ANL),
          y = varname_w_label(resp_var, ANL, prefix = "Proportion of "),
          fill = varname_w_label(resp_var, ANL)
        ),
        theme = list(legend.position = "bottom")
      )

      if (rotate_xaxis_labels) {
        dev_ggplot2_args$theme[["axis.text.x"]] <- quote(ggplot2::element_text(angle = 45, hjust = 1))
      }

      all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args,
        module_plot = dev_ggplot2_args
      )

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args,
        ggtheme = ggtheme
      )

      plot_call <- substitute(expr = {
        plot <- plot_call + labs + ggthemes + themes
      }, env = list(
        plot_call = plot_call,
        labs = parsed_ggplot2_args$labs,
        themes = parsed_ggplot2_args$theme,
        ggthemes = parsed_ggplot2_args$ggtheme
      ))

      teal.code::eval_code(qenv, plot_call)
    })

    decorated_output_plot_q <- srv_decorate_teal_data(
      id = "decorator",
      data = output_q,
      decorators = select_decorators(decorators, "plot"),
      expr = plot
    )

    plot_r <- reactive(req(decorated_output_plot_q())[["plot"]])

    # Insert the plot into a plot_with_settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    # Render R code.
    source_code_r <- reactive(teal.code::get_code(req(decorated_output_plot_q())))

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = "Show R Code for Response"
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Response Plot",
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
