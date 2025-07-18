#' `teal` module: Stack plots of variables and show association with reference variable
#'
#' Module provides functionality for visualizing the distribution of variables and
#' their association with a reference variable.
#' It supports configuring the appearance of the plots, including themes and whether to show associations.
#'
#'
#' @note For more examples, please see the vignette "Using association plot" via
#' `vignette("using-association-plot", package = "teal.modules.general")`.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param ref (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Reference variable, must accepts a `data_extract_spec` with `select_spec(multiple = FALSE)`
#' to ensure single selection option.
#' @param vars (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' Variables to be associated with the reference variable.
#' @param show_association (`logical`) optional, whether show association of `vars`
#' with reference variable. Defaults to `TRUE`.
#' @param distribution_theme,association_theme (`character`) optional, `ggplot2` themes to be used by default.
#' Default to `"gray"`.
#'
#' @param ggplot2_args `r roxygen_ggplot2_args_param("Bivariate1", "Bivariate2")`
#'
#' @inherit shared_params return
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `plot` (`grob` created with [ggplot2::ggplotGrob()])
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_g_association(
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
#'   CO2 <- CO2
#'   factors <- names(Filter(isTRUE, vapply(CO2, is.factor, logical(1L))))
#'   CO2[factors] <- lapply(CO2[factors], as.character)
#' })
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_g_association(
#'       ref = data_extract_spec(
#'         dataname = "CO2",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(data[["CO2"]], c("Plant", "Type", "Treatment")),
#'           selected = "Plant",
#'           fixed = FALSE
#'         )
#'       ),
#'       vars = data_extract_spec(
#'         dataname = "CO2",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = variable_choices(data[["CO2"]], c("Plant", "Type", "Treatment")),
#'           selected = "Treatment",
#'           multiple = TRUE,
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
#'     tm_g_association(
#'       ref = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(
#'             data[["ADSL"]],
#'             c("SEX", "RACE", "COUNTRY", "ARM", "STRATA1", "STRATA2", "ITTFL", "BMRKR2")
#'           ),
#'           selected = "RACE",
#'           fixed = FALSE
#'         )
#'       ),
#'       vars = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = variable_choices(
#'             data[["ADSL"]],
#'             c("SEX", "RACE", "COUNTRY", "ARM", "STRATA1", "STRATA2", "ITTFL", "BMRKR2")
#'           ),
#'           selected = "BMRKR2",
#'           multiple = TRUE,
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
tm_g_association <- function(label = "Association",
                             ref,
                             vars,
                             show_association = TRUE,
                             plot_height = c(600, 400, 5000),
                             plot_width = NULL,
                             distribution_theme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"), # nolint: line_length.
                             association_theme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"), # nolint: line_length.
                             pre_output = NULL,
                             post_output = NULL,
                             ggplot2_args = teal.widgets::ggplot2_args(),
                             transformators = list(),
                             decorators = list()) {
  message("Initializing tm_g_association")

  # Normalize the parameters
  if (inherits(ref, "data_extract_spec")) ref <- list(ref)
  if (inherits(vars, "data_extract_spec")) vars <- list(vars)
  if (inherits(ggplot2_args, "ggplot2_args")) ggplot2_args <- list(default = ggplot2_args)

  # Start of assertions
  checkmate::assert_string(label)

  checkmate::assert_list(ref, types = "data_extract_spec")
  if (!all(vapply(ref, function(x) !x$select$multiple, logical(1)))) {
    stop("'ref' should not allow multiple selection")
  }

  checkmate::assert_list(vars, types = "data_extract_spec")
  checkmate::assert_flag(show_association)

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  distribution_theme <- match.arg(distribution_theme)
  association_theme <- match.arg(association_theme)

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)

  plot_choices <- c("Bivariate1", "Bivariate2")
  checkmate::assert_list(ggplot2_args, types = "ggplot2_args")
  checkmate::assert_subset(names(ggplot2_args), c("default", plot_choices))

  assert_decorators(decorators, "plot")
  # End of assertions

  # Make UI args
  args <- as.list(environment())

  data_extract_list <- list(
    ref = ref,
    vars = vars
  )

  ans <- module(
    label = label,
    server = srv_tm_g_association,
    ui = ui_tm_g_association,
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

# UI function for the association module
ui_tm_g_association <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(args$ref, args$vars)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      textOutput(ns("title")),
      tags$br(),
      teal.widgets::plot_with_settings_ui(id = ns("myplot"))
    ),
    encoding = tags$div(
      ### Reporter
      teal.reporter::simple_reporter_ui(ns("simple_reporter")),
      ###
      tags$label("Encodings", class = "text-primary"),
      teal.transform::datanames_input(args[c("ref", "vars")]),
      teal.transform::data_extract_ui(
        id = ns("ref"),
        label = "Reference variable",
        data_extract_spec = args$ref,
        is_single_dataset = is_single_dataset_value
      ),
      teal.transform::data_extract_ui(
        id = ns("vars"),
        label = "Associated variables",
        data_extract_spec = args$vars,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(
        ns("association"),
        "Association with reference variable",
        value = args$show_association
      ),
      checkboxInput(
        ns("show_dist"),
        "Scaled frequencies",
        value = FALSE
      ),
      checkboxInput(
        ns("log_transformation"),
        "Log transformed",
        value = FALSE
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(args$decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(ns("alpha"), "Scatterplot opacity:", c(0.5, 0, 1), ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(ns("size"), "Scatterplot points size:", c(2, 1, 8), ticks = FALSE),
          checkboxInput(ns("swap_axes"), "Swap axes", value = FALSE),
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = FALSE),
          selectInput(
            inputId = ns("distribution_theme"),
            label = "Distribution theme (by ggplot):",
            choices = ggplot_themes,
            selected = args$distribution_theme,
            multiple = FALSE
          ),
          selectInput(
            inputId = ns("association_theme"),
            label = "Association theme (by ggplot):",
            choices = ggplot_themes,
            selected = args$association_theme,
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

# Server function for the association module
srv_tm_g_association <- function(id,
                                 data,
                                 reporter,
                                 filter_panel_api,
                                 ref,
                                 vars,
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

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(ref = ref, vars = vars),
      datasets = data,
      select_validation_rule = list(
        ref = shinyvalidate::compose_rules(
          shinyvalidate::sv_required("A reference variable needs to be selected."),
          ~ if ((.) %in% selector_list()$vars()$select) {
            "Associated variables and reference variable cannot overlap"
          }
        ),
        vars = shinyvalidate::compose_rules(
          shinyvalidate::sv_required("An associated variable needs to be selected."),
          ~ if (length(selector_list()$ref()$select) != 0 && selector_list()$ref()$select %in% (.)) {
            "Associated variables and reference variable cannot overlap"
          }
        )
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    anl_merged_input <- teal.transform::merge_expression_srv(
      datasets = data,
      selector_list = selector_list
    )

    qenv <- reactive(
      teal.code::eval_code(data(), 'library("ggplot2");library("dplyr");library("tern");library("ggmosaic")') # nolint quotes
    )
    anl_merged_q <- reactive({
      req(anl_merged_input())
      qenv() %>% teal.code::eval_code(as.expression(anl_merged_input()$expr))
    })

    merged <- list(
      anl_input_r = anl_merged_input,
      anl_q_r = anl_merged_q
    )

    output_q <- reactive({
      teal::validate_inputs(iv_r())

      ANL <- merged$anl_q_r()[["ANL"]]
      teal::validate_has_data(ANL, 3)

      vars_names <- merged$anl_input_r()$columns_source$vars

      ref_name <- as.vector(merged$anl_input_r()$columns_source$ref)
      association <- input$association
      show_dist <- input$show_dist
      log_transformation <- input$log_transformation
      rotate_xaxis_labels <- input$rotate_xaxis_labels
      swap_axes <- input$swap_axes
      distribution_theme <- input$distribution_theme
      association_theme <- input$association_theme

      is_scatterplot <- is.numeric(ANL[[ref_name]]) && any(vapply(ANL[vars_names], is.numeric, logical(1)))
      if (is_scatterplot) {
        shinyjs::show("alpha")
        shinyjs::show("size")
        alpha <- input$alpha
        size <- input$size
      } else {
        shinyjs::hide("alpha")
        shinyjs::hide("size")
        alpha <- 0.5
        size <- 2
      }

      teal::validate_has_data(ANL[, c(ref_name, vars_names)], 3, complete = TRUE, allow_inf = FALSE)

      # reference
      ref_class <- class(ANL[[ref_name]])[1]
      if (is.numeric(ANL[[ref_name]]) && log_transformation) {
        # works for both integers and doubles
        ref_cl_name <- call("log", as.name(ref_name))
        ref_cl_lbl <- varname_w_label(ref_name, ANL, prefix = "Log of ")
      } else {
        # silently ignore when non-numeric even if `log` is selected because some
        # variables may be numeric and others not
        ref_cl_name <- as.name(ref_name)
        ref_cl_lbl <- varname_w_label(ref_name, ANL)
      }

      user_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Bivariate1"]],
        user_default = ggplot2_args$default
      )

      ref_call <- bivariate_plot_call(
        data_name = "ANL",
        x = ref_cl_name,
        x_class = ref_class,
        x_label = ref_cl_lbl,
        freq = !show_dist,
        theme = distribution_theme,
        rotate_xaxis_labels = rotate_xaxis_labels,
        swap_axes = FALSE,
        size = size,
        alpha = alpha,
        ggplot2_args = user_ggplot2_args
      )

      # association
      ref_class_cov <- ifelse(association, ref_class, "NULL")

      var_calls <- lapply(vars_names, function(var_i) {
        var_class <- class(ANL[[var_i]])[1]
        if (is.numeric(ANL[[var_i]]) && log_transformation) {
          # works for both integers and doubles
          var_cl_name <- call("log", as.name(var_i))
          var_cl_lbl <- varname_w_label(var_i, ANL, prefix = "Log of ")
        } else {
          # silently ignore when non-numeric even if `log` is selected because some
          # variables may be numeric and others not
          var_cl_name <- as.name(var_i)
          var_cl_lbl <- varname_w_label(var_i, ANL)
        }

        user_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Bivariate2"]],
          user_default = ggplot2_args$default
        )

        bivariate_plot_call(
          data_name = "ANL",
          x = ref_cl_name,
          y = var_cl_name,
          x_class = ref_class_cov,
          y_class = var_class,
          x_label = ref_cl_lbl,
          y_label = var_cl_lbl,
          theme = association_theme,
          freq = !show_dist,
          rotate_xaxis_labels = rotate_xaxis_labels,
          swap_axes = swap_axes,
          alpha = alpha,
          size = size,
          ggplot2_args = user_ggplot2_args
        )
      })

      # helper function to format variable name
      format_varnames <- function(x) {
        if (is.numeric(ANL[[x]]) && log_transformation) {
          varname_w_label(x, ANL, prefix = "Log of ")
        } else {
          varname_w_label(x, ANL)
        }
      }
      new_title <-
        if (association) {
          switch(as.character(length(vars_names)),
            "0" = sprintf("Value distribution for %s", ref_cl_lbl),
            "1" = sprintf(
              "Association between %s and %s",
              ref_cl_lbl,
              format_varnames(vars_names)
            ),
            sprintf(
              "Associations between %s and: %s",
              ref_cl_lbl,
              paste(lapply(vars_names, format_varnames), collapse = ", ")
            )
          )
        } else {
          switch(as.character(length(vars_names)),
            "0" = sprintf("Value distribution for %s", ref_cl_lbl),
            sprintf(
              "Value distributions for %s and %s",
              ref_cl_lbl,
              paste(lapply(vars_names, format_varnames), collapse = ", ")
            )
          )
        }
      teal.code::eval_code(
        merged$anl_q_r(),
        substitute(
          expr = title <- new_title,
          env = list(new_title = new_title)
        )
      ) %>%
        teal.code::eval_code(
          substitute(
            expr = {
              plots <- plot_calls
              plot_top <- plots[[1]]
              plot_bottom <- plots[[2]]
              plot <- tern::stack_grobs(grobs = lapply(list(plot_top, plot_bottom), ggplot2::ggplotGrob))
            },
            env = list(
              plot_calls = do.call(
                "call",
                c(list("list", ref_call), var_calls),
                quote = TRUE
              )
            )
          )
        )
    })

    decorated_output_grob_q <- srv_decorate_teal_data(
      id = "decorator",
      data = output_q,
      decorators = select_decorators(decorators, "plot"),
      expr = {
        grid::grid.newpage()
        grid::grid.draw(plot)
      }
    )

    plot_r <- reactive({
      req(iv_r()$is_valid())
      req(decorated_output_grob_q())[["plot"]]
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    output$title <- renderText(output_q()[["title"]])

    # Render R code.
    source_code_r <- reactive(teal.code::get_code(req(decorated_output_grob_q())))

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = "Association Plot"
    )

    ### REPORTER
    if (with_reporter) {
      card_fun <- function(comment, label) {
        card <- teal::report_card_template(
          title = "Association Plot",
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
