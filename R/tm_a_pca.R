#' `teal` module: Principal component analysis
#'
#' Module conducts principal component analysis (PCA) on a given dataset and offers different
#' ways of visualizing the outcomes, including elbow plot, circle plot, biplot, and eigenvector plot.
#' Additionally, it enables dynamic customization of plot aesthetics, such as opacity, size, and
#' font size, through UI inputs.
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param dat (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#' specifying columns used to compute PCA.
#' @param font_size (`numeric`) optional, specifies font size.
#' It controls the font size for plot titles, axis labels, and legends.
#' - If vector of `length == 1` then the font sizes will have a fixed size.
#' - while vector of `value`, `min`, and `max` allows dynamic adjustment.
#' @param ggplot2_args `r roxygen_ggplot2_args_param("Elbow plot", "Circle plot", "Biplot", "Eigenvector plot")`
#'
#' @inherit shared_params return
#'
#' @section Decorating Module:
#'
#' This module generates the following objects, which can be modified in place using decorators:
#' - `elbow_plot` (`ggplot`)
#' - `circle_plot` (`ggplot`)
#' - `biplot` (`ggplot`)
#' - `eigenvector_plot` (`ggplot`)
#'
#' A Decorator is applied to the specific output using a named list of `teal_transform_module` objects.
#' The name of this list corresponds to the name of the output to which the decorator is applied.
#' See code snippet below:
#'
#' ```
#' tm_a_pca(
#'    ..., # arguments for module
#'    decorators = list(
#'      elbow_plot = teal_transform_module(...), # applied to the `elbow_plot` output
#'      circle_plot = teal_transform_module(...), # applied to the `circle_plot` output
#'      biplot = teal_transform_module(...), # applied to the `biplot` output
#'      eigenvector_plot = teal_transform_module(...) # applied to the `eigenvector_plot` output
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
#'
#' # general data example
#' data <- teal_data()
#' data <- within(data, {
#'   require(nestcolor)
#'   USArrests <- USArrests
#' })
#'
#' app <- init(
#'   data = data,
#'   modules = modules(
#'     tm_a_pca(
#'       "PCA",
#'       dat = data_extract_spec(
#'         dataname = "USArrests",
#'         select = select_spec(
#'           choices = variable_choices(
#'             data = data[["USArrests"]], c("Murder", "Assault", "UrbanPop", "Rape")
#'           ),
#'           selected = c("Murder", "Assault"),
#'           multiple = TRUE
#'         ),
#'         filter = NULL
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
#'
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
#'     tm_a_pca(
#'       "PCA",
#'       dat = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = variable_choices(
#'             data = data[["ADSL"]], c("BMRKR1", "AGE", "EOSDY")
#'           ),
#'           selected = c("BMRKR1", "AGE"),
#'           multiple = TRUE
#'         ),
#'         filter = NULL
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
tm_a_pca <- function(label = "Principal Component Analysis",
                     dat,
                     plot_height = c(600, 200, 2000),
                     plot_width = NULL,
                     ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
                     ggplot2_args = teal.widgets::ggplot2_args(),
                     rotate_xaxis_labels = FALSE,
                     font_size = c(12, 8, 20),
                     alpha = c(1, 0, 1),
                     size = c(2, 1, 8),
                     pre_output = NULL,
                     post_output = NULL,
                     transformators = list(),
                     decorators = list()) {
  message("Initializing tm_a_pca")

  # Normalize the parameters
  if (inherits(dat, "data_extract_spec")) dat <- list(dat)
  if (inherits(ggplot2_args, "ggplot2_args")) ggplot2_args <- list(default = ggplot2_args)

  # Start of assertions
  checkmate::assert_string(label)
  checkmate::assert_list(dat, types = "data_extract_spec")

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  ggtheme <- match.arg(ggtheme)

  plot_choices <- c("Elbow plot", "Circle plot", "Biplot", "Eigenvector plot")
  checkmate::assert_list(ggplot2_args, types = "ggplot2_args")
  checkmate::assert_subset(names(ggplot2_args), c("default", plot_choices))

  checkmate::assert_flag(rotate_xaxis_labels)

  if (length(font_size) == 1) {
    checkmate::assert_numeric(font_size, any.missing = FALSE, finite = TRUE, lower = 8, upper = 20)
  } else {
    checkmate::assert_numeric(font_size, len = 3, any.missing = FALSE, finite = TRUE, lower = 8, upper = 20)
    checkmate::assert_numeric(font_size[1], lower = font_size[2], upper = font_size[3], .var.name = "font_size")
  }

  if (length(alpha) == 1) {
    checkmate::assert_numeric(alpha, any.missing = FALSE, finite = TRUE, lower = 0, upper = 1)
  } else {
    checkmate::assert_numeric(alpha, len = 3, any.missing = FALSE, finite = TRUE, lower = 0, upper = 1)
    checkmate::assert_numeric(alpha[1], lower = alpha[2], upper = alpha[3], .var.name = "alpha")
  }

  if (length(size) == 1) {
    checkmate::assert_numeric(size, any.missing = FALSE, finite = TRUE, lower = 1, upper = 8)
  } else {
    checkmate::assert_numeric(size, len = 3, any.missing = FALSE, finite = TRUE, lower = 1, upper = 8)
    checkmate::assert_numeric(size[1], lower = size[2], upper = size[3], .var.name = "size")
  }

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)

  available_decorators <- c("elbow_plot", "circle_plot", "biplot", "eigenvector_plot")
  assert_decorators(decorators, available_decorators)

  # Make UI args
  args <- as.list(environment())

  data_extract_list <- list(dat = dat)

  ans <- module(
    label = label,
    server = srv_a_pca,
    ui = ui_a_pca,
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
  attr(ans, "teal_bookmarkable") <- FALSE
  ans
}

# UI function for the PCA module
ui_a_pca <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  is_single_dataset_value <- teal.transform::is_single_dataset(args$dat)

  color_selector <- args$dat
  for (i in seq_along(color_selector)) {
    color_selector[[i]]$select$multiple <- FALSE
    color_selector[[i]]$select$always_selected <- NULL
    color_selector[[i]]$select$selected <- NULL
  }

  tagList(
    include_css_files("custom"),
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        uiOutput(ns("all_plots"))
      ),
      encoding = tags$div(
        tags$label("Encodings", class = "text-primary"),
        teal.transform::datanames_input(args["dat"]),
        teal.transform::data_extract_ui(
          id = ns("dat"),
          label = "Data selection",
          data_extract_spec = args$dat,
          is_single_dataset = is_single_dataset_value
        ),
        bslib::accordion(
          open = TRUE,
          bslib::accordion_panel(
            title = "Display",
            checkboxGroupInput(
              ns("tables_display"),
              "Tables display",
              choices = c("PC importance" = "importance", "Eigenvectors" = "eigenvector"),
              selected = c("importance", "eigenvector")
            ),
            radioButtons(
              ns("plot_type"),
              label = "Plot type",
              choices = args$plot_choices,
              selected = args$plot_choices[1]
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Elbow plot'", ns("plot_type")),
              ui_decorate_teal_data(
                ns("d_elbow_plot"),
                decorators = select_decorators(args$decorators, "elbow_plot")
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Circle plot'", ns("plot_type")),
              ui_decorate_teal_data(
                ns("d_circle_plot"),
                decorators = select_decorators(args$decorators, "circle_plot")
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Biplot'", ns("plot_type")),
              ui_decorate_teal_data(
                ns("d_biplot"),
                decorators = select_decorators(args$decorators, "biplot")
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Eigenvector plot'", ns("plot_type")),
              ui_decorate_teal_data(
                ns("d_eigenvector_plot"),
                decorators = select_decorators(args$decorators, "eigenvector_plot")
              )
            )
          ),
          bslib::accordion_panel(
            title = "Pre-processing",
            radioButtons(
              ns("standardization"), "Standardization",
              choices = c("None" = "none", "Center" = "center", "Center & Scale" = "center_scale"),
              selected = "center_scale"
            ),
            radioButtons(
              ns("na_action"), "NA action",
              choices = c("None" = "none", "Drop" = "drop"),
              selected = "none"
            )
          ),
          bslib::accordion_panel(
            title = "Selected plot specific settings",
            uiOutput(ns("plot_settings")),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Biplot'", ns("plot_type")),
              list(
                teal.transform::data_extract_ui(
                  id = ns("response"),
                  label = "Color by",
                  data_extract_spec = color_selector,
                  is_single_dataset = is_single_dataset_value
                ),
                teal.widgets::optionalSliderInputValMinMax(ns("alpha"), "Opacity:", args$alpha, ticks = FALSE),
                teal.widgets::optionalSliderInputValMinMax(ns("size"), "Points size:", args$size, ticks = FALSE)
              )
            )
          ),
          bslib::accordion_panel(
            title = "Plot settings",
            collapsed = TRUE,
            conditionalPanel(
              condition = sprintf(
                "input['%s'] == 'Elbow plot' || input['%s'] == 'Eigenvector plot'",
                ns("plot_type"),
                ns("plot_type")
              ),
              list(checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = args$rotate_xaxis_labels))
            ),
            selectInput(
              inputId = ns("ggtheme"),
              label = "Theme (by ggplot):",
              choices = ggplot_themes,
              selected = args$ggtheme,
              multiple = FALSE
            ),
            teal.widgets::optionalSliderInputValMinMax(ns("font_size"), "Font Size", args$font_size, ticks = FALSE)
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

# Server function for the PCA module
srv_a_pca <- function(id, data, reporter, filter_panel_api, dat, plot_height, plot_width, ggplot2_args, decorators) {
  with_reporter <- !missing(reporter) && inherits(reporter, "Reporter")
  with_filter <- !missing(filter_panel_api) && inherits(filter_panel_api, "FilterPanelAPI")
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    response <- dat

    for (i in seq_along(response)) {
      response[[i]]$select$multiple <- FALSE
      response[[i]]$select$always_selected <- NULL
      response[[i]]$select$selected <- NULL
      all_cols <- teal.data::col_labels(isolate(data())[[response[[i]]$dataname]])
      ignore_cols <- unlist(teal.data::join_keys(isolate(data()))[[response[[i]]$dataname]])
      color_cols <- all_cols[!names(all_cols) %in% ignore_cols]
      response[[i]]$select$choices <- teal.transform::choices_labeled(names(color_cols), color_cols)
    }

    selector_list <- teal.transform::data_extract_multiple_srv(
      data_extract = list(dat = dat, response = response),
      datasets = data,
      select_validation_rule = list(
        dat = ~ if (length(.) < 2L) "Please select more than 1 variable to perform PCA.",
        response = shinyvalidate::compose_rules(
          shinyvalidate::sv_optional(),
          ~ if (isTRUE(is.element(., selector_list()$dat()$select))) {
            "Response must not have been used for PCA."
          }
        )
      )
    )

    iv_r <- reactive({
      iv <- shinyvalidate::InputValidator$new()
      teal.transform::compose_and_enable_validators(iv, selector_list)
    })

    iv_extra <- shinyvalidate::InputValidator$new()
    iv_extra$add_rule("x_axis", function(value) {
      if (isTRUE(input$plot_type %in% c("Circle plot", "Biplot"))) {
        if (!shinyvalidate::input_provided(value)) {
          "Need X axis"
        }
      }
    })
    iv_extra$add_rule("y_axis", function(value) {
      if (isTRUE(input$plot_type %in% c("Circle plot", "Biplot"))) {
        if (!shinyvalidate::input_provided(value)) {
          "Need Y axis"
        }
      }
    })
    rule_dupl <- function(...) {
      if (isTRUE(input$plot_type %in% c("Circle plot", "Biplot"))) {
        if (isTRUE(input$x_axis == input$y_axis)) {
          "Please choose different X and Y axes."
        }
      }
    }
    iv_extra$add_rule("x_axis", rule_dupl)
    iv_extra$add_rule("y_axis", rule_dupl)
    iv_extra$add_rule("variables", function(value) {
      if (identical(input$plot_type, "Circle plot")) {
        if (!shinyvalidate::input_provided(value)) {
          "Need Original Coordinates"
        }
      }
    })
    iv_extra$add_rule("pc", function(value) {
      if (identical(input$plot_type, "Eigenvector plot")) {
        if (!shinyvalidate::input_provided(value)) {
          "Need PC"
        }
      }
    })
    iv_extra$enable()

    anl_merged_input <- teal.transform::merge_expression_srv(
      selector_list = selector_list,
      datasets = data
    )
    qenv <- teal.code::eval_code(data(), 'library("ggplot2");library("dplyr");library("tidyr")') # nolint quotes
    anl_merged_q <- reactive({
      req(anl_merged_input())
      qenv %>%
        teal.code::eval_code(as.expression(anl_merged_input()$expr))
    })

    merged <- list(
      anl_input_r = anl_merged_input,
      anl_q_r = anl_merged_q
    )

    validation <- reactive({
      req(merged$anl_q_r())
      # inputs
      keep_cols <- as.character(merged$anl_input_r()$columns_source$dat)
      na_action <- input$na_action
      standardization <- input$standardization
      center <- standardization %in% c("center", "center_scale")
      scale <- standardization == "center_scale"
      ANL <- merged$anl_q_r()[["ANL"]]

      teal::validate_has_data(ANL, 10)
      validate(need(
        na_action != "none" | !anyNA(ANL[keep_cols]),
        paste(
          "There are NAs in the dataset. Please deal with them in preprocessing",
          "or select \"Drop\" in the NA actions inside the encodings panel (left)."
        )
      ))
      if (scale) {
        not_single <- vapply(ANL[keep_cols], function(column) length(unique(column)) != 1, FUN.VALUE = logical(1))

        msg <- paste0(
          "You have selected `Center & Scale` under `Standardization` in the `Pre-processing` panel, ",
          "but one or more of your columns has/have a variance value of zero, indicating all values are identical"
        )
        validate(need(all(not_single), msg))
      }
    })

    # computation ----
    computation_model <- reactive({
      validation()

      # inputs
      keep_cols <- as.character(merged$anl_input_r()$columns_source$dat)
      na_action <- input$na_action
      standardization <- input$standardization
      center <- standardization %in% c("center", "center_scale")
      scale <- standardization == "center_scale"
      ANL <- merged$anl_q_r()[["ANL"]]

      qenv <- teal.code::eval_code(
        merged$anl_q_r(),
        substitute(
          expr = keep_columns <- keep_cols,
          env = list(keep_cols = keep_cols)
        )
      )

      if (na_action == "drop") {
        qenv <- teal.code::eval_code(
          qenv,
          quote(ANL <- tidyr::drop_na(ANL, keep_columns))
        )
      }

      teal.code::eval_code(
        qenv,
        substitute(
          expr = pca <- summary(stats::prcomp(ANL[keep_columns], center = center, scale. = scale, retx = TRUE)),
          env = list(center = center, scale = scale)
        )
      )
    })
    computation_tbl_imp <- reactive({
      teal.code::eval_code(
        computation_model(),
        quote({
          tbl_importance <- dplyr::as_tibble(pca$importance, rownames = "Metric")
          tbl_importance
        })
      )
    })
    computation <- reactive({
      teal.code::eval_code(
        computation_tbl_imp(),
        quote({
          tbl_eigenvector <- dplyr::as_tibble(pca$rotation, rownames = "Variable")
          tbl_eigenvector
        })
      )
    })

    # plot args ----
    output$plot_settings <- renderUI({
      # reactivity triggers
      req(iv_r()$is_valid())
      req(computation())
      qenv <- computation()

      ns <- session$ns

      pca <- qenv[["pca"]]
      chcs_pcs <- colnames(pca$rotation)
      chcs_vars <- qenv[["keep_columns"]]

      tagList(
        conditionalPanel(
          condition = sprintf(
            "input['%s'] == 'Biplot' || input['%s'] == 'Circle plot'",
            ns("plot_type"), ns("plot_type")
          ),
          list(
            teal.widgets::optionalSelectInput(ns("x_axis"), "X axis", choices = chcs_pcs, selected = chcs_pcs[1]),
            teal.widgets::optionalSelectInput(ns("y_axis"), "Y axis", choices = chcs_pcs, selected = chcs_pcs[2]),
            teal.widgets::optionalSelectInput(
              ns("variables"), "Original coordinates",
              choices = chcs_vars, selected = chcs_vars,
              multiple = TRUE
            )
          )
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'Elbow plot'", ns("plot_type")),
          helpText("No plot specific settings available.")
        ),
        conditionalPanel(
          condition = paste0("input['", ns("plot_type"), "'] == 'Eigenvector plot'"),
          teal.widgets::optionalSelectInput(ns("pc"), "PC", choices = chcs_pcs, selected = chcs_pcs[1])
        )
      )
    })

    # plot elbow ----
    plot_elbow <- function(base_q) {
      ggtheme <- input$ggtheme
      rotate_xaxis_labels <- input$rotate_xaxis_labels
      font_size <- input$font_size

      angle_value <- ifelse(isTRUE(rotate_xaxis_labels), 45, 0)
      hjust_value <- ifelse(isTRUE(rotate_xaxis_labels), 1, 0.5)

      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        labs = list(x = "Principal component", y = "Proportion of variance explained", color = "", fill = "Legend"),
        theme = list(
          legend.position = "right",
          legend.spacing.y = quote(grid::unit(-5, "pt")),
          legend.title = quote(ggplot2::element_text(vjust = 25)),
          axis.text.x = substitute(
            ggplot2::element_text(angle = angle_value, hjust = hjust_value),
            list(angle_value = angle_value, hjust_value = hjust_value)
          ),
          text = substitute(ggplot2::element_text(size = font_size), list(font_size = font_size))
        )
      )

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Elbow plot"]],
          user_default = ggplot2_args$default,
          module_plot = dev_ggplot2_args
        ),
        ggtheme = ggtheme
      )

      teal.code::eval_code(
        base_q,
        substitute(
          expr = {
            elb_dat <- pca$importance[c("Proportion of Variance", "Cumulative Proportion"), ] %>%
              dplyr::as_tibble(rownames = "metric") %>%
              tidyr::gather("component", "value", -metric) %>%
              dplyr::mutate(
                component = factor(component, levels = unique(stringr::str_sort(component, numeric = TRUE)))
              )

            cols <- c(getOption("ggplot2.discrete.colour"), c("lightblue", "darkred", "black"))[1:3]
            elbow_plot <- ggplot2::ggplot(mapping = ggplot2::aes_string(x = "component", y = "value")) +
              ggplot2::geom_bar(
                ggplot2::aes(fill = "Single variance"),
                data = dplyr::filter(elb_dat, metric == "Proportion of Variance"),
                color = "black",
                stat = "identity"
              ) +
              ggplot2::geom_point(
                ggplot2::aes(color = "Cumulative variance"),
                data = dplyr::filter(elb_dat, metric == "Cumulative Proportion")
              ) +
              ggplot2::geom_line(
                ggplot2::aes(group = 1, color = "Cumulative variance"),
                data = dplyr::filter(elb_dat, metric == "Cumulative Proportion")
              ) +
              labs +
              ggplot2::scale_color_manual(values = c("Cumulative variance" = cols[2], "Single variance" = cols[3])) +
              ggplot2::scale_fill_manual(values = c("Cumulative variance" = cols[2], "Single variance" = cols[1])) +
              ggthemes +
              themes
          },
          env = list(
            ggthemes = parsed_ggplot2_args$ggtheme,
            labs = parsed_ggplot2_args$labs,
            themes = parsed_ggplot2_args$theme
          )
        )
      )
    }

    # plot circle ----
    plot_circle <- function(base_q) {
      x_axis <- input$x_axis
      y_axis <- input$y_axis
      variables <- input$variables
      ggtheme <- input$ggtheme

      rotate_xaxis_labels <- input$rotate_xaxis_labels
      font_size <- input$font_size

      angle <- ifelse(isTRUE(rotate_xaxis_labels), 45, 0)
      hjust <- ifelse(isTRUE(rotate_xaxis_labels), 1, 0.5)

      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        theme = list(
          text = substitute(ggplot2::element_text(size = font_size), list(font_size = font_size)),
          axis.text.x = substitute(
            ggplot2::element_text(angle = angle_val, hjust = hjust_val),
            list(angle_val = angle, hjust_val = hjust)
          )
        )
      )

      all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Circle plot"]],
        user_default = ggplot2_args$default,
        module_plot = dev_ggplot2_args
      )

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args,
        ggtheme = ggtheme
      )

      teal.code::eval_code(
        base_q,
        substitute(
          expr = {
            pca_rot <- pca$rotation[, c(x_axis, y_axis)] %>%
              dplyr::as_tibble(rownames = "label") %>%
              dplyr::filter(label %in% variables)

            circle_data <- data.frame(
              x = cos(seq(0, 2 * pi, length.out = 100)),
              y = sin(seq(0, 2 * pi, length.out = 100))
            )

            circle_plot <- ggplot2::ggplot(pca_rot) +
              ggplot2::geom_point(ggplot2::aes_string(x = x_axis, y = y_axis)) +
              ggplot2::geom_label(
                ggplot2::aes_string(x = x_axis, y = y_axis, label = "label"),
                nudge_x = 0.1, nudge_y = 0.05,
                fontface = "bold"
              ) +
              ggplot2::geom_path(ggplot2::aes(x, y, group = 1), data = circle_data) +
              ggplot2::geom_point(ggplot2::aes(x = x, y = y), data = data.frame(x = 0, y = 0), shape = "x", size = 5) +
              labs +
              ggthemes +
              themes
          },
          env = list(
            x_axis = x_axis,
            y_axis = y_axis,
            variables = variables,
            ggthemes = parsed_ggplot2_args$ggtheme,
            labs = `if`(is.null(parsed_ggplot2_args$labs), quote(labs()), parsed_ggplot2_args$labs),
            themes = parsed_ggplot2_args$theme
          )
        )
      )
    }

    # plot biplot ----
    plot_biplot <- function(base_q) {
      qenv <- base_q

      ANL <- qenv[["ANL"]]

      resp_col <- as.character(merged$anl_input_r()$columns_source$response)
      dat_cols <- as.character(merged$anl_input_r()$columns_source$dat)
      x_axis <- input$x_axis
      y_axis <- input$y_axis
      variables <- input$variables
      pca <- qenv[["pca"]]

      ggtheme <- input$ggtheme

      rotate_xaxis_labels <- input$rotate_xaxis_labels
      alpha <- input$alpha
      size <- input$size
      font_size <- input$font_size

      qenv <- teal.code::eval_code(
        qenv,
        substitute(
          expr = pca_rot <- dplyr::as_tibble(pca$x[, c(x_axis, y_axis)]),
          env = list(x_axis = x_axis, y_axis = y_axis)
        )
      )

      # rot_vars = data frame that displays arrows in the plot, need to be scaled to data
      if (!is.null(input$variables)) {
        qenv <- teal.code::eval_code(
          qenv,
          substitute(
            expr = {
              r <- sqrt(qchisq(0.69, df = 2)) * prod(colMeans(pca_rot ^ 2)) ^ (1 / 4) # styler: off
              v_scale <- rowSums(pca$rotation ^ 2) # styler: off

              rot_vars <- pca$rotation[, c(x_axis, y_axis)] %>%
                dplyr::as_tibble(rownames = "label") %>%
                dplyr::mutate_at(vars(c(x_axis, y_axis)), function(x) r * x / sqrt(max(v_scale)))
            },
            env = list(x_axis = x_axis, y_axis = y_axis)
          )
        ) %>%
          teal.code::eval_code(
            if (is.logical(pca$center) && !pca$center) {
              substitute(
                expr = {
                  rot_vars <- rot_vars %>%
                    tibble::column_to_rownames("label") %>%
                    sweep(1, apply(ANL[keep_columns], 2, mean, na.rm = TRUE)) %>%
                    tibble::rownames_to_column("label") %>%
                    dplyr::mutate(
                      xstart = mean(pca$x[, x_axis], na.rm = TRUE),
                      ystart = mean(pca$x[, y_axis], na.rm = TRUE)
                    )
                },
                env = list(x_axis = x_axis, y_axis = y_axis)
              )
            } else {
              quote(rot_vars <- rot_vars %>% dplyr::mutate(xstart = 0, ystart = 0))
            }
          ) %>%
          teal.code::eval_code(
            substitute(
              expr = rot_vars <- rot_vars %>% dplyr::filter(label %in% variables),
              env = list(variables = variables)
            )
          )
      }

      pca_plot_biplot_expr <- list(quote(ggplot()))

      if (length(resp_col) == 0) {
        pca_plot_biplot_expr <- c(
          pca_plot_biplot_expr,
          substitute(
            ggplot2::geom_point(ggplot2::aes_string(x = x_axis, y = y_axis),
              data = pca_rot, alpha = alpha, size = size
            ),
            list(x_axis = input$x_axis, y_axis = input$y_axis, alpha = input$alpha, size = input$size)
          )
        )
        dev_labs <- list()
      } else {
        rp_keys <- setdiff(colnames(ANL), as.character(unlist(merged$anl_input_r()$columns_source)))

        response <- ANL[[resp_col]]

        aes_biplot <- substitute(
          ggplot2::aes_string(x = x_axis, y = y_axis, color = "response"),
          env = list(x_axis = x_axis, y_axis = y_axis)
        )

        qenv <- teal.code::eval_code(
          qenv,
          substitute(response <- ANL[[resp_col]], env = list(resp_col = resp_col))
        )

        dev_labs <- list(color = varname_w_label(resp_col, ANL))

        scales_biplot <-
          if (
            is.character(response) ||
              is.factor(response) ||
              (is.numeric(response) && length(unique(response)) <= 6)
          ) {
            qenv <- teal.code::eval_code(
              qenv,
              quote(pca_rot$response <- as.factor(response))
            )
            quote(ggplot2::scale_color_brewer(palette = "Dark2"))
          } else if (inherits(response, "Date")) {
            qenv <- teal.code::eval_code(
              qenv,
              quote(pca_rot$response <- numeric(response))
            )

            quote(
              ggplot2::scale_color_gradient(
                low = c(getOption("ggplot2.discrete.colour")[2], "darkred")[1],
                high = c(getOption("ggplot2.discrete.colour"), "lightblue")[1],
                labels = function(x) as.Date(x, origin = "1970-01-01")
              )
            )
          } else {
            qenv <- teal.code::eval_code(
              qenv,
              quote(pca_rot$response <- response)
            )
            quote(ggplot2::scale_color_gradient(
              low = c(getOption("ggplot2.discrete.colour")[2], "darkred")[1],
              high = c(getOption("ggplot2.discrete.colour"), "lightblue")[1]
            ))
          }

        pca_plot_biplot_expr <- c(
          pca_plot_biplot_expr,
          substitute(
            ggplot2::geom_point(aes_biplot, data = pca_rot, alpha = alpha, size = size),
            env = list(aes_biplot = aes_biplot, alpha = alpha, size = size)
          ),
          scales_biplot
        )
      }

      if (!is.null(input$variables)) {
        pca_plot_biplot_expr <- c(
          pca_plot_biplot_expr,
          substitute(
            ggplot2::geom_segment(
              ggplot2::aes_string(x = "xstart", y = "ystart", xend = x_axis, yend = y_axis),
              data = rot_vars,
              lineend = "round", linejoin = "round",
              arrow = grid::arrow(length = grid::unit(0.5, "cm"))
            ),
            env = list(x_axis = x_axis, y_axis = y_axis)
          ),
          substitute(
            ggplot2::geom_label(
              ggplot2::aes_string(
                x = x_axis,
                y = y_axis,
                label = "label"
              ),
              data = rot_vars,
              nudge_y = 0.1,
              fontface = "bold"
            ),
            env = list(x_axis = x_axis, y_axis = y_axis)
          ),
          quote(ggplot2::geom_point(ggplot2::aes(x = xstart, y = ystart), data = rot_vars, shape = "x", size = 5))
        )
      }

      angle <- ifelse(isTRUE(rotate_xaxis_labels), 45, 0)
      hjust <- ifelse(isTRUE(rotate_xaxis_labels), 1, 0.5)

      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        labs = dev_labs,
        theme = list(
          text = substitute(ggplot2::element_text(size = font_size), list(font_size = font_size)),
          axis.text.x = substitute(
            ggplot2::element_text(angle = angle_val, hjust = hjust_val),
            list(angle_val = angle, hjust_val = hjust)
          )
        )
      )

      all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Biplot"]],
        user_default = ggplot2_args$default,
        module_plot = dev_ggplot2_args
      )

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args,
        ggtheme = ggtheme
      )

      pca_plot_biplot_expr <- c(
        pca_plot_biplot_expr,
        parsed_ggplot2_args
      )

      teal.code::eval_code(
        qenv,
        substitute(
          expr = {
            biplot <- plot_call
          },
          env = list(
            plot_call = Reduce(function(x, y) call("+", x, y), pca_plot_biplot_expr)
          )
        )
      )
    }

    # plot eigenvector_plot ----
    plot_eigenvector <- function(base_q) {
      req(input$pc)
      pc <- input$pc
      ggtheme <- input$ggtheme

      rotate_xaxis_labels <- input$rotate_xaxis_labels
      font_size <- input$font_size

      angle <- ifelse(rotate_xaxis_labels, 45, 0)
      hjust <- ifelse(rotate_xaxis_labels, 1, 0.5)

      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        theme = list(
          text = substitute(ggplot2::element_text(size = font_size), list(font_size = font_size)),
          axis.text.x = substitute(
            ggplot2::element_text(angle = angle_val, hjust = hjust_val),
            list(angle_val = angle, hjust_val = hjust)
          )
        )
      )

      all_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Eigenvector plot"]],
        user_default = ggplot2_args$default,
        module_plot = dev_ggplot2_args
      )

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        all_ggplot2_args,
        ggtheme = ggtheme
      )

      ggplot_exprs <- c(
        list(
          quote(ggplot(pca_rot)),
          substitute(
            ggplot2::geom_bar(
              ggplot2::aes_string(x = "Variable", y = pc),
              stat = "identity",
              color = "black",
              fill = c(getOption("ggplot2.discrete.colour"), "lightblue")[1]
            ),
            env = list(pc = pc)
          ),
          substitute(
            ggplot2::geom_text(
              ggplot2::aes(
                x = Variable,
                y = pc_name,
                label = round(pc_name, 3),
                vjust = ifelse(pc_name > 0, -0.5, 1.3)
              )
            ),
            env = list(pc_name = as.name(pc))
          )
        ),
        parsed_ggplot2_args$labs,
        parsed_ggplot2_args$ggtheme,
        parsed_ggplot2_args$theme
      )

      teal.code::eval_code(
        base_q,
        substitute(
          expr = {
            pca_rot <- pca$rotation[, pc, drop = FALSE] %>%
              dplyr::as_tibble(rownames = "Variable")
            eigenvector_plot <- plot_call
          },
          env = list(
            pc = pc,
            plot_call = Reduce(function(x, y) call("+", x, y), ggplot_exprs)
          )
        )
      )
    }

    # qenvs ---
    output_q <- lapply(
      list(
        elbow_plot = plot_elbow,
        circle_plot = plot_circle,
        biplot = plot_biplot,
        eigenvector_plot = plot_eigenvector
      ),
      function(fun) {
        reactive({
          req(computation())
          teal::validate_inputs(iv_r())
          teal::validate_inputs(iv_extra, header = "Plot settings are required")
          fun(computation())
        })
      }
    )

    decorated_q <- mapply(
      function(obj_name, q) {
        srv_decorate_teal_data(
          id = sprintf("d_%s", obj_name),
          data = q,
          decorators = select_decorators(decorators, obj_name),
          expr = reactive({
            substitute(print(.plot), env = list(.plot = as.name(obj_name)))
          }),
          expr_is_reactive = TRUE
        )
      },
      names(output_q),
      output_q
    )

    # plot final ----
    decorated_output_q <- reactive({
      switch(req(input$plot_type),
        "Elbow plot" = decorated_q$elbow_plot(),
        "Circle plot" = decorated_q$circle_plot(),
        "Biplot" = decorated_q$biplot(),
        "Eigenvector plot" = decorated_q$eigenvector_plot(),
        stop("Unknown plot")
      )
    })

    plot_r <- reactive({
      plot_name <- gsub(" ", "_", tolower(req(input$plot_type)))
      req(decorated_output_q())[[plot_name]]
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id = "pca_plot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width,
      graph_align = "center"
    )

    # tables ----
    output$tbl_importance <- renderTable(
      expr = {
        req("importance" %in% input$tables_display, computation())
        computation()[["tbl_importance"]]
      },
      bordered = TRUE,
      align = "c",
      digits = 3
    )

    output$tbl_importance_ui <- renderUI({
      req("importance" %in% input$tables_display)
      tags$div(
        align = "center",
        tags$h4("Principal components importance"),
        tableOutput(session$ns("tbl_importance")),
        tags$hr()
      )
    })

    output$tbl_eigenvector <- renderTable(
      expr = {
        req("eigenvector" %in% input$tables_display, req(computation()))
        computation()[["tbl_eigenvector"]]
      },
      bordered = TRUE,
      align = "c",
      digits = 3
    )

    output$tbl_eigenvector_ui <- renderUI({
      req("eigenvector" %in% input$tables_display)
      tags$div(
        align = "center",
        tags$h4("Eigenvectors"),
        tableOutput(session$ns("tbl_eigenvector")),
        tags$hr()
      )
    })

    output$all_plots <- renderUI({
      teal::validate_inputs(iv_r())
      teal::validate_inputs(iv_extra, header = "Plot settings are required")

      validation()
      tags$div(
        class = "overflow-scroll",
        uiOutput(session$ns("tbl_importance_ui")),
        uiOutput(session$ns("tbl_eigenvector_ui")),
        teal.widgets::plot_with_settings_ui(id = session$ns("pca_plot"))
      )
    })

    # Render R code.
    subset_code <- function(code, data) {
      gsub(code, "", teal.data::get_code(data), fixed = TRUE)
    }
    setup_code_r <- reactive(teal.data::get_code(qenv))
    data_prep_code_r <-
      reactive(
        subset_code(
          setup_code_r(),
          req(anl_merged_q())
        )
      )

    computation_model_code_r <-
      reactive(
        subset_code(
          paste0(setup_code_r(), data_prep_code_r()),
          req(computation_model())
        )
      )

    computation_tbl_imp_code_r <-
      reactive(
        subset_code(
          paste0(setup_code_r(), data_prep_code_r(), computation_model_code_r()),
          req(computation_tbl_imp())
        )
      )

    computation_tbl_eig_code_r <-
      reactive(
        subset_code(
          paste0(setup_code_r(), data_prep_code_r(), computation_model_code_r(), computation_tbl_imp_code_r()),
          req(computation())
        )
      )

    plot_code_r <-
      reactive(
        subset_code(
          paste0(setup_code_r(), data_prep_code_r(), computation_model_code_r(), computation_tbl_imp_code_r(), computation_tbl_eig_code_r()),
          req(decorated_output_q())
        )
      )

    source_code_r <- reactive(teal.code::get_code(req(decorated_output_q())))

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = "R Code for PCA"
    )

    card_fun <- reactive({
      req(setup_code_r(), data_prep_code_r(), computation_model_code_r(), computation(),
          computation_tbl_imp_code_r(), computation_tbl_eig_code_r(), plot_code_r(), plot_r())

      teal.reporter::report_document(

        "## Setup",
        teal.reporter::code_chunk(setup_code_r()),

        "## Data Preparations",
        teal.reporter::code_chunk(data_prep_code_r()),

        "## PCA Model",
        teal.reporter::code_chunk(computation_model_code_r()),

        "### Principal Components Table",
        teal.reporter::code_chunk(computation_tbl_imp_code_r()) |>
          teal.reporter::link_output(computation()[["tbl_importance"]]),

        "### Eigenvectors Table",
        teal.reporter::code_chunk(computation_tbl_eig_code_r()) |>
          teal.reporter::link_output(computation()[["tbl_eigenvector"]]),

        "### Plot",
        teal.reporter::code_chunk(
          plot_code_r() |> styler::style_text() |> paste(collapse = "\n")
        ) |>
          teal.reporter::link_output(plot_r())
      )
    })

    ###
    list(
      report_card = card_fun
    )
  })
}
