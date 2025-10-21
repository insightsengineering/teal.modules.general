#' @export
tm_a_regression.picks <- function(label = "Regression Analysis",
                                  regressor = picks(
                                    datasets(),
                                    variables(
                                      choices = tidyselect::where(is.numeric),
                                      selected = tidyselect::last_col(),
                                      multiple = TRUE
                                    ),
                                    values()
                                  ),
                                  response = picks(
                                    datasets(),
                                    variables(choices = tidyselect::where(is.numeric)),
                                    values()
                                  ),
                                  outlier = picks(
                                    regressor$datasets,
                                    variables(choices = where(~ is.factor(.) || is.character(.))),
                                    values()
                                  ), # default should be picks(datasets(), variables(primary_keys())
                                  plot_height = c(600, 200, 2000),
                                  plot_width = NULL,
                                  alpha = c(1, 0, 1),
                                  size = c(2, 1, 8),
                                  ggtheme = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void"),
                                  ggplot2_args = teal.widgets::ggplot2_args(),
                                  pre_output = NULL,
                                  post_output = NULL,
                                  default_plot_type = 1,
                                  default_outlier_label,
                                  label_segment_threshold = c(0.5, 0, 10),
                                  transformators = list(),
                                  decorators = list()) {
  message("Initializing tm_a_regression")

  # Start of assertions
  checkmate::assert_string(label)
  checkmate::assert_class(regressor, "picks")

  checkmate::assert_class(response, "picks")
  if (isTRUE(attr(response$variables, "multiple"))) {
    warning("`response` accepts only a single variable selection. Forcing `variables(multiple) to FALSE`")
    attr(response$variables, "multiple") <- FALSE
  }
  checkmate::assert_class(outlier, "picks", null.ok = TRUE)
  if (isTRUE(attr(outlier$variables, "multiple"))) {
    warning("`outlier` accepts only a single variable selection. Forcing `variables(multiple) to FALSE`")
    attr(outlier$variables, "multiple") <- FALSE
  }

  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")

  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2],
    upper = plot_width[3],
    null.ok = TRUE,
    .var.name = "plot_width"
  )

  if (length(alpha) == 1) {
    checkmate::assert_numeric(alpha, any.missing = FALSE, finite = TRUE)
  } else {
    checkmate::assert_numeric(alpha, len = 3, any.missing = FALSE, finite = TRUE)
    checkmate::assert_numeric(alpha[1], lower = alpha[2], upper = alpha[3], .var.name = "alpha")
  }

  if (length(size) == 1) {
    checkmate::assert_numeric(size, any.missing = FALSE, finite = TRUE)
  } else {
    checkmate::assert_numeric(size, len = 3, any.missing = FALSE, finite = TRUE)
    checkmate::assert_numeric(size[1], lower = size[2], upper = size[3], .var.name = "size")
  }

  ggtheme <- match.arg(ggtheme)

  if (inherits(ggplot2_args, "ggplot2_args")) ggplot2_args <- list(default = ggplot2_args)
  plot_choices <- c(
    "Response vs Regressor", "Residuals vs Fitted", "Normal Q-Q", "Scale-Location",
    "Cook's distance", "Residuals vs Leverage", "Cook's dist vs Leverage"
  )
  checkmate::assert_list(ggplot2_args, types = "ggplot2_args")
  checkmate::assert_subset(names(ggplot2_args), c("default", plot_choices))

  checkmate::assert_multi_class(pre_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_multi_class(post_output, c("shiny.tag", "shiny.tag.list", "html"), null.ok = TRUE)
  checkmate::assert_choice(default_plot_type, seq.int(1L, length(plot_choices)))
  if (!missing(default_outlier_label)) {
    warning("`default_outlier_label` is not supported when using picks. Please use `outlier` argument.")
  }
  checkmate::assert_list(decorators, "teal_transform_module")

  if (length(label_segment_threshold) == 1) {
    checkmate::assert_numeric(label_segment_threshold, any.missing = FALSE, finite = TRUE)
  } else {
    checkmate::assert_numeric(label_segment_threshold, len = 3, any.missing = FALSE, finite = TRUE)
    checkmate::assert_numeric(
      label_segment_threshold[1],
      lower = label_segment_threshold[2],
      upper = label_segment_threshold[3],
      .var.name = "label_segment_threshold"
    )
  }
  assert_decorators(decorators, "plot")
  # End of assertions

  # Make UI args
  args <- as.list(environment())

  ans <- module(
    label = label,
    server = srv_a_regression.picks,
    ui = ui_a_regression.picks,
    ui_args = args[names(args) %in% names(formals(ui_a_regression.picks))],
    server_args = args[names(args) %in% names(formals(srv_a_regression.picks))], ,
    transformators = transformators,
    datanames = {
      datanames <- datanames(list(regressor, response))
      if (length(datanames)) datanames else "all"
    }
  )
  attr(ans, "teal_bookmarkable") <- FALSE
  ans
}

# UI function for the regression module
ui_a_regression.picks <- function(id,
                                  response,
                                  regressor,
                                  outlier,
                                  plot_choices,
                                  default_plot_type,
                                  alpha,
                                  size,
                                  label_segment_threshold,
                                  ggtheme,
                                  pre_output,
                                  post_output,
                                  decorators) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(tags$div(
      teal.widgets::plot_with_settings_ui(id = ns("myplot")),
      tags$div(verbatimTextOutput(ns("text")))
    )),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"), tags$br(),
      teal::teal_nav_item(
        label = tags$strong("Response variable"),
        teal.transform::picks_ui(id = ns("response"), spec = response)
      ),
      teal::teal_nav_item(
        label = tags$strong("Regressor variables"),
        teal.transform::picks_ui(id = ns("regressor"), spec = regressor)
      ),
      radioButtons(
        ns("plot_type"),
        label = "Plot type:",
        choices = plot_choices,
        selected = plot_choices[default_plot_type]
      ),
      checkboxInput(ns("show_outlier"), label = "Display outlier labels", value = FALSE),
      conditionalPanel(
        condition = "input['show_outlier']",
        ns = ns,
        teal.widgets::optionalSliderInput(
          ns("outlier_cutoff"),
          tags$div(
            tagList(
              "Outlier definition:",
              bslib::tooltip(
                icon("fas fa-circle-info"),
                paste(
                  "Use the slider to choose the cut-off value to define outliers.",
                  "Points with a Cook's distance greater than",
                  "the value on the slider times the mean of the Cook's distance of the dataset will have labels."
                )
              )
            )
          ),
          min = 1, max = 10, value = 9, ticks = FALSE, step = .1
        ),
        teal.transform::picks_ui(id = ns("outlier"), spec = outlier)
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Plot settings",
          teal.widgets::optionalSliderInputValMinMax(ns("alpha"), "Opacity:", alpha, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(ns("size"), "Points size:", size, ticks = FALSE),
          teal.widgets::optionalSliderInputValMinMax(
            inputId = ns("label_min_segment"),
            label = tags$div(
              tagList(
                "Label min. segment:",
                bslib::tooltip(
                  icon("circle-info"),
                  tags$span(
                    paste(
                      "Use the slider to choose the cut-off value to define minimum distance between label and point",
                      "that generates a line segment.",
                      "It's only valid when 'Display outlier labels' is checked."
                    )
                  )
                )
              )
            ),
            value_min_max = label_segment_threshold,
            # Extra parameters to sliderInput
            ticks = FALSE,
            step = .1,
            round = FALSE
          ),
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
}

# Server function for the regression module
srv_a_regression.picks <- function(id,
                                   data,
                                   response,
                                   regressor,
                                   outlier,
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
      spec = list(response = response, regressor = regressor, outlier = outlier),
      data = data
    )

    validated_q <- reactive({
      req(data())
      validate_input(
        inputId = "response-variables-selected",
        condition = is.numeric(
          data()[[selectors$response()$datasets$selected]][[selectors$response()$variables$selected]]
        ),
        message = "A response variable needs to be numeric."
      )
      validate_input(
        inputId = "regressor-variables-selected",
        condition = length(selectors$regressor()$variables$selected) > 0,
        message = "A regressor variables need to be selected."
      )
      validate_input(
        inputId = c("regressor-variables-selected", "response-variables-selected"),
        condition = !any(selectors$regressor()$variables$selected %in% selectors$response()$variables$selected),
        message = "Response and Regressor must be different."
      )
      validate_input(
        inputId = c("show_outlier", "outlier-variables-selected"),
        condition = !(isTRUE(input$show_outlier) && length(selectors$outlier()$variables$selected) == 0),
        message = "Please provide an `Outlier label` variable"
      )

      obj <- data()
      teal.reporter::teal_card(obj) <- c(
        teal.reporter::teal_card("# Linear Regression Plot"),
        teal.reporter::teal_card(obj),
        teal.reporter::teal_card("## Module's code")
      )
      teal.code::eval_code(obj, 'library("ggplot2");library("dplyr")')
    })

    merged <- teal.transform::merge_srv("merge", data = validated_q, selectors = selectors, output_name = "anl")

    # sets qenv object and populates it with data merge call and fit expression
    fit_r <- reactive({
      obj <- req(merged$data())
      anl <- obj[["anl"]]
      teal::validate_has_data(anl, 10)

      teal::validate_has_data(
        anl[, c(merged$variables()$response, merged$variables()$regressor)], 10,
        complete = TRUE, allow_inf = FALSE
      )

      form <- stats::as.formula(
        paste(
          merged$variables()$response,
          paste(
            merged$variables()$regressor,
            collapse = " + "
          ),
          sep = " ~ "
        )
      )

      anl_fit <- within(obj, form = form, {
        fit <- stats::lm(form, data = anl)
        for (regressor in names(fit$contrasts)) {
          alts <- paste0(levels(anl[[regressor]]), collapse = "|")
          names(fit$coefficients) <- gsub(
            paste0("^(", regressor, ")(", alts, ")$"), paste0("\\1", ": ", "\\2"), names(fit$coefficients)
          )
        }
        fit_summary <- summary(fit)
        fit_summary
      })
      teal.reporter::teal_card(anl_fit) <- c(teal.reporter::teal_card(anl_fit), "## Plot")
      anl_fit
    })

    outlier_label_call <- reactive({
      substitute(
        expr = dplyr::if_else(
          data$.cooksd > outlier_cutoff * mean(data$.cooksd, na.rm = TRUE),
          as.character(stats::na.omit(anl)[[label_var]]),
          ""
        ) %>%
          dplyr::if_else(is.na(.), "cooksd == NaN", .),
        env = list(outlier_cutoff = input$outlier_cutoff, label_var = merged$variables()$outlier)
      )
    })

    outlier_label_geom <- reactive({
      substitute(
        expr = ggrepel::geom_text_repel(
          label = label_col,
          color = "red",
          hjust = 0,
          vjust = 1,
          max.overlaps = Inf,
          min.segment.length = label_min_segment,
          segment.alpha = 0.5,
          seed = 123
        ),
        env = list(label_col = outlier_label_call(), label_min_segment = input$label_min_segment)
      )
    })

    output_plot_base <- reactive({
      obj <- fit_r()
      teal.code::eval_code(
        obj,
        quote({
          class(fit$residuals) <- NULL

          data <- ggplot2::fortify(fit)

          smooth <- function(x, y) {
            as.data.frame(stats::lowess(x, y, f = 2 / 3, iter = 3))
          }

          smoothy_aes <- ggplot2::aes_string(x = "x", y = "y")

          reg_form <- deparse(fit$call[[2]])
        })
      )
    })

    output_plot_0 <- reactive({
      obj <- req(fit_r())
      fit <- obj[["fit"]]
      anl <- obj[["anl"]]

      if (!is.factor(anl[[merged$variables()$regressor]])) {
        shinyjs::show("size")
        shinyjs::show("alpha")
        plot <- substitute(
          expr = ggplot2::ggplot(fit$model[, 2:1], ggplot2::aes_string(regressor, response)) +
            ggplot2::geom_point(size = size, alpha = alpha) +
            ggplot2::stat_smooth(method = "lm", formula = y ~ x, se = FALSE),
          env = list(
            regressor = merged$variables()$regressor,
            response = merged$variables()$response,
            size = input$size,
            alpha = input$alpha
          )
        )
        if (input$show_outlier) {
          plot <- substitute(
            expr = plot + outlier_label,
            env = list(plot = plot, outlier_label = outlier_label_geom())
          )
        }
      } else {
        shinyjs::hide("size")
        shinyjs::hide("alpha")
        plot <- substitute(
          expr = ggplot2::ggplot(fit$model[, 2:1], ggplot2::aes_string(regressor, response)) +
            ggplot2::geom_boxplot(),
          env = list(regressor = merged$variables()$regressor, response = merged$variables()$response)
        )
        if (input$show_outlier) {
          plot <- substitute(
            expr = plot + outlier_label,
            env = list(plot = plot, outlier_label = outlier_label_geom())
          )
        }
      }

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Response vs Regressor"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(
            labs = list(
              title = "Response vs Regressor",
              x = varname_w_label(merged$variables()$regressor, anl),
              y = varname_w_label(merged$variables()$response, anl)
            ),
            theme = list()
          )
        ),
        ggtheme = input$ggtheme
      )

      teal.code::eval_code(
        obj,
        substitute(
          expr = {
            class(fit$residuals) <- NULL
            data <- ggplot2::fortify(fit)
            plot <- graph
          },
          env = list(
            graph = Reduce(function(x, y) call("+", x, y), c(plot, parsed_ggplot2_args))
          )
        )
      )
    })

    output_plot_1 <- reactive({
      obj <- req(output_plot_base())
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot <- substitute(
        expr = ggplot2::ggplot(data = data, ggplot2::aes(.fitted, .resid)) +
          ggplot2::geom_point(size = size, alpha = alpha) +
          ggplot2::geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
          ggplot2::geom_line(data = smoothy, mapping = smoothy_aes),
        env = list(size = input$size, alpha = input$alpha)
      )
      if (input$show_outlier) {
        plot <- substitute(
          expr = plot + outlier_label,
          env = list(plot = plot, outlier_label = outlier_label_geom())
        )
      }

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Residuals vs Fitted"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(
            labs = list(
              x = quote(paste0("Fitted values\nlm(", reg_form, ")")),
              y = "Residuals",
              title = "Residuals vs Fitted"
            )
          )
        ),
        ggtheme = input$ggtheme
      )

      within(
        obj,
        expr = {
          smoothy <- smooth(data$.fitted, data$.resid)
          plot <- graph
        },
        graph = Reduce(function(x, y) call("+", x, y), c(plot, parsed_ggplot2_args))
      )
    })

    output_plot_2 <- reactive({
      obj <- req(output_plot_base())
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot <- substitute(
        expr = ggplot2::ggplot(data = data, ggplot2::aes(sample = .stdresid)) +
          ggplot2::stat_qq(size = size, alpha = alpha) +
          ggplot2::geom_abline(linetype = "dashed"),
        env = list(size = input$size, alpha = input$alpha)
      )
      if (input$show_outlier) {
        plot <- substitute(
          expr = plot +
            ggplot2::stat_qq(
              geom = ggrepel::GeomTextRepel,
              label = label_col,
              color = "red",
              hjust = 0,
              vjust = 0,
              max.overlaps = Inf,
              min.segment.length = label_min_segment,
              segment.alpha = .5,
              seed = 123
            ),
          env = list(plot = plot, label_col = outlier_label_call(), label_min_segment = input$label_min_segment)
        )
      }

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Normal Q-Q"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(
            labs = list(
              x = quote(paste0("Theoretical Quantiles\nlm(", reg_form, ")")),
              y = "Standardized residuals",
              title = "Normal Q-Q"
            )
          )
        ),
        ggtheme = input$ggtheme
      )

      within(
        obj,
        expr = plot <- graph,
        graph = Reduce(function(x, y) call("+", x, y), c(plot, parsed_ggplot2_args))
      )
    })

    output_plot_3 <- reactive({
      obj <- req(output_plot_base())
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot <- substitute(
        expr = ggplot2::ggplot(data = data, ggplot2::aes(.fitted, sqrt(abs(.stdresid)))) +
          ggplot2::geom_point(size = size, alpha = alpha) +
          ggplot2::geom_line(data = smoothy, mapping = smoothy_aes),
        env = list(size = input$size, alpha = input$alpha)
      )
      if (input$show_outlier) {
        plot <- substitute(
          expr = plot + outlier_label,
          env = list(plot = plot, outlier_label = outlier_label_geom())
        )
      }

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Scale-Location"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(
            labs = list(
              x = quote(paste0("Fitted values\nlm(", reg_form, ")")),
              y = quote(expression(sqrt(abs(`Standardized residuals`)))),
              title = "Scale-Location"
            )
          )
        ),
        ggtheme = input$ggtheme
      )

      within(
        obj,
        expr = {
          smoothy <- smooth(data$.fitted, sqrt(abs(data$.stdresid)))
          plot <- graph
        },
        graph = Reduce(function(x, y) call("+", x, y), c(plot, parsed_ggplot2_args))
      )
    })

    output_plot_4 <- reactive({
      obj <- output_plot_base()
      shinyjs::hide("size")
      shinyjs::show("alpha")
      plot <- substitute(
        expr = ggplot2::ggplot(data = data, ggplot2::aes(seq_along(.cooksd), .cooksd)) +
          ggplot2::geom_col(alpha = alpha),
        env = list(alpha = input$alpha)
      )
      if (input$show_outlier) {
        plot <- substitute(
          expr = plot +
            ggplot2::geom_hline(
              yintercept = c(
                outlier * mean(data$.cooksd, na.rm = TRUE),
                mean(data$.cooksd, na.rm = TRUE)
              ),
              color = "red",
              linetype = "dashed"
            ) +
            ggplot2::annotate(
              geom = "text",
              x = 0,
              y = mean(data$.cooksd, na.rm = TRUE),
              label = paste("mu", "=", round(mean(data$.cooksd, na.rm = TRUE), 4)),
              vjust = -1,
              hjust = 0,
              color = "red",
              angle = 90
            ) +
            outlier_label,
          env = list(plot = plot, outlier = input$outlier_cutoff, outlier_label = outlier_label_geom())
        )
      }

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Cook's distance"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(
            labs = list(
              x = quote(paste0("Obs. number\nlm(", reg_form, ")")),
              y = "Cook's distance",
              title = "Cook's distance"
            )
          )
        ),
        ggtheme = input$ggtheme
      )

      within(
        obj,
        expr = plot <- graph,
        graph = Reduce(function(x, y) call("+", x, y), c(plot, parsed_ggplot2_args))
      )
    })

    output_plot_5 <- reactive({
      obj <- output_plot_base()
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot <- substitute(
        expr = ggplot2::ggplot(data = data, ggplot2::aes(.hat, .stdresid)) +
          ggplot2::geom_vline(
            size = 1,
            colour = "black",
            linetype = "dashed",
            xintercept = 0
          ) +
          ggplot2::geom_hline(
            size = 1,
            colour = "black",
            linetype = "dashed",
            yintercept = 0
          ) +
          ggplot2::geom_point(size = size, alpha = alpha) +
          ggplot2::geom_line(data = smoothy, mapping = smoothy_aes),
        env = list(size = input$size, alpha = input$alpha)
      )
      if (input$show_outlier) {
        plot <- substitute(
          expr = plot + outlier_label,
          env = list(plot = plot, outlier_label = outlier_label_geom())
        )
      }

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Residuals vs Leverage"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(
            labs = list(
              x = quote(paste0("Standardized residuals\nlm(", reg_form, ")")),
              y = "Leverage",
              title = "Residuals vs Leverage"
            )
          )
        ),
        ggtheme = input$ggtheme
      )

      within(
        obj,
        expr = {
          smoothy <- smooth(data$.hat, data$.stdresid)
          plot <- graph
        },
        graph = Reduce(function(x, y) call("+", x, y), c(plot, parsed_ggplot2_args))
      )
    })

    output_plot_6 <- reactive({
      obj <- output_plot_base()
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot <- substitute(
        expr = ggplot2::ggplot(data = data, ggplot2::aes(.hat, .cooksd)) +
          ggplot2::geom_vline(xintercept = 0, colour = NA) +
          ggplot2::geom_abline(
            slope = seq(0, 3, by = 0.5),
            colour = "black",
            linetype = "dashed",
            size = 1
          ) +
          ggplot2::geom_line(data = smoothy, mapping = smoothy_aes) +
          ggplot2::geom_point(size = size, alpha = alpha),
        env = list(size = input$size, alpha = input$alpha)
      )
      if (input$show_outlier) {
        plot <- substitute(
          expr = plot + outlier_label,
          env = list(plot = plot, outlier_label = outlier_label_geom())
        )
      }

      parsed_ggplot2_args <- teal.widgets::parse_ggplot2_args(
        teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Cook's dist vs Leverage"]],
          user_default = ggplot2_args$default,
          module_plot = teal.widgets::ggplot2_args(
            labs = list(
              x = quote(paste0("Leverage\nlm(", reg_form, ")")),
              y = "Cooks's distance",
              title = "Cook's dist vs Leverage"
            )
          )
        ),
        ggtheme = input$ggtheme
      )

      within(
        obj,
        expr = {
          smoothy <- smooth(data$.hat, data$.cooksd)
          plot <- graph
        },
        graph = Reduce(function(x, y) call("+", x, y), c(plot, parsed_ggplot2_args))
      )
    })

    output_q <- reactive({
      req(input$plot_type)
      validate_input(
        inputId = c("plot_type", "regressor-variables-selected"),
        condition = !(
          identical(input$plot_type, "Response vs Regressor") && length(selectors$regressor()$variables$selected) > 1
        ),
        message = "This plot works only with single Regressor variable"
      )

      switch(input$plot_type,
        "Response vs Regressor" = req(output_plot_0()),
        "Residuals vs Fitted" = req(output_plot_1()),
        "Normal Q-Q" = req(output_plot_2()),
        "Scale-Location" = req(output_plot_3()),
        "Cook's distance" = req(output_plot_4()),
        "Residuals vs Leverage" = req(output_plot_5()),
        "Cook's dist vs Leverage" = req(output_plot_6())
      )
    })

    decorated_output_q <- srv_decorate_teal_data(
      "decorator",
      data = output_q,
      decorators = select_decorators(decorators, "plot"),
      expr = quote(plot)
    )

    fitted <- reactive({
      req(decorated_output_q())
      decorated_output_q()[["fit"]]
    })
    plot_r <- reactive({
      req(decorated_output_q())
      decorated_output_q()[["plot"]]
    })

    # Insert the plot into a plot_with_settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    output$text <- renderText({
      paste(utils::capture.output(summary(fitted()))[-1], collapse = "\n")
    })

    set_chunk_dims(pws, decorated_output_q)
  })
}
