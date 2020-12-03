#' Scatterplot and Regression Model
#' @md
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#' @param regressor (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'  Regressor variables from an incoming dataset with filtering and selecting.
#' @param response (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'  Response variables from an incoming dataset with filtering and selecting.
#' @param alpha optional, (`numeric`) If scalar then the plot points will have a fixed opacity. If a
#'   slider should be presented to adjust the plot point opacity dynamically then it can be a vector of
#'   length three with `c(value, min, max)`.
#' @param size optional, (`numeric`) If scalar then the plot point sizes will have a fixed size
#'   If a slider should be presented to adjust the plot point sizes dynamically then it can be a
#'   vector of length three with `c(value, min, max)`.
#' @param default_outlier_label optional, (`character`) The default column selected to label outliers.
#' @param default_plot_type optional, (`numeric`) Defaults to Response vs Regressor.
#'
#' 1. Response vs Regressor
#' 2. Residuals vs Fitted
#' 3. Normal Q-Q
#' 4. Scale-Location
#' 5. Cooks distance
#' 6. Residuals vs Leverage
#' 7. Cooks dist vs Leverage
#'
#' @note For more examples, please see the vignette "Using regression plots" via
#'   `vignette("using-regression-plots", package = "teal.modules.general")`.
#' @export
#'
#' @examples
#' # Regression graphs from selected response variable (BMRKR1) and
#' # selected regressors (AGE)
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
#'     tm_a_regression(
#'       label = "Regression",
#'       response = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = "BMRKR1",
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       ),
#'       regressor = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = variable_choices(ADSL, c("AGE", "SEX", "RACE")),
#'           selected = "AGE",
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_a_regression <- function(label = "Regression Analysis",
                            regressor,
                            response,
                            plot_height = c(600, 200, 2000),
                            plot_width = NULL,
                            alpha = c(1, 0, 1),
                            size = c(2, 1, 8),
                            ggtheme = gg_themes,
                            pre_output = NULL,
                            post_output = NULL,
                            default_plot_type = 1,
                            default_outlier_label = "USUBJID") {
  if (!is_class_list("data_extract_spec")(regressor)) {
    regressor <- list(regressor)
  }
  if (!is_class_list("data_extract_spec")(response)) {
    response <- list(response)
  }

  ggtheme <- match.arg(ggtheme)

  stop_if_not(
    is_character_single(label),
    is_class_list("data_extract_spec")(response),
    list(
      all(vapply(response, function(x) {
        !isTRUE(x$select$multiple)
        }, logical(1))),
      "Response variable should not allow multiple selection"
      ),
    is_class_list("data_extract_spec")(regressor),
    # No check necessary for regressor and response, as checked in data_extract_input
    is_character_single(ggtheme),
    is_character_single(default_outlier_label)
    )
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  # Send ui args
  args <- as.list(environment())

  data_extract_list <- list(
    regressor = regressor,
    response = response
  )

  module(
    label = label,
    server = srv_a_regression,
    ui = ui_a_regression,
    ui_args = args,
    server_args = c(
      data_extract_list,
      list(plot_height = plot_height, plot_width = plot_width, default_outlier_label = default_outlier_label)),
    filters = get_extract_datanames(data_extract_list)
  )
}

ui_a_regression <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  is_single_dataset_value <- is_single_dataset(args$regressor, args$response)

  plot_choices <- c(
    "Response vs Regressor",
    "Residuals vs Fitted",
    "Normal Q-Q",
    "Scale-Location",
    "Cook's distance",
    "Residuals vs Leverage",
    "Cook's dist vs Leverage h[ii]/(1 - h[ii])"
  )

  standard_layout(
    output = white_small_well(tags$div(
      plot_with_settings_ui(id = ns("myplot"), height = args$plot_height, width = args$plot_width),
      tags$div(verbatimTextOutput(ns("text")))
    )),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("response", "regressor")]),
      data_extract_input(
        id = ns("response"),
        label = "Response variable",
        data_extract_spec = args$response,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("regressor"),
        label = "Regressor variables",
        data_extract_spec = args$regressor,
        is_single_dataset = is_single_dataset_value
      ),
      radioButtons(
        ns("plot_type"),
        label = "Plot type:",
        choices = plot_choices,
        selected = plot_choices[args$default_plot_type]
      ),
      checkboxInput(ns("show_outlier"), label = "Display outlier labels", value = TRUE),
      shinyjs::hidden(optionalSliderInput(
        ns("outlier"),
        div(
          "Outlier definition:",
          title = paste(
            "Use the slider to choose the cut-off value to define outliers. Points with a Cook's distance greater than",
            "the value on the slider times the mean of the Cook's distance of the dataset will have labels."),
          icon("info-circle")
        ), min = 1, max = 10, value = 9, ticks = FALSE, step = .1)),
      shinyjs::hidden(optionalSelectInput(
        ns("label_var"),
        multiple = FALSE,
        label = "Outlier label"
      )),
      panel_group(
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("alpha"), "Opacity:", args$alpha, ticks = FALSE),
          optionalSliderInputValMinMax(ns("size"), "Points size:", args$size, ticks = FALSE),
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
#' @importFrom dplyr if_else filter
#' @importFrom methods is substituteDirect
#' @importFrom stats as.formula
#' @importFrom stats lowess
#' @importFrom shinyjs show hide
srv_a_regression <- function(input,
                             output,
                             session,
                             datasets,
                             response,
                             regressor,
                             plot_height,
                             plot_width,
                             default_outlier_label) {
  init_chunks()

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(response, regressor),
    input_id = c("response", "regressor")
  )

  regression_var <- reactive(
    list(
      response = as.vector(merged_data()$columns_source$response),
      regressor = as.vector(merged_data()$columns_source$regressor))
  )

  # sets chunk object and populates it with data merge call and fit expression
  fit <- reactive({
    chunks_stack <- chunks$new()

    chunks_reset(chunks = chunks_stack)

    chunks_push_chunks(merged_data()$chunks, chunks = chunks_stack)

    ANL <- chunks_get_var("ANL", chunks = chunks_stack) # nolint
    validate_has_data(ANL, 10)

    opts <- variable_choices(ANL)
    selected <- if (!is.null(isolate(input$label_var)) && isolate(input$label_var) %in% as.character(opts)) {
      isolate(input$label_var)
      } else {
        if_empty(opts[as.character(opts) == default_outlier_label], opts[[1]])
      }
    updateOptionalSelectInput(
      session = session,
      inputId = "label_var",
      choices = opts,
      selected = selected)

    # validation
    validate(
      need(
        length(regression_var()$regressor) > 0,
        "At least one regressor should be selected."
        )
      )
    validate(
      need(
        length(regression_var()$response) == 1,
        "Response variable should be of length one."
        )
      )
    validate(need(is.numeric(ANL[regression_var()$response][[1]]), "Response variable should be numeric."))
    validate(
      need(
        input$plot_type != "Response vs Regressor" || length(regression_var()$regressor) == 1,
        "Response vs Regressor is only provided for exactly one regressor"
        )
      )

    validate_has_data(
      ANL[, c(regression_var()$response, regression_var()$regressor)], 10, complete = TRUE, allow_inf = FALSE)

    form <- stats::as.formula(
      paste(
        regression_var()$response,
        paste(
          regression_var()$regressor,
          collapse = " + "
        ),
        sep = " ~ "
      )
    )

    chunks_push_new_line(chunks = chunks_stack)

    chunks_push(
      id = "form",
      expression = bquote(form <- .(form)),
      chunks = chunks_stack
    )

    chunks_push(
      id = "fit_lm",
      expression = quote(fit <- lm(form, data = ANL)) %>% substituteDirect(list(form = form)),
      chunks = chunks_stack
    )

    chunks_push(id = "summary", expression = quote(summary(fit)), chunks = chunks_stack)

    chunks_safe_eval(chunks = chunks_stack)

    data <- fortify(lm(form, data = ANL))

    cooksd <- data$.cooksd[!is.nan(data$.cooksd)]
    max_outlier <- max(ceiling(max(cooksd) / mean(cooksd)), 2)

    cur_outlier <- isolate(input$outlier)
    updateSliderInput(
      session = session,
      inputId = "outlier",
      min = 1,
      max = max_outlier,
      value = if (cur_outlier < max_outlier) cur_outlier else max_outlier * .9)

    chunks_stack
  })

  label_col <- reactive({
    validate(need(input$label_var, "`Display outlier labels` field is checked but `Outlier label` field is empty"))
    bquote(
      dplyr::if_else(
        data$.cooksd > .(input$outlier) * mean(data$.cooksd, na.rm = TRUE),
        as.character(ANL[[.(input$label_var)]]),
        "") %>% dplyr::if_else(is.na(.), "cooksd == NaN", .)
    )
  })

  outlier_label <- reactive({
    bquote(geom_text(
      label = .(label_col()),
      hjust = 0,
      vjust = 1,
      color = "red"))
  })

  observeEvent(input$show_outlier, {
    if (input$show_outlier) {
      shinyjs::show("outlier")
      shinyjs::show("label_var")
    } else {
      shinyjs::hide("outlier")
      shinyjs::hide("label_var")
    }
  })


  plot_r <- reactive({
    alpha <- input$alpha # nolint
    size <- input$size # nolint
    ggtheme <- input$ggtheme # nolint
    input_type <- input$plot_type
    show_outlier <- input$show_outlier

    chunks_reset()
    chunks_push_chunks(fit())

    validate(need(!is.null(ggtheme), "Please select a theme."))

    plot_type_0 <- function() {
      fit <- chunks_get_var("fit") # chunk already evaluated
      ANL <- chunks_get_var("ANL") # nolint

      stopifnot(ncol(fit$model) == 2)

      if (!is.factor(ANL[[regression_var()$regressor]])) {
        shinyjs::show("size")
        shinyjs::show("alpha")
        plot <- bquote(ggplot(
          fit$model[, 2:1],
          aes_string(.(regression_var()$regressor), .(regression_var()$response))
          ) +
          geom_point(size = .(size), alpha = .(alpha)) +
          stat_smooth(
            method = "lm",
            formula = y ~ x,
            se = FALSE
          )
        )
        if (show_outlier) {
          plot <- bquote(.(plot) + .(outlier_label()))
        }
        chunks_push(id = "plot_0_a", expression = bquote({
          class(fit$residuals) <- NULL
          data <- fortify(fit)
          gg <- .(plot)
        }))
      } else {
        shinyjs::hide("size")
        shinyjs::hide("alpha")
        plot <- bquote(
          ggplot(
            fit$model[, 2:1],
            aes_string(.(regression_var()$regressor), .(regression_var()$response))
          ) +
          geom_boxplot()
        )
        if (show_outlier) {
          plot <- bquote(.(plot) + .(outlier_label()))
        }
        chunks_push(id = "plot_0_a", expression = bquote({
          class(fit$residuals) <- NULL
          data <- fortify(fit)
          gg <- .(plot)
        }))
      }

      chunks_push(id = "plot_0_b", expression = bquote({
        g_final <- gg +
          xlab(.(varname_w_label(regression_var()$regressor, ANL))) +
          ylab(.(varname_w_label(regression_var()$response, ANL))) +
          ggtitle("Response vs Regressor") +
          .(call(paste0("theme_", ggtheme)))
        print(g_final)
      }))
    }

    plot_base <- function() {
      chunks_push(id = "plot_0_c", expression = bquote({
        class(fit$residuals) <- NULL

        data <- fortify(fit)

        smooth <- function(x, y) {
          as.data.frame(stats::lowess(x, y, f = 2 / 3, iter = 3))
        }

        smoothy_aes <- ggplot2::aes_string(x = "x", y = "y")

        reg_form <- deparse(fit$call[[2]])
      }))
    }

    plot_type_1 <- function() {
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot <- bquote(
        ggplot(data = data, aes(.fitted, .resid)) +
        geom_point(size = .(size), alpha = .(alpha)) +
        geom_hline(yintercept = 0, linetype = "dashed", size = 1) +
        geom_line(data = smoothy, mapping = smoothy_aes))
      if (show_outlier) {
        plot <- bquote(.(plot) + .(outlier_label()))
      }
      chunks_push(id = "plot_1", expression = bquote({
        smoothy <- smooth(data$.fitted, data$.resid)
        g_base <- .(plot)
        g_final <- g_base + labs(
          x = paste0("Fitted values\nlm(", reg_form, ")"),
          y = "Residuals",
          title = .(input_type)
        ) + .(call(paste0("theme_", ggtheme)))
        print(g_final)
      }))
    }

    plot_type_2 <- function() {
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot <- bquote(
        ggplot(data = data, aes(sample = .stdresid)) +
          stat_qq(size = .(size), alpha = .(alpha)) +
          geom_abline(linetype = "dashed")
      )
      if (show_outlier) {
        plot <- bquote(
          .(plot) +
            stat_qq(
              geom = "text",
              label = .(label_col()) %>%
                data.frame(label = .) %>%
                dplyr::filter(label != "cooksd == NaN") %>%
                unlist(),
              hjust = 0,
              vjust = 1,
              color = "red"
            )
          )
      }
      chunks_push(id = "plot_2", expression = bquote({
        g_base <- .(plot)
        g_final <- g_base + labs(
          x = paste0("Theoretical Quantiles\nlm(", reg_form, ")"),
          y = "Standardized residuals",
          title = .(input_type)
        ) +
          .(call(paste0("theme_", ggtheme)))
        print(g_final)
      }))
    }

    plot_type_3 <- function() {
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot <- bquote(
        ggplot(data = data, aes(.fitted, sqrt(abs(.stdresid)))) +
          geom_point(size = .(size), alpha = .(alpha)) +
          geom_line(data = smoothy, mapping = smoothy_aes)
      )
      if (show_outlier) {
        plot <- bquote(.(plot) + .(outlier_label()))
      }
      chunks_push(id = "plot_3", expression = bquote({
        smoothy <- smooth(data$.fitted, sqrt(abs(data$.stdresid)))
        g_base <- .(plot)
        g_final <- g_base + labs(
          x = paste0("Fitted values\nlm(", reg_form, ")"),
          y = expression(sqrt(abs(`Standardized residuals`))),
          title = .(input_type)
        ) +
          .(call(paste0("theme_", ggtheme)))
        print(g_final)
      }))
    }

    plot_type_4 <- function() {
      shinyjs::hide("size")
      shinyjs::show("alpha")
      plot <- bquote(
        ggplot(data = data, aes(seq_along(.cooksd), .cooksd)) +
          geom_col(alpha = .(alpha))
      )
      if (show_outlier) {
        plot <- bquote(
          .(plot) +
            geom_hline(
              yintercept = c(
                .(input$outlier) * mean(data$.cooksd, na.rm = TRUE),
                mean(data$.cooksd, na.rm = TRUE)),
              color = "red",
              linetype = "dashed") +
            geom_text(
              aes(
                x = 0,
                y = mean(data$.cooksd, na.rm = TRUE),
                label = paste("mu", "=", round(mean(data$.cooksd, na.rm = TRUE), 4)),
                vjust = -1,
                hjust = 0,
                color = "red",
                angle = 90),
              parse = TRUE,
              show.legend = FALSE) +
            .(outlier_label())
          )
      }
      chunks_push(id = "plot_4", expression = bquote({
        g_base <- .(plot)
        g_final <- g_base + labs(
          x = paste0("Obs. number\nlm(", reg_form, ")"),
          y = "Cook's distance",
          title = .(input_type)
        ) +
          .(call(paste0("theme_", ggtheme)))
        print(g_final)
      }))
    }


    plot_type_5 <- function() {
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot <- bquote(
        ggplot(data = data, aes(.hat, .stdresid)) +
          geom_vline(
            size = 1,
            colour = "black",
            linetype = "dashed",
            xintercept = 0
          ) +
          geom_hline(
            size = 1,
            colour = "black",
            linetype = "dashed",
            yintercept = 0
          ) +
          geom_point(size = .(size), alpha = .(alpha)) +
          geom_line(data = smoothy, mapping = smoothy_aes)
      )
      if (show_outlier) {
        plot <- bquote(.(plot) + .(outlier_label()))
      }
      chunks_push(id = "plot_5", expression = bquote({
        smoothy <- smooth(data$.hat, data$.stdresid)
        g_base <- .(plot)
        g_final <- g_base + labs(
          x = paste0("Standardized residuals\nlm(", reg_form, ")"),
          y = "Leverage",
          title = .(input_type)
        ) +
          .(call(paste0("theme_", ggtheme)))
        print(g_final)
      }))
    }


    plot_type_6 <- function() {
      shinyjs::show("size")
      shinyjs::show("alpha")
      plot <- bquote(
        ggplot(data = data, aes(.hat, .cooksd)) +
          geom_vline(xintercept = 0, colour = NA) +
          geom_abline(
            slope = seq(0, 3, by = 0.5),
            colour = "black",
            linetype = "dashed",
            size = 1
          ) +
          geom_line(data = smoothy, mapping = smoothy_aes) +
          geom_point(size = .(size), alpha = .(alpha))
      )
      if (show_outlier) {
        plot <- bquote(.(plot) + .(outlier_label()))
      }
      chunks_push(id = "plot_6", expression = bquote({
        smoothy <- smooth(data$.hat, data$.cooksd)
        g_base <- .(plot)
        g_final <- g_base + labs(
          x = paste0("Leverage\nlm(", reg_form, ")"),
          y = "Cooks's distance",
          title = .(input_type)
        ) +
          .(call(paste0("theme_", ggtheme)))
        print(g_final)
      }))
    }

    if (input_type == "Response vs Regressor") {
      plot_type_0()
    } else {
      plot_base()
      switch(input_type,
        "Residuals vs Fitted"  = plot_type_1(),
        "Normal Q-Q" = plot_type_2(),
        "Scale-Location" =  plot_type_3(),
        "Cook's distance" =  plot_type_4(),
        "Residuals vs Leverage" =  plot_type_5(),
        "Cook's dist vs Leverage h[ii]/(1 - h[ii])" =  plot_type_6()
      )
    }

    chunks_safe_eval()
  })


  code_header <- reactive(chunks_get_var("form", chunks = fit()))

  # Insert the plot into a plot_with_settings module from teal.devel
  callModule(
    plot_with_settings_srv,
    id = "myplot",
    plot_r = plot_r,
    height = plot_height,
    width = plot_width
  )

  fitted <- reactive(chunks_get_var("fit", chunks = fit()))

  output$text <- renderPrint({
    summary(fitted())
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(response, regressor)),
    modal_title = "R code for the regression plot",
    code_header = paste0(
      "Regression plot of ",
      format(code_header())
    )
  )
}
