#' @export
tm_g_association.picks <- function(label = "Association",
                                   ref = picks(
                                     datasets(),
                                     variables(
                                       choices = tidyselect::where(is.numeric) |
                                         teal.transform::is_categorical(min.len = 2, max.len = 10),
                                       selected = 1
                                     ),
                                     values()
                                   ),
                                   vars = picks(
                                     datasets(),
                                     variables(
                                       choices = tidyselect::where(is.numeric) |
                                         teal.transform::is_categorical(min.len = 2, max.len = 10),
                                       selected = 2,
                                       multiple = TRUE
                                     )
                                   ),
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
  if (inherits(ggplot2_args, "ggplot2_args")) ggplot2_args <- list(default = ggplot2_args)

  # Start of assertions
  checkmate::assert_string(label)
  checkmate::assert_class(ref, "picks")
  if (isTRUE(attr(ref$variables, "multiple"))) {
    warning("`ref` accepts only a single variable selection. Forcing `variables(multiple) to FALSE`")
    attr(ref$variables, "multiple") <- FALSE
  }
  checkmate::assert_class(vars, "picks")
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

  args <- as.list(environment())
  ans <- module(
    label = label,
    ui = ui_g_association.picks,
    server = srv_g_association.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_association.picks))],
    server_args = args[names(args) %in% names(formals(srv_g_association.picks))],
    transformators = transformators,
    datanames = {
      datanames <- datanames(list(ref = ref, vars = vars))
      if (length(datanames)) datanames else "all"
    }
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the association module
ui_g_association.picks <- function(id,
                                   ref,
                                   vars,
                                   show_association,
                                   distribution_theme,
                                   association_theme,
                                   pre_output,
                                   post_output,
                                   decorators) {
  ns <- NS(id)

  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      textOutput(ns("title")),
      tags$br(),
      teal.widgets::plot_with_settings_ui(id = ns("myplot"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      teal::teal_nav_item(
        label = tags$strong("Reference variable"),
        teal.transform::module_input_ui(id = ns("ref"), spec = ref)
      ),
      teal::teal_nav_item(
        label = tags$strong("Associated variables"),
        teal.transform::module_input_ui(id = ns("vars"), spec = vars)
      ),
      checkboxInput(ns("association"), "Association with reference variable", value = show_association),
      checkboxInput(ns("show_dist"), "Scaled frequencies", value = FALSE),
      checkboxInput(ns("log_transformation"), "Log transformed", value = FALSE),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "plot")),
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
            selected = distribution_theme,
            multiple = FALSE
          ),
          selectInput(
            inputId = ns("association_theme"),
            label = "Association theme (by ggplot):",
            choices = ggplot_themes,
            selected = association_theme,
            multiple = FALSE
          )
        )
      )
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

# Server function for the association module
srv_g_association.picks <- function(id,
                                    data,
                                    ref,
                                    vars,
                                    plot_height,
                                    plot_width,
                                    ggplot2_args,
                                    decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")

  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    selectors <- teal.transform::module_input_srv(spec = list(ref = ref, vars = vars), data = data)

    validated_q <- reactive({
      obj <- req(data())
      validate_input(
        inputId = "ref-variables-selected",
        condition = !is.null(selectors$ref()$variables$selected),
        message = "A reference variable must be selected."
      )
      validate_input(
        inputId = "vars-variables-selected",
        condition = !is.null(selectors$vars()$variables$selected),
        message = "A associated variables must be selected."
      )
      validate_input(
        inputId = c("ref-variables-selected", "vars-variables-selected"),
        condition = !any(selectors$ref()$variables$selected %in% selectors$vars()$variables$selected),
        message = "Associated variables and reference variable cannot overlap"
      )
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Association Plot"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      teal.code::eval_code(obj, 'library("ggplot2");library("dplyr");library("tern");library("ggmosaic")')
    })

    merged <- teal.transform::merge_srv("merge", data = validated_q, selectors = selectors, output_name = "anl")

    output_q <- reactive({
      req(merged$data())
      logger::log_debug("srv_g_association@1 recalculating a plot")
      anl <- merged$data()[["anl"]]
      ref_name <- merged$merge_vars()$ref
      vars_names <- merged$merge_vars()$vars
      teal::validate_has_data(anl, 3)
      teal::validate_has_data(anl[, c(ref_name, vars_names)], 3, complete = TRUE, allow_inf = FALSE)

      association <- input$association
      show_dist <- input$show_dist
      log_transformation <- input$log_transformation
      rotate_xaxis_labels <- input$rotate_xaxis_labels
      swap_axes <- input$swap_axes
      distribution_theme <- input$distribution_theme
      association_theme <- input$association_theme

      is_scatterplot <- is.numeric(anl[[ref_name]]) && any(vapply(anl[vars_names], is.numeric, logical(1)))
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

      # reference
      ref_class <- class(anl[[ref_name]])[1]
      if (is.numeric(anl[[ref_name]]) && log_transformation) {
        # works for both integers and doubles
        ref_cl_name <- call("log", as.name(ref_name))
        ref_cl_lbl <- varname_w_label(ref_name, anl, prefix = "Log of ")
      } else {
        # silently ignore when non-numeric even if `log` is selected because some
        # variables may be numeric and others not
        ref_cl_name <- as.name(ref_name)
        ref_cl_lbl <- varname_w_label(ref_name, anl)
      }

      user_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
        user_plot = ggplot2_args[["Bivariate1"]],
        user_default = ggplot2_args$default
      )

      ref_call <- bivariate_plot_call(
        data_name = "anl",
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
        if (is.numeric(anl[[var_i]]) && log_transformation) {
          # works for both integers and doubles
          var_cl_name <- call("log", as.name(var_i))
          var_cl_lbl <- varname_w_label(var_i, anl, prefix = "Log of ")
        } else {
          # silently ignore when non-numeric even if `log` is selected because some
          # variables may be numeric and others not
          var_cl_name <- as.name(var_i)
          var_cl_lbl <- varname_w_label(var_i, anl)
        }

        user_ggplot2_args <- teal.widgets::resolve_ggplot2_args(
          user_plot = ggplot2_args[["Bivariate2"]],
          user_default = ggplot2_args$default
        )

        bivariate_plot_call(
          data_name = "anl",
          x = ref_cl_name,
          y = var_cl_name,
          x_class = ref_class_cov,
          y_class = class(anl[[var_i]])[1],
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
        if (is.numeric(anl[[x]]) && log_transformation) {
          varname_w_label(x, anl, prefix = "Log of ")
        } else {
          varname_w_label(x, anl)
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
      obj <- merged$data()

      teal.reporter::teal_card(obj) <- c(teal.reporter::teal_card(obj), "## Plot")
      within(
        obj,
        expr = {
          title <- new_title
          ref_plot <- plot1
          var_plot <- plot2
          plot <- gridExtra::arrangeGrob(ref_plot, var_plot, ncol = 1)
        },
        new_title = new_title,
        plot1 = ref_call,
        plot2 = var_calls[[1]]
      )
    })

    decorated_output_grob_q <- srv_decorate_teal_data(
      id = "decorator",
      data = output_q,
      decorators = select_decorators(decorators, "plot"),
      expr = quote({
        grid::grid.newpage()
        grid::grid.draw(plot)
      })
    )

    plot_r <- reactive({
      req(decorated_output_grob_q())[["plot"]]
    })

    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )


    output$title <- renderText(output_q()[["title"]])

    set_chunk_dims(pws, decorated_output_grob_q)
  })
}
