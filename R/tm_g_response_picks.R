#' @export
tm_g_response.picks <- function(label = "Response Plot",
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

  # Start of assertions
  checkmate::assert_string(label)

  checkmate::assert_class(response, "picks")
  if (isTRUE(attr(response$variables, "multiple"))) {
    warning("`response` accepts only a single variable selection. Forcing `variables(multiple) to FALSE`")
    attr(response$variables, "multiple") <- FALSE
  }

  checkmate::assert_class(x, "picks")
  if (isTRUE(attr(x$variables, "multiple"))) {
    warning("`x` accepts only a single variable selection. Forcing `variables(multiple) to FALSE`")
    attr(x$variables, "multiple") <- FALSE
  }

  checkmate::assert_class(row_facet, "picks", null.ok = TRUE)
  checkmate::assert_class(col_facet, "picks", null.ok = TRUE)
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


  args <- as.list(environment())
  ans <- module(
    label = label,
    ui = ui_g_response.picks,
    server = srv_g_response.picks,
    ui_args = args[names(args) %in% names(formals(ui_g_response.picks))],
    server_args = args[names(args) %in% names(formals(srv_g_response.picks))],
    transformators = transformators,
    datanames = {
      datanames <- datanames(list(response, x, row_facet, col_facet))
      if (length(datanames)) datanames else "all"
    }
  )
  attr(ans, "teal_bookmarkable") <- TRUE
  ans
}

# UI function for the response module
ui_g_response.picks <- function(id,
                                response,
                                x,
                                row_facet,
                                col_facet,
                                freq,
                                count_labels,
                                rotate_xaxis_labels,
                                coord_flip,
                                ggtheme,
                                pre_output,
                                post_output,
                                decorators) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = teal.widgets::white_small_well(
      teal.widgets::plot_with_settings_ui(id = ns("myplot"))
    ),
    encoding = tags$div(
      tags$label("Encodings", class = "text-primary"),
      teal::teal_nav_item(
        label = tags$strong("Response variable"),
        teal.transform::module_input_ui(id = ns("response"), spec = response)
      ),
      teal::teal_nav_item(
        label = tags$strong("X variable"),
        teal.transform::module_input_ui(id = ns("x"), spec = x)
      ),
      if (!is.null(row_facet)) {
        teal::teal_nav_item(
          label = tags$strong("Row facetting"),
          teal.transform::module_input_ui(id = ns("row_facet"), spec = row_facet)
        )
      },
      if (!is.null(col_facet)) {
        teal::teal_nav_item(
          label = tags$strong("Column facetting"),
          teal.transform::module_input_ui(id = ns("col_facet"), spec = col_facet)
        )
      },
      shinyWidgets::radioGroupButtons(
        inputId = ns("freq"),
        label = NULL,
        choices = c("frequency", "density"),
        selected = ifelse(freq, "frequency", "density"),
        justified = TRUE
      ),
      ui_decorate_teal_data(ns("decorator"), decorators = select_decorators(decorators, "plot")),
      bslib::accordion(
        open = TRUE,
        bslib::accordion_panel(
          title = "Plot settings",
          checkboxInput(ns("count_labels"), "Add count labels", value = count_labels),
          checkboxInput(ns("coord_flip"), "Swap axes", value = coord_flip),
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = rotate_xaxis_labels),
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
    forms = tagList(
      teal.widgets::verbatim_popup_ui(ns("rcode"), "Show R code")
    ),
    pre_output = pre_output,
    post_output = post_output
  )
}

# Server function for the response module
srv_g_response.picks <- function(id,
                                 data,
                                 response,
                                 x,
                                 row_facet,
                                 col_facet,
                                 plot_height,
                                 plot_width,
                                 ggplot2_args,
                                 decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    selectors <- teal.transform::module_input_srv(
      spec = list(
        response = response,
        x = x,
        row_facet = row_facet,
        col_facet = col_facet
      ),
      data = data
    )

    validated_q <- reactive({
      validate_input(
        inputId = "response-variables-selected",
        condition = !is.null(selectors$response()$variables$selected),
        message = "A `response` variable needs to be selected."
      )
      validate_input(
        inputId = "x-variables-selected",
        condition = !is.null(selectors$x()$variables$selected),
        message = "A `x` variable needs to be selected."
      )
      validate_input(
        inputId = c("response-variables-selected", "x-variables-selected"),
        condition = !any(selectors$response()$variables$selected %in% selectors$x()$variables$selected),
        message = "Response and X variables must be different."
      )
      validate_input(
        inputId = "row_facet-variables-selected",
        condition = is.null(row_facet) || length(selectors$row_facet()$variables$selected) < 2,
        message = "Only single Row Facetting variable is allowed."
      )
      validate_input(
        inputId = "col_facet-variables-selected",
        condition = is.null(col_facet) || length(selectors$col_facet()$variables$selected) < 2,
        message = "Only single Column Facetting variable is allowed."
      )
      validate_input(
        inputId = c("row_facet-variables-selected", "col_facet-variables-selected"),
        condition = is.null(row_facet) || is.null(col_facet) ||
          !any(selectors$row_facet()$variables$selected %in% selectors$col_facet()$variables$selected),
        message = "Row and Column Facetting variables must be different."
      )

      obj <- req(data())
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Response Plot"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      teal.code::eval_code(obj, 'library("ggplot2");library("dplyr");')
    })

    merged <- teal.transform::merge_srv("merge", data = validated_q, selectors = selectors, output_name = "anl")


    output_q <- reactive({
      validate_input(
        inputId = "ggtheme",
        condition = length(input$ggtheme) > 0,
        message = "Row and Col Facetting variables must be different."
      )

      qenv <- merged$data()
      anl <- qenv[["anl"]]
      response_var <- merged$merge_vars()$response
      x_var <- merged$merge_vars()$x

      validate(need(is.factor(anl[[response_var]]), "Please select a factor variable as the response."))
      validate(need(is.factor(anl[[x_var]]), "Please select a factor variable as the X-Variable."))
      teal::validate_has_data(anl, 10)
      teal::validate_has_data(anl[, c(response_var, x_var)], 10, complete = TRUE, allow_inf = FALSE)

      row_facet_var <- merged$merge_vars()$row_facet
      col_facet_var <- merged$merge_vars()$col_facet

      freq <- input$freq == "frequency"
      swap_axes <- input$coord_flip
      counts <- input$count_labels
      rotate_xaxis_labels <- input$rotate_xaxis_labels
      ggtheme <- input$ggtheme

      arg_position <- if (freq) "stack" else "fill"

      if (swap_axes) {
        qenv <- within(
          qenv,
          expr = anl[[x_var]] <- with(anl, forcats::fct_rev(x_cl)),
          x_var = x_var,
          x_cl = as.name(x_var)
        )
      }

      qenv <- within(
        qenv,
        expr = {
          anl[[response_var]] <- factor(anl[[response_var]])

          anl2 <- anl %>%
            dplyr::group_by_at(dplyr::vars(x_cl, response_cl, row_facet_cl, col_facet_cl)) %>%
            dplyr::summarise(ns = dplyr::n()) %>%
            dplyr::group_by_at(dplyr::vars(x_cl, row_facet_cl, col_facet_cl)) %>%
            dplyr::mutate(sums = sum(ns), percent = round(ns / sums * 100, 1))

          anl3 <- anl %>%
            dplyr::group_by_at(dplyr::vars(x_cl, row_facet_cl, col_facet_cl)) %>%
            dplyr::summarise(ns = dplyr::n())
        },
        response_var = response_var,
        response_cl = as.name(response_var),
        x_cl = as.name(x_var),
        row_facet_cl = if (length(row_facet_var)) as.name(row_facet_var),
        col_facet_cl = if (length(col_facet_var)) as.name(col_facet_var)
      )

      plot_call <- substitute(
        expr = ggplot2::ggplot(anl2, ggplot2::aes(x = x_cl, y = ns)) +
          ggplot2::geom_bar(ggplot2::aes(fill = response_cl), stat = "identity", position = arg_position),
        env = list(
          x_cl = as.name(x_var),
          response_cl = as.name(response_var),
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
              data = anl2,
              ggplot2::aes(label = ns, x = x_cl, y = ns, group = response_cl),
              col = "white",
              vjust = "middle",
              hjust = "middle",
              position = position_anl2_value
            ) +
            ggplot2::geom_text(
              data = anl3, ggplot2::aes(label = ns, x = x_cl, y = anl3_y),
              hjust = hjust_value,
              vjust = vjust_value,
              position = position_anl3_value
            ),
          env = list(
            plot_call = plot_call,
            x_cl = as.name(x_var),
            response_cl = as.name(response_var),
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

      facet_cl <- facet_ggplot_call(row_facet_var, col_facet_var)

      if (!is.null(facet_cl)) {
        plot_call <- substitute(expr = plot_call + facet_cl, env = list(plot_call = plot_call, facet_cl = facet_cl))
      }

      dev_ggplot2_args <- teal.widgets::ggplot2_args(
        labs = list(
          x = varname_w_label(x_var, anl),
          y = varname_w_label(response_var, anl, prefix = "Proportion of "),
          fill = varname_w_label(response_var, anl)
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

      teal.reporter::teal_card(qenv) <- c(teal.reporter::teal_card(qenv), "## Plot")
      teal.code::eval_code(qenv, plot_call)
    })

    decorated_output_plot_q <- srv_decorate_teal_data(
      id = "decorator",
      data = output_q,
      decorators = select_decorators(decorators, "plot"),
      expr = quote(plot)
    )

    plot_r <- reactive(req(decorated_output_plot_q())[["plot"]])

    # Insert the plot into a plot_with_settings module from teal.widgets
    pws <- teal.widgets::plot_with_settings_srv(
      id = "myplot",
      plot_r = plot_r,
      height = plot_height,
      width = plot_width
    )

    decorated_output_dims_q <- set_chunk_dims(pws, decorated_output_plot_q)

    # Render R code.
    source_code_r <- reactive(teal.code::get_code(req(decorated_output_dims_q())))

    teal.widgets::verbatim_popup_srv(
      id = "rcode",
      verbatim_content = source_code_r,
      title = "Show R Code for Response"
    )
    decorated_output_dims_q
  })
}
