#' Create a simple scatterplot
#'
#' Create a plot with the \code{\link{ggplot2}[geom_point]} function
#' @md
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#' @param x (`data_extract_spec` or `list` of multiple `data_extract_spec`) Variable
#'   names selected to plot along the x-axis by default.
#' @param y (`data_extract_spec` or `list` of multiple `data_extract_spec`) Variable
#'   names selected to plot along the y-axis by default.
#' @param color_by optional (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Defines the color encoding. If `NULL` then no color encoding option will be displayed.
#' @param size_by optional (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Defines the point size encoding. If `NULL` then no size encoding option will be displayed.
#' @param row_facet optional, (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Which data columns to use for faceting rows.
#' @param col_facet optional, (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Which data to use for faceting columns.
#' @param alpha optional, (`numeric`) If scalar then the plot points will have a fixed opacity. If a
#'   slider should be presented to adjust the plot point opacity dynamically then it can be a vector of
#'   length three with `c(value, min, max)`.
#' @param size optional, (`numeric`) If scalar then the plot point sizes will have a fixed size
#'   If a slider should be presented to adjust the plot point sizes dynamically then it can be a
#'   vector of length three with `c(value, min, max)`.
#' @param shape optional, (`character`) A character vector with the English names of the
#'   shape, e.g. `c("triangle", "square", "circle")`. It defaults to `shape_names`. This is a complete list from
#'   `vignette("ggplot2-specs", package="ggplot2")`.
#' @param max_deg optional, (`integer`) The maximum degree for the polynomial trend line. Must not be less than 1.
#' @param table_dec optional, (`integer`) Number of decimal places used to round numeric values in the table.
#'
#' @importFrom shinyjs show hide hidden
#' @importFrom stats coef lm
#'
#' @note For more examples, please see the vignette "Using scatterplot" via
#'   `vignette("using-scatterplot", package = "teal.modules.general")`.
#'
#' @export
#' @examples
#' # Scatterplot of variables from ADSL dataset
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
#'     tm_g_scatterplot(
#'       label = "Scatterplot Choices",
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL, c("AGE", "BMRKR1", "BMRKR2")),
#'           selected = "AGE",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'            choices = variable_choices(ADSL, c("AGE", "BMRKR1", "BMRKR2")),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
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
                             ggtheme = gg_themes,
                             pre_output = NULL,
                             post_output = NULL,
                             table_dec = 4) {
  if (!is_class_list("data_extract_spec")(x)) {
    x <- list(x)
  }
  if (!is_class_list("data_extract_spec")(y)) {
    y <- list(y)
  }
  if (!is_class_list("data_extract_spec")(color_by)) {
    color_by <- list_or_null(color_by)
  }
  if (!is_class_list("data_extract_spec")(size_by)) {
    size_by <- list_or_null(size_by)
  }
  if (!is_class_list("data_extract_spec")(row_facet)) {
    row_facet <- list_or_null(row_facet)
  }
  if (!is_class_list("data_extract_spec")(col_facet)) {
    col_facet <- list_or_null(col_facet)
  }

  ggtheme <- match.arg(ggtheme)

  stop_if_not(
    is_character_single(label),
    is_class_list("data_extract_spec")(x),
    is_class_list("data_extract_spec")(y),
    list(is_character_vector(shape) && length(shape) > 0, "`shape` must be a character vector of length 1 or more"),
    is.null(size_by) || is_class_list("data_extract_spec")(size_by),
    is.null(color_by) || is_class_list("data_extract_spec")(color_by),
    is.null(row_facet) || is_class_list("data_extract_spec")(row_facet),
    is.null(col_facet) || is_class_list("data_extract_spec")(col_facet),
    is_character_single(ggtheme),
    list(is_numeric_single(max_deg), "`max_deg` must be an integer vector of length of 1"),
    list(
      max_deg < Inf && max_deg == as.integer(max_deg) && max_deg >= 1,
      "`max_deg` must be a finite whole number greater than zero"),
    is_numeric_single(table_dec)
  )

  check_slider_input(alpha, allow_null = FALSE, allow_single = TRUE)
  check_slider_input(size, allow_null = FALSE, allow_single = TRUE)
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())

  data_extract_list <- list(
    x = x,
    y = y,
    color_by = color_by,
    size_by = size_by,
    row_facet = row_facet,
    col_facet = col_facet
  )

  module(
    label = label,
    server = srv_g_scatterplot,
    ui = ui_g_scatterplot,
    ui_args = args,
    server_args = c(
      data_extract_list,
      list(plot_height = plot_height, plot_width = plot_width, table_dec = table_dec)),
    filters = get_extract_datanames(data_extract_list)
  )
}

#' @importFrom colourpicker colourInput
ui_g_scatterplot <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  is_single_dataset_value <- is_single_dataset(
    args$x, args$y, args$color_by, args$size_by, args$row_facet, args$col_facet)

  standard_layout(
    output = white_small_well(
      plot_with_settings_ui(id = ns("scatter_plot")),
      tags$h1("Selected points:", style = "text-align:center; font-weight: bold; font-size:150%;"),
      DT::dataTableOutput(ns("data_table"), width = "100%")
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("x", "y", "color_by", "size_by", "row_facet", "col_facet")]),
      data_extract_input(
        id = ns("x"),
        label = "X variable",
        data_extract_spec = args$x,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_input(
        id = ns("y"),
        label = "Y variable",
        data_extract_spec = args$y,
        is_single_dataset = is_single_dataset_value
      ),
      if (!is.null(args$color_by)) {
        data_extract_input(
          id = ns("color_by"),
          label = "Color by variable",
          data_extract_spec = args$color_by,
          is_single_dataset = is_single_dataset_value
        )
      },
      if (!is.null(args$size_by)) {
        data_extract_input(
          id = ns("size_by"),
          label = "Size by variable",
          data_extract_spec = args$size_by,
          is_single_dataset = is_single_dataset_value
        )
      },
      if (!is.null(args$row_facet)) {
        data_extract_input(
          id = ns("row_facet"),
          label = "Row facetting",
          data_extract_spec = args$row_facet,
          is_single_dataset = is_single_dataset_value
        )
      },
      if (!is.null(args$col_facet)) {
        data_extract_input(
          id = ns("col_facet"),
          label = "Column facetting",
          data_extract_spec = args$col_facet,
          is_single_dataset = is_single_dataset_value
        )
      },
      panel_group(
        panel_item(
          title = "Add trend line",
          shinyjs::hidden(helpText(id = ns("line_msg"), "first select numeric X and Y variables")),
          optionalSelectInput(ns("smoothing_degree"), "Smoothing degree", seq_len(args$max_deg)),
          shinyjs::hidden(optionalSelectInput(ns("color_sub"), label = "", multiple = TRUE)),
          optionalSliderInputValMinMax(ns("ci"), "Confidence", c(.95, .8, .99), ticks = FALSE),
          shinyjs::hidden(checkboxInput(ns("show_form"), "Show formula", value = TRUE)),
          shinyjs::hidden(checkboxInput(ns("show_r2"), "Show R Squared", value = TRUE)),
          shinyjs::hidden(checkboxInput(ns("show_warn"), "", value = TRUE)),
          shinyjs::hidden(checkboxInput(ns("trans_label"), "Transparent label", value = FALSE)),
          div(
            id = ns("label_pos"),
            div(style = "display: inline-block; width: 10%", helpText("Left")),
            div(
              style = "display: inline-block; width: 70%",
              optionalSliderInput(ns("pos"), "Stats Position", min = 0, max = 1, value = 1, ticks = FALSE, step = .1)),
            div(style = "display: inline-block; width: 10%", helpText("Right"))
          )
        )
      ),
      panel_group(
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("alpha"), "Opacity:", args$alpha, ticks = FALSE),
          optionalSelectInput(
            inputId = ns("shape"),
            label = "Points shape:",
            choices = args$shape,
            selected = args$shape[1],
            multiple = FALSE
          ),
          colourpicker::colourInput(ns("color"), "Points color:", "black"),
          optionalSliderInputValMinMax(ns("size"), "Points size:", args$size, ticks = FALSE, step = .1),
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = args$rotate_xaxis_labels),
          checkboxInput(ns("add_density"), "Add marginal density", value = FALSE),
          checkboxInput(ns("rug_plot"), "Include rug plot", value = FALSE),
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
#' @importFrom ggExtra ggMarginal
#' @importFrom shinyjs hide show
#' @importFrom purrr map2 map map_chr
srv_g_scatterplot <- function(input,
                              output,
                              session,
                              datasets,
                              x,
                              y,
                              color_by,
                              size_by,
                              row_facet,
                              col_facet,
                              plot_height,
                              plot_width,
                              table_dec) {
  init_chunks()

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract  = Reduce(
      f = append,
      init = list(x, y),
      x = list(color_by, size_by, row_facet, col_facet)
    ),
    input_id = c(
      "x", "y",
      if_not_null(color_by, "color_by"),
      if_not_null(size_by, "size_by"),
      if_not_null(row_facet, "row_facet"),
      if_not_null(col_facet, "col_facet"))
  )

  eval_merged_data <- reactive({
    data_chunk <- chunks$new()
    chunks_push_data_merge(merged_data(), chunks = data_chunk)
    chunks_safe_eval(data_chunk) # evaluation here results in minor performance improvement
    data_chunk
  })

  trend_line_is_applicable <- reactive({
    ANL <- merged_data()$data() # nolint
    x_var <- as.vector(merged_data()$columns_source$x)
    y_var <- as.vector(merged_data()$columns_source$y)
    is.numeric(ANL[[x_var]]) && is.numeric(ANL[[y_var]])
  })

  add_trend_line <- reactive({
    smoothing_degree <- as.integer(input$smoothing_degree)
    trend_line_is_applicable() && !is_empty(smoothing_degree)
  })

  if (!is.null(color_by)) {
    # The reason why a reactive is used instead of observer is because there is a bug in shiny 1.4 that automatically
    # unhides selectInput/optionalSelectInput whenever you call their update functions.
    # This behavior creates a bug whenever some validation step fails and observer would update the selectInput
    # widget anyways causing it to unhide.
    # The observer should replace the reactive whenever we upgrade to shiny 1.5, where the bug is fixed.
    update_color_sub <- reactive({
      ANL <- merged_data()$data() # nolint
      color_by_var <- as.vector(merged_data()$columns_source$color_by)
      choices <- value_choices(ANL, color_by_var)
      updateOptionalSelectInput(
        session = session,
        inputId = "color_sub",
        label = color_by_var,
        choices = choices,
        selected = if_empty(isolate(input$color_sub)[isolate(input$color_sub) %in% choices], NULL))
      NULL
    })

    observeEvent(merged_data()$columns_source$color_by, {
      color_by_var <- as.vector(merged_data()$columns_source$color_by)
      if (!is_empty(color_by_var)) {
        shinyjs::hide("color")
      } else {
        shinyjs::show("color")
      }
    })
  }

  color_by_var_is_categorical <- reactive({
    color_by_var <- as.vector(merged_data()$columns_source$color_by)
    ANL <- merged_data()$data() # nolint
    !is_empty(color_by_var) && (is.character(ANL[[color_by_var]]) || is.factor(ANL[[color_by_var]]))
  })

  color_sub_chunk <- reactive({
    color_sub <- input$color_sub
    color_by_var <- as.vector(merged_data()$columns_source$color_by)
    ANL <- merged_data()$data() # nolint
    if (color_by_var_is_categorical()) {
      isolate(update_color_sub())
      if ((num_choices <- length(value_choices(ANL, color_by_var))) > 1) {
        if (!is.null(color_sub)) {
          validate(need(color_sub %in% value_choices(ANL, color_by_var), "processing..."))
          # if all choices are selected filtering makes no difference
          if (length(color_sub) < num_choices) {
            expr <- bquote(ANL <- dplyr::filter(ANL, .(as.name(color_by_var)) %in% .(color_sub))) # nolint
            list(expr = expr, ANL_sub = eval(expr)) # nolint
          } else 1 # to indicate not null
        } else 1
      }
    }
  })

  is_faceted <- reactive({
    row_facet_name <- as.vector(if_empty(merged_data()$columns_source$row_facet, character(0)))
    col_facet_name <- as.vector(if_empty(merged_data()$columns_source$col_facet, character(0)))
    if (!is_empty(row_facet_name) || !is_empty(col_facet_name)) {
      TRUE
    } else {
      FALSE
    }
  })

  warn_na <- reactive({
    ANL <- merged_data()$data() # nolint
    x_var <- as.vector(merged_data()$columns_source$x)
    y_var <- as.vector(merged_data()$columns_source$y)
    if (is.list(color_sub_chunk())) {
      ANL <- color_sub_chunk()$ANL_sub # nolint
    }
    if ((num_total_na <- nrow(ANL) - nrow(na.omit(ANL[, c(x_var, y_var)]))) > 0) {
      warn_na <- paste(num_total_na, "total row(s) with NA removed")
      updateCheckboxInput(session, "show_warn", label = warn_na)
      code_chunk <- bquote(ANL <- dplyr::filter(ANL, !is.na(.(as.name(x_var))) & !is.na(.(as.name(y_var))))) # nolint
    } else {
      warn_na <- NULL
      code_chunk <- NULL
    }
    list(msg = warn_na, code_chunk = code_chunk)
  })

  show_line_label <- reactive({
    input$show_form || input$show_r2 || (!is.null(warn_na()$msg) && input$show_warn)
  })

  plot_labels_eval <- reactive({
    color_by_var <- as.vector(merged_data()$columns_source$color_by)
    x_var <- as.vector(merged_data()$columns_source$x)
    y_var <- as.vector(merged_data()$columns_source$y)
    row_facet_name <- as.vector(if_empty(merged_data()$columns_source$row_facet, character(0)))
    col_facet_name <- as.vector(if_empty(merged_data()$columns_source$col_facet, character(0)))
    smoothing_degree <- as.integer(input$smoothing_degree)
    color_sub_chunk <- color_sub_chunk()
    warn_na <- warn_na()

    formula_tbl_chunk <- chunks$new()
    chunks_push_chunks(eval_merged_data(), chunks = formula_tbl_chunk)

    if (is.list(color_sub_chunk)) {
      chunks_push(color_sub_chunk$expr, chunks = formula_tbl_chunk)
    }

    label_generator <- bquote({
      df_no_na <- na.omit(df)
      warn_na <- switch((num_local_na <- nrow(df) - nrow(df_no_na)) > 0, paste(num_local_na, "row(s) with NA removed"))

      m <- try(lm(df_no_na[[.(y_var)]] ~ poly(df_no_na[[.(x_var)]], .(smoothing_degree)), df_no_na), silent = TRUE)
      label <- ifelse(
        !inherits(m, "try-error"), {
          r_2 <- paste("R^2:", round(summary(m)$r.squared, 8))
          form <- sprintf(
            "%s = %#.4f %s %#.4f * %s%s",
            .(y_var),
            coef(m)[1],
            ifelse(coef(m)[2] < 0, "-", "+"),
            abs(coef(m)[2]),
            .(x_var),
            paste(
              vapply(
                X = seq_len(.(smoothing_degree))[-1],
                FUN = function(deg) {
                  sprintf(
                    " %s %#.4f*%s^%s",
                    ifelse(coef(m)[deg + 1] < 0, "-", "+"),
                    abs(coef(m)[deg + 1]),
                    .(x_var),
                    deg
                  )
                },
                FUN.VALUE = character(1)),
              collapse = ""
            )
          )
          list(
            form = form,
            r_2 = r_2,
            msg = .(if (!is.null(color_sub_chunk))
              bquote(if (length(unique(df_no_na[[.(color_by_var)]])) > 1) "Stats from combined selected color groups")),
            warn_na = warn_na)
      },
      list(paste("Not enough unique x values to fit line with degree:", .(smoothing_degree))))
    })

    select_columns <- bquote(
      .(if (!is.null(color_sub_chunk)) {
        bquote(dplyr::select(.(x_var), .(y_var), .(color_by_var)))
      } else {
        bquote(dplyr::select(.(x_var), .(y_var)))
      })
    )

    plot_labels_chunk <- if (!is_empty(row_facet_name) && !is_empty(col_facet_name)) {
      bquote({
        plot_labels <- data.frame(
          row_facet = rep(unique(ANL[[.(row_facet_name)]]), length(unique(ANL[[.(col_facet_name)]]))),
          col_facet = rep(unique(ANL[[.(col_facet_name)]]), each = length(unique(ANL[[.(row_facet_name)]])))
        ) %>% mutate(label = purrr::map2(row_facet, col_facet, function(row_facet, col_facet) {
          df <- ANL %>%
            filter(.(as.name(row_facet_name)) == row_facet & .(as.name(col_facet_name)) == col_facet) %>%
            .(select_columns)
          # extracting expressions individually to avoid the extra open and close brackets
          .(label_generator[[2]])
          .(label_generator[[3]])
          .(label_generator[[4]])
          .(label_generator[[5]])
        }))
        names(plot_labels) <- .(c(row_facet_name, col_facet_name, "label"))
      })
    } else if (!is_empty(row_facet_name)) {
      bquote({
        plot_labels <- data.frame(row_facet = unique(ANL[[.(row_facet_name)]])
        ) %>% mutate(label = purrr::map(row_facet, function(row_facet) {
          df <- ANL %>% filter(.(as.name(row_facet_name)) == row_facet) %>% .(select_columns) # nolint
          .(label_generator[[2]])
          .(label_generator[[3]])
          .(label_generator[[4]])
          .(label_generator[[5]])
        }))
        names(plot_labels) <- .(c(row_facet_name, "label"))
      })
    } else if (!is_empty(col_facet_name)) {
      bquote({
        plot_labels <- data.frame(col_facet = unique(ANL[[.(col_facet_name)]])
        ) %>% mutate(label = purrr::map(col_facet, function(col_facet) {
          df <- ANL %>% filter(.(as.name(col_facet_name)) == col_facet) %>% .(select_columns) # nolint
          .(label_generator[[2]])
          .(label_generator[[3]])
          .(label_generator[[4]])
          .(label_generator[[5]])
        }))
        names(plot_labels) <- .(c(col_facet_name, "label"))
      })
    } else {
      bquote({
        df <- ANL %>% .(select_columns)
        .(label_generator[[2]])
        .(label_generator[[3]])
        .(label_generator[[4]])
        .(label_generator[[5]])
      })
    }
    chunks_push(plot_labels_chunk, chunks = formula_tbl_chunk)
    if (!is.null(warn_na$code_chunk)) {
      chunks_push(warn_na$code_chunk, chunks = formula_tbl_chunk)
    }
    # since this reactive performs evaluation, it is essential that it contains the minimum number of dependencies so
    # that unrelated changes do not trigger re-evaluation of coeffecients and r-squared
    chunks_safe_eval(formula_tbl_chunk)
    formula_tbl_chunk
  })

  formula_label <- reactive({
    pos <- input$pos # nolint
    x_var <- as.vector(merged_data()$columns_source$x)
    trans_label <- input$trans_label  # nolint
    bquote(.(if (trans_label) quote(geom_text) else quote(geom_label))(
      data = .(if (is_faceted()) quote(plot_labels)),
      mapping = aes(label = label, fontface = "plain"),
      x = min(ANL[[.(x_var)]], na.rm = TRUE) * (.(1 - pos)) + max(ANL[[.(x_var)]], na.rm = TRUE) * .(pos),
      y = Inf,
      hjust = .(if (pos > .5) 1 else if (pos == .5) .5 else 0),
      vjust = 1,
      inherit.aes = FALSE)
    )
  })

  plot_labels_fix_call <- reactive({
    concat_lab <- bquote(
      ifelse(
        length(label) == 1,
        label[[1]],
        trimws(paste0(
          .(if (input$show_form) quote(paste0(label$form, "\n"))),
          .(if (input$show_r2) quote(paste0(label$r_2, "\n"))),
          .(if ((input$show_form || input$show_r2)) quote(if (!is.null(label$msg)) paste0(label$msg, "\n"))),
          .(if (input$show_warn) quote(label$warn_na))
        ))
      )
    )
    if (is_faceted()) {
      bquote(plot_labels <- plot_labels %>%
        mutate(label = purrr::map_chr(label, function(label) {
        .(concat_lab)})
        )
      )
    } else {
      bquote(label <- .(concat_lab))
    }
  })

  initialize_data_chunk <- reactive({
    if (add_trend_line()) {
      data_chunk <- chunks$new()
      if (show_line_label()) {
        chunks_push_chunks(plot_labels_eval(), chunks = data_chunk)
        chunks_push(plot_labels_fix_call(), chunks = data_chunk)
      } else {
        chunks_push_chunks(eval_merged_data(), chunks = data_chunk)
        if (is.list(color_sub_chunk())) {
          chunks_push(color_sub_chunk()$expr, chunks = data_chunk)
        }
      }
      data_chunk
    } else {
      eval_merged_data()
    }
  })

  plot_r <- reactive({
    chunks_reset()

    ANL <- merged_data()$data() # nolint
    validate_has_data(ANL, 10)

    x_var <- as.vector(merged_data()$columns_source$x)
    y_var <- as.vector(merged_data()$columns_source$y)
    color_by_var <- as.vector(merged_data()$columns_source$color_by)
    size_by_var <- as.vector(merged_data()$columns_source$size_by)
    row_facet_name <- as.vector(if_empty(merged_data()$columns_source$row_facet, character(0)))
    col_facet_name <- as.vector(if_empty(merged_data()$columns_source$col_facet, character(0)))
    alpha <- input$alpha # nolint
    size <- input$size # nolint
    rotate_xaxis_labels <- input$rotate_xaxis_labels # nolint
    add_density <- input$add_density
    ggtheme <- input$ggtheme
    rug_plot <- input$rug_plot
    color <- input$color # nolint
    shape <- if_empty_string(if_null(input$shape, "circle"), "circle") # nolint
    smoothing_degree <- as.integer(input$smoothing_degree)
    ci <- input$ci # nolint

    validate(need(!is.null(ggtheme), "Please select a theme."))
    validate(need(length(x_var) == 1, "There must be exactly one x var."))
    validate(need(length(y_var) == 1, "There must be exactly one y var."))
    validate(need(is.null(color_by_var) || length(color_by_var) <= 1, "There must be 1 or no color variable."))
    validate(need(is.null(size_by_var) || length(size_by_var) <= 1, "There must be 1 or no size variable."))

    validate(need(
      is_empty(row_facet_name) || any(class(ANL[[row_facet_name]]) %in% c("character", "factor", "Date", "integer")),
      "`Row facetting` variable must be of class `character`, `factor`, `Date`, or `integer`"))
    validate(need(
      is_empty(col_facet_name) || any(class(ANL[[col_facet_name]]) %in% c("character", "factor", "Date", "integer")),
      "`Column facetting` variable must be of class `character`, `factor`, `Date`, or `integer`"))
    if (add_density && !is_empty(color_by_var)) {
      validate(need(
        !is.numeric(ANL[[color_by_var]]),
        "Marginal plots cannot be produced when the points are colored by numeric variables.
        \n Uncheck the 'Add marginal density' checkbox to display the plot."
      ))
      validate(need(
        !(inherits(ANL[[color_by_var]], "Date") ||
          inherits(ANL[[color_by_var]], "POSIXct") ||
          inherits(ANL[[color_by_var]], "POSIXlt")),
        "Marginal plots cannot be produced when the points are colored by Date or POSIX variables.
        \n Uncheck the 'Add marginal density' checkbox to display the plot."
      ))
    }

    validate_has_data(ANL[, c(x_var, y_var)], 10, complete = TRUE, allow_inf = FALSE)

    facet_cl <- facet_ggplot_call(row_facet_name, col_facet_name)
    if (!is.null(facet_cl)) {
      validate(need(
        !add_density,
        "Marginal density is not supported when faceting is used. Please uncheck `Add marginal density`
        or remove facetting."))
    }

    point_sizes <- if (!is_empty(size_by_var)) {
      validate(need(is.numeric(ANL[[size_by_var]]), "Variable to size by must be numeric"))
      bquote(.(size) * ANL[[.(size_by_var)]] / max(ANL[[.(size_by_var)]], na.rm = TRUE))
    } else {
      size
    }

    chunks_push_chunks(initialize_data_chunk())

    plot_call <- quote(ANL %>% ggplot())
    plot_call <- if (is_empty(color_by_var)) {
      bquote(
        .(plot_call) +
          aes(x = .(as.name(x_var)), y = .(as.name(y_var))) +
          geom_point(alpha = .(alpha), size = .(point_sizes), shape = .(shape), color = .(color))
      )
    } else {
      bquote(
        .(plot_call) +
          aes(x = .(as.name(x_var)), y = .(as.name(y_var)), color = .(as.name(color_by_var))) +
          geom_point(alpha = .(alpha), size = .(point_sizes), shape = .(shape)) +
          labs(color = .(varname_w_label(color_by_var, ANL)))
      )
    }

    plot_call <- bquote(
      .(plot_call) +
        ylab(.(varname_w_label(y_var, ANL))) +
        xlab(.(varname_w_label(x_var, ANL))) +
        .(call(paste0("theme_", ggtheme))) +
        theme(
          legend.position = "bottom",
          axis.text.x = .(if (rotate_xaxis_labels) quote(element_text(angle = 45, hjust = 1))))
    )

    if (rug_plot) {
      plot_call <- bquote(
        .(plot_call) +
          geom_rug()
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
        shinyjs::hide("label_pos")
        shinyjs::hide("show_warn")
        shinyjs::hide("trans_label")
      } else {
        shinyjs::show("ci")
        shinyjs::show("show_form")
        shinyjs::show("show_r2")
        if (!is.null(warn_na()$msg)) {
          shinyjs::show("show_warn")
        } else {
          shinyjs::hide("show_warn")
        }
        if (show_line_label()) {
          shinyjs::show("label_pos")
          shinyjs::show("trans_label")
          plot_call <- bquote(.(plot_call) + .(formula_label()))
        } else {
          shinyjs::hide("label_pos")
          shinyjs::hide("trans_label")
        }
        if (color_by_var_is_categorical()) {
          shinyjs::show("color_sub")
        } else {
          shinyjs::hide("color_sub")
        }
        plot_call <- bquote(
          .(plot_call) +
            geom_smooth(formula = y ~ poly(x, .(smoothing_degree)), se = TRUE, level = .(ci), method = "lm")
        )
      }
    } else {
      shinyjs::hide("smoothing_degree")
      shinyjs::hide("ci")
      shinyjs::hide("color_sub")
      shinyjs::hide("show_form")
      shinyjs::hide("show_r2")
      shinyjs::hide("label_pos")
      shinyjs::hide("show_warn")
      shinyjs::hide("trans_label")
      shinyjs::show("line_msg")
    }

    if (!is.null(facet_cl)) {
      plot_call <- bquote(.(plot_call) + .(facet_cl))
    }

    if (add_density) {
      plot_call <- bquote(
        ggExtra::ggMarginal(
          .(plot_call),
          type = "density",
          groupColour = .(if (!is_empty(color_by_var)) TRUE else FALSE)
        )
      )
    }

    plot_call <- bquote(p <- .(plot_call))
    chunks_push(plot_call)

    #explicitly calling print on the plot inside the chunk evaluates
    #the ggplot call and therefore catches errors
    plot_print_call <- quote(print(p))
    chunks_push(plot_print_call)
    chunks_safe_eval()
    chunks_get_var(var = "p")
  })

  # Insert the plot into a plot_with_settings module from teal.devel
  brush <- callModule(
    plot_with_settings_srv,
    id = "scatter_plot",
    plot_r = plot_r,
    height = plot_height,
    width = plot_width,
    brushing = TRUE
  )

  output$data_table <- DT::renderDataTable({
    # if not dependent on plot_r() it tries to print a table before chunks are populated with the correct ANL in plot_r
    # which ends up in a bunch of warnings that ANL is not in chunks
    plot_r()
    plot_brush <- brush$brush()

    if (!is.null(plot_brush)) {
      validate(need(!input$add_density, "Brushing feature is currently not supported when plot has marginal density"))
    }

    merged_data <- isolate(chunks_get_var("ANL"))

    df <- brushedPoints(merged_data, plot_brush)
    numeric_cols <- names(df)[vapply(df, function(x) is.numeric(x), FUN.VALUE = logical(1))]

    DT::formatRound(
      DT::datatable(df, rownames = FALSE, options = list(scrollX = TRUE)),
      numeric_cols,
      table_dec)
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(x, y, color_by, size_by, row_facet, col_facet)),
    modal_title = "R Code for a scatterplot",
    code_header = "Scatterplot"
  )
}
