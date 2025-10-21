#' @export
tm_a_pca.picks <- function(label = "Principal Component Analysis",
                           dat = picks(
                             datasets(),
                             variables(
                               choices = tidyselect::where(~ is.numeric(.x) && all(!is.na(.x))),
                               selected = tidyselect::everything(),
                               multiple = TRUE
                             )
                           ),
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
  if (inherits(ggplot2_args, "ggplot2_args")) ggplot2_args <- list(default = ggplot2_args)

  # Start of assertions
  checkmate::assert_string(label)
  checkmate::assert_class(dat, "picks")

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

  ans <- module(
    label = label,
    ui = ui_a_pca.picks,
    server = srv_a_pca.picks,
    ui_args = args[names(args) %in% names(formals(ui_a_pca.picks))],
    server_args = args[names(args) %in% names(formals(srv_a_pca.picks))],
    transformators = transformators,
    datanames = {
      datanames <- datanames(list(dat))
      if (length(datanames)) datanames else "all"
    }
  )
  attr(ans, "teal_bookmarkable") <- FALSE
  ans
}

# UI function for the PCA module
ui_a_pca.picks <- function(id,
                           dat,
                           plot_choices,
                           ggtheme,
                           rotate_xaxis_labels,
                           font_size,
                           alpha,
                           size,
                           pre_output,
                           post_output,
                           decorators) {
  ns <- NS(id)
  tagList(
    teal.widgets::standard_layout(
      output = teal.widgets::white_small_well(
        tags$div(
          tags$div(
            align = "center",
            tags$h4("Principal components importance"),
            tableOutput(ns("tbl_importance")),
            tags$hr()
          ),
          tags$div(
            align = "center",
            tags$h4("Eigenvectors"),
            tableOutput(ns("tbl_eigenvector")),
            tags$hr()
          ),
          teal.widgets::plot_with_settings_ui(id = ns("pca_plot"))
        )
      ),
      encoding = tags$div(
        tags$label("Encodings", class = "text-primary"),
        teal::teal_nav_item(
          label = tags$strong("Data selection"),
          teal.transform::picks_ui(id = ns("dat"), spec = dat)
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
              choices = plot_choices,
              selected = plot_choices[1]
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Elbow plot'", ns("plot_type")),
              ui_decorate_teal_data(
                ns("d_elbow_plot"),
                decorators = select_decorators(decorators, "elbow_plot")
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Circle plot'", ns("plot_type")),
              ui_decorate_teal_data(
                ns("d_circle_plot"),
                decorators = select_decorators(decorators, "circle_plot")
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Biplot'", ns("plot_type")),
              ui_decorate_teal_data(
                ns("d_biplot"),
                decorators = select_decorators(decorators, "biplot")
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] == 'Eigenvector plot'", ns("plot_type")),
              ui_decorate_teal_data(
                ns("d_eigenvector_plot"),
                decorators = select_decorators(decorators, "eigenvector_plot")
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
                shinyWidgets::pickerInput(inputId = ns("response"), label = "Color by", choices = NULL),
                teal.widgets::optionalSliderInputValMinMax(ns("alpha"), "Opacity:", alpha, ticks = FALSE),
                teal.widgets::optionalSliderInputValMinMax(ns("size"), "Points size:", size, ticks = FALSE)
              )
            )
          ),
          bslib::accordion_panel(
            title = "Plot settings",
            collapsed = TRUE,
            conditionalPanel(
              condition = sprintf(
                "input['%1$s'] == 'Elbow plot' || input['%1$s'] == 'Eigenvector plot'", ns("plot_type")
              ),
              list(checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = rotate_xaxis_labels))
            ),
            selectInput(
              inputId = ns("ggtheme"),
              label = "Theme (by ggplot):",
              choices = ggplot_themes,
              selected = ggtheme,
              multiple = FALSE
            ),
            teal.widgets::optionalSliderInputValMinMax(ns("font_size"), "Font Size", font_size, ticks = FALSE)
          )
        )
      ),
      pre_output = pre_output,
      post_output = post_output
    )
  )
}

# Server function for the PCA module
srv_a_pca.picks <- function(id, data, dat, plot_height, plot_width, ggplot2_args, decorators) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    teal.logger::log_shiny_input_changes(input, namespace = "teal.modules.general")

    selectors <- teal.transform::picks_srv(spec = list(dat = dat), data = data)

    qenv <- reactive({
      validate_input(
        "dat-variables-selected",
        length(selectors$dat()$variables$selected) > 1,
        "Please select more than 1 variable to perform PCA."
      )
      obj <- req(data())
      teal.reporter::teal_card(obj) <-
        c(
          teal.reporter::teal_card("# Principal Component Analysis"),
          teal.reporter::teal_card(obj),
          teal.reporter::teal_card("## Module's code")
        )
      teal.code::eval_code(obj, 'library("ggplot2");library("dplyr");library("tidyr")')
    })

    merged <- merge_srv("merge", data = qenv, selectors = selectors, output_name = "anl")
    anl_merged_q <- merged$data
    selected_vars <- reactive(merged$variables()$dat)

    validate_data <- reactive({
      obj <- req(anl_merged_q())
      anl <- obj[["anl"]]
      validate_input(
        "dat-variables-selected",
        condition = sum(stats::complete.cases(anl[selected_vars()])) > 10,
        message = "Number of complete cases is less than 10"
      )
      validate_input(
        "na_action",
        condition = input$na_action != "none" | !anyNA(anl[selected_vars()]),
        message = paste(
          "There are NAs in the dataset. Please deal with them in preprocessing",
          "or select \"Drop\" in the NA actions."
        )
      )
      standardization <- input$standardization
      scale <- standardization == "center_scale"

      if (scale) {
        not_single <- vapply(
          anl[selected_vars()],
          function(column) length(unique(column)) != 1,
          FUN.VALUE = logical(1)
        )
        validate_input(
          "standarization",
          condition = all(not_single),
          message = paste0(
            "You have selected `Center & Scale` under `Standardization` in the `Pre-processing` panel, ",
            "but one or more of your columns has/have a variance value of zero, indicating all values are identical"
          )
        )
      }
    })

    validate_xy_axis <- reactive({
      validate_input(
        "x_axis",
        condition = input$x_axis != input$y_axis,
        message = "Please choose different X and Y axes."
      )
    })

    observeEvent(selected_vars(), {
      shinyWidgets::updatePickerInput(
        inputId = "response",
        choices = selected_vars(),
        selected = input$response
      )
    })

    computation <- reactive({
      validate_data()
      # inputs
      anl_cols <- selected_vars()
      na_action <- input$na_action
      standardization <- input$standardization
      center <- standardization %in% c("center", "center_scale")
      scale <- standardization == "center_scale"
      anl <- anl_merged_q()[["anl"]]

      qenv <- within(anl_merged_q(), anl_cols <- cols, cols = unname(anl_cols))

      if (na_action == "drop") {
        qenv <- within(qenv, anl <- tidyr::drop_na(anl, any_of(anl_cols)))
      }

      qenv <- within(
        qenv,
        pca <- summary(stats::prcomp(anl[anl_cols], center = center, scale. = scale, retx = TRUE)),
        center = center, scale = scale
      )

      teal.reporter::teal_card(qenv) <- c(teal.reporter::teal_card(qenv), "## Principal Components Table")

      qenv <- within(qenv, {
        tbl_importance <- dplyr::as_tibble(pca$importance, rownames = "Metric")
        tbl_importance
      })

      teal.reporter::teal_card(qenv) <- c(teal.reporter::teal_card(qenv), "## Eigenvectors Table")

      within(qenv, {
        tbl_eigenvector <- dplyr::as_tibble(pca$rotation, rownames = "Variable")
        tbl_eigenvector
      })
    })

    output$plot_settings <- renderUI({
      # reactivity triggers
      req(computation())
      qenv <- computation()

      ns <- session$ns
      pca <- qenv[["pca"]]
      chcs_pcs <- colnames(pca$rotation)
      chcs_vars <- qenv$anl_cols

      tagList(
        conditionalPanel(
          condition = sprintf("input['%1$s'] == 'Biplot' || input['%1$s'] == 'Circle plot'", ns("plot_type")),
          list(
            shinyWidgets::pickerInput(ns("x_axis"), "X axis", choices = chcs_pcs, selected = chcs_pcs[1]),
            shinyWidgets::pickerInput(ns("y_axis"), "Y axis", choices = chcs_pcs, selected = chcs_pcs[2]),
            shinyWidgets::pickerInput(
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
          shinyWidgets::pickerInput(ns("pc"), "PC", choices = chcs_pcs, selected = chcs_pcs[1])
        )
      )
    })

    plot_elbow <- function(base_q) {
      logger::log_debug("srv_a_pca recalculate plot_elbow")
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
      teal.reporter::teal_card(base_q) <- c(teal.reporter::teal_card(base_q), "## Elbow plot")
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

    plot_circle <- function(base_q) {
      logger::log_debug("srv_a_pca recalculate plot_circle")
      validate_xy_axis()
      validate_input(
        "variables",
        condition = length(input$variables) > 0,
        message = "Please select Original Coordinates for this visualization."
      )
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

      teal.reporter::teal_card(base_q) <- c(teal.reporter::teal_card(base_q), "## Circle plot")
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

    plot_biplot <- function(base_q) {
      logger::log_debug("srv_a_pca recalculate plot_biplot")
      validate_xy_axis()
      validate_input(
        "response",
        condition = length(input$response) == 1,
        message = "Please select Response variable to see this visualization."
      )
      qenv <- base_q
      anl <- qenv[["anl"]]
      anl_cols <- selected_vars()

      resp_col <- input$response
      x_axis <- input$x_axis
      y_axis <- input$y_axis
      variables <- input$variables
      pca <- qenv[["pca"]]

      ggtheme <- input$ggtheme

      rotate_xaxis_labels <- input$rotate_xaxis_labels
      alpha <- input$alpha
      size <- input$size
      font_size <- input$font_size

      teal.reporter::teal_card(base_q) <- c(teal.reporter::teal_card(base_q), "## Biplot")
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
                    sweep(1, apply(anl[anl_cols], 2, mean, na.rm = TRUE)) %>%
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
        response <- anl[[resp_col]]

        aes_biplot <- substitute(
          ggplot2::aes_string(x = x_axis, y = y_axis, color = "response"),
          env = list(x_axis = x_axis, y_axis = y_axis)
        )

        qenv <- teal.code::eval_code(
          qenv,
          substitute(response <- anl[[resp_col]], env = list(resp_col = resp_col))
        )

        dev_labs <- list(color = varname_w_label(resp_col, anl))

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

    plot_eigenvector <- function(base_q) {
      logger::log_debug("srv_a_pca recalculate plot_eigenvector")
      validate_input(
        "pc",
        condition = length(input$pc) > 0,
        "Please select a Principal Component for this visualization"
      )
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

      teal.reporter::teal_card(base_q) <- c(teal.reporter::teal_card(base_q), "## Eigenvector plot")
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
            substitute(.plot, env = list(.plot = as.name(obj_name)))
          })
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
        logger::log_debug("srv_a_pca rerender tbl_importance")
        computation()[["tbl_importance"]]
      },
      bordered = TRUE,
      align = "c",
      digits = 3
    )

    output$tbl_eigenvector <- renderTable(
      expr = {
        req("eigenvector" %in% input$tables_display, req(computation()))
        logger::log_debug("srv_a_pca rerender tbl_eigenvector")
        computation()[["tbl_eigenvector"]]
      },
      bordered = TRUE,
      align = "c",
      digits = 3
    )

    output$tbl_eigenvector_ui <- renderUI({
      req("eigenvector" %in% input$tables_display)
    })

    set_chunk_dims(pws, decorated_output_q)
  })
}
