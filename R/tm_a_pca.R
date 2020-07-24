#' Principal component analysis module
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#' @param dat (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Datasets used to compute PCA.
#'
#' @export
#'
#' @examples
#'
#' # ADSL example
#'
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#'
#' app <- teal::init(
#'   data = cdisc_data(cdisc_dataset("ADSL", ADSL),
#'                    code = "ADSL <- radsl(cached = TRUE)", check = TRUE),
#'   modules = root_modules(
#'     tm_a_pca("PCA",
#'              data_extract_spec(
#'                dataname = "ADSL",
#'                select = select_spec(
#'                  choices = variable_choices(data = ADSL),
#'                  selected = c("BMRKR1", "AGE"),
#'                  multiple = TRUE
#'                ),
#'                filter = NULL
#'              )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_a_pca <- function(label = "Principal component analysis",
                     dat,
                     plot_height = c(600, 200, 2000),
                     pre_output = NULL,
                     post_output = NULL) {

  stopifnot(is_character_single(label))
  stopifnot(is_class_list("data_extract_spec")(dat) || is(dat, "data_extract_spec"))

  if (!is_class_list("data_extract_spec")(dat)) {
    dat <- list(dat)
  }

  stopifnot(is_numeric_vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])

  args <- as.list(environment())

  data_extract_list <- list(dat = dat)

  module(
    label = label,
    server = srv_a_pca,
    ui = ui_a_pca,
    ui_args = args,
    server_args = data_extract_list,
    filters = get_extract_datanames(data_extract_list)
  )
}


ui_a_pca <- function(id, ...) {

  ns <- NS(id)
  args <- list(...)

  color_selector <- args$dat
  for (i in seq_along(color_selector)) {
    color_selector[[i]]$select$multiple <- FALSE
    color_selector[[i]]$select$always_selected <- NULL
    color_selector[[i]]$select$selected <- NULL
  }

  plot_choices <- c(
    "Elbow plot" = "elbow",
    "Circle plot" = "circle",
    "Biplot" = "biplot",
    "Eigenvector plot" = "pc_var"
  )

  standard_layout(
    output = white_small_well(
      tags$div(
        uiOutput(ns("tbl_importance_ui")),
        hr(),
        uiOutput(ns("tbl_eigenvector_ui")),
        hr(),
        plot_height_output(id = ns("pca_plot"))
      )
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args["dat"]),
      data_extract_input(
        id = ns("dat"),
        label = "Data selection",
        data_extract_spec = args$dat
      ),
      panel_group(
        panel_item(
          title = "Display",
          collapsed = FALSE,
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
            selected = plot_choices["Elbow plot"]
          )
        ),
        panel_item(
          title = "Pre-processing",
          radioButtons(ns("standardization"), "Standardization",
                       choices = c("None" = "none", "Center" = "center", "Center & Scale" = "center_scale"),
                       selected = "center_scale"),
          radioButtons(ns("na_action"), "NA action",
                       choices = c("None" = "none", "Drop" = "drop"),
                       selected = "none")
        ),
        panel_item(
          title = "Plot settings",
          collapsed = FALSE,
          plot_height_input(id = ns("pca_plot"), value = args$plot_height),
          uiOutput(ns("plot_settings")),
          conditionalPanel(
            condition = paste0("input['", ns("plot_type"), "'] == 'biplot'"),
            data_extract_input(
              id = ns("response"),
              label = "Color by",
              data_extract_spec = color_selector
            )
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
#' @importFrom stringr str_sort
#' @importFrom dplyr as_tibble filter mutate mutate_at
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom tidyr gather drop_na
#' @importFrom rlang !! !!! quo sym syms
srv_a_pca <- function(input, output, session, datasets, dat) {

  response <- dat

  for (i in seq_along(response)) {
    response[[i]]$select$multiple <- FALSE
    response[[i]]$select$always_selected <- NULL
    response[[i]]$select$selected <- NULL
    response[[i]]$select$choices <- names(datasets$get_data_attr(response[[i]]$dataname, "column_labels"))
    response[[1]]$select$choices <- setdiff(response[[1]]$select$choices,
                                            datasets$get_keys(response[[i]]$dataname)$primary)
  }

  init_chunks()

  anl_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(dat),
    input_id = c("dat")
  )

  response_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(response),
    input_id = c("response"),
    anl_name = "RP"
  )

  # computation ----
  computation <- reactive({
    chunks_stack <- chunks$new()

    keep_cols <- as.character(anl_data()$columns_source$dat)
    na_action <- input$na_action #nolint
    standardization <- input$standardization
    center <- standardization %in% c("center", "center_scale") #nolint
    scale <- standardization == "center_scale" #nolint

    validate(need(length(keep_cols) > 1, "Please select more than 1 variable to perform PCA."))

    chunks_reset(chunks = chunks_stack)
    chunks_push_chunks(anl_data()$chunks, chunks = chunks_stack)

    ANL <- anl_data()$data() #nolint

    validate_has_data(ANL, 10)
    validate_has_elements(keep_cols, "Please select columns")
    validate(need(
      all(vapply(ANL[keep_cols], is.numeric, logical(1))),
      "PCA is only defined for numeric columns."
    ))
    validate(need(
      na_action != "none" | !anyNA(ANL[keep_cols]),
      paste("There are NAs in the dataset. Please deal with them in preprocessing",
            'or select "Drop" in the NA actions inside the encodings panel (left).')
    ))


    chunks_push(
      id = "pca_1",
      expression = bquote({
        keep_columns <- .(keep_cols)
      }),
      chunks = chunks_stack
    )

    if (na_action == "drop") {
      chunks_push(
        id = "pca_2",
        expression = bquote({
          ANL <- tidyr::drop_na(ANL, !!!syms(keep_columns)) # nolint
        }),
        chunks = chunks_stack
      )
    }

    chunks_push(
      id = "pca_3",
      expression = bquote({
        pca <- summary(prcomp(ANL[keep_columns], center = .(center), scale. = .(scale), retx = TRUE))
      }),
      chunks = chunks_stack
    )

    chunks_push(
      id = "pca_tbl_importance",
      expression = bquote({
        tbl_importance <- dplyr::as_tibble(pca$importance, rownames = "Metric")
        tbl_importance
      }),
      chunks = chunks_stack
    )

    chunks_push(
      id = "pca_tbl_eigenvector",
      expression = bquote({
        tbl_eigenvector <- dplyr::as_tibble(pca$rotation, rownames = "Variable")
        tbl_eigenvector
      }),
      chunks = chunks_stack
    )

    chunks_safe_eval(chunks = chunks_stack)

    chunks_stack
  })

  # plot args ----
  output$plot_settings <- renderUI({
    # reactivity triggers
    chunks_stack <- computation()

    ns <- session$ns

    pca <- chunks_get_var("pca", chunks = chunks_stack)
    chcs_pcs <- colnames(pca$rotation)
    chcs_vars <- chunks_get_var("keep_cols", chunks = chunks_stack)

    out <- if (input$plot_type == "elbow") {
      helpText("No additional plot settings available.")

    } else if (input$plot_type == "circle") {
      list(
        optionalSelectInput(ns("x_axis"), "X axis",
                            choices = chcs_pcs, selected = chcs_pcs[1]),
        optionalSelectInput(ns("y_axis"), "Y axis",
                            choices = chcs_pcs, selected = chcs_pcs[2]),
        optionalSelectInput(ns("variables"), "Original coordinates",
                            choices = chcs_vars, selected = chcs_vars,
                            multiple = TRUE)
      )

    } else if (input$plot_type == "biplot") {
      list(
        optionalSelectInput(ns("x_axis"), "X axis",
                            choices = chcs_pcs, selected = chcs_pcs[1]),
        optionalSelectInput(ns("y_axis"), "Y axis",
                            choices = chcs_pcs, selected = chcs_pcs[2]),
        optionalSelectInput(ns("variables"), "Original coordinates",
                            choices = chcs_vars, selected = chcs_vars,
                            multiple = TRUE)
      )

    } else if (input$plot_type == "pc_var") {
      list(
        optionalSelectInput(ns("pc"), "PC",
                            choices = chcs_pcs, selected = chcs_pcs[1])
      )

    } else {
      stop("Unknown plot")
    }

    return(out)
  })


  callModule(
    plot_with_height,
    id = "pca_plot",
    plot_height = reactive(input$pca_plot),
    plot_id = session$ns("plot")
  )


  # plot elbow ----
  plot_elbow <- function() {
    chunks_push(
      id = "pca_plot",
      expression = bquote({
        elb_dat <- pca$importance[c("Proportion of Variance", "Cumulative Proportion"), ] %>%
          dplyr::as_tibble(rownames = "metric") %>%
          tidyr::gather("component", "value", -metric) %>%
          dplyr::mutate(component = factor(component,
                                           levels = unique(stringr::str_sort(component, numeric = T))))

        g <- ggplot(mapping = aes_string(x = "component", y = "value")) +
          geom_bar(
            aes(fill = "Single variance"),
            data = dplyr::filter(elb_dat, metric == "Proportion of Variance"),
            color = "black",
            stat = "identity") +
          geom_point(
            aes(color = "Cumulative variance"),
            data = dplyr::filter(elb_dat, metric == "Cumulative Proportion")) +
          geom_line(
            aes(group = 1, color = "Cumulative variance"),
            data = dplyr::filter(elb_dat, metric == "Cumulative Proportion")) +
          theme_bw() +
          labs(x = "Principal component",
               y = "Proportion of variance explained",
               color = "",
               fill = "Legend") +
          scale_color_manual(values = c("Cumulative variance" = "darkred", "Single variance" = "black")) +
          scale_fill_manual(values = c("Cumulative variance" = "darkred", "Single variance" = "lightblue")) +
          theme(legend.position = "right", legend.spacing.y = unit(-5, "pt"),
                legend.title = element_text(vjust = 8))
        print(g)
      })
    )

    invisible(NULL)
  }

  # plot circle ----
  plot_circle <- function() {
    validate(
      need(input$x_axis, "Need additional plot settings - x axis"),
      need(input$y_axis, "Need additional plot settings - y axis"),
      need(input$variables, "Need additional plot settings - variables"),
      need(input$x_axis != input$y_axis, "Please choose different axis.")
    )

    x_axis <- input$x_axis #nolint
    y_axis <- input$y_axis #nolint
    variables <- input$variables #nolint

    chunks_push(
      id = "pca_plot",
      expression = bquote({
        pca_rot <- pca$rotation[, c(.(x_axis), .(y_axis))] %>%
          dplyr::as_tibble(rownames = "label") %>%
          dplyr::filter(label %in% .(variables))

        circle_data <- data.frame(
          x = cos(seq(0, 2 * pi, length.out = 100)),
          y = sin(seq(0, 2 * pi, length.out = 100))
        )

        g <- ggplot(pca_rot) +
          geom_point(aes_string(x = .(x_axis), y = .(y_axis))) +
          geom_label(aes_string(x = .(x_axis), y = .(y_axis), label = "label"),
                     nudge_x = 0.1, nudge_y = 0.05,
                     fontface = "bold") +
          geom_path(aes(x, y, group = 1), data = circle_data) +
          geom_point(aes(x = x, y = y), data = data.frame(x = 0, y = 0), shape = "x", size = 5) +
          theme_bw()
        print(g)
      })
    )

    invisible(NULL)
  }

  # plot biplot ----
  plot_biplot <- function() {
    validate(
      need(input$x_axis, "Need additional plot settings - x axis"),
      need(input$y_axis, "Need additional plot settings - y axis"),
      need(input$x_axis != input$y_axis, "Please choose different axis.")
    )

    rd <- response_data()
    resp_col <- as.character(rd$columns_source$response)
    x_axis <- input$x_axis #nolint
    y_axis <- input$y_axis #nolint
    variables <- input$variables #nolint
    pca <- chunks_get_var("pca")

    chunks_push(
      id = "pca_plot_data_rot",
      expression = bquote({
        pca_rot <- dplyr::as_tibble(pca$x[, c(.(x_axis), .(y_axis))])
      })
    )

    # rot_vars = data frame that displays arrows in the plot, need to be scaled to data
    if (!is.null(input$variables)) {

      chunks_push(
        id = "pca_plot_vars_rot_1",
        expression = bquote({
          r <- sqrt(qchisq(0.69, df = 2)) * prod(colMeans(pca_rot ^ 2)) ^ (1 / 4)
          v_scale <- rowSums(pca$rotation ^ 2)

          rot_vars <- pca$rotation[, c(.(x_axis), .(y_axis))] %>%
            dplyr::as_tibble(rownames = "label") %>%
            dplyr::mutate_at(vars(c(.(x_axis), .(y_axis))), function(x) r * x / sqrt(max(v_scale)))
        })
      )

      # determine start of arrows
      chunks_push(
        id = "pca_plot_vars_rot_2",
        expression = if (is.logical(pca$center) && !pca$center) {
          bquote({
            rot_vars <- rot_vars %>%
              tibble::column_to_rownames("label") %>%
              sweep(1, apply(ANL[keep_columns], 2, mean, na.rm = TRUE)) %>%
              tibble::rownames_to_column("label") %>%
              dplyr::mutate(xstart = mean(pca$x[, .(x_axis)], na.rm = TRUE),
                            ystart = mean(pca$x[, .(y_axis)], na.rm = TRUE))
          })
        } else {
          bquote({
            rot_vars <- rot_vars %>% dplyr::mutate(xstart = 0, ystart = 0)
          })
        }
      )

      chunks_push(
        id = "pca_plot_vars_rot_3",
        expression = bquote({
          rot_vars <- rot_vars %>%
            dplyr::filter(label %in% .(variables))
        })
      )
    }

    if (length(resp_col) == 0) {

      chunks_push(
        id = "pca_plot_biplot",
        bquote({
          g <- ggplot() +
            geom_point(aes_string(x = .(x_axis), y = .(y_axis)), data = pca_rot) +
            theme_bw()
        })
      )

    } else {

      ANL <- chunks_get_var("ANL") # nolint
      validate(need(!resp_col %in% colnames(ANL),
                    paste("Response column must be different from the original variables",
                          "(that were used for PCA).")))

      RP <- chunks_get_var("RP") # nolint
      rp_keys <- setdiff(colnames(RP), as.character(unlist(rd$columns_source))) # nolint

      response <- RP[[resp_col]]

      chunks_push(
        id = "pca_plot_response",
        expression = if (is.character(response) ||
                         is.factor(response) ||
                         (is.numeric(response) && length(unique(response)) <= 6)) {
          bquote({
            response <- merge(ANL, RP, by = .(rp_keys))[[.(resp_col)]]
            pca_rot$response <- as.factor(response)
            scale_colors <- scale_color_brewer(palette = "Dark2")
            aes_biplot <- aes_string(x = .(x_axis), y = .(y_axis), color = "response")
          })
        } else if (class(response) == "Date") {
          bquote({
            response <- RP[[.(resp_col)]]
            pca_rot$response <- as.numeric(response)
            scale_colors <- scale_color_gradient(low = "darkred", high = "lightblue",
                                                 labels = function(x) as.Date(x, origin = "1970-01-01"))
            aes_biplot <- aes_string(x = .(x_axis), y = .(y_axis), color = "response")
          })
        } else {
          bquote({
            response <- RP[[.(resp_col)]]
            pca_rot$response <- response
            scale_colors <- scale_color_gradient(low = "darkred", high = "lightblue")
            aes_biplot <- aes_string(x = .(x_axis), y = .(y_axis), color = "response")
          })
        }
      )

      chunks_push(
        id = "pca_plot_biplot",
        expression = bquote({
          g <- ggplot() +
            geom_point(aes_biplot, data = pca_rot) +
            scale_colors +
            theme_bw() +
            labs(color = .(varname_w_label(resp_col, RP)))
        })
      )
    }

    if (!is.null(input$variables)) {
      chunks_push(
        id = "pca_plot_arrow_plot",
        expression = bquote({
          g <- g +
            geom_segment(aes_string(x = "xstart", y = "ystart", xend = .(x_axis), yend = .(y_axis)),
                         data = rot_vars,
                         lineend = "round", linejoin = "round",
                         arrow = arrow(length = unit(0.5, "cm"))) +
            geom_label(aes_string(x = .(x_axis), y = .(y_axis), label = "label"),
                       data = rot_vars,
                       nudge_y = 0.1,
                       fontface = "bold") +
            geom_point(aes(x = xstart, y = ystart),
                       data = rot_vars,
                       shape = "x",
                       size = 5)
        })
      )
    }

    chunks_push(
      id = "pca_plot_final",
      expression = bquote({
        print(g)
      })
    )

    invisible(NULL)
  }

  # plot pc_var ----
  plot_pc_var <- function() {
    validate(need(input$pc, "Need additional plot settigns - PC"))

    pc <- input$pc #nolint

    chunks_push(
      id = "pca_plot",
      expression = bquote({
        pca_rot <- pca$rotation[, .(pc), drop = FALSE] %>%
          dplyr::as_tibble(rownames = "Variable")

        g <- ggplot(pca_rot) +
          geom_bar(aes_string(x = "Variable", y = .(pc)),
                   stat = "identity",
                   color = "black",
                   fill = "lightblue") +
          geom_text(
            aes_q(x = quote(Variable),
                  y = as.name(.(pc)),
                  label = quo(round(!!sym(.(pc)), 3)),
                  vjust = quo(ifelse(!!sym(.(pc)) > 0, -0.5, 1.3)))) +
          theme_bw()
        print(g)
      })
    )

    invisible(NULL)
  }

  # plot final ----
  output$plot <- renderPlot({
    chunks_reset()
    chunks_push_chunks(computation())

    if (input$plot_type == "elbow") {
      plot_elbow()

    } else if (input$plot_type == "circle") {
      plot_circle()

    } else if (input$plot_type == "biplot") {
      if (length(response_data()$columns_source$response) > 0) {
        chunks_push_chunks(response_data()$chunks, overwrite = TRUE)
      }

      plot_biplot()

    } else if (input$plot_type == "pc_var") {
      plot_pc_var()

    } else {
      stop("Unknown plot")
    }

    chunks_safe_eval()
  })

  # tables ----
  output$tbl_importance <- renderTable({
    req("importance" %in% input$tables_display)
    chunks_stack <- computation()
    chunks_get_var("tbl_importance", chunks = chunks_stack)
  }, bordered = TRUE, align = "c", digits = 3)

  output$tbl_importance_ui <- renderUI({
    req("importance" %in% input$tables_display)
    div(
      tags$h4("Principal components importance"),
      tableOutput(session$ns("tbl_importance")),
      align = "center"
    )
  })

  output$tbl_eigenvector <- renderTable({
    req("eigenvector" %in% input$tables_display)
    chunks_stack <- computation()
    chunks_get_var("tbl_eigenvector", chunks = chunks_stack)
  }, bordered = TRUE, align = "c", digits = 3)

  output$tbl_eigenvector_ui <- renderUI({
    req("eigenvector" %in% input$tables_display)
    div(
      tags$h4("Eigenvectors"),
      tableOutput(session$ns("tbl_eigenvector")),
      align = "center"
    )
  })


  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(dat)),
    modal_title = "R code for PCA"
  )
}
