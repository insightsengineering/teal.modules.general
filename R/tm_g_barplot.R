#' Bar plot for safety events
#'
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param dataname (\code{character}) Dataset to be selected inside the app
#' @param arm_var (\code{character}) Name of the ARM var inside the dataset
#' @param arm_var_choices (\code{character}) Which variables can be used as the ARM var
#' @param bep_var (\code{character}) Which variables can be used as the BEP var
#' @param bep_var_choices (\code{character}) Which variables can be used as the BEP var
#' @param cov_var (\code{character}) Which variables can be used as the Covariance var
#' @param cov_var_choices (\code{character}) Which variables can be used as the Covariance var
#' @param response_var (\code{character}) Which variables can be used as the response var
#' @param response_var_choices (\code{character}) Which variables can be used as the response var
#' @param responder_var (\code{character}) Which variables can be used as the Responder var
#' @param responder_var_choices (\code{character}) Which variables can be used as the Responder var
#' @param non_responder_var (\code{character}) Which variables can be used as the non-Responder var
#' @param non_responder_var_choices (\code{character}) Which variables can be used as the non-Responder var
#' @param exclude_var (\code{character}) Which variables can be used as the Exclude var
#' @param exclude_var_choices (\code{character}) Which variables can be used as the Exclude var
#' @param color_coding_var (\code{character}) Which variables can be used as the Color Coding var
#' @param color_coding_var_choices (\code{character}) Which variables can be used as the Color Coding var
#' @param plot_height if scalar then the plot will have a fixed height. If a
#'   slider should be presented to adjust the plot height dynamically then it
#'   can be a vector of length three with vlaue, min and max.
#'
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#'
#' asl <- radsl(N = 600)
#' adte <- radaette(asl)
#'
#' app <- teal::init(
#'   data = list(ASL = asl, ADTE = adte),
#'   modules = root_modules(
#'     tm_bep_safety0(
#'       label = "My label",
#'       dataname = "ADTE",
#'       arm_var=c("STUDYID"),
#'       bep_var = "",
#'       cov_var = "",
#'       response_var = c("AETTE1"),
#'       responder_var_choices = c("AETTE2","AETTE1"),
#'       responder_var = "",
#'       non_responder_var = "",
#'       exclude_var = "",
#'       color_coding_var = ""
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_bep_safety0 <- function(label,
                           dataname,
                           arm_var,
                           arm_var_choices = arm_var,
                           bep_var,
                           bep_var_choices = bep_var,
                           cov_var,
                           cov_var_choices = cov_var,
                           response_var,
                           response_var_choices = response_var,
                           responder_var,
                           responder_var_choices = responder_var,
                           non_responder_var,
                           non_responder_var_choices = non_responder_var,
                           exclude_var,
                           exclude_var_choices = exclude_var,
                           color_coding_var,
                           color_coding_var_choices = color_coding_var,
                           plot_height = c(1200, 400, 5000),
                           pre_output = NULL,
                           post_output = NULL) {
  args <- as.list(environment())
  teal::module(
    label = label,
    server = srv_tm_bep_safety0,
    ui = ui_tm_bep_safety0,
    ui_args = args,
    server_args = list(
      dataname = dataname
    ),
    filters = dataname
  )
}

#' @importFrom teal.devel optionalSelectInput optionalSliderInputValMinMax standard_layout
ui_tm_bep_safety0 <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"),
        "Facetting by",
        a$arm_var_choices,
        a$arm_var,
        multiple = TRUE
      ),
      optionalSelectInput(ns("cov_var"),
        "Color by",
        # "Biomarker/Covariate Variable",
        a$cov_var_choices,
        a$cov_var,
        multiple = FALSE
      ),
      conditionalPanel(
        condition = ns("output.is_numeric"),
        uiOutput(ns("partition_slider"))
      ),
      optionalSelectInput(ns("bep_var"),
        "Biomarker Population",
        a$bep_var_choices,
        a$bep_var,
        multiple = FALSE
      ),
      optionalSelectInput(ns("response_var"),
        "Variable (Endpoint)",
        a$response_var_choices,
        a$response_var,
        multiple = FALSE
      ),
      checkboxInput(ns("stacked_check_box"),
        "Stacked bars",
        value = FALSE
      ),
      checkboxInput(ns("order_check_box"),
        "Descending order",
        value = FALSE
      ),
      checkboxInput(ns("table_check_box"),
        "As table",
        value = FALSE
      ),
      optionalSliderInputValMinMax(ns("plot_height"),
        "plot height",
        a$plot_height,
        ticks = FALSE
      )
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @importFrom gridExtra grid.arrange
srv_tm_bep_safety0 <- function(input,
                               output,
                               session,
                               datasets,
                               dataname) {

  # not USED, think of removing: global_merged_data <- reactiveVal(NULL)
  # not USED, think of removing: global_partition_slider_flag <- FALSE
  global_cov_var_selection <- reactiveValues(
    selection = "",
    is_numeric = FALSE,
    min_val = 0,
    max_val = 0,
    mid_val = 0
  )

  output$is_numeric <- reactive({
    if (global_cov_var_selection$is_numeric) {
      output$partition_slider <- renderUI({
        sliderInput(session$ns("partition_slider"),
          paste0("Low and high range for ", global_cov_var_selection$selection),
          min = floor(global_cov_var_selection$min_val),
          max = ceiling(global_cov_var_selection$max_val),
          value = global_cov_var_selection$mid_val,
          step = 0.1
        )
      })
      return(TRUE)
    } else {
      # TODO: This should not be needed!
      output$partition_slider <- renderUI({
        NULL
      })
      return(FALSE)
    }
  })

  outputOptions(output, "is_numeric", suspendWhenHidden = FALSE)

  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    ns <- session$ns
    plotOutput(ns("plot"), height = plot_height)
  })

  output$plot <- renderPlot({

    # Get filtered ASL dataset.
    asl_filtered <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)

    # Get filtered ARS dataset.
    anl_filtered <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    # Get user input:
    arm_var <- input$arm_var
    if (is.null(arm_var) || arm_var == "") {
      arm_var <- NULL
    }
    bep_var <- input$bep_var
    if (is.null(bep_var) || bep_var == "") {
      bep_var <- NULL
    }
    cov_var <- input$cov_var
    if (is.null(cov_var) || cov_var == "") {
      cov_var <- NULL
    }
    response_var <- input$response_var
    if (is.null(response_var) || response_var == "") {
      response_var <- NULL
    }
    frequency_check_box <- input$frequency_check_box # nolint
    order_check_box <- input$order_check_box
    stacked_check_box <- input$stacked_check_box

    partition_val <- input$partition_slider
    if (is.null(partition_val) || partition_val == "") {
      partition_val <- NULL
    }

    validate(need(!is.null(arm_var), "Please provide a facet variable."))
    validate(need(is.element(arm_var, "STUDYID"), "Please provide STUDYID."))

    asl_vars <- unique(c("USUBJID", cov_var, bep_var))
    asl_vars <- as.vector(rbind(unlist(arm_var), asl_vars))
    asl_vars <- asl_vars[asl_vars != ""]
    anl_vars <- c("USUBJID", "STUDYID", "AVAL", "AVALU", "PARAMCD", "EVNTDESC")

    anl_endpoint <- subset(anl_filtered, anl_filtered$PARAMCD == response_var)

    if (any(duplicated(anl_endpoint[, c("USUBJID", "STUDYID")]))) {
      stop("only one row per patient expected")
    }

    asl_p <- asl_filtered[, asl_vars, drop = FALSE]
    anl_p <- anl_endpoint[, anl_vars, drop = FALSE]

    merged_data <- merge(
      x = asl_p,
      y = anl_p,
      all.x = FALSE,
      all.y = FALSE,
      by = c("USUBJID", "STUDYID")
    )

    if (!is.null(bep_var)) {
      merged_data <- merged_data[merged_data[[bep_var]] == TRUE, ]
    }

    merged_data <- as.data.frame(merged_data)

    min_cov_val <- 0
    mid_cov_val <- 0
    max_cov_val <- 0
    cov_var_numeric_flag <- FALSE
    if (!is.null(cov_var)) {
      global_cov_var_selection$selection <- cov_var
      cov_var_values <- merged_data[[cov_var]]
      if (is.numeric(cov_var_values)) {
        cov_var_numeric_flag <- TRUE
        min_cov_val <- as.numeric(min(cov_var_values, na.rm = TRUE))
        mid_cov_val <- as.numeric(median(cov_var_values, na.rm = TRUE))
        max_cov_val <- as.numeric(max(cov_var_values, na.rm = TRUE))
        global_cov_var_selection$is_numeric <- TRUE
        global_cov_var_selection$min_val <- min_cov_val
        global_cov_var_selection$mid_val <- mid_cov_val
        global_cov_var_selection$max_val <- max_cov_val
      } else {
        global_cov_var_selection$is_numeric <- FALSE
      }
    } else {
      global_cov_var_selection$is_numeric <- FALSE
    }

    # Add factor variable for partitioned continuous covariate.
    if (!is.null(cov_var) && !is.null(partition_val) && cov_var_numeric_flag) {
      new_cov_var <- paste0(cov_var, "_PARTITIONED")
      merged_data[[new_cov_var]] <- ifelse(merged_data[[cov_var]] >= partition_val, "high", "low")
      cov_var <- new_cov_var
    }

    bar_type <- NULL
    text_position <- NULL

    if (stacked_check_box) {
      bar_type <- "position_stack()"
      text_position <- "position_stack()"
    } else {
      bar_type <- "position_dodge2(preserve='single')"
      text_position <- "position_dodge(width=1)"
    }

    x_axis_label_position <- NULL
    count_var <- NULL
    group_var <- NULL
    facet_var <- NULL
    hjust_var <- -1
    vjust_var <- -1

    if (is.null(cov_var)) {
      x_axis_label_position <- "bottom"
      count_var <- "..count.."
      group_var <- "STUDYID"
      facet_var <- paste0(arm_var, collapse = "+")
      hjust_var <- 1
      vjust_var <- 1
      cov_var <- NULL
    } else {
      x_axis_label_position <- "top"
      count_var <- "..count.."
      group_var <- cov_var
      facet_var <- paste0(arm_var, collapse = "+")
      hjust_var <- 0
      vjust_var <- 0
    }

    if (order_check_box) {
      merged_data <- within(
        merged_data,
        EVNTDESC <- factor(EVNTDESC, # nolint
          levels = names(sort(table(EVNTDESC),
            decreasing = TRUE
          ))
        )
      )
    }

    plot_list <- list()
    if (!cov_var_numeric_flag || (cov_var_numeric_flag && !is.null(partition_val))) {
      p <- ggplot(data = merged_data, aes_string(x = "EVNTDESC", y = "..count..")) + # nolint
        geom_bar(aes_string(fill = cov_var), stat = "count", position = eval(parse(text = bar_type))) +
        labs(x = "Event description", y = "Counts") +
        scale_x_discrete(label = function(x) abbreviate(x, minlength = 13), position = x_axis_label_position) +
        theme(
          axis.text.x = element_text(angle = 40, hjust = hjust_var, vjust = vjust_var),
          legend.title = element_text(size = 13),
          legend.text = element_text(size = 12)
        ) +
        geom_text(
          stat = "count",
          aes_string(label = count_var, group = group_var),
          position = eval(parse(text = text_position)),
          vjust = -0.1
        ) +
        ggtitle("Safety events without censoring")
      if (!is.null(facet_var)) {
        p <- p + facet_grid(as.formula(paste(facet_var, "~ .")))
      }
      if (!is.null(cov_var) && cov_var_numeric_flag) {
        high_label <- paste0("high [", partition_val, ",", max_cov_val, "]")
        low_label <- paste0("low [", min_cov_val, ",", partition_val, "[")
        cols <- c("#F8766D", "#00BFC4")
        p <- p + scale_fill_manual(
          labels = c(high_label, low_label), values = cols,
          na.value = "#808080"
        )
      }
      plot_list[[1]] <- p
    }

    if (length(plot_list) > 0) {
      do.call("grid.arrange", c(plot_list, ncol = 1))
    }
  })
}
