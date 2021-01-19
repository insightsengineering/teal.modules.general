#' Create a scatterplot matrix
#'
#' The available datasets to choose from for each dataset selector is the same and
#' determined by the argument `variables`.
#' @md
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @inheritParams tm_g_scatterplot
#' @inheritParams shared_params
#'
#' @param variables (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'  Plotting variables from an incoming dataset with filtering and selecting.
#'
#' @note For more examples, please see the vignette "Using scatterplot matrix" via
#'   \code{vignette("using-scatterplot-matrix", package = "teal.modules.general")}.
#' @export
#' @importFrom shinyjs show hide
#' @examples
#' # Scatterplot matrix of variables from ADSL dataset
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#' ADRS <- radrs(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- radsl(cached = TRUE)"),
#'     cdisc_dataset("ADRS", ADRS, code = "ADRS <- radrs(cached = TRUE)"),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix",
#'       variables = list(
#'         data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = variable_choices(ADSL),
#'             selected = c("AGE", "RACE", "SEX"),
#'             multiple = TRUE,
#'             fixed = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADRS",
#'           filter = filter_spec(
#'             label = "Select endpoints:",
#'             vars = c("PARAMCD", "AVISIT"),
#'             choices = value_choices(ADRS, c("PARAMCD", "AVISIT"), c("PARAM", "AVISIT")),
#'             selected = "INVET - END OF INDUCTION",
#'             multiple = TRUE
#'           ),
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = variable_choices(ADRS),
#'             selected = c("AGE", "AVAL", "ADY"),
#'             multiple = TRUE,
#'             fixed = FALSE
#'           )
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_scatterplotmatrix <- function(label = "Scatterplot matrix",
                                   variables,
                                   plot_height = c(600, 200, 2000),
                                   plot_width = NULL,
                                   pre_output = NULL,
                                   post_output = NULL) {
  if (!is_class_list("data_extract_spec")(variables)) {
    variables <- list(variables)
  }

  stopifnot(is_character_single(label))
  stopifnot(is_class_list("data_extract_spec")(variables))
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())
  module(
    label = label,
    server = srv_g_scatterplotmatrix,
    ui = ui_g_scatterplotmatrix,
    ui_args = args,
    server_args = list(variables = variables, plot_height = plot_height, plot_width = plot_width),
    filters = get_extract_datanames(variables)
  )
}


ui_g_scatterplotmatrix <- function(id, ...) {
  args <- list(...)
  is_single_dataset_value <- is_single_dataset(args$variables)
  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      textOutput(ns("message")),
      br(),
      plot_with_settings_ui(id = ns("myplot"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args$variables),
      data_extract_input(
        id = ns("variables"),
        label = "Variables",
        data_extract_spec = args$variables,
        is_single_dataset = is_single_dataset_value
      ),
      hr(),
      panel_group(
        panel_item(
          title = "Plot settings",
          sliderInput(
            ns("alpha"), "Opacity:", min = 0, max = 1,
            step = .05, value = .5, ticks = FALSE
          ),
          sliderInput(
            ns("cex"), "Points size:", min = 0.2, max = 3,
            step = .05, value = .65, ticks = FALSE
          ),
          checkboxInput(ns("cor"), "Add Correlation", value = FALSE),
          radioButtons(
            ns("cor_method"), "Select Correlation Method",
            choiceNames = c("Pearson", "Kendall", "Spearman"),
            choiceValues = c("pearson", "kendall", "spearman"),
            inline = TRUE),
          checkboxInput(ns("cor_na_omit"), "Omit Missing Values", value = TRUE)
          )
        )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}


#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if
#' @importFrom lattice splom panel.splom panel.text current.panel.limits
#' @importFrom stats cor.test na.omit
srv_g_scatterplotmatrix <- function(input,
                                    output,
                                    session,
                                    datasets,
                                    variables,
                                    plot_height,
                                    plot_width) {

  init_chunks()

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(variables),
    input_id = "variables"
  )

  # plot
  plot_r <- reactive({
    chunks_reset()
    chunks_push_data_merge(merged_data())

    ANL <- chunks_get_var("ANL") # nolint
    validate_has_data(ANL, 10)

    alpha <- input$alpha # nolint
    cex <- input$cex # nolint
    add_cor <- input$cor # nolint
    cor_method <- input$cor_method # nolint
    cor_na_omit <- input$cor_na_omit # nolint

    cor_na_action <- if (cor_na_omit) {
      "na.omit"
    } else {
      "na.fail"
    }

    cols_names <- unique(unname(do.call(c, merged_data()$columns_source)))
    validate(need(length(cols_names) > 1, "Need at least 2 columns."))
    validate_has_data(ANL[, cols_names], 10, complete = TRUE, allow_inf = FALSE)

    # get labels and proper variable names
    varnames <- varname_w_label(cols_names, ANL, wrap_width = 20) # nolint

    # check character columns. If any, then those are converted to factors
    check_char <- vapply(ANL[, cols_names], is.character, logical(1))
    if (any(check_char)) {
      chunks_push(bquote({
        ANL <- ANL[, .(cols_names)] %>% # nolint
          dplyr::mutate_if(is.character, as.factor) %>%
          droplevels()
      }))
    } else {
      chunks_push(bquote({
        ANL <- ANL[, .(cols_names)] %>% # nolint
          droplevels()
      }))
    }

    # create plot
    if (add_cor) {
      shinyjs::show("cor_method")
      shinyjs::show("cor_use")
      shinyjs::show("cor_na_omit")

      chunks_push(bquote({
        plot <- lattice::splom(
          ANL,
          varnames = .(varnames),
          panel = function(x, y, ...) {
            lattice::panel.splom(x = x, y = y, ...)
            cpl <- lattice::current.panel.limits()
            lattice::panel.text(
              mean(cpl$xlim),
              mean(cpl$ylim),
              get_scatterplotmatrix_stats(
                x,
                y,
                .f = cor.test,
                .f_args = list(method = .(cor_method),
                na.action = .(cor_na_action))),
              alpha = 0.6,
              fontsize = 18,
              fontface = "bold")
          },
          pch = 16,
          alpha = .(alpha),
          cex = .(cex))
        print(plot)
      }))
    } else {
      shinyjs::hide("cor_method")
      shinyjs::hide("cor_use")
      shinyjs::hide("cor_na_omit")
      chunks_push(bquote({
        lattice::splom(ANL, varnames = .(varnames),
                       pch = 16, alpha = .(alpha), cex = .(cex))
      }))
    }
    chunks_safe_eval()
  })

  # Insert the plot into a plot_with_settings module
  callModule(
    plot_with_settings_srv,
    id = "myplot",
    plot_r = plot_r,
    height = plot_height,
    width = plot_width
  )

  # show a message if conversion to factors took place
  output$message <- renderText({
    ANL <- merged_data()$data() # nolint
    cols_names <- unique(unname(do.call(c, merged_data()$columns_source)))
    check_char <- vapply(ANL[, cols_names], is.character, logical(1))
    if (any(check_char)) {
      is_single <- sum(check_char) == 1
      paste(
        "Character",
        ifelse(is_single, "variable", "variables"),
        paste0("(", paste(cols_names[check_char], collapse = ", "), ")"),
        ifelse(is_single, "was", "were"),
        "converted to",
        ifelse(is_single, "factor.", "factors.")
      )
    } else {
      ""
    }
  })

  # show r code
  observeEvent(input$show_rcode, {
    title <- paste0(
      "Scatterplotmatrix of ",
      paste(merged_data()$cols, collapse = ", ")
    )

    show_rcode_modal(
      title = "R Code for a Scatterplotmatrix",
      rcode = get_rcode(
        datasets = datasets,
        title = title
      )
    )
  })

  show_r_code_title <- reactive(
    paste0(
      "Scatterplotmatrix of ",
      paste(unlist(merged_data()$columns_source), collapse = ", ")
    )
  )

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(variables)),
    modal_title = show_r_code_title(),
    code_header = show_r_code_title()
  )
}


#' Get stats for x-y pairs in scatterplot matrix
#' @description uses cor.test per default for all numerical input variables and converts results
#'  to character vector. Could be extended if different stats for different variable
#'  types are needed. Meant to be called from \code{lattice::panel.text}.
#' @param x \code{numeric}
#' @param y \code{numeric}
#' @param .f \code{function}, function that accepts x and y as formula input \code{~ x + y}.
#' Default \code{cor.test}
#' @param .f_args \code{list} of arguments to be passed to \code{.f}
#' @param round_stat \code{integer}
#' @param round_pval \code{integer}
#' @details presently we need to use a formula input for \code{cor.test} because
#' \code{na.fail} only gets evaluated when a formula is passed (see below).
#' \preformatted{
#' x = c(1,3,5,7,NA)
#' y = c(3,6,7,8,1)
#' cor.test(x, y, na.action = "na.fail")
#' cor.test(~ x + y,  na.action = "na.fail")
#' }
#' @return \code{character} with stats. For \code{cor.test} correlation coefficient and p-value.
#' @export
#' @examples
#' set.seed(1)
#' x <- runif(25, 0, 1)
#' y <- runif(25, 0, 1)
#' x[c(3,10,18)] <- NA
#'
#' get_scatterplotmatrix_stats(x, y, .f = cor.test, .f_args = list(method = "pearson"))
#' get_scatterplotmatrix_stats(x, y, .f = cor.test, .f_args = list(method = "pearson",
#'   na.action = na.fail))
get_scatterplotmatrix_stats <- function(x, y,
                                        .f = cor.test,
                                        .f_args = list(),
                                        round_stat = 2,
                                        round_pval = 4) {
  if (is.numeric(x) && is.numeric(y)) {

    stat <- tryCatch(do.call(.f, c(list(~ x + y), .f_args)), error = function(e) NA)

    if (anyNA(stat)) {
      return("NA")
    } else if (all(c("estimate", "p.value") %in% names(stat))) {
      return(paste(c(
        paste0(names(stat$estimate), ":", round(stat$estimate, round_stat)),
        paste0("P:", round(stat$p.value, round_pval))),
        collapse = "\n"))
    } else {
      stop("function not supported")
    }

  } else {
    if ("method" %in% names(.f_args)) {
      if (.f_args$method == "pearson") {
        return("cor:-")
      }
      if (.f_args$method == "kendall") {
        return("tau:-")
      }
      if (.f_args$method == "spearman") {
        return("rho:-")
      }
    }
    return("-")
  }

}
