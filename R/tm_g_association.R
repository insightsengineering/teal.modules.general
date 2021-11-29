#' Stack Plots of variables and show association with reference variable
#' @md
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param ref (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   reference variable, must set `multiple = FALSE`.
#' @param vars (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   associated variables.
#' @param show_association optional, (`logical`) Whether show association of `vars`
#'   with reference variable. Defaults to `TRUE`.
#' @param distribution_theme optional, (`character`) `ggplot` Theme to be used by default
#'   for the distribution plot. All themes can be chosen by the user. Defaults to `gray`.
#'   `gg_themes` is defined internally as
#'   `c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test")`
#' @param association_theme optional, (`character`) `ggplot` Theme to be used by default
#'   for the association plots. All themes can be chosen by the user. Defaults to `gray`.
#'   `gg_themes` is defined internally as
#'   `c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test")`
#'
#' @note For more examples, please see the vignette "Using association plot" via
#'   \code{vignette("using-association-plot", package = "teal.modules.general")}.
#' @export
#' @examples
#' # Association plot of selected reference variable (SEX)
#' # against other selected variables (BMRKR1)
#' library(scda)
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_association(
#'       ref = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(
#'             ADSL,
#'             c("SEX", "RACE", "COUNTRY", "ARM", "STRATA1", "STRATA2", "ITTFL", "BMRKR2")
#'           ),
#'           selected = "RACE",
#'           fixed = FALSE
#'         )
#'       ),
#'       vars = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = variable_choices(
#'             ADSL,
#'             c("SEX", "RACE", "COUNTRY", "ARM", "STRATA1", "STRATA2", "ITTFL", "BMRKR2")
#'           ),
#'           selected = "BMRKR2",
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
tm_g_association <- function(label = "Association",
                             ref,
                             vars,
                             show_association = TRUE,
                             plot_height = c(600, 400, 5000),
                             plot_width = NULL,
                             distribution_theme = gg_themes,
                             association_theme = gg_themes,
                             pre_output = NULL,
                             post_output = NULL) {
  logger::log_info("Initializing tm_g_association")
  if (!is_class_list("data_extract_spec")(ref)) {
    ref <- list(ref)
  }
  if (!is_class_list("data_extract_spec")(vars)) {
    vars <- list(vars)
  }

  stopifnot(is_character_single(label))
  stopifnot(is_class_list("data_extract_spec")(ref))
  stop_if_not(list(
    all(vapply(ref, function(x) !(x$select$multiple), logical(1))),
    "'ref' should not allow multiple selection"))
  stopifnot(is_class_list("data_extract_spec")(vars))
  stopifnot(is_logical_single(show_association))
  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)
  distribution_theme <- match.arg(distribution_theme)
  stopifnot(is_character_single(distribution_theme))
  association_theme <- match.arg(association_theme)
  stopifnot(is_character_single(association_theme))

  args <- as.list(environment())

  data_extract_list <- list(
    ref = ref,
    vars = vars
  )

  module(
    label = label,
    server = srv_tm_g_association,
    ui = ui_tm_g_association,
    ui_args = args,
    server_args = c(data_extract_list, list(plot_height = plot_height, plot_width = plot_width)),
    filters = get_extract_datanames(data_extract_list)
  )
}


ui_tm_g_association <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  is_single_dataset_value <- is_single_dataset(args$ref, args$vars)

  standard_layout(
    output = white_small_well(
      textOutput(ns("title")),
      tags$br(),
      plot_with_settings_ui(id = ns("myplot"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("ref", "vars")]),
      data_extract_ui(
        id = ns("ref"),
        label = "Reference variable",
        data_extract_spec = args$ref,
        is_single_dataset = is_single_dataset_value
      ),
      data_extract_ui(
        id = ns("vars"),
        label = "Associated variables",
        data_extract_spec = args$vars,
        is_single_dataset = is_single_dataset_value
      ),
      checkboxInput(ns("association"),
        "Association with the reference variable",
        value = args$show_association
      ),
      checkboxInput(ns("show_dist"),
        "Distribution",
        value = FALSE
      ),
      checkboxInput(ns("log_transformation"),
        "Log transformed",
        value = FALSE
      ),
      panel_group(
        panel_item(
          title = "Plot settings",
          optionalSliderInputValMinMax(ns("alpha"), "Scatterplot opacity:", c(0.5, 0, 1), ticks = FALSE),
          optionalSliderInputValMinMax(ns("size"), "Scatterplot points size:", c(2, 1, 8), ticks = FALSE),
          checkboxInput(ns("swap_axes"), "Swap axes", value = FALSE),
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = FALSE),
          optionalSelectInput(
            inputId = ns("distribution_theme"),
            label = "Distribution theme (by ggplot):",
            choices = gg_themes,
            selected = args$distribution_theme,
            multiple = FALSE
          ),
          optionalSelectInput(
            inputId = ns("association_theme"),
            label = "Association theme (by ggplot):",
            choices = gg_themes,
            selected = args$association_theme,
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


#' @importFrom grid grid.newpage grid.draw
#' @importFrom shinyjs show hide
srv_tm_g_association <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 ref,
                                 vars,
                                 plot_height,
                                 plot_width) {

  init_chunks()

  selector_list <- data_extract_multiple_srv(data_extract = list(ref = ref, vars = vars), datasets = datasets)

  merged_data <- data_merge_srv(
    datasets = datasets,
    selector_list = selector_list
  )

  chunks_reactive <- reactive({
    chunks_reset()
    chunks_push_data_merge(merged_data())

    ANL <- chunks_get_var("ANL") # nolint
    validate_has_data(ANL, 3)

    vars_names <- selector_list()$vars()$select_ordered

    ref_name <- as.vector(merged_data()$columns_source$ref)
    association <- input$association
    show_dist <- input$show_dist
    log_transformation <- input$log_transformation
    rotate_xaxis_labels <- input$rotate_xaxis_labels
    swap_axes <- input$swap_axes
    distribution_theme <- input$distribution_theme
    association_theme <- input$association_theme

    validate(need(ref_name, "need at least one variable selected"))

    is_scatterplot <- is.numeric(ANL[[ref_name]]) && any(vapply(ANL[vars_names], is.numeric, logical(1)))
    if (is_scatterplot) {
      shinyjs::show("alpha")
      shinyjs::show("size")
      alpha <- input$alpha # nolint
      size <- input$size
    } else {
      shinyjs::hide("alpha")
      shinyjs::hide("size")
      alpha <- 0.5
      size <- 2
    }

    validate(need(!(ref_name %in% vars_names), "associated variables and reference variable cannot overlap"))
    validate(need(!is.null(distribution_theme) && !is.null(association_theme), "Please select a theme."))

    validate_has_data(ANL[, c(ref_name, vars_names)], 3, complete = TRUE, allow_inf = FALSE)

    # reference
    ref_class <- class(ANL[[ref_name]])
    if (is.numeric(ANL[[ref_name]]) && log_transformation) {
      # works for both integers and doubles
      ref_cl_name <- call("log", as.name(ref_name))
      ref_cl_lbl <- varname_w_label(ref_name, ANL, prefix = "Log of ")
    } else {
      # silently ignore when non-numeric even if `log` is selected because some
      # variables may be numeric and others not
      ref_cl_name <- as.name(ref_name)
      ref_cl_lbl <- varname_w_label(ref_name, ANL)
    }
    ref_call <- bivariate_plot_call(
      data_name = "ANL",
      x = ref_cl_name,
      x_class = ref_class,
      x_label = ref_cl_lbl,
      freq = !show_dist,
      theme = call(paste0("theme_", distribution_theme)),
      rotate_xaxis_labels = rotate_xaxis_labels,
      swap_axes = FALSE,
      size = size,
      alpha = alpha
    )

    # association
    ref_class_cov <- ifelse(association, ref_class, "NULL")

    chunks_push(substitute(
      expr = title <- new_title,
      env = list(new_title = paste(
        "Association",
        ifelse(ref_class_cov == "NULL", "for", "between"),
        paste(lapply(vars_names, function(x) {
          if (is.numeric(ANL[[x]]) && log_transformation) {
            varname_w_label(x, ANL, prefix = "Log of ")
          } else {
            varname_w_label(x, ANL)
          }
        }), collapse = " / "),
        ifelse(ref_class_cov == "NULL", "", paste("and", ref_cl_lbl))
      ))
    ))

    chunks_push(quote(print(title)))

    chunks_safe_eval()

    var_calls <- lapply(vars_names, function(var_i) {
      var_class <- class(ANL[[var_i]])
      if (is.numeric(ANL[[var_i]]) && log_transformation) {
        # works for both integers and doubles
        var_cl_name <- call("log", as.name(var_i))
        var_cl_lbl <- varname_w_label(var_i, ANL, prefix = "Log of ")
      } else {
        # silently ignore when non-numeric even if `log` is selected because some
        # variables may be numeric and others not
        var_cl_name <- as.name(var_i)
        var_cl_lbl <- varname_w_label(var_i, ANL)
      }
      bivariate_plot_call(
        data_name = "ANL",
        x = ref_cl_name,
        y = var_cl_name,
        x_class = ref_class_cov,
        y_class = var_class,
        x_label = ref_cl_lbl,
        y_label = var_cl_lbl,
        theme = call(paste0("theme_", association_theme)),
        freq = !show_dist,
        rotate_xaxis_labels = rotate_xaxis_labels,
        swap_axes = swap_axes,
        alpha = alpha,
        size = size
      )
    })

    chunks_push(
      expression = substitute(
        expr = {
          plots <- plot_calls
          p <- tern::stack_grobs(grobs = lapply(plots, ggplotGrob))
          grid::grid.newpage()
          grid::grid.draw(p)
        },
        env = list(plot_calls = do.call("call", c(list("list", ref_call), var_calls), quote = TRUE))
      ),
      id = "plotCall"
    )
  })

  plot_r <- reactive({
    chunks_uneval()
    chunks_reactive()
    chunks_safe_eval()
    chunks_get_var(var = "p")
  })

  callModule(
    plot_with_settings_srv,
    id = "myplot",
    plot_r = plot_r,
    height = plot_height,
    width = plot_width
  )

  output$title <- renderText({
    chunks_reactive()
    chunks_get_var("title")
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(ref, vars)),
    modal_title = "R Code for the Association Plot",
    code_header = "Association Plot"
  )
}
