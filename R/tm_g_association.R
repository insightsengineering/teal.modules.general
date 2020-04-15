#' Stack Plots of variables and show association with reference variable
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams teal::module
#' @param ref (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   reference variable, must set \code{multiple = FALSE}
#' @param vars (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   associated variables
#' @param show_association (\code{logical}) whether show association of \code{vars} with reference variable
#' @param plot_height (\code{numeric}) vector with three elements defining selected, min and max plot height
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @export
#' @examples
#' # Association plot of selected reference variable (SEX)
#' # against other selected variables (BMRKR1)
#' library(random.cdisc.data)
#' ADSL <- radsl(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- radsl(cached = TRUE)",
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
                             pre_output = NULL,
                             post_output = NULL) {
  if (!is_class_list("data_extract_spec")(ref)) {
    ref <- list(ref)
  }
  if (!is_class_list("data_extract_spec")(vars)) {
    vars <- list(vars)
  }

  stopifnot(is_character_single(label))
  stopifnot(is_class_list("data_extract_spec")(ref))
  stop_if_not(list(all(vapply(ref, function(x) !(x$select$multiple), logical(1))),
                   "'ref' should not allow multiple selection"))
  stopifnot(is_class_list("data_extract_spec")(vars))
  stopifnot(is_logical_single(show_association))
  stopifnot(is_numeric_vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])

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
    server_args = data_extract_list,
    filters = get_extract_datanames(data_extract_list)
  )
}


ui_tm_g_association <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)

  standard_layout(
    output = white_small_well(
      textOutput(ns("title")),
      tags$br(),
      plot_height_output(id = ns("myplot"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("ref", "vars")]),
      data_extract_input(
        id = ns("ref"),
        label = "Reference variable",
        data_extract_spec = args$ref
      ),
      data_extract_input(
        id = ns("vars"),
        label = "Associated variables",
        data_extract_spec = args$vars
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
      plot_height_input(id = ns("myplot"), value = args$plot_height),
      panel_group(
        panel_item(
          title = "Plot settings",
          checkboxInput(ns("swap_axes"), "Swap axes", value = FALSE),
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = FALSE)
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}


#' @importFrom grid grid.newpage grid.draw
#' @importFrom tern stack_grobs
srv_tm_g_association <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 ref,
                                 vars) {

  init_chunks()

  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(ref, vars),
    input_id = c("ref", "vars")
  )

  chunks_reactive <- reactive({
    ANL <- merged_data()$data() # nolint
    validate_has_data(ANL, 3)
    chunks_reset()

    ref_name <- as.vector(merged_data()$columns_source$ref)
    vars_names <- as.vector(merged_data()$columns_source$vars)

    association <- input$association
    show_dist <- input$show_dist
    log_transformation <- input$log_transformation
    rotate_xaxis_labels <- input$rotate_xaxis_labels
    swap_axes <- input$swap_axes

    validate(
      need(length(ref_name) > 0, "need at least one variable selected"),
      need(!(ref_name %in% vars_names), "associated variables and reference variable cannot overlap")
    )

    # reference
    ref_class <- class(ANL[[ref_name]])
    ref_cl_name <- if (ref_class == "numeric" && log_transformation) {
      call("log", as.name(ref_name))
    } else {
      as.name(ref_name)
    }
    ref_call <- bivariate_plot_call(
      data_name = "ANL",
      x = ref_cl_name,
      x_class = ref_class,
      x_label = attr(ANL[[ref_name]], "label"),
      freq = !show_dist,
      theme = quote(theme(panel.background = element_rect(fill = "papayawhip", colour = "papayawhip"))),
      rotate_xaxis_labels = rotate_xaxis_labels,
      swap_axes = FALSE
    )

    # association
    ref_class_cov <- ifelse(association, ref_class, "NULL")

    chunks_push(bquote(title <- .(paste(
      "Association",
      ifelse(ref_class_cov == "NULL", "for", "between"),
      paste(
        vapply(vars_names, function(x) paste(attr(ANL[[x]], "label"), paste0("[", x, "]")), character(1)),
        collapse = " / "
      ),
      ifelse(ref_class_cov == "NULL", "", paste("and", attr(ANL[[ref_name]], "label"), paste0("[", ref_cl_name, "]")))
    ))))

    chunks_push(quote(print(title)))

    chunks_safe_eval()

    var_calls <- lapply(vars_names, function(var_i) {
      var_class <- class(ANL[[var_i]])
      var_cl_name <- if (var_class == "numeric" && log_transformation) {
        call("log", as.name(var_i))
      } else {
        as.name(var_i)
      }
      bivariate_plot_call(
        data_name = "ANL",
        x = ref_cl_name,
        y = var_cl_name,
        x_class = ref_class_cov,
        y_class = var_class,
        x_label = attr(ANL[[ref_name]], "label"),
        y_label = attr(ANL[[var_i]], "label"),
        freq = !show_dist,
        rotate_xaxis_labels = rotate_xaxis_labels,
        swap_axes = swap_axes,
        alpha = 1
      )
    })

    chunks_push(
      expression = bquote({
        plots <- .(do.call("call", c(list("list", ref_call), var_calls), quote = TRUE))
        p <- tern::stack_grobs(grobs = lapply(plots, ggplotGrob))
        grid::grid.newpage()
        grid::grid.draw(p)
      }),
      id = "plotCall"
    )
  })

  output$plot <- renderPlot({
    chunks_reactive()
    chunks_safe_eval()
  })

  output$title <- renderText({
    chunks_reactive()
    chunks_get_var("title")
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    merge_expression = reactive(merged_data()$expr),
    modal_title = "R Code for the Association Plot",
    code_header = "Association Plot"
  )
}
