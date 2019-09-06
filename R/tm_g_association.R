#' Stack Plots of variables and show association with reference variable
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams teal::module
#' @param ref (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   reference variable, must set \code{multiple = FALSE}
#' @param vars (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   associated variables
#' @param show_association (\code{logical}) wheater show association of \code{vars} with refference variable
#' @param plot_height (\code{numeric}) vector with three elements defining selected, min and max plot height
#' @param with_show_r_code (\code{logical}) Whether show R Code button shall be enabled
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @export
#' @examples
#' # Association plot of selected reference variable (SEX)
#' # against other selected variables (BMRKR1)
#' library(random.cdisc.data)
#' ADSL <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- cadsl",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_association(
#'       ref = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = names(ADSL),
#'           selected = "AGE",
#'           fixed = FALSE
#'         )
#'       ),
#'       vars = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = names(ADSL),
#'           selected = "BMRKR1",
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
                             post_output = NULL,
                             with_show_r_code = TRUE) {
  if (!is.class.list("data_extract_spec")(ref)) {
    ref <- list(ref)
  }
  if (!is.class.list("data_extract_spec")(vars)) {
    vars <- list(vars)
  }

  stopifnot(is.character.single(label))
  stopifnot(is.class.list("data_extract_spec")(ref))
  stop_if_not(list(all(vapply(ref, function(x) !(x$select$multiple), logical(1))),
                   "'ref' should not allow multiple selection"))
  stopifnot(is.class.list("data_extract_spec")(vars))
  stopifnot(is.logical.single(show_association))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])
  stopifnot(is.logical(with_show_r_code))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_tm_g_association,
    ui = ui_tm_g_association,
    ui_args = args,
    server_args = list(
      ref = ref,
      vars = vars
    ),
    filters = "all"
  )
}


ui_tm_g_association <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)

  standard_layout(
    output = white_small_well(plot_height_output(id = ns("myplot"))),
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
        "Association with the first variable",
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
      plot_height_input(id = ns("myplot"), value = args$plot_height)
    ),
    forms = if (args$with_show_r_code) actionButton(ns("show_rcode"), "Show R code", width = "100%") else NULL,
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

  output$plot <- renderPlot({
    ANL <- merged_data()$data() # nolint

    chunks_reset()

    ref_name <- merged_data()$columns_source$ref
    vars_names <- merged_data()$columns_source$vars

    association <- input$association
    show_dist <- input$show_dist
    log_transformation <- input$log_transformation

    validate(
      need(nrow(ANL) > 3, "need at least three rows"),
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
    ref_call <- call(
      "+",
      bivariate_plot_call("ANL", ref_cl_name, character(0), ref_class, "NULL", freq = !show_dist),
      quote(theme(panel.background = element_rect(fill = "papayawhip", colour = "papayawhip")))
    )

    # association
    ref_class_cov <- ifelse(association, ref_class, "NULL")
    var_calls <- lapply(vars_names, function(var_i) {
      var_class <- class(ANL[[var_i]])
      var_cl_name <- if (var_class == "numeric" && log_transformation) {
        call("log", as.name(var_i))
      } else {
        as.name(var_i)
      }
      bivariate_plot_call("ANL", var_cl_name, ref_cl_name, var_class, ref_class_cov, freq = !show_dist)
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

    chunks_safe_eval()
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "R Code for the Association Plot",
      rcode = get_rcode(
        datasets = datasets,
        merge_expression = merged_data()$expr,
        title = "Association Plot"
      )
    )
  })
}
