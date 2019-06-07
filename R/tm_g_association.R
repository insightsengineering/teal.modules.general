#' Stack Plots of variables and show association with reference variable
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams teal::module
#' @param dataname (\code{character}) data set name to analyze
#' @param ref_var (\code{choices_selected}) reference variable
#' @param vars (\code{choices_selected}) associated variables
#' @param show_association (\code{logical}) wheater show association of \code{vars} with refference variable
#' @param plot_height (\code{numeric}) vector with three elements defining selected, min and max plot height
#' @param with_show_r_code (\code{logical}) Whether show R Code button shall be enabled
#'
#' @export
#'
#' @examples
#'
#' library(random.cdisc.data)
#' ASL <- radsl(seed = 1)
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     code = "ASL <- radsl(seed = 1)",
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_association(
#'       dataname = "ASL",
#'       ref_var = choices_selected(names(ASL), "AGE"),
#'       vars = choices_selected(names(ASL), "SEX")
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_association <- function(label = "Association",
                             dataname,
                             ref_var,
                             vars,
                             show_association = TRUE,
                             plot_height = c(600, 400, 5000),
                             pre_output = NULL,
                             post_output = NULL,
                             with_show_r_code = TRUE) {
  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  stopifnot(is.choices_selected(ref_var))
  stopifnot(is.choices_selected(vars))
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
    server_args = list(dataname = dataname),
    filters = dataname
  )
}

#' @importFrom teal.devel optionalSliderInputValMinMax standard_layout
ui_tm_g_association <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  # standard_layout2(
  standard_layout(
    output = white_small_well(plot_height_output(id = ns("myplot"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(
        ns("ref_var"),
        "Reference variable",
        a$ref_var$choices,
        a$ref_var$selected,
        multiple = FALSE
      ),
      optionalSelectInput(
        ns("vars"),
        "Associated variables",
        a$vars$choices,
        a$vars$selected,
        multiple = TRUE
      ),
      checkboxInput(ns("association"),
        "Association with First Variable",
        value = a$show_association
      ),
      checkboxInput(ns("show_dist"),
        "Distribution",
        value = FALSE
      ),
      checkboxInput(ns("log_transformation"),
        "Log transformed",
        value = FALSE
      ),
      plot_height_input(id = ns("myplot"), value = a$plot_height)
    ),
    forms = if (a$with_show_r_code) actionButton(ns("show_rcode"), "Show R Code", width = "100%") else NULL,
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @importFrom tern stack_grobs
srv_tm_g_association <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 dataname) {
  stopifnot(all(dataname %in% datasets$datanames()))

  use_chunks(session)

  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  output$plot <- renderPlot({
    ref_var <- input$ref_var
    vars <- input$vars
    association <- input$association
    show_dist <- input$show_dist
    log_transformation <- input$log_transformation

    anl_f <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_f)

    validate(
      need(nrow(anl_f) > 3, "need at least three rows"),
      need(length(ref_var) > 0, "need at least one variable selected"),
      need(!(ref_var %in% vars), "associated variables and reference variable cannot overlap"),
      need(ref_var %in% names(anl_f), paste("reference variable not found in ", dataname)),
      need(all(vars %in% names(anl_f)), paste("not all selected variables are in ", dataname))
    )

    ref_var_class <- class(anl_f[[ref_var]])

    if (ref_var_class == "numeric" && log_transformation) {
      ref_var <- call("log", as.name(ref_var))
    }

    ref_cl <- call(
      "+",
      bivariate_plot_call(anl_name, ref_var, NULL, ref_var_class, "NULL", freq = !show_dist),
      quote(theme(panel.background = element_rect(fill = "papayawhip", colour = "papayawhip")))
    )

    ref_var_class_cov <- if (association) {
      ref_var_class
    } else {
      "NULL"
    }

    var_cls <- lapply(vars, function(var_i) {
      class_i <- class(anl_f[[var_i]])
      if (class_i == "numeric" && log_transformation) {
        var_i <- call("log", as.name(var_i))
      }

      bivariate_plot_call(anl_name, var_i, ref_var, class_i, ref_var_class_cov, freq = !show_dist)
    })


    cl1 <- call("<-", quote(plots), do.call("call", c(list("list", ref_cl), var_cls), quote = TRUE))

    cl2 <- call("<-", quote(p), call("stack_grobs", grobs = quote(lapply(plots, ggplotGrob))))

    cl <- call("{", cl1, cl2, quote(grid.newpage()), quote(grid.draw(p)))

    renew_chunk_environment(envir = environment())
    renew_chunks()

    set_chunk("plotCall", cl)

    p <- eval_remaining()

    if (is(p, "try-error")) {
      validate(need(FALSE, p))
    } else {
      p
    }

  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "R Code for the Association Plot",
      rcode = get_rcode(
        datasets = datasets,
        merged_dataname = "anl",
        title = "Association Plot"
      )
    )
  })
}
