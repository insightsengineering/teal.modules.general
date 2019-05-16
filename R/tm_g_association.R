#' Stack Plots of variables and show association with reference variable
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams teal::module
#' @param dataname (\code{character}) data set name to analyze
#' @param var (\code{choices_selected}) TODO
#' @param show_association (\code{logical}) TODO
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
#'   data = list(ASL = ASL),
#'   modules = root_modules(
#'     tm_g_association(
#'       dataname = "ASL",
#'       var = choices_selected(names(ASL), "AGE")
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_association <- function(label = "Association",
                             dataname,
                             var,
                             show_association = TRUE,
                             plot_height = c(600, 400, 5000),
                             pre_output = NULL,
                             post_output = NULL,
                             with_show_r_code = TRUE) {
  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  stopifnot(is.choices_selected(var))
  stopifnot(is.logical.single(show_association))
  stopifnot(is_numeric_vector(plot_height) && length(plot_height) == 3)
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
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("var"),
        "Variables",
        a$var$choices,
        a$var$selected,
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
      optionalSliderInputValMinMax(ns("plot_height"),
        "plot height",
        a$plot_height,
        ticks = FALSE
      )
    ),
    forms = if (a$with_show_r_code) actionButton(ns("show_rcode"), "Show R Code", width = "100%") else NULL,
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @importFrom teal.devel get_rcode_header
#' @importFrom tern stack_grobs
srv_tm_g_association <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 dataname) {
  stopifnot(all(dataname %in% datasets$datanames()))

  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    ns <- session$ns
    plotOutput(ns("plot"), height = plot_height)
  })


  anl_head <- head(datasets$get_data(dataname, filtered = FALSE, reactive = FALSE))

  anl_name <- paste0(dataname, "_FILTERED")

  plot_call <- reactive({
    var <- input$var
    association <- input$association
    show_dist <- input$show_dist
    log_transformation <- input$log_transformation

    # annotate globals as.global(anl_head, var, association, show_dist, log_transformation)

    validate(
      need(nrow(anl_head) > 3, "need at least three rows"),
      need(length(var) > 0, "need at least one variable selected"),
      need(all(var %in% names(anl_head)), paste("not all selected variables are in ", dataname))
    )


    ref_var <- var[1]
    ref_var_class <- class(anl_head[[ref_var]])

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

    var_cls <- lapply(var[-1], function(var_i) {
      class_i <- class(anl_head[[var_i]])
      if (class_i == "numeric" && log_transformation) {
        var_i <- call("log", as.name(var_i))
      }

      bivariate_plot_call(anl_name, var_i, ref_var, class_i, ref_var_class_cov, freq = !show_dist)
    })


    cl1 <- call("<-", quote(plots), do.call("call", c(list("list", ref_cl), var_cls), quote = TRUE))

    cl2 <- call("<-", quote(p), call("stack_grobs", grobs = quote(lapply(plots, ggplotGrob))))

    call("{", cl1, cl2, quote(grid.newpage()), quote(grid.draw(p)))
  })


  output$plot <- renderPlot({
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    plot_call <- plot_call()
    # annotate globals as.global(plot_call, anl_filtered)

    p <- try(eval(plot_call, list2env(setNames(list(anl_filtered, emptyenv()), c(anl_name, "parent")))))

    if (is(p, "try-error")) {
      validate(need(FALSE, p))
    } else {
      p
    }
  })

  observeEvent(input$show_rcode, {
    header <- teal.devel::get_rcode_header(title = "Association Plot")

    str_rcode <- paste(c(
      "",
      header,
      "",
      deparse(plot_call(), width.cutoff = 60)
    ), collapse = "\n")

    # log code .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Association Plot",
      tags$pre(tags$code(class = "R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
}
