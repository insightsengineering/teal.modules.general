
#' Stack Plots of variables and show association with reference variable
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(random.cdisc.data)
#' ASL <- radsl()
#' ASL$BOOLEAN <- sample(c(TRUE, FALSE), nrow(ASL), TRUE)
#'
#' attr(ASL, "source") <- "# asl import"
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   modules = root_modules(
#'     tm_g_association(
#'       dataname = "ASL",
#'       var = choices_selected(names(ASL), "AGE")
#'     )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#' }
#'
tm_g_association <- function(
  label = "Association",
  dataname,
  var,
  show_association = TRUE,
  plot_height = c(600, 400, 5000),
  pre_output = NULL,
  post_output = NULL,
  code_data_processing = NULL
) {

  args <- as.list(environment())

  stopifnot(is.choices_selected(var))

  module(
    label = label,
    server = srv_tm_g_association,
    ui = ui_tm_g_association,
    ui_args = args,
    server_args = list(dataname = dataname,
                       code_data_processing = code_data_processing),
    filters = dataname
  )

}

ui_tm_g_association <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  #standard_layout2(
  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("var"),
                          "Variables",
                          a$var$choices,
                          a$var$selected, multiple = TRUE),
      checkboxInput(ns("association"),
                    "Association with First Variable",
                    value = a$show_association),
      checkboxInput(ns("show_dist"),
                    "Distribution",
                    value = FALSE),
      checkboxInput(ns("log_transformation"),
                    "Log transformed",
                    value = FALSE),
      optionalSliderInputValMinMax(ns("plot_height"),
                                   "plot height",
                                   a$plot_height,
                                   ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


srv_tm_g_association <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 dataname,
                                 code_data_processing) {

  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    ns <- session$ns
    plotOutput(ns("plot"), height=plot_height)
  })


  ANL_head <- head(datasets$get_data(dataname, filtered = FALSE, reactive = FALSE))

  plot_call <- reactive({

    var <- input$var
    association <- input$association
    show_dist <- input$show_dist
    log_transformation <- input$log_transformation

    # as.global(ANL_head, var, association, show_dist, log_transformation)

    validate(
      need(nrow(ANL_head) > 3, "need at least three rows"),
      need(length(var) > 0, "need at least one variable selected"),
      need(all(var %in% names(ANL_head)), paste("not all selected variables are in ", dataname))
    )


    ref_var <- var[1]
    ref_var_class <- class(ANL_head[[ref_var]])

    if (ref_var_class == "numeric" && log_transformation) {
      ref_var <- call('log', as.name(ref_var))
    }

    ref_cl <- call("+",
                   g_bp_cl("ANL_filtered", ref_var, NULL, ref_var_class, "NULL", freq = !show_dist),
                   quote(theme(panel.background = element_rect(fill = "papayawhip", colour = "papayawhip"))))

    ref_var_class_cov <- if (association) ref_var_class else "NULL"

    var_cls <- lapply(var[-1], function(var_i) {

      class_i <- class(ANL_head[[var_i]])
      if (class_i == "numeric" && log_transformation) {
        var_i <- call("log", as.name(var_i))
      }

      g_bp_cl("ANL_filtered", var_i, ref_var, class_i, ref_var_class_cov, freq = !show_dist)

    })


    cl1 <- call("<-", quote(plots), do.call("call", c(list("list", ref_cl), var_cls), quote = TRUE))

    cl2 <- call("<-", quote(p), call("stack_grobs", grobs = quote(lapply(plots, ggplotGrob))))

    call("{", cl1, cl2, quote(grid.newpage()), quote(grid.draw(p)))
  })


  output$plot <- renderPlot({

    ANL_filtered <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    plot_call <- plot_call()
    # as.global(plot_call, ANL_filtered)

    p <- try(eval(plot_call, list2env(list(ANL_filtered = ANL_filtered, parent = emptyenv()))))

    if (is(p, "try-error")) {
      validate(need(FALSE, p))
    } else {
      p
    }
  })

  observeEvent(input$show_rcode, {

    header <- get_rcode_header(
      title = "Association Plot",
      datanames = dataname,
      datasets = datasets,
      code_data_processing,
      packages = c("ggplot2", "ggmosaic")
    )

    str_rcode <- paste(c(
      "",
      header,
      "",
      deparse(plot_call(), width.cutoff = 60)
    ), collapse = "\n")

    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current Plot",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })

}
