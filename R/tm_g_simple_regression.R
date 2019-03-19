
#' Scatterplot and Regression Model
#'
#' This module is designed for horizontal data
#'
#' @inheritParams teal::module
#' @inheritParams teal::standard_layout
#' @param dataname name of dataset used to generate table
#' @param response_var (\code{choices_selected}) Output of \code{teal::choices_selected} to define the
#' 	response variable
#' @param regressor_var (\code{choices_selected}) Output of \code{teal::choices_selected} to define the
#' 	regressor variable
#' @param plot_height if scalar then the plot will have a fixed height. If a
#'   slider should be presented to adjust the plot height dynamically then it
#'   can be a vector of length three with \code{c(value, min and max)}.
#'
#'
#' @import ggplot2
#'
#' @export
#'
#'
#' @examples
#'
#' library(random.cdisc.data)
#' asl  <- radsl(seed = 1)
#'
#' asl$cont  <- rnorm(nrow(asl))
#' asl$cont2 <- rnorm(nrow(asl))
#' asl$cont3 <- asl$cont * 3 + 2 + rnorm(nrow(asl), sd = .3)
#'
#' attr(asl, "source") <- "random.cdisc.data::radsl(seed = 1)"
#'
#' x <- teal::init(
#'   data = list(ASL = asl),
#'   modules = root_modules(
#'     tm_g_simple_regression(
#'       dataname = "ASL",
#'       response_var = choices_selected(c("cont", "cont2"), "cont"),
#'       regressor_var = choices_selected(names(asl), "cont3"),
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(x$ui, x$server)
#'
#'
#' fit <- lm(cont ~ cont2, data = ASL)
#'
#' plot(fit)
#' }
tm_g_simple_regression <- function(label = "Simple Regression Analysis",
                                   dataname,
                                   response_var,
                                   regressor_var,
                                   plot_height = c(600, 200, 2000),
                                   pre_output = NULL,
                                   post_output = NULL) {
  args <- as.list(environment())

  teal::module(
    label = label,
    server = srv_g_simple_regression,
    ui = ui_g_simple_regression,
    ui_args = args,
    server_args = list(dataname = dataname),
    filters = dataname
  )
}


#' @import teal
#' @importFrom teal.devel white_small_well
ui_g_simple_regression <- function(id, ...) {
  a <- list(...)

  ns <- NS(id)

  standard_layout(
    output = teal.devel::white_small_well(
      tags$div(
        tags$div(teal.devel::white_small_well(uiOutput(ns("plot_ui")))),
        tags$div(verbatimTextOutput(ns("text")))
      )
    ),
    encoding = div(
      helpText("Dataset:", tags$code(a$dataname)),
      optionalSelectInput(
        ns("response_var"), "Response Variable",
        a$response_var$choices, a$response_var$selected
      ),
      optionalSelectInput(
        ns("regressor_var"), "Regressor Variables",
        a$regressor_var$choices, a$regressor_var$selected
      ),
      radioButtons(ns("plot_type"),
        label = "Plot Type",
        choices = c(
          "Response vs Regressor", "Residuals vs Fitted",
          "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
          "Cook's dist vs Leverage h[ii]/(1 - h[ii]"
        ),
        selected = "Response vs Regressor"
      ),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    )
  )
}

#' @importFrom teal.devel as.global
#' @importFrom graphics plot abline
srv_g_simple_regression <- function(input, output, session, datasets, dataname) {
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("plot"), height = plot_height)
  })

  anl_head <- head(datasets$get_data(dataname, reactive = FALSE, filtered = FALSE))

  fit_cl <- reactive({
    response_var <- input$response_var
    regressor_var <- input$regressor_var

    validate(
      need(length(intersect(response_var, regressor_var)) == 0, "response and regressor variables cannot intersect"),
      need(length(regressor_var) == 1, "please select a regressor variable"),
      need(is.numeric(anl_head[[response_var]]), "response variable needs to be numeric")
    )


    call("lm", as.formula(paste0(response_var, "~", regressor_var)), data = as.name("anl_filtered"))
  })

  fit <- reactive({
    anl_filtered <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    fit_cl <- fit_cl()

    validate(
      need(nrow(anl_filtered) >= 10, paste("need at lease 10 observations, currenty have only", nrow(anl_filtered)))
    )

    attr(fit_cl[[2]], ".Environment") <- environment()

    teal.devel::as.global(fit_cl)
    fit <- eval(fit_cl)
    fit
  })

  output$plot <- renderPlot({
    fit <- fit()
    plot_type <- input$plot_type

    if (plot_type == "Response vs Regressor") {
      plot(fit$model[, 2:1])
      graphics::abline(fit)
    } else {
      i <- which(plot_type == c(
        "Residuals vs Fitted",
        "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
        "Cook's dist vs Leverage h[ii]/(1 - h[ii]"
      ))

      plot(fit, which = i)
    }
  })


  output$text <- renderPrint({
    fit <- fit()

    validate(need(is(fit, "lm"), "there seem to problems fitting the model"))

    summary(fit)
  })
}
