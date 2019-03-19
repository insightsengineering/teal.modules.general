#' Scatterplot and Regression Model
#'
#' This module is designed for horizontal data
#'
#' @import ggplot2
#'
#' @param dataname name of dataset used to generate table
#' @param response_var (\code{choices_selected}) Output of \code{teal::choices_selected} to define the
#' 	response variable
#' @param regressor_var (\code{choices_selected}) Output of \code{teal::choices_selected} to define the
#' 	regressor variable
#' @param plot_height if scalar then the plot will have a fixed height. If a
#'   slider should be presented to adjust the plot height dynamically then it
#'   can be a vector of length three with \code{c(value, min and max)}.
#'
#' @export
#' @inheritParams teal::module
#' @inheritParams teal::standard_layout
#'
#' @examples
#'
#' library(random.cdisc.data)
#' asl  <- radsl(seed = 1)
#'
#' asl$cont  <- rnorm(nrow(asl))
#' asl$cont2 <- rnorm(nrow(asl))
#'
#' attr(asl, "source") <- "random.cdisc.data::radsl(seed = 1)"
#'
#' x <- teal::init(
#'   data = list(ASL = asl),
#'   modules = root_modules(
#'     tm_g_regression(
#'       dataname = "ASL",
#'       response_var = choices_selected(c("cont", "cont2"), "cont"),
#'       regressor_var = choices_selected(names(asl), c("cont2", "ARM")),
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(x$ui, x$server)
#' }
tm_g_regression <- function(
  label = "Regression Analysis",
  dataname,
  response_var,
  regressor_var,
  plot_height = c(600, 200, 2000),
  pre_output = NULL,
  post_output = NULL) {

  # Error prevention
  stopifnot(!is.null(dataname))
  stopifnot(dataname != "")

  args <- as.list(environment())

  teal::module(
    label = label,
    server = srv_g_regression,
    ui = ui_g_regression,
    ui_args = args,
    server_args = list(dataname = dataname),
    filters = dataname
  )
}



#' @import teal
#' @importFrom teal.devel white_small_well plot_height_input plot_height_output
ui_g_regression <- function(id, ...) {
  a <- list(...)

  ns <- NS(id)

  standard_layout(
    output = teal.devel::white_small_well(
      tags$div(
        # This shall be wrapped in a teal::plot
        plot_height_output(id = ns("myplot")),
        tags$div(verbatimTextOutput(ns("text")))
      )
    ),
    encoding = div(
      helpText("Dataset:", tags$code(a$dataname)),
      optionalSelectInput(inputId = ns("response_var"), label = "Response Variable", choices = a$response_var$choices,
                          selected = a$response_var$selected),
      optionalSelectInput(inputId = ns("regressor_var"), label = "Regressor Variables",
                          choices = a$regressor_var$choices, selected = a$regressor_var$selected,
                          multiple = TRUE
      ),
      radioButtons(ns("plot_type"),
                   label = "Plot Type",
                   choices = c(
                     "Residuals vs Fitted",
                     "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
                     "Cook's dist vs Leverage h[ii]/(1 - h[ii]"
                   ),
                   selected = "Residuals vs Fitted"
      ),
      # This shall be wrapped in a teal::plot
      plot_height_input(id = ns("myplot"))
    )
  )
}

#' @importFrom teal.devel as.global
#' @importFrom graphics plot
#' @importFrom methods is
srv_g_regression <- function(input, output, session, datasets, dataname) {
  anl_head <- head(datasets$get_data(dataname, reactive = FALSE, filtered = FALSE))

  fit_cl <- reactive({
    response_var <- input$response_var
    regressor_var <- input$regressor_var

    validate(
      need(length(intersect(response_var, regressor_var)) == 0, "response and regressor variables cannot intersect"),
      need(length(regressor_var) > 0, "please select regressor variable"),
      need(is.numeric(anl_head[[response_var]]), "response variable needs to be numeric")
    )


    call("lm", as.formula(paste0(response_var, "~", paste(regressor_var, collapse = " + "))),
         data = as.name("anl_filtered")
    )
  })

  fit <- reactive({
    anl_filtered <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    fit_cl <- fit_cl()

    validate(
      need(nrow(anl_filtered) >= 10, paste("need at lease 10 observations, currently have only", nrow(anl_filtered)))
    )

    attr(fit_cl[[2]], ".Environment") <- environment()

    fit <- eval(fit_cl)
    fit
  })

  output$plot <- renderPlot({
      fit <- fit()
      plot_type <- input$plot_type
      message(input$plot_type)
      message(is.null(input$plot_type))
      i <- which(plot_type == c(
        "Residuals vs Fitted",
        "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
        "Cook's dist vs Leverage h[ii]/(1 - h[ii]"
      ))
      graphics::plot(fit, which = i, id.n = NULL)
    }
  )


  callModule(plot_with_height, id = "myplot", plot_height = reactive(input$myplot), plot_id = session$ns("plot"))

  output$text <- renderPrint({
    fit <- fit()

    validate(need(methods::is(fit, "lm"), "there seem to problems fitting the model"))

    summary(fit)
  })
}
