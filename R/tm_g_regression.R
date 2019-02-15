
#' Scatterplot and Regression Model
#'
#' This module is designed for horizontal data
#'
#' @import ggplot2
#'
#' @export
#'
#'
#' @examples
#'
#' \dontrun{
#' N <- 100
#' ASL <- data.frame(
#'   USUBJID = paste("id", seq_len(N), sep = "-"),
#'   STUDYID = "study1",
#'   F1 = factor(sample(paste0("facet_1_", c("A", "B")), N, TRUE)),
#'   F2 = factor(sample(paste0("facet_2_", c("a", "b", "c")), N, TRUE)),
#'   cont = rnorm(N),
#'   disc = factor(sample(letters[1:5], N, TRUE)),
#'   cont2 = runif(N),
#'   disc2 = factor(sample(LETTERS[1:5], N, TRUE))
#' )
#'
#' attr(ASL, "source") <- "# ASL is random data"
#'
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   modules = root_modules(
#'     tm_g_regression(
#'       dataname = "ASL",
#'       response_var = choices_selected(c("cont", "cont2"), "cont"),
#'       regressor_var = choices_selected(names(ASL), "cont2"),
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#'
#'
#' fit <- lm(cont ~ cont2, data = ASL)
#'
#'
#' plot(fit$model[, 2:1])
#' abline(fit)
#'
#'
#' plot(fit)
#' }
#'
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
ui_g_regression <- function(id, ...) {
  a <- list(...)

  ns <- NS(id)

  standard_layout(
    output = white_small_well(
      tags$div(
        tags$div(white_small_well(uiOutput(ns("plot_ui")))),
        tags$div(verbatimTextOutput(ns("text")))
      )
    ),
    encoding = div(
      helpText("Dataset:", tags$code(a$dataname)),
      optionalSelectInput(ns("response_var"), "Response Variable", a$response_var$choices, a$response_var$selected),
      optionalSelectInput(ns("regressor_var"), "Regressor Variables",
        a$regressor_var$choices, a$regressor_var$selected,
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
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    )
  )
}

srv_g_regression <- function(input, output, session, datasets, dataname) {
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
      need(nrow(anl_filtered) >= 10, paste("need at lease 10 observations, currenty have only", nrow(anl_filtered)))
    )

    attr(fit_cl[[2]], ".Environment") <- environment()

    as.global(fit_cl)
    fit <- eval(fit_cl)
    fit
  })

  output$plot <- renderPlot({
    fit <- fit()
    plot_type <- input$plot_type

    i <- which(plot_type == c(
      "Residuals vs Fitted",
      "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
      "Cook's dist vs Leverage h[ii]/(1 - h[ii]"
    ))

    plot(fit, which = i, id.n = NULL)
  })


  output$text <- renderPrint({
    fit <- fit()

    validate(need(is(fit, "lm"), "there seem to problems fitting the model"))

    summary(fit)
  })
}
