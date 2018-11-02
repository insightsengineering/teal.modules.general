
#' Scatterplot and Regression Model
#'
#'
#'
#' @import ggplot2
#'
#' @export
#'
#'
#' @examples
#'
#' ASL <- data.frame(
#'   USUBJID = paste("id", 1:100, sep = "-")
#'
#' )
#'
tm_g_regression <- function(
  label = "Regression Analysis",
  dataname
) {

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_bivariate,
    ui = ui_g_bivariate,
    ui_args = args,
    server_args = list(dataname = dataname),
    filters = dataname
  )

}


ui_g_regression <- function(id, ...) {

  a <- list(...)

  ns <- NS(id)

  standard_layout(
    output = whiteSmallWell(
      tags$p(
        plotOutput(ns("plot")),
        verbatimTextOutput(ns("text"))
      )
    ),
    encoding = div(
      helpText("Dataset:", tags$code(a$dataname))#,
      #optionalSelectInput(ns("x_var"), "x var", a$x_var_choices, a$x_var),
      #optionalSelectInput(ns("y_var"), "y var", a$y_var_choices, a$y_var)
    )
  )
}

srv_g_regression <- function(input, output, session, datasets, dataname) {


  output$plot <- renderPlot({

    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)

    ggplot(ASL_FILTERED) +
      aes(x = BM1, y = BM2) +
      geom_point() + geom_smooth(method= "lm")

  })


  output$text <- renderPrint({

    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)

    txt <- if (nrow(ASL_FILTERED) >= 10) {

      fit <- lm(BM1 ~ BM2, data = ASL_FILTERED)
      summary(fit)

    } else {
      "less than 10 observations"
    }

    txt
  })
}

