

#' Cross table based on rtables
#'
#' @param x_var variable name selected variable when shiny app starts
#' @param x_var_choices vector with variable names available as choices
#' @param y_var variable name selected variable when shiny app starts
#' @param y_var_choices vector with variable names available as choices
#'
#' @import ggplot2
#' @import tern
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' N <- 100
#' ASL <- data.frame(
#'   USUBJID = paste("id", seq_len(N), sep ="-"),
#'   STUDYID = "study1",
#'   cont = rnorm(N),
#'   disc = factor(sample(letters[1:5], N, TRUE)),
#'   cont2 = runif(N),
#'   disc2 = factor(sample(LETTERS[1:5], N, TRUE))
#' )
#'
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   modules = root_modules(
#'     tm_g_bivariate(
#'       dataname = "ASL",
#'       x_var = "cont",
#'       x_var_choices = names(ASL),
#'       y_var = "cont2",
#'       y_var_choices = names(ASL)
#'     )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#' }
#'
tm_g_bivariate <- function(
  label = "Bivariate Plots",
  dataname,
  x_var,
  x_var_choices = x_var,
  y_var,
  y_var_choices = y_var
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


ui_g_bivariate <- function(id, ...) {

  a <- list(...)

  ns <- NS(id)

  standard_layout(
    output = whiteSmallWell(plotOutput(ns("plot"))),
    encoding = div(
      helpText("Dataset:", tags$code(a$dataname)),
      optionalSelectInput(ns("x_var"), "x var", a$x_var_choices, a$x_var),
      optionalSelectInput(ns("y_var"), "y var", a$y_var_choices, a$y_var)
    )
  )
}

srv_g_bivariate <- function(input, output, session, datasets, dataname) {


  output$plot <- renderPlot({

    ANL_filtered <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    x_var <- input$x_var
    y_var <- input$y_var


    as.global(ANL_filtered, x_var, y_var)

    validate(need(ANL_filtered, "data missing"))
    validate(need(nrow(ANL_filtered) > 10, "need at least 10 patients"))

    x <- if (!is.null(x_var)) {
      validate(need(x_var %in% names(ANL_filtered), "selected x_var does not exist"))
      x <- ANL_filtered[[x_var]]
      validate(need(is.numeric(x) || is.factor(x), "currently only works if x is of type factor or numeric"))
      x
    } else {
      NULL
    }
    y <- if (!is.null(y_var)) {
      validate(need(y_var %in% names(ANL_filtered), "selected y_var does not exist"))
      y <- ANL_filtered[[y_var]]
      validate(need(is.numeric(y) || is.factor(y), "currently only works if y is of type factor or numeric"))
      y
    } else {
      NULL
    }


    lab <- if(!is.null(label(var))) {
      label(var)
    } else {
      if (is.null(x)) y_var else x_var
    }
    plot
    var <- factor(sample(letters[1:4], 100, TRUE))



    p <- if (is.null(x) && is.null(y)) {

      validate(need(FALSE, "need to select a variable"))

    } else if (xor(is.null(x), is.null(y))) { # 1d
      #
      var <- if (is.null(x)) y else x

      if (is.factor(var)) {
        ggplot() + aes(x = var) + geom_bar()
      } else if (is.numeric(var)) {
        ggplot() + aes(x = var) + geom_histogram() + geom_density()
      } else {
        validate(need(FALSE, "module inconsistency"))
      }

    } else { # bivariate

      if (is.numeric(x) && is.numeric(y)) {

        ggplot() + aes(x = x, y = y) + geom_point()

      } else if (is.numeric(x) && is.factor(y) || is.numeric(y) && is.factor(x)) {

        ggplot() + aes(x = x, y = y) + geom_boxplot()

      } else if (is.factor(y) && is.factor(y)) {
        validate(need(FALSE, "need to implement mosaic plot"))
        #ggplot() + aes(x = x, y = y) + geom_mosaic()
      }

    }
    p


  })

}
