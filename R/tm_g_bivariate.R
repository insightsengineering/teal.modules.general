#' Univariate and bivariate visualizations.
#'
#' @param x_var variable name selected to plot along the x-axis by default
#' @param x_var_choices character vector with variable names available as choices to plot along x-axis
#' @param y_var variable name selected to plot along the y-axis by default
#' @param y_var_choices character vector with variable names available as choices to plot along y-axis
#'
#' @import ggplot2
#' @import ggmosaic
#'
#'
#' @details
#' This is a general module to visualize 1 & 2 dimensional data.
#'
#'
#'  1d
#'
#'  cont  abs  hist
#'  cat.  abs  barplot
#'
#'
#'  2d
#'  x         y     interest    plot
#'  cont.   cont.   abs       scatterplot
#'  cont.   cat.    abs       boxplot
#'  cat.    cat.    abs       mosaic plot
#'  cont.   cont.   density
#'  cont.   cat.    density
#'  cat.    cat.    density
#'
#'
#'  color_by
#'  facet_by
#'
#'
#'
#' @export
#'
#'
#' @examples
#'
#' \dontrun{
#'
#' N <- 100
#' ASL <- data.frame(
#'   USUBJID = paste("id", seq_len(N), sep = "-"),
#'   STUDYID = "study1",
#'   cont = rnorm(N),
#'   disc = factor(sample(letters[1:5], N, TRUE)),
#'   cont2 = runif(N),
#'   disc2 = factor(sample(LETTERS[1:5], N, TRUE))
#' )
#'
#' x <- teal::init(data = list(ASL = ASL),
#'                 modules = root_modules(
#'                   tm_g_bivariate(
#'                     dataname = "ASL",
#'                     x_var = "cont",
#'                     x_var_choices = c("", names(ASL)),
#'                     y_var = "cont2",
#'                     y_var_choices = c("", names(ASL))
#'                   )
#'                 ))
#'
#' shinyApp(x$ui, x$server)
#'
#' }
#'
tm_g_bivariate <- function(label = "Uni- and Bivariate Plots",
                           dataname,
                           x_var,
                           x_var_choices = x_var,
                           y_var,
                           y_var_choices = y_var) {

  args <- as.list(environment())

  module(label = label,
         server = srv_g_bivariate,
         ui = ui_g_bivariate,
         ui_args = args,
         server_args = list(dataname = dataname),
         filters = dataname)

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

srv_g_bivariate <- function(input,
                            output,
                            session,
                            datasets,
                            dataname) {

  output$plot <- renderPlot({

    ANL_filtered <- datasets$get_data(dataname,
                                      filtered = TRUE,
                                      reactive = TRUE)

    x_var <- input$x_var
    y_var <- input$y_var

    if (x_var == '')
      x_var <- NULL
    if (y_var == '')
      y_var <- NULL

    validate(need(ANL_filtered, "data missing"))
    validate(need(nrow(ANL_filtered) > 10, "need at least 10 patients"))
    validate(need(!is.null(x_var) || !is.null(y_var),
                  "At least one variable needs to be provided"))

    x <- if (!is.null(x_var)) {
      #validate(need(x_var %in% names(ANL_filtered), "selected x_var does not exist"))
      x <- ANL_filtered[[x_var]]
      validate(need(is.numeric(x) || is.factor(x),
                    "currently only works if x is of type factor or numeric"))
      x
    } else {
      NULL
    }

    y <- if (!is.null(y_var)) {
      #validate(need(y_var %in% names(ANL_filtered), "selected y_var does not exist"))
      y <- ANL_filtered[[y_var]]
      validate(need(is.numeric(y) || is.factor(y),
                    "currently only works if y is of type factor or numeric"))
      y
    } else {
      NULL
    }

    lab <- if (!is.null(label(var))) {
      label(var)
    } else {
      if (is.null(x)) y_var else x_var
    }

    p <- if (is.null(x) && is.null(y)) {
      validate(need(FALSE, "need to select a variable"))
    } else if (xor(is.null(x), is.null(y))) {
      # 1d (univariate) case.
      var <- if (is.null(x)) y else x
      if (is.factor(var)) {
        # Visualization of conditional distribution of an univariate
        # categorical variable via stacked barplot.
        # Here only one variable is available.
        ggplot() + aes(x = var) + geom_bar()
      } else if (is.numeric(var)) {
        # Visualization of distribution of an univariate continuous
        # variable via histogram and/or density plot.
        ggplot() + aes(x = var) + geom_histogram() + geom_density()
      } else {
        validate(need(FALSE, "module inconsistency"))
      }
    } else {
      # 2d (bivariate) case.
      if (is.numeric(x) && is.numeric(y)) {
        # Visualization of bivariate continuous variables via scatterplot.
        ggplot() + aes(x = x, y = y) + geom_point()
      } else if (is.numeric(x) && is.factor(y) ||
                 is.factor(x) && is.numeric(y)) {
        # Visualization of bivariate categorical and continuous
        # variables via boxplot.
        ggplot() + aes(x = x, y = y) + geom_boxplot()
      } else if (is.factor(x) && is.factor(y)) {
        # Visualization of bivariate categorical variables via mosaic plot.
        ggplot(data = data.frame(x, y)) +
          geom_mosaic(aes(x = product(x), fill = y), na.rm = TRUE)
      }
    }
    p
  })
}
