#' Univariate and bivariate visualizations.
#'
#' @param x_var variable name selected to plot along the x-axis by default. Variable can be numeric, factor or character.
#' @param x_var_choices character vector with variable names available as choices to plot along x-axis
#' @param y_var variable name selected to plot along the y-axis by default. Variable can be numeric, factor or character.
#' @param use_density boolean value for whether density is plotted
#' @param y_var_choices character vector with variable names available as choices to plot along y-axis
#' @param xfacet_var variable for x facets
#' @param xfacet_var_choices vector with \code{xfacet_var} choices
#' @param yfacet_var variable for y facets
#' @param yfacet_var_choices vector with \code{yfacet_var} choices
#' @param plot_height range of plot height
#'
#'
#' @import ggplot2
#' @import ggmosaic
#'
#'
#' @details
#' This is a general module to visualize 1 & 2 dimensional data.
#'
#' @export
#'
#'
#' @examples
#'
#' ## TODO:
#'  1) Review bivariate plots -- output is not correct when 2 discrete vars are selected, also need to add density
#'  2) add line_colorby_var and line_colorby_var_choices
#'  3) Add x- y- faceting for 1D and 2D plots
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
#'                     x_var_choices = names(ASL),
#'                     y_var = "cont2",
#'                     y_var_choices = names(ASL),
#'                     use_density = FALSE,
#'                     xfacet_var = "SEX",
#'                     xfacet_var_choices = c("SEX", "ARM"),
#'                     yfacet_var = "ARM",
#'                     yfacet_var_choices = c("ARM", "SEX"),
#'                     plot_height = c(600, 200, 2000)
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
                           y_var_choices = y_var,
                           use_density = FALSE,
                           xfacet_var,
                           xfacet_var_choices,
                           yfacet_var,
                           yfacet_var_choices,
                           plot_height = c(600, 200, 2000) ) {

  x_var_choices <- unique(c("-- no x --", x_var_choices))
  y_var_choices <- unique(c("-- no y --", y_var_choices))

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
    output = whiteSmallWell(uiOutput(ns("plot_ui"))),
    encoding = div(
      helpText("Dataset:", tags$code(a$dataname)),
      optionalSelectInput(ns("x_var"), "x var", a$x_var_choices, a$x_var),
      optionalSelectInput(ns("y_var"), "y var", a$y_var_choices, a$y_var),
      checkboxInput(ns("use_density"), "Use Density", value = a$use_density),
      #optionalSelectInput(ns("line_colorby_var"), "Color By Variable", a$line_colorby_var_choices, a$line_colorby_var, multiple = FALSE),
      optionalSelectInput(ns("xfacet_var"), "X-facet By Variable", a$xfacet_var_choices, a$xfacet_var, multiple = TRUE),
      optionalSelectInput(ns("yfacet_var"), "Y-facet By Variable", a$yfacet_var_choices, a$yfacet_var, multiple = TRUE),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    )
  )
}

srv_g_bivariate <- function(input,
                            output,
                            session,
                            datasets,
                            dataname) {

  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("plot"), height=plot_height)
  })

  output$plot <- renderPlot({

    ANL_filtered <- datasets$get_data(dataname,
                                      filtered = TRUE,
                                      reactive = TRUE)

    x_var <- input$x_var
    y_var <- input$y_var
    use_density <- input$use_density

    if (x_var == "-- no x --")
      x_var <- NULL
    if (y_var == "-- no y --")
      y_var <- NULL

    validate(need(ANL_filtered, "data missing"))
    validate(need(nrow(ANL_filtered) > 10, "need at least 10 records"))
    validate(need(!is.null(x_var) || !is.null(y_var),
                  "At least one variable needs to be provided"))

    x <- if (!is.null(x_var)) {
      validate(need(x_var %in% names(ANL_filtered), "selected x_var does not exist"))
      x <- ANL_filtered[[x_var]]

      if (is.character(x)) x <- as.factor(x)
      validate(need(is.numeric(x) || is.factor(x),
                    "currently only works if x is of type factor, numeric or character"))
      x
    } else {
      NULL
    }

    y <- if (!is.null(y_var)) {

      validate(need(y_var %in% names(ANL_filtered), "selected y_var does not exist"))
      y <- ANL_filtered[[y_var]]

      if (is.character(y)) y <- as.factor(y)
      validate(need(is.numeric(y) || is.factor(y),
                    "currently only works if y is of type factor, numeric or character"))
      y
    } else {
      NULL
    }

    x_lab <- if (!is.null(attributes(x)[["label"]])) {
      attributes(x)[["label"]]
    } else {
      x_var
    }

    y_lab <- if (!is.null(attributes(y)[["label"]])) {
      attributes(y)[["label"]]
    } else {
      y_var
    }

    ### Create Plot ###
    p <- if (xor(is.null(x), is.null(y))) {

      # 1d (univariate) case.
      var <- if (is.null(x)) y else x
      lab <- if (is.null(x)) y_lab else x_lab

      if (is.factor(var)) {

        if(use_density){
          ggplot() + aes(x = var) + geom_bar(aes(y = (..count..)/sum(..count..))) + xlab(lab) + ylab("Density")
        } else {
          ggplot() + aes(x = var) + geom_bar() + xlab(lab) + ylab("Frequency")
        }

      } else if (is.numeric(var)) {

        if(use_density){
          ggplot() + aes(x = var) + geom_histogram(aes(y=..density..)) + xlab(lab) + ylab("Density")
        } else {
          ggplot() + aes(x = var) + geom_histogram() + xlab(lab) + ylab("Frequency")
        }
      }

      #### TO DO#####
      ## Review code below

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
