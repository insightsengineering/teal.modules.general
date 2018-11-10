#' Univariate and bivariate visualizations.
#'
#' @param x_var variable name selected to plot along the x-axis by default. Variable can be numeric, factor or character.
#' @param x_var_choices character vector with variable names available as choices to plot along x-axis
#' @param y_var variable name selected to plot along the y-axis by default. Variable can be numeric, factor or character.
#' @param use_density boolean value for whether density is plotted
#' @param y_var_choices character vector with variable names available as choices to plot along y-axis
#' @param x_facet_var variable for x facets
#' @param x_facet_var_choices vector with \code{x_facet_var} choices
#' @param y_facet_var variable for y facets
#' @param y_facet_var_choices vector with \code{y_facet_var} choices
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
#' # 1) Review bivariate plots -- output is not correct when 2 discrete vars are selected, also need to add density
#' # 2) add line_colorby_var and line_colorby_var_choices
#' # 3) Add x- y- faceting for 1D and 2D plots
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
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   modules = root_modules(
#'     tm_g_bivariate(
#'       dataname = "ASL",
#'       x_var = "cont",
#'       x_var_choices = names(ASL),
#'       y_var = "cont2",
#'       y_var_choices = names(ASL),
#'       use_density = FALSE,
#'       x_facet_var = "SEX",
#'       x_facet_var_choices = c("SEX", "ARM"),
#'       y_facet_var = "ARM",
#'       y_facet_var_choices = c("ARM", "SEX"),
#'       plot_height = c(600, 200, 2000)
#'     )
#'   ))
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
                           color_by_var,
                           color_by_var_choices,
                           x_facet_var ,
                           x_facet_var_choices,
                           y_facet_var,
                           y_facet_var_choices,
                           plot_height = c(600, 200, 2000) ) {


  !("-- no x --" %in% x_var_choices) || stop("'-- no x --' is a keyword and cannot be in x_var_choices")
  !("-- no y --" %in% y_var_choices) || stop("'-- no y --' is a keyword and cannot be in y_var_choices")
  is.logical(use_density) || stop("use_density needs to be boolean")

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
      checkboxInput(ns("use_density"), "Show Density", value = a$use_density),
      optionalSelectInput(ns("x_facet_var"), "X-facet By Variable", a$x_facet_var_choices, a$x_facet_var, multiple = TRUE),
      optionalSelectInput(ns("y_facet_var"), "Y-facet By Variable", a$y_facet_var_choices, a$y_facet_var, multiple = TRUE),
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

    if (x_var == "-- no x --")  x_var <- NULL
    if (y_var == "-- no y --")  y_var <- NULL

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



#' Bivariate Plot
#'
#' @export
#'
#' @examples
#'
#' c1 <- rnorm(100)
#' c2 <- rnorm(100)
#' d1 <- factor(sample(letters[1:5], 100, TRUE), levels = letters[1:5])
#' d2 <- factor(sample(LETTERS[24:26], 100, TRUE), levels = LETTERS[24:26])
#'
#' g_bp(x = c1)
#' g_bp(x = c1, freq = FALSE)
#' g_bp(y = c1)
#' g_bp(y = c1, freq = FALSE)
#' g_bp(x = d1)
#' g_bp(x = d1, freq = FALSE)
#' g_bp(y = d1)
#' g_bp(y = d1, freq = FALSE)
#'
#'
#' g_bp(x = c1, y = c2)
#' g_bp(x = c1, y = d1)
#' g_bp(x = d1, y = c1)
#' g_bp(x = d1, y = d2) ## no color
#'
#'
#'
#'
#' x <- d1
#' library(scales)
#' ggplot() + aes(x = x) + geom_bar(aes(y = ..prop.., group = 1))
#'
#'
#'
#'
g_bp <- function(x=NULL, y=NULL, freq = TRUE) {

  df <- if (!is.null(x) && !is.null(y)) {
    data.frame(x = x, y = y)
  } else if(is.null(x) && !is.null(y)) {
    data.frame(y = y)
  } else if(!is.null(x) && is.null(y)) {
    data.frame(x = x)
  } else {
    stop("not both variables can be null")
  }

  eval(g_bp_cl("df", "x", "y", class(x), class(y), freq = freq))
}


#' Get Substituted ggplot call
#'
#' @noRd
#'
#' @examples
#'
#' g_bp_cl("ANL", "BAGE", "RACE", "numeric", "factor")
#' g_bp_cl("ANL", "BAGE", NULL, "numeric", "NULL")
#'
#'
g_bp_cl <- function(data_name, x_var, y_var, x_class, y_class,
                 freq = TRUE,
                 col_var = NULL,
                 x_facet = NULL, y_facet = NULL) {

  cl <- aes_geom_call(x_class, y_class, freq = freq)

  if (is.null(x_var)) x_var <- "-"
  if (is.null(y_var)) y_var <- "-"

  cl_plot <- substitute_q(cl, list(
    .gg = bquote(ggplot(.(as.name(data_name)))),
    .x_var = as.name(x_var),
    .y_var = as.name(y_var)
  ))

  cl_plot
}

subsitute_q <- function (x, env) {
  stopifnot(is.language(x))
  env <- to_env(env)
  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}

#' Create ggplot part of call
#'
#' @noRd
#'
#' @examples
#' aes_geom_call("numeric", "NULL")
#' aes_geom_call("numeric", "NULL", freq = FALSE)
#'
#' aes_geom_call("NULL", "numeric")
#' aes_geom_call("NULL", "numeric", freq = FALSE)
#'
#' aes_geom_call("NULL", "factor")
#' aes_geom_call("NULL", "factor", freq = FALSE)
#'
#' aes_geom_call("factor", "NULL")
#' aes_geom_call("factor", "NULL", freq = FALSE)
#'
#' aes_geom_call("numeric", "numeric")
#' aes_geom_call("numeric", "factor")
#' aes_geom_call("factor", "numeric")
#' aes_geom_call("factor", "factor")
#'
aes_geom_call <- function(x_class = c("NULL", "numeric", "factor"),
                          y_class  = c("NULL", "numeric", "factor"),
                          freq = TRUE) {

  x_class <- match.arg(x_class)
  y_class <- match.arg(y_class)

  all(c(x_class, y_class) == "NULL") && stop("either x or y is required")

  if (x_class == "numeric" && y_class == "NULL") {

    if (freq)
      quote(.gg + aes(x = .x_var) + geom_histogram() + ylab("Frequency"))
    else
      quote(.gg + aes(x = .x_var) + geom_histogram(aes(y=..density..)) + ylab("Density"))

  } else if (x_class == "NULL" && y_class == "numeric") {

    if (freq)
      quote(.gg + aes(x = .y_var) + geom_histogram() + ylab("Frequency") + coord_flip())
    else
      quote(.gg + aes(x = .y_var) + geom_histogram(aes(y=..density..)) + ylab("Density") + coord_flip())

  } else if (x_class == "factor" && y_class == "NULL") {

    if (freq)
      quote(.gg + aes(x = .x_var) + geom_bar() + ylab("Frequency"))
    else
      quote(.gg + aes(x = .x_var) + geom_bar(aes(y = ..prop.., group = 1)) + ylab("Proportion"))

  } else if (x_class == "NULL" && y_class == "factor") {

    if (freq)
      quote(.gg + aes(x = .y_var) + geom_bar() + ylab("Frequency") + coord_flip())
    else
      quote(.gg + aes(x = .y_var) + geom_bar(aes(y = ..prop.., group = 1)) + ylab("Proportion") + coord_flip())

  } else if (x_class == "numeric" && y_class == "numeric") {

    quote(.gg + aes(x = .x_var, y = .y_var) + geom_point())

  } else if (x_class == "numeric" && y_class == "factor") {

    quote(.gg + aes(x = .y_var, y = .x_var) + geom_boxplot() + coord_flip())

  } else if (x_class == "factor" && y_class == "numeric") {

    quote(.gg + aes(x = .x_var, y = .y_var) + geom_boxplot())

  } else if (x_class == "factor" && y_class == "factor") {

    quote(.gg + geom_mosaic(aes(x = product(.x_var), fill = .y_var), na.rm = TRUE))

  } else {
    stop("x y type combination not allowed")
  }
}

label_over_name <- function(x, name) {
  if (is.null(label)) name else label
}

