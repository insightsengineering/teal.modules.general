#' Univariate and bivariate visualizations.
#'
#' @param label (\code{character}) Label of the module
#' @param x_var variable name selected to plot along the x-axis by default. Variable can be numeric,
#'  factor or character.
#' @param y_var variable name selected to plot along the y-axis by default. Variable can be numeric,
#'  factor or character.
#' @param use_density boolean value for whether density is plotted
#' @param row_facet_var variable for x facets
#' @param col_facet_var variable for y facets
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
#' \dontrun{
#'
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
#'     tm_g_bivariate(
#'       dataname = "ASL",
#'       x_var = choices_selected(names(ASL), "cont"),
#'       y_var = choices_selected(names(ASL), "cont"),
#'       use_density = FALSE,
#'       color_by_var = choices_selected("STUDYID"),
#'       row_facet_var = choices_selected(c("F1", "F2"), NULL),
#'       col_facet_var = choices_selected(c("F1", "F2"), NULL),
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#' }
#' @importFrom teal add_no_selected_choices
tm_g_bivariate <- function(label = "Bivariate Plots",
                           dataname,
                           x_var,
                           y_var,
                           use_density = FALSE,
                           color_by_var,
                           row_facet_var,
                           col_facet_var,
                           free_x_scales = FALSE,
                           free_y_scales = FALSE,
                           plot_height = c(600, 200, 2000),
                           pre_output = NULL,
                           post_output = NULL,
                           with_show_r_code = TRUE,
                           code_data_processing = NULL) {
  stopifnot(is.choices_selected(x_var))
  stopifnot(is.choices_selected(y_var))
  stopifnot(is.choices_selected(row_facet_var))
  stopifnot(is.choices_selected(col_facet_var))
  stopifnot(is.logical(with_show_r_code))

  x_var <- teal::add_no_selected_choices(x_var)
  y_var <- teal::add_no_selected_choices(y_var)

  stopifnot(is.logical(free_x_scales))
  stopifnot(is.logical(free_y_scales))
  stopifnot(is.logical(use_density))

  args <- as.list(environment())

  teal::module(
    label = label,
    server = srv_g_bivariate,
    ui = ui_g_bivariate,
    ui_args = args,
    server_args = list(dataname = dataname, code_data_processing = code_data_processing),
    filters = dataname
  )
}


#' @import teal
#' @importFrom teal.devel white_small_well
ui_g_bivariate <- function(id, ...) {
  a <- list(...)

  ns <- NS(id)

  standard_layout(
    output = teal.devel::white_small_well(uiOutput(ns("plot_ui"))),
    encoding = div(
      helpText("Dataset:", tags$code(a$dataname)),
      optionalSelectInput(ns("x_var"), "x var", a$x_var$choices, a$x_var$selected),
      optionalSelectInput(ns("y_var"), "y var", a$y_var$choices, a$y_var$selected),
      radioButtons(ns("use_density"), NULL,
        choices = c("frequency", "density"),
        selected = ifelse(a$use_density, "density", "frequency"), inline = TRUE
      ),
      optionalSelectInput(ns("row_facet_var"), "Row facetting Variables",
        a$row_facet_var$choices, a$row_facet_var$selected,
        multiple = TRUE
      ),
      optionalSelectInput(ns("col_facet_var"), "Column facetting Variables",
        a$col_facet_var$choices, a$col_facet_var$selected,
        multiple = TRUE
      ),
      checkboxInput(ns("free_x_scales"), "free x scales", value = a$free_x_scales),
      checkboxInput(ns("free_y_scales"), "free y scales", value = a$free_y_scales),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    ),
    forms = if (a$with_show_r_code) actionButton(ns("show_rcode"), "Show R Code", width = "100%") else NULL,
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @importFrom methods is
#' @importFrom teal.devel get_rcode_header
srv_g_bivariate <- function(input,
                            output,
                            session,
                            datasets,
                            dataname,
                            code_data_processing) {


  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("plot"), height = plot_height)
  })

  anl_head <- head(datasets$get_data(dataname, filtered = FALSE, reactive = FALSE))

  anl_name <- paste0(dataname, "_FILTERED")

  plot_call <- reactive({
    x_var <- input$x_var
    y_var <- input$y_var
    use_density <- input$use_density == "density"
    row_facet_var <- input$row_facet_var
    col_facet_var <- input$col_facet_var
    free_x_scales <- input$free_x_scales
    free_y_scales <- input$free_y_scales

    if (x_var == "-- no x --") x_var <- NULL
    if (y_var == "-- no y --") y_var <- NULL

    x <- if (!is.null(x_var)) anl_head[[x_var]] else NULL
    y <- if (!is.null(y_var)) anl_head[[y_var]] else NULL

    validate(need(anl_head, "data missing"))
    validate(need(nrow(anl_head) > 3, "need at least 10 records"))
    validate(need(
      !is.null(x) || !is.null(y),
      "At least one variable needs to be provided"
    ))
    if (!is.null(row_facet_var)) {
      validate(need(all(row_facet_var %in% names(anl_head)), "not all x facet variables are in data"))
    }
    if (!is.null(col_facet_var)) {
      validate(need(all(col_facet_var %in% names(anl_head)), "not all y facet variables are in data"))
    }
    if (!is.null(col_facet_var) && !is.null(col_facet_var)) {
      validate(need(length(intersect(row_facet_var, col_facet_var)) == 0, "x and y facet variables cannot overlap"))
    }


    cl <- g_bp_cl(anl_name, x_var, y_var, class(x), class(y), freq = !use_density)

    facet_cl <- g_facet_cl(row_facet_var, col_facet_var, free_x_scales, free_y_scales)

    if (!is.null(facet_cl)) cl <- call("+", cl, facet_cl)

    cl
  })

  output$plot <- renderPlot({
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)

    plot_call <- plot_call()
    # call global like as.global(plot_call, anl_filtered)

    p <- try(eval(plot_call, list2env(setNames(list(anl_filtered, emptyenv()), c(anl_name, "parent")))))

    if (is(p, "try-error")) {
      validate(need(FALSE, p))
    } else {
      p
    }
  })


  observeEvent(input$show_rcode, {
    header <- teal.devel::get_rcode_header(
      title = "Bivariate and Univariate Plot",
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

    # log like .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current Plot",
      tags$pre(tags$code(class = "R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
}


#' Create facet call
#'
#' @noRd
#'
#' @examples
#'
#' g_facet_cl(LETTERS[1:3])
#' g_facet_cl(NULL, LETTERS[23:26])
#' g_facet_cl(LETTERS[1:3], LETTERS[23:26])
g_facet_cl <- function(row_facet_var = NULL, col_facet_var = NULL, free_x_scales = FALSE, free_y_scales = FALSE) {
  scales <- if (free_x_scales && free_y_scales) {
    "free"
  } else if (free_x_scales) {
    "free_x"
  } else if (free_y_scales) {
    "free_y"
  } else {
    "fixed"
  }

  if (is.null(row_facet_var) && is.null(col_facet_var)) {
    NULL
  } else if (!is.null(row_facet_var) && is.null(col_facet_var)) {
    call("facet_grid", rows = call_fun_dots("vars", row_facet_var), scales = scales)
  } else if (is.null(row_facet_var) && !is.null(col_facet_var)) {
    call("facet_grid", cols = call_fun_dots("vars", col_facet_var), scales = scales)
  } else {
    call("facet_grid",
      rows = call_fun_dots("vars", row_facet_var),
      cols = call_fun_dots("vars", col_facet_var),
      scales = scales
    )
  }
}



#' Bivariate Plot
#'
#'
#' @noRd
#'
#' @examples
#' # need to temporary export in order to test
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
g_bp <- function(x = NULL, y = NULL, freq = TRUE) {
  df <- if (!is.null(x) && !is.null(y)) {
    data.frame(x = x, y = y)
  } else if (is.null(x) && !is.null(y)) {
    data.frame(y = y)
  } else if (!is.null(x) && is.null(y)) {
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
g_bp_cl <- function(data_name, x_var, y_var, x_class, y_class,
                    freq = TRUE,
                    col_var = NULL,
                    x_facet = NULL, y_facet = NULL) {
  cl <- aes_geom_call(x_class, y_class, freq = freq)

  if (is.null(x_var)) x_var <- "-"
  if (is.null(y_var)) y_var <- "-"

  cl_plot <- substitute_q(cl, list(
    .gg = bquote(ggplot(.(as.name(data_name)))),
    .x_var = if (is.call(x_var)) x_var else as.name(x_var),
    .y_var = if (is.call(y_var)) y_var else as.name(y_var)
  ))

  cl_plot
}

substitute_q <- function(x, env) {
  stopifnot(is.language(x))
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
aes_geom_call <- function(x_class = c("NULL", "numeric", "factor", "character", "logical"),
                          y_class = c("NULL", "numeric", "factor", "character", "logical"),
                          freq = TRUE) {
  x_class <- match.arg(x_class)
  y_class <- match.arg(y_class)

  if (x_class %in% c("character", "logical")) x_class <- "factor"
  if (y_class %in% c("character", "logical")) y_class <- "factor"

  all(c(x_class, y_class) == "NULL") && stop("either x or y is required")

  if (x_class == "numeric" && y_class == "NULL") {
    if (freq) {
      quote(.gg + aes(x = .x_var) + geom_histogram() + ylab("Frequency"))
    } else {
      quote(.gg + aes(x = .x_var) + geom_histogram(aes(y = ..density..)) + ylab("Density")) # nolint
    }
  } else if (x_class == "NULL" && y_class == "numeric") {
    if (freq) {
      quote(.gg + aes(x = .y_var) + geom_histogram() + ylab("Frequency") + coord_flip())
    } else {
      quote(.gg + aes(x = .y_var) + geom_histogram(aes(y = ..density..)) + ylab("Density") + coord_flip()) # nolint
    }
  } else if (x_class == "factor" && y_class == "NULL") {
    if (freq) {
      quote(.gg + aes(x = .x_var) + geom_bar() + ylab("Frequency"))
    } else {
      quote(.gg + aes(x = .x_var) + geom_bar(aes(y = ..prop.., group = 1)) + ylab("Proportion")) # nolint
    }
  } else if (x_class == "NULL" && y_class == "factor") {
    if (freq) {
      quote(.gg + aes(x = .y_var) + geom_bar() + ylab("Frequency") + coord_flip()) # nolint
    } else {
      quote(.gg + aes(x = .y_var) + geom_bar(aes(y = ..prop.., group = 1)) + # nolint
        ylab("Proportion") + coord_flip())
    }
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
  if (is.null(x)) name else x
}
