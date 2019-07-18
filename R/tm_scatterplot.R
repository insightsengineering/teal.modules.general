#' Create a simple scatterplot
#'
#' Create a plot with the \code{\link{plot}[base]} function
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param dataname name of dataset used to generate plot
#' @param xvar variable name of x varbiable
#' @param yvar variable name of y variable
#' @param xvar_choices vector with variable names of possible x variables. If
#'   missing or identincal to \code{xvar} then the plot will be fixed to the
#'   \code{xvar}.
#' @param yvar_choices vector with variable names of possible y variables. If
#'   missing or identincal to \code{xvar} then the plot will be fixed to the
#'   \code{yvar}.
#' @param color_by variable name of variable that defines the color encoding. If
#'   \code{NULL} then no color encoding option will be displayed. Note
#'   \code{_none_} is a keyword and means that no color encoding should be used.
#' @param color_by_choices vector with variable names that can be used for color
#'   encodings. If missing or identical to \code{color_by} then the color
#'   encoding of the scatterplot points will be fixed to the \code{color_by}
#'   variable.
#' @param plot_height if scalar then the plot will have a fixed height. If a
#'   slider should be presented to adjust the plot height dynamically then it
#'   can be a vector of length three with \code{c(value, min and max)}.
#' @param alpha if scalar then the plot points will have a fixed opacity. If a
#'   slider should be presented to adjust the plot point opacity dynamically
#'   then it can be a vector of length three with vlaue, min and max.
#' @param size if scalar then the plot points sizes will have a fixed opacity.
#'   If a slider should be presented to adjust the plot point sizes dynamically
#'   then it can be a vector of length three with vlaue, min and max.
#'
#' @noRd
#'
#' @examples
#' library(random.cdisc.data)
#'
#' ASL <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     code = "ASL <- cadsl",
#'     check = FALSE),
#'   root_modules(
#'     tm_scatterplot(
#'      "Scatterplot Choices",
#'       dataname = "ASL",
#'       xvar = "BMRKR1", xvar_choices = c("AGE", "BMRKR1", "BMRKR2"),
#'       yvar = "BMRKR2", yvar_choices = c("AGE", "BMRKR1", "BMRKR2"),
#'       color_by = "_none_", color_by_choices = c("_none_", "RACE")
#'     ),
#'     tm_scatterplot(
#'      "Scatterplot No Color Choices",
#'       dataname = "ASL",
#'       xvar = "AGE", yvar = "BMRKR1", size = 3, alpha = 1, plot_height = 600
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_scatterplot <- function(label,
                           dataname,
                           xvar,
                           yvar,
                           xvar_choices = xvar,
                           yvar_choices = yvar,
                           color_by = NULL,
                           color_by_choices = color_by,
                           plot_height = c(600, 200, 2000),
                           alpha = c(1, 0, 1),
                           size = c(4, 1, 12),
                           pre_output = NULL,
                           post_output = NULL) {
  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  stopifnot(is.character.vector(xvar))
  stopifnot(is.character.vector(yvar))
  stopifnot(is.character.vector(xvar_choices))
  stopifnot(is.character.vector(yvar_choices))
  stopifnot(is.null(color_by) || is.character.single(color_by))
  stopifnot(is.null(color_by_choices) || is.character.vector(color_by_choices))
  stopifnot(is.numeric.vector(plot_height) && (length(plot_height) == 3 || length(plot_height) == 1))
  stopifnot(`if`(length(plot_height) == 3, plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3], TRUE))
  stopifnot(is.numeric.vector(alpha) && (length(alpha) == 3 || length(alpha) == 1))
  stopifnot(`if`(length(alpha) == 3, alpha[1] >= alpha[2] && alpha[1] <= alpha[3], TRUE))
  stopifnot(is.numeric.vector(size) && (length(size) == 3 || length(size) == 1))
  stopifnot(`if`(length(size) == 3, size[1] >= size[2] && size[1] <= size[3], TRUE))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_scatterplot,
    server_args = list(dataname = dataname),
    ui = ui_scatterplot,
    ui_args = args,
    filters = dataname
  )
}

ui_scatterplot <- function(id,
                           label,
                           dataname,
                           xvar,
                           yvar,
                           xvar_choices,
                           yvar_choices,
                           color_by,
                           color_by_choices,
                           plot_height,
                           alpha,
                           size,
                           pre_output,
                           post_output) {
  if (plot_height < 200 || plot_height > 2000) {
    stop("plot_height must be between 200 and 2000")
  }


  ns <- NS(id)

  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(dataname)),
      optionalSelectInput(ns("xvar"), "x variable", xvar_choices, xvar, multiple = FALSE),
      optionalSelectInput(ns("yvar"), "y variable", yvar_choices, yvar, multiple = FALSE),
      optionalSelectInput(ns("color_by"), "color by", color_by_choices, color_by, multiple = FALSE),

      if (all(c(
        length(plot_height) == 1,
        length(size) == 1,
        length(alpha) == 1
      ))) {
        NULL
      } else {
        tags$label("Plot Settings", class = "text-primary", style = "margin-top: 15px;")
      },
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", plot_height, ticks = FALSE),
      optionalSliderInputValMinMax(ns("alpha"), "opacity", alpha, ticks = FALSE),
      optionalSliderInputValMinMax(ns("size"), "point size", size, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @importFrom magrittr %>%
#' @importFrom methods substituteDirect
srv_scatterplot <- function(input, output, session, datasets, dataname) {
  stopifnot(all(dataname %in% datasets$datanames()))

  init_chunks()

  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("scatterplot"), height = plot_height)
  })

  output$scatterplot <- renderPlot({
    anl <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)
    xvar <- input$xvar
    yvar <- input$yvar
    alpha <- input$alpha
    color_by <- check_color(input$color_by)
    size <- input$size

    data_name <- paste0(dataname, "_FILTERED")
    assign(data_name, anl)

    validate(need(alpha, "need alpha"))
    validate(need(!is.null(anl) && is.data.frame(anl), "no data left"))
    validate(need(nrow(anl) > 0, "no observations left"))
    validate(need(xvar, "no valid x variable selected"))
    validate(need(yvar, "no valid y variable selected"))
    validate(need(
      xvar %in% names(anl),
      paste("variable", xvar, " is not available in data", dataname)
    ))
    validate(need(
      yvar %in% names(anl),
      paste("variable", yvar, " is not available in data", dataname)
    ))

    chunks_reset()

    if (is.null(color_by)) {
      chunks_push(expression = bquote(
        ggplot(
          .(as.name(data_name)),
          aes_string(x = xvar, y = yvar)
        ) +
          geom_point(alpha = alpha, size = size)
      ) %>% substituteDirect(
        list(
          alpha = alpha,
          size = size,
          xvar = xvar,
          yvar = yvar
        )
      ))
    } else {
      chunks_push(expression = bquote(
        ggplot(
          .(as.name(data_name)),
          aes_string(x = xvar, y = yvar, color = color_by)
        ) +
          geom_point(alpha = alpha, size = size)
      ) %>% substituteDirect(
        list(
          alpha = alpha,
          size = size,
          xvar = xvar,
          yvar = yvar,
          color_by = color_by
        )
      ))
    }

    p <- chunks_eval()

    chunks_validate_is_ok()

    p
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Scatter-Plot",
      rcode = get_rcode(
        datasets = datasets,
        title = "Scatter-Plot"
      )
    )
  })
}

check_color <- function(x) {
  if (!is.null(x)) {
    if (x %in% c("", "_none_")) {
      return(NULL)
    }
  }
  return(x)
}
