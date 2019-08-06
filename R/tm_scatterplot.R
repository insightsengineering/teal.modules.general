#' Create a simple scatterplot
#'
#' Create a plot with the \code{\link{plot}[base]} function
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param dataname name of dataset used to generate plot
#' @param x (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   x variable
#' @param y (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   y variable
#' @param color_by (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Defines the color encoding. If
#'   \code{NULL} then no color encoding option will be displayed. Note
#'   \code{_none_} is a keyword and means that no color encoding should be used.
#' @param plot_height if scalar then the plot will have a fixed height. If a
#'   slider should be presented to adjust the plot height dynamically then it
#'   can be a vector of length three with \code{c(value, min and max)}.
#' @param alpha if scalar then the plot points will have a fixed opacity. If a
#'   slider should be presented to adjust the plot point opacity dynamically
#'   then it can be a vector of length three with vlaue, min and max.
#' @param size if scalar then the plot points sizes will have a fixed opacity.
#'   If a slider should be presented to adjust the plot point sizes dynamically
#'   then it can be a vector of length three with vlaue, min and max.
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#'
#' @export
#' @examples
#'
#' # datasets: single wide
#'
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- cadsl",
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_scatterplot(
#'       label = "Scatterplot Choices",
#'       dataname = "ADSL",
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         columns = columns_spec(
#'           label = "Select variable",
#'           choices = c("AGE", "BMRKR1", "BMRKR2"),
#'           selected = "AGE",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADSL",
#'         columns = columns_spec(
#'           label = "Select variable",
#'           choices = c("AGE", "BMRKR1", "BMRKR2"),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       color_by = data_extract_spec(
#'         dataname = "ADSL",
#'         columns = columns_spec(
#'           label = "Select variables",
#'           choices = c("RACE", "SEX"),
#'           selected = NULL,
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#'   shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: different wide
#'
#' library(random.cdisc.data)
#' library(tern)
#' library(dplyr)
#'
#' ADSL <- cadsl
#' ADSL <- mutate_at(ADSL,
#'   .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'   .funs = list(~as.factor(.))
#' ) %>% select(
#'   "ARM", "ACTARM", "ACTARMCD",
#'   "SEX", "BMRKR1", "BMRKR2", "AGE", "USUBJID", "STUDYID"
#' )
#' keys(ADSL) <- c("STUDYID", "USUBJID")
#'
#'
#' ADSL_2 <- mutate_at(cadsl,
#'   .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'   .funs = list(~as.factor(.))
#' ) %>% select("ACTARM", "AGE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
#' keys(ADSL_2) <- c("STUDYID", "USUBJID")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     dataset("ADSL_2", ADSL_2),
#'     code = 'ADSL <- cadsl
#' ADSL_2 <- mutate_at(cadsl,
#' .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#' .funs = list(~as.factor(.))) %>% select("ACTARM", "AGE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
#' keys(ADSL) <- keys(ADSL_2) <- c("STUDYID", "USUBJID")',
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_scatterplot("Scatterplot for different wide data",
#'      dataname = c("ADSL", "ADSL_2"),
#'      x = data_extract_spec(
#'         dataname = "ADSL",
#'         columns = columns_spec(
#'          label = "Select variable",
#'           choices = c("BMRKR1", "BMRKR2"),
#'           selected = c("BMRKR1"),
#'           multiple = FALSE
#'         )),
#'       y = data_extract_spec(
#'         dataname = "ADSL_2",
#'         columns = columns_spec(
#'           label = "Select variables",
#'           choices = c("AGE", "RACE"),
#'           selected = "AGE",
#'           multiple = FALSE
#'         )),
#'       color_by = data_extract_spec(
#'         dataname = "ADSL_2",
#'         columns = columns_spec(
#'           label = "Select variables",
#'           choices = c("COUNTRY", "AGE", "RACE"),
#'           selected = "COUNTRY",
#'           multiple = FALSE
#'         ))
#'      )
#'    )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'

#' # datasets: multiple long datasets
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#' ADTTE <- cadtte
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     cdisc_dataset("ADTTE", ADTTE),
#'     code = "ADSL <- cadsl; ADRS <- cadrs; ADTTE <- cadtte",
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_scatterplot(
#'       label = "Scatterplot on two long datasets",
#'       dataname = c("ADSL", "ADRS", "ADTTE"),
#'       x = data_extract_spec(
#'         dataname = "ADRS",
#'         columns = columns_spec(
#'           choices = names(ADRS),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         ),
#'         filter = filter_spec(
#'           vars = c("PARAMCD", "AVISIT"),
#'           choices = apply(expand.grid(
#'           levels(ADRS$PARAMCD), levels(ADRS$AVISIT)), 1, paste, collapse = " - "),
#'           selected = "OVRINV - Screening",
#'           multiple = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADTTE",
#'         columns = columns_spec(
#'           choices = names(ADTTE),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         ),
#'         filter = filter_spec(
#'           vars = c("PARAMCD"),
#'           choices = unique(ADTTE$PARAMCD),
#'           selected = "OS",
#'           multiple = TRUE
#'         )
#'       ),
#'       color_by = data_extract_spec(
#'         dataname = "ADSL",
#'         columns = columns_spec(
#'           choices = c("AGE", "SEX"),
#'           selected = "AGE",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: wide, long
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#' keys(ADSL) <- c("STUDYID", "USUBJID")
#' keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     code = 'ADSL <- cadsl
#' ADRS <- cadrs
#' keys(ADSL) <- c("STUDYID", "USUBJID")
#' keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'    tm_scatterplot("Scatterplot for wide and long data",
#'      dataname = c("ADSL", "ADRS"),
#'      x = data_extract_spec(
#'           dataname = "ADSL",
#'           columns = columns_spec(
#'             choices = c("SEX", "AGE", "RACE", "COUNTRY"),
#'             selected = "AGE",
#'             multiple = FALSE,
#'             fixed = FALSE
#'           )
#'         ),
#'      y = data_extract_spec(
#'           dataname = "ADRS",
#'           filter = list(
#'             filter_spec(
#'               vars = "PARAM",
#'               choices = levels(ADRS$PARAM),
#'               selected = levels(ADRS$PARAM)[1],
#'               multiple = FALSE,
#'               label = "Choose response"
#'             ),
#'             filter_spec(
#'               vars = "AVISIT",
#'               choices = levels(ADRS$AVISIT),
#'               selected = levels(ADRS$AVISIT)[1],
#'               multiple = FALSE,
#'               label = "Choose visit"
#'             )
#'           ),
#'           columns = columns_spec(
#'             choices = "AVAL",
#'             selected = "AVAL",
#'             multiple = FALSE,
#'             label = "variable"
#'           )
#'         ),
#'      color_by = data_extract_spec(
#'           dataname = "ADSL",
#'           columns = columns_spec(
#'             choices = c("SEX", "AGE", "RACE", "COUNTRY"),
#'             selected = NULL,
#'             multiple = FALSE,
#'             fixed = FALSE
#'           )
#'         )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: same long
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#' keys(ADSL) <- c("STUDYID", "USUBJID")
#' keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     code = "", check = FALSE),
#'   modules = root_modules(
#'     tm_scatterplot(
#'       "Scatterplot for same long dataset",
#'       dataname = "ADRS",
#'       x = data_extract_spec(
#'         dataname = "ADRS",
#'         columns = columns_spec(
#'           choices = c("AVAL", "BMRKR1", "BMRKR2"),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADRS",
#'         columns = columns_spec(
#'           choices = c("AVAL", "BMRKR1", "BMRKR2"),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       color_by = data_extract_spec(
#'         dataname = "ADRS",
#'         columns = columns_spec(
#'           choices = c("AGE", "SEX", "RACE"),
#'           selected = "SEX", # todo: how to select nothing
#'           multiple = FALSE, #todo: 0 or 1
#'           fixed = FALSE,
#'           label = "Color"
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#'   shinyApp(app$ui, app$server)
#' }
#'
#'
#'
#' # datasets: different subsets of long dataset
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ADSL <- cadsl
#' ADLB <- cadlb
#'
#' keys(ADSL) <- c("STUDYID", "USUBJID")
#' keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = 'ADSL <- cadsl
#'            ADLB <- cadlb
#'            keys(ADSL) <- c("STUDYID", "USUBJID")
#'            keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_scatterplot(
#'       "Scatterplot Choices",
#'       dataname = "ADLB",
#'       x = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Choose measurement"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Choose visit"
#'           )
#'         ),
#'         columns = columns_spec(
#'           choices = c("AVAL", "CHG", "BMRKR1", "BMRKR2", "AGE", "AVISITN"),
#'           selected = "AVAL",
#'           multiple = TRUE,
#'           fixed = FALSE,
#'           label = "Variable"
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[2],
#'             multiple = FALSE,
#'             label = "Choose measurement"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[2],
#'             multiple = FALSE,
#'             label = "Choose visit"
#'           )
#'         ),
#'         columns = columns_spec(
#'           choices = c("AVAL", "CHG", "BMRKR1", "BMRKR2", "AGE", "AVISITN"),
#'           selected = "AVAL",
#'           multiple = TRUE,
#'           fixed = FALSE,
#'           label = "Variable"
#'         )
#'       ),
#'       color_by = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Choose measurement"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Choose visit"
#'           )
#'         ),
#'         columns = columns_spec(
#'           choices = c("RACE", "SEX"),
#'           selected = "SEX",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Variable"
#'         )
#'       ),
#'     )
#'   )
#' )
#' \dontrun{
#'   shinyApp(app$ui, app$server)
#' }
tm_scatterplot <- function(label,
                           dataname,
                           x,
                           y,
                           color_by = NULL,
                           plot_height = c(600, 200, 2000),
                           alpha = c(1, 0, 1),
                           size = c(4, 1, 12),
                           pre_output = NULL,
                           post_output = NULL) {
  if (!is.class.list("data_extract_spec")(x)) {
    x <- list(x)
  }
  if (!is.class.list("data_extract_spec")(y)) {
    y <- list(y)
  }
  if (!is.class.list("data_extract_spec")(color_by)) {
    color_by <- list_or_null(color_by)
  }

  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  stopifnot(is.class.list("data_extract_spec")(x))
  stopifnot(is.class.list("data_extract_spec")(y))
  stopifnot(is.class.list("data_extract_spec")(color_by))
  stopifnot(is.numeric.vector(plot_height) && (length(plot_height) == 3 || length(plot_height) == 1))
  stopifnot(`if`(length(plot_height) == 3, plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3], TRUE))
  stopifnot(is.numeric.vector(alpha) && (length(alpha) == 3 || length(alpha) == 1))
  stopifnot(`if`(length(alpha) == 3, alpha[1] >= alpha[2] && alpha[1] <= alpha[3], TRUE))
  stopifnot(is.numeric.vector(size) && (length(size) == 3 || length(size) == 1))
  stopifnot(`if`(length(size) == 3, size[1] >= size[2] && size[1] <= size[3], TRUE))

  args <- as.list(environment())

  module(
    label = label,
    server = function(input, output, session, datasets, ...) return(NULL),
    server_args = list(dataname = dataname),
    ui = ui_scatterplot,
    ui_args = args,
    filters = "all"
  )
}

ui_scatterplot <- function(id,
                           label,
                           dataname,
                           x,
                           y,
                           color_by,
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
      data_extract_input(
        id = ns("x"),
        label = "x variable",
        data_extract_spec = x
      ),
      data_extract_input(
        id = ns("y"),
        label = "y variable",
        data_extract_spec = y
      ),
      data_extract_input(
        id = ns("color_by"),
        label = "color by",
        data_extract_spec = color_by
      ),
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
    x <- input$x
    y <- input$y
    alpha <- input$alpha
    color_by <- check_color(input$color_by)
    size <- input$size

    data_name <- paste0(dataname, "_FILTERED")
    assign(data_name, anl)

    validate(need(alpha, "need alpha"))
    validate(need(!is.null(anl) && is.data.frame(anl), "no data left"))
    validate(need(nrow(anl) > 0, "no observations left"))
    validate(need(x, "no valid x variable selected"))
    validate(need(y, "no valid y variable selected"))
    validate(need(
      x %in% names(anl),
      paste("variable", x, " is not available in data", dataname)
    ))
    validate(need(
      y %in% names(anl),
      paste("variable", y, " is not available in data", dataname)
    ))

    chunks_reset()

    if (is.null(color_by)) {
      chunks_push(expression = bquote(
        ggplot(
          .(as.name(data_name)),
          aes_string(x = x, y = y)
        ) +
          geom_point(alpha = alpha, size = size)
      ) %>% substituteDirect(
        list(
          alpha = alpha,
          size = size,
          x = x,
          y = y
        )
      ))
    } else {
      chunks_push(expression = bquote(
        ggplot(
          .(as.name(data_name)),
          aes_string(x = x, y = y, color = color_by)
        ) +
          geom_point(alpha = alpha, size = size)
      ) %>% substituteDirect(
        list(
          alpha = alpha,
          size = size,
          x = x,
          y = y,
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
