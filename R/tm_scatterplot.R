#' @include utils.R
NULL

#' Create a simple scatterplot
#'
#' Create a plot with the \code{\link{plot}[base]} function
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
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
#' # Scatterplot of variables from ADSL dataset
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- cadsl",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_scatterplot(
#'       label = "Scatterplot Choices",
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("AGE", "BMRKR1", "BMRKR2"),
#'           selected = "AGE",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("AGE", "BMRKR1", "BMRKR2"),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       color_by = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
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
#' # Scatterplot of AGE and BMRKR1 colored by COUNTRY
#' library(random.cdisc.data)
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
#'
#' ADSL_2 <- mutate_at(cadsl,
#'   .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'   .funs = list(~as.factor(.))
#' ) %>% select("ACTARM", "AGE", "RACE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     dataset("ADSL_2", ADSL_2,
#'             keys = list(primary = c("STUDYID", "USUBJID"), foreign = NULL, parent = NULL)),
#'     code = 'ADSL <- cadsl
#' ADSL_2 <- mutate_at(cadsl,
#' .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#' .funs = list(~as.factor(.))) %>%
#' select("ACTARM", "AGE", "RACE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")',
#'     check = FALSE #TODO
#'   ),
#'   modules = root_modules(
#'     tm_scatterplot("Scatterplot for different wide data",
#'      x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'          label = "Select variable:",
#'           choices = c("BMRKR1", "BMRKR2"),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )),
#'       y = data_extract_spec(
#'         dataname = "ADSL_2",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("AGE", "RACE"),
#'           selected = "AGE",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )),
#'       color_by = data_extract_spec(
#'         dataname = "ADSL_2",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("COUNTRY", "AGE", "RACE"),
#'           selected = "COUNTRY",
#'           multiple = FALSE,
#'           fixed = FALSE
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
#' #' Scatterplot of parameters from ADTTE and ADRS datasets.
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
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_scatterplot(
#'       label = "Scatterplot on two long datasets",
#'       x = data_extract_spec(
#'         dataname = "ADRS",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = names(ADRS),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         ),
#'         filter = filter_spec(
#'           label = "Select endpoint:",
#'           vars = c("PARAMCD", "AVISIT"),
#'           choices = apply(as.matrix(expand.grid(
#'           levels(ADRS$PARAMCD), levels(ADRS$AVISIT))), 1, paste, collapse = " - "),
#'           selected = "OVRINV - Screening",
#'           multiple = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADTTE",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = names(ADTTE),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         ),
#'         filter = filter_spec(
#'           label = "Select parameters:",
#'           vars = c("PARAMCD"),
#'           choices = unique(ADTTE$PARAMCD),
#'           selected = "OS",
#'           multiple = TRUE
#'         )
#'       ),
#'       color_by = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
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
#' # Scatterplot of response duration and patient age
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     code = "ADSL <- cadsl; ADRS <- cadrs",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'    tm_scatterplot("Scatterplot for wide and long data",
#'      x = data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             label = "Select variable:",
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
#'               label = "Select response:"
#'             ),
#'             filter_spec(
#'               vars = "AVISIT",
#'               choices = levels(ADRS$AVISIT),
#'               selected = levels(ADRS$AVISIT)[1],
#'               multiple = FALSE,
#'               label = "Select visit:"
#'             )
#'           ),
#'           select = select_spec(
#'             label = "Selected variable:",
#'             choices = "AVAL",
#'             selected = "AVAL",
#'             multiple = FALSE,
#'             fixed = TRUE
#'           )
#'         ),
#'      color_by = data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             label = "Select variable:",
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
#' # Scatterplot of different continuous variables from ADRS dataset
#'
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     code = "ADSL <- cadsl; ADRS <- cadrs",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_scatterplot(
#'       "Scatterplot for same long dataset",
#'       x = data_extract_spec(
#'         dataname = "ADRS",
#'         select = select_spec(
#'           choices = c("AVAL", "BMRKR1", "BMRKR2"),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADRS",
#'         select = select_spec(
#'           choices = c("AVAL", "BMRKR1", "BMRKR2"),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       color_by = data_extract_spec(
#'         dataname = "ADRS",
#'         select = select_spec(
#'           choices = c("AGE", "SEX", "RACE"),
#'           selected = NULL,
#'           multiple = FALSE, #todo: 0 or 1
#'           fixed = FALSE,
#'           label = "Select variable:"
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
#' # Scatterplot of different continuous variables from ADLB dataset
#'
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADLB <- cadlb
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = "ADSL <- cadsl; ADLB <- cadlb",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_scatterplot(
#'       "Scatterplot Choices",
#'       x = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Select lab:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = TRUE
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
#'             label = "Select lab:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = TRUE
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
#'             label = "Select lab:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = c("RACE", "SEX"),
#'           selected = "SEX",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'     )
#'   )
#' )
#' \dontrun{
#'   shinyApp(app$ui, app$server)
#' }
tm_scatterplot <- function(label,
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
    server = srv_scatterplot,
    ui = ui_scatterplot,
    ui_args = args,
    server_args = list(x = x, y = y, color_by = color_by),
    filters = "all"
  )
}

ui_scatterplot <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)

  if (args$plot_height < 200 || args$plot_height > 2000) {
    stop("plot_height must be between 200 and 2000")
  }

  standard_layout(
    output = white_small_well(
      plot_height_output(id = ns("myplot"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Dataset:",
               tags$code(paste(
                 get_extract_datanames(args[c("x", "y", "color_by")]),
                 collapse = ", "
               ))
      ),
      data_extract_input(
        id = ns("x"),
        label = "X variable",
        data_extract_spec = args$x
      ),
      data_extract_input(
        id = ns("y"),
        label = "Y variable",
        data_extract_spec = args$y
      ),
      data_extract_input(
        id = ns("color_by"),
        label = "Color by variable",
        data_extract_spec = args$color_by
      ),
      plot_height_input(id = ns("myplot"), value = args$plot_height),
      optionalSliderInputValMinMax(ns("alpha"), "Opacity:", args$alpha, ticks = FALSE),
      optionalSliderInputValMinMax(ns("size"), "Points size:", args$size, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R code", width = "100%"),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

#' @importFrom magrittr %>%
#' @importFrom methods substituteDirect
srv_scatterplot <- function(input, output, session, datasets, x, y, color_by) {
  init_chunks(session)

  # Insert the plot into a plot_height module from teal.devel
  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(x, y, color_by),
    input_id = c("x", "y", "color_by")
  )

  output$plot <- renderPlot({
    ANL <- merged_data()$data()
    chunks_reset()
    x_var <- merged_data()$columns_source$x
    y_var <- merged_data()$columns_source$y
    color_by_var <- merged_data()$columns_source$color_by
    alpha <- input$alpha
    size <- input$size

    validate(need(alpha, "need opacity alpha"))
    validate_has_data(ANL, 10)
    validate(need(length(x_var) == 1, "there must be exactly one x var"))
    validate(need(length(y_var) == 1, "there must be exactly one y var"))
    validate(need(length(color_by_var) <= 1, "can color by at most 1 color"))

    if (is.character.empty(color_by_var)) {
      chunks_push(expression = bquote(
        ggplot(
          ANL,
          aes_string(x = .(x_var), y = .(y_var))
        ) +
          geom_point(alpha = .(alpha), size = .(size))
      ))
    } else {
      chunks_push(expression = bquote(
        ggplot(
          ANL,
          aes_string(x = .(x_var), y = .(y_var), color = .(color_by_var))
        ) +
          geom_point(alpha = .(alpha), size = .(size))
      ))
    }

    chunks_safe_eval()
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "R Code for a scatterplot matrix",
      rcode = get_rcode(
        datasets = datasets,
        merge_expression = merged_data()$expr,
        title = "Scatterplot matrix",
      )
    )
  })
}
