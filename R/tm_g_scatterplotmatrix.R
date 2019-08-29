#' Create a scatterplot matrix
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param selected (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'  Plotting variables from an incoming dataset with filtering and selecting.
#' @param plot_height (\code{numeric}) A vector of length three with \code{c(value, min and max)} for a slider
#'  encoding the plot height.
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @export
#' @examples
#'
#' # datasets: single wide dataset
#' # Scatterplot matrix of variables from ADSL dataset
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADTTE <- radtte(ADSL, seed = 1, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- cadsl",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix",
#'       selected = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = colnames(ADSL),
#'           selected = c("AGE", "RACE", "SEX"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#' # datasets: different wide
#' # Scatterplot matrix with AGE, ACTARM, SEX, COUNTRY and STRATA2
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- cadsl
#' ADSL <- mutate_at(ADSL,
#'   .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'   .funs = list(~as.factor(.))
#' ) %>% select(
#'   "ARM", "ACTARM", "ACTARMCD",
#'   "SEX", "STRATA1", "AGE", "USUBJID", "STUDYID", "STRATA2"
#' )
#'
#' ADSL_2 <- mutate_at(cadsl,
#'   .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'   .funs = list(~as.factor(.))
#' ) %>% select("ACTARM", "AGE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
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
#' select("ACTARM", "AGE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")',
#'     check = FALSE #TODO
#'   ),
#'   modules = root_modules(
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix",
#'       selected = list(
#'         data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = colnames(ADSL),
#'             selected = c("AGE", "ACTARM", "SEX"),
#'             multiple = TRUE,
#'             fixed = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADSL_2",
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = colnames(ADSL_2),
#'             selected = c("COUNTRY", "ACTARM", "STRATA2"),
#'             multiple = TRUE,
#'             fixed = FALSE
#'           )
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: same long
#' # Display scatterplots of multiple continuous variables from ADTTE dataset
#'
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADTTE <- cadtte
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADTTE", ADTTE),
#'     code = "ADSL <- cadsl; ADTTE <- cadtte",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_scatterplotmatrix(
#'       "Scatterplot matrix for same long dataset",
#'       selected = data_extract_spec(
#'         dataname = "ADTTE",
#'         select = select_spec(
#'           choices = c("AVAL", "BMRKR1", "BMRKR2"),
#'           selected = c("AVAL", "BMRKR1", "BMRKR2"),
#'           multiple = TRUE,
#'           fixed = FALSE,
#'           label = "Select variables:"
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#'   shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: multiple long datasets
#' # Scatterplot matrix of parameters from ADTTE and ADRS datasets.
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
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix on two long datasets",
#'       selected = list(
#'         data_extract_spec(
#'           dataname = "ADRS",
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = names(ADRS),
#'             selected = c("AVAL", "AVALC"),
#'             multiple = TRUE,
#'             fixed = FALSE
#'           ),
#'           filter = filter_spec(
#'             label = "Select endpoints:",
#'             vars = c("PARAMCD", "AVISIT"),
#'             choices = apply(expand.grid(
#'               levels(ADRS$PARAMCD), levels(ADRS$AVISIT)), 1, paste, collapse = " - "),
#'             selected = "OVRINV - Screening",
#'             multiple = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADTTE",
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = names(ADTTE),
#'             selected = c("AVAL", "CNSR"),
#'             multiple = TRUE,
#'             fixed = FALSE
#'           ),
#'           filter = filter_spec(
#'             label = "Select parameters:",
#'             vars = "PARAMCD",
#'             choices = unique(ADTTE$PARAMCD),
#'             selected = "OS",
#'             multiple = TRUE
#'           )
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
#' # datasets: different subsets of long dataset
#' # Display scatterplots of multiple continuous variables from ADLB dataset
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
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix on two long datasets with subsets",
#'       selected = data_extract_spec(
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
#'           choices = c("AVAL", "CHG", "BMRKR1", "BMRKR2"),
#'           selected = c("AVAL", "CHG"),
#'           multiple = TRUE,
#'           fixed = FALSE,
#'           label = "Select variables:"
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_scatterplotmatrix <- function(label = "Scatterplot matrix",
                                   selected,
                                   plot_height = c(600, 200, 2000),
                                   pre_output = NULL,
                                   post_output = NULL) {
  if (!is.class.list("data_extract_spec")(selected)) {
    selected <- list(selected)
  }

  stopifnot(is.character.single(label))
  stopifnot(is.class.list("data_extract_spec")(selected))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_scatterplotmatrix,
    ui = ui_g_scatterplotmatrix,
    ui_args = args,
    server_args = list(selected = selected),
    filters = "all"
  )
}


ui_g_scatterplotmatrix <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)

  standard_layout(
    output = white_small_well(
      tags$div(verbatimTextOutput(ns("outtext"))),
      tags$div(verbatimTextOutput(ns("strtext"))),
      tags$div(DT::dataTableOutput(ns("outtable")))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args["selected"]),
      data_extract_input(
        id = ns("selected"),
        label = "Selected variables",
        data_extract_spec = args$selected
      ),
      sliderInput(ns("alpha"), "Opacity:",
        min = 0, max = 1, step = .05, value = .5, ticks = FALSE
      ),
      sliderInput(ns("cex"), "Points size:",
        min = 0.2, max = 3, step = .05, value = .65, ticks = FALSE
      ),
      plot_height_input(id = ns("myplot"), value = args$plot_height)
    ),
    pre_output = args$pre_output,
    post_output = args$post_output,
    forms = actionButton(ns("show_rcode"), "Show R code", width = "100%")
  )
}


#' @importFrom dplyr mutate_if
#' @importFrom lattice splom
#' @importFrom methods substituteDirect
srv_g_scatterplotmatrix <- function(input,
                                    output,
                                    session,
                                    datasets,
                                    selected) {
  init_chunks(session)
  dataname <- get_extract_datanames(list(selected))
  data_extract <- list(selected)

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = data_extract,
    input_id = c("selected")
  )

  output$outtext <- renderText({
    chunks_reset()
    chunks_validate_is_ok()

    merged_data()$expr
  })

  output$strtext <- renderText({
    paste0(capture.output(str(merged_data())), collapse = "\n")
  })

  output$outtable <- DT::renderDataTable({
    merged_data()$data
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "R Code for a Scatterplotmatrix",
      rcode = get_rcode(
        datasets = datasets,
        merge_expression = merged_data()$expr,
        title = "",
        description = ""
      )
    )
  })
}
