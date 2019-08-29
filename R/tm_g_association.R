#' Stack Plots of variables and show association with reference variable
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams teal::module
#' @param ref (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   reference variable, must set \code{multiple = FALSE}
#' @param vars (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   associated variables
#' @param show_association (\code{logical}) wheater show association of \code{vars} with refference variable
#' @param plot_height (\code{numeric}) vector with three elements defining selected, min and max plot height
#' @param with_show_r_code (\code{logical}) Whether show R Code button shall be enabled
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @export
#' @examples
#'
#' # datasets: single wide
#' # Association plot of selected reference variable (SEX)
#' # against other selected variables (BMRKR1)
#' library(random.cdisc.data)
#' ADSL <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- cadsl",
#'     check = FALSE #TODO
#'   ),
#'   modules = root_modules(
#'     tm_g_association(
#'       ref = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = names(ADSL),
#'           selected = "AGE",
#'           fixed = FALSE
#'         )
#'       ),
#'       vars = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = names(ADSL),
#'           selected = "BMRKR1",
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
#'
#' # datasets: different wide
#' # Create an association plot with the stratification (STRATA1) as the reference
#' # and the AGE, COUNTRY, RACE as plotted variables
#'
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- cadsl
#' ADSL <- mutate_at(ADSL,
#'                  .vars = vars(ARM, ACTARM, ACTARMCD, SEX, STRATA1, STRATA2),
#'                  .funs = list(~as.factor(.)))
#'
#' ADSL_2 <- ADSL
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     dataset("ADSL_2", ADSL_2,
#'             keys = list(primary = c("STUDYID", "USUBJID"), foreign = NULL, parent = NULL)),
#'     code = "ADSL <- cadsl
#'     ADSL <- mutate_at(ADSL,
#'     .vars = vars(ARM, ACTARM, ACTARMCD, SEX, STRATA1, STRATA2),
#'     .funs = list(~as.factor(.)))
#'     ADSL2 <- ADSL",
#'     check = FALSE #TODO
#'   ),
#'   modules = root_modules(
#'     tm_g_association(
#'       label = "Regression",
#'       ref = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("AGE", "SEX", "STRATA1", "RACE"),
#'           selected = c("STRATA1"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )),
#'       vars = data_extract_spec(
#'         dataname = "ADSL_2",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = c("COUNTRY", "AGE", "RACE"),
#'           selected = c("AGE", "COUNTRY", "RACE"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         ))
#'     )
#'   )
#'   )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: multiple long datasets
#' # Association plot of different parameters from ADRS or ADTTE datasets
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
#'     code = "ADSL <- cadsl
#'             ADRS <- cadrs
#'             ADTTE <- cadtte",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_association(
#'       ref = list(
#'         data_extract_spec(
#'           dataname = "ADRS",
#'           select = select_spec(
#'             label = "Select variable:",
#'             choices = c("AGE", "SEX"),
#'             selected = "AGE",
#'             multiple = FALSE,
#'             fixed = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADTTE",
#'           select = select_spec(
#'             label = "Select variable:",
#'             choices = c("AVAL", "CNSR"),
#'             selected = "AVAL",
#'             multiple = FALSE,
#'             fixed = FALSE
#'           )
#'         )
#'       ),
#'       vars = list(
#'         data_extract_spec(
#'           dataname = "ADTTE",
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = names(ADTTE),
#'             selected = c("AVAL", "AVALU"),
#'             multiple = TRUE,
#'             fixed = FALSE
#'           ),
#'           filter = filter_spec(
#'             label = "Select endpoint:",
#'             vars = "PARAMCD",
#'             choices = unique(ADTTE$PARAMCD),
#'             selected = "OS",
#'             multiple = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADRS",
#'           filter = filter_spec(
#'             label = "Select endpoints:",
#'             vars = c("PARAMCD", "AVISIT"),
#'             choices = apply(expand.grid(levels(ADRS$PARAMCD),
#'             levels(ADRS$AVISIT)), 1, paste, collapse = " - "),
#'             selected = "OVRINV - Screening",
#'             multiple = TRUE
#'           ),
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = names(ADRS),
#'             selected = NULL,
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
#' # datasets: wide and long
#' # Create an association plot with the patient AGE (ADSL$AGE) as the reference
#' # and a measurement the response time filtered by visit and response as
#' # the plotted variable
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#'
#' app <- init(
#'  data = cdisc_data(
#'    cdisc_dataset("ADSL", ADSL),
#'    cdisc_dataset("ADRS", ADRS),
#'    code = "ADSL <- cadsl; ADRS <- cadrs",
#'    check = TRUE
#'  ),
#'  modules = root_modules(
#'    tm_g_association(
#'      label = "Association Plots",
#'      ref = data_extract_spec(
#'        dataname = "ADSL",
#'        select = select_spec(
#'          choices = c("SEX", "AGE", "RACE", "COUNTRY"),
#'          selected = c("AGE"),
#'          multiple = FALSE,
#'          fixed = FALSE,
#'          label = "Select variable:"
#'       )
#'      ),
#'      vars = data_extract_spec(
#'        dataname = "ADRS",
#'        filter = list(
#'          filter_spec(
#'            vars = "PARAM",
#'            choices = levels(ADRS$PARAM),
#'            selected = levels(ADRS$PARAM)[1],
#'            multiple = FALSE,
#'            label = "Select response"
#'          ),
#'          filter_spec(
#'            vars = "AVISIT",
#'            choices = levels(ADRS$AVISIT),
#'            selected = levels(ADRS$AVISIT)[1],
#'            multiple = FALSE,
#'            label = "Select visit:"
#'          )
#'        ),
#'        select = select_spec(
#'          choices = "AVAL",
#'          selected = "AVAL",
#'          multiple = FALSE,
#'          fixed = TRUE,
#'          label = "Selected variable:"
#'        )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: same long
#' # Examine association between two different variables from ADRS dataset.
#'
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#'
#' app <- init(
#'  data = cdisc_data(
#'    cdisc_dataset("ADSL", ADSL),
#'    cdisc_dataset("ADRS", ADRS),
#'    code = "ADSL <- cadsl; ADRS <- cadrs",
#'    check = TRUE
#'  ),
#'  modules = root_modules(
#'    tm_g_association(
#'      label = "Association Plots",
#'      ref = data_extract_spec(
#'        dataname = "ADRS",
#'        select = select_spec(
#'          choices = names(ADRS),
#'          selected = "AVAL",
#'          multiple = FALSE,
#'          fixed = FALSE,
#'          label = "Select variable:"
#'       )
#'      ),
#'      vars = data_extract_spec(
#'        dataname = "ADRS",
#'        select = select_spec(
#'          choices = names(ADRS),
#'          selected = "PARAMCD",
#'          multiple = FALSE,
#'          fixed = FALSE,
#'          label = "Select variable:"
#'        )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#'
#' # datasets: different subsets of long dataset
#' # bug: PARAMCD and AVISIT not renamed
#' # Examine association between two different measurements from ALB dataset.
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
#'     tm_g_association(
#'       ref = data_extract_spec(
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
#'       vars = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[2:3],
#'             multiple = TRUE,
#'             label = "Select labs:"
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
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_association <- function(label = "Association",
                             ref,
                             vars,
                             show_association = TRUE,
                             plot_height = c(600, 400, 5000),
                             pre_output = NULL,
                             post_output = NULL,
                             with_show_r_code = TRUE) {
  if (!is.class.list("data_extract_spec")(ref)) {
    ref <- list(ref)
  }
  if (!is.class.list("data_extract_spec")(vars)) {
    vars <- list(vars)
  }

  stopifnot(is.character.single(label))
  stopifnot(is.class.list("data_extract_spec")(ref))
  stop_if_not(list(all(vapply(ref, function(x) !(x$select$multiple), logical(1))),
                   "'ref' should not allow multiple selection"))
  stopifnot(is.class.list("data_extract_spec")(vars))
  stopifnot(is.logical.single(show_association))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])
  stopifnot(is.logical(with_show_r_code))

  args <- as.list(environment())
  module(
    label = label,
    server = function(input, output, session, datasets, ...) {
      return(NULL)
    },
    ui = ui_tm_g_association,
    ui_args = args,
    server_args = list(ref = ref, vars = vars),
    filters = "all"
  )
}


ui_tm_g_association <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)

  standard_layout(
    output = white_small_well(
      tags$div(verbatimTextOutput(ns("outtext"))),
      tags$div(verbatimTextOutput(ns("strtext"))),
      tags$div(DT::dataTableOutput(ns("outtable")))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("ref", "vars")]),
      data_extract_input(
        id = ns("ref"),
        label = "Reference Variable",
        data_extract_spec = args$ref
      ),
      data_extract_input(
        id = ns("vars"),
        label = "Association variables",
        data_extract_spec = args$vars
      ),
      checkboxInput(ns("association"),
        "Association with the first variable",
        value = args$show_association
      ),
      checkboxInput(ns("show_dist"),
        "Distribution",
        value = FALSE
      ),
      checkboxInput(ns("log_transformation"),
        "Log transformed",
        value = FALSE
      ),
      plot_height_input(id = ns("myplot"), value = args$plot_height)
    ),
    forms = if (args$with_show_r_code) actionButton(ns("show_rcode"), "Show R code", width = "100%") else NULL,
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}


#' @importFrom grid grid.newpage grid.draw
#' @importFrom tern stack_grobs
srv_tm_g_association <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 ref,
                                 vars) {
  init_chunks(session)

  dataname <- get_extract_datanames(list(ref, vars))
  data_extract <- list(ref, vars)
  data_extract <- data_extract[!vapply(data_extract, is.null, logical(1))]

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = data_extract,
    input_id = c("ref", "vars")
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
