#' Create a scatterplot matrix
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param dataname Name of datasets used to generate the regression plot (just used for labeling).
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
#' library(tern)
#'
#' ADSL <- cadsl
#' ADTTE <- radtte(ADSL, seed = 1, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))
#' keys(ADSL) <- c("STUDYID", "USUBJID")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = 'ADSL <- cadsl
#' keys(ADSL) <- c("STUDYID", "USUBJID")',
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix",
#'       dataname = c("ADSL"),
#'       selected = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select Variables",
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
#' library(tern)
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
#' keys(ADSL) <- c("STUDYID", "USUBJID")
#'
#'
#' ADSL_2 <- mutate_at(cadsl,
#'   .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'   .funs = list(~as.factor(.))
#' ) %>% select("ACTARM", "AGE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
#' keys(ADSL_2) <- c("STUDYID", "USUBJID")
#'
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
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix",
#'       dataname = c("ADSL", "ADSL_2"),
#'       selected = list(
#'         data_extract_spec(
#'           dataname = "ADSL",
#'           select = select_spec(
#'             label = "Select Variables",
#'             choices = colnames(ADSL),
#'             selected = c("AGE", "ACTARM", "SEX"),
#'             multiple = TRUE,
#'             fixed = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADSL_2",
#'           select = select_spec(
#'             label = "Select Variables",
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
#' library(tern)
#'
#' ADSL <- cadsl
#' ADTTE <- cadtte
#' keys(ADSL) <- c("STUDYID", "USUBJID")
#' keys(ADTTE) <- c("STUDYID", "USUBJID", "PARAMCD")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADTTE", ADTTE),
#'     code = "", check = FALSE),
#'   modules = root_modules(
#'     tm_g_scatterplotmatrix(
#'       "Scatterplot matrix for same long dataset",
#'       dataname = "ADTTE",
#'       selected = data_extract_spec(
#'         dataname = "ADTTE",
#'         select = select_spec(
#'           choices = c("AVAL", "BMRKR1", "BMRKR2"),
#'           selected = c("AVAL", "BMRKR1", "BMRKR2"),
#'           multiple = TRUE,
#'           fixed = FALSE,
#'           label = "Select Variables"
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
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix on two long datasets",
#'       dataname = c("ADSL", "ADRS", "ADTTE"),
#'       selected = list(
#'         data_extract_spec(
#'           dataname = "ADRS",
#'           select = select_spec(
#'             label = "Select Variables",
#'             choices = names(ADRS),
#'             selected = c("AVAL", "AVALC"),
#'             multiple = TRUE,
#'             fixed = FALSE
#'           ),
#'           filter = filter_spec(
#'             label = "Select Endpoint",
#'             vars = c("PARAMCD", "AVISIT"),
#'           choices = apply(expand.grid(
#'           levels(ADRS$PARAMCD), levels(ADRS$AVISIT)), 1, paste, collapse = " - "),
#'             selected = "OVRINV - Screening",
#'             multiple = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADTTE",
#'           select = select_spec(
#'             label = "Select Variables",
#'             choices = names(ADTTE),
#'             selected = c("AVAL", "CNSR"),
#'             multiple = TRUE,
#'             fixed = FALSE
#'           ),
#'           filter = filter_spec(
#'             label = "Select Parameters",
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
#'            ADLB <- cadrs
#'            keys(ADSL) <- c("USUBJID", "STUDYID")
#'            keys(ADLB) <- c("USUBJID", "STUDYID", "PARAMCD", "AVISIT")',
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix on two long datasets with subsets",
#'       dataname = "ADLB",
#'       selected = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Lab"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Visit"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = c("AVAL", "CHNG", "BMRKR1", "BMRKR2"),
#'           selected = c("AVAL", "CHNG"),
#'           multiple = TRUE,
#'           fixed = FALSE,
#'           label = "Select Variables"
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
                                   dataname,
                                   selected,
                                   plot_height = c(600, 200, 2000),
                                   pre_output = NULL,
                                   post_output = NULL) {
  if (!is.class.list("data_extract_spec")(selected)) {
    selected <- list(selected)
  }

  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  stopifnot(is.class.list("data_extract_spec")(selected))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])

  args <- as.list(environment())

  module(
    label = label,
    server = function(input, output, session, datasets, ...) return(NULL),
    ui = ui_g_scatterplotmatrix,
    ui_args = args,
    server_args = list(selected = selected, dataname = dataname),
    filters = "all"
  )
}


ui_g_scatterplotmatrix <- function(id, ...) {
  args <- list(...)

  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      tags$div(
        plot_height_output(ns("myplot"))
      )
    ),
    encoding = div(
      helpText("Datasets: ", lapply(args$dataname, tags$code)),
      data_extract_input(
        id = ns("selected"),
        label = "Selected variables",
        data_extract_spec = args$selected
      ),
      sliderInput(ns("alpha"), "Opacity",
        min = 0, max = 1, step = .05, value = .5, ticks = FALSE
      ),
      sliderInput(ns("cex"), "Point Size",
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
                                    dataname,
                                    selected) {
  stopifnot(all(dataname %in% datasets$datanames()))

  # setup to use chunks
  init_chunks()

  # data extraction
  col_extract <- callModule(
    data_extract_module,
    id = "selected",
    datasets = datasets,
    data_extract_spec = selected
  )

  # set plot output
  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot"))

  # plot
  output$plot <- renderPlot({
    # get inputs
    alpha <- input$alpha
    cex <- input$cex

    # get selected columns
    cols <- get_dataset_prefixed_col_names(col_extract())

    # merge datasets
    merged_ds <- merge_datasets(list(col_extract()))

    # check columns selected
    validate(need(cols, "Please select variables first."))

    # lattice need at least 2 columns for the plot
    validate(need(length(cols) >= 2, "Please select at least two variables."))


    # check that data are available
    validate(need(nrow(merged_ds) > 0, "There are zero observations in the (filtered) dataset."))

    # check proper input values
    validate(need(cex, "Need a proper cex value."))

    # check proper input values
    validate(need(alpha, "Need a proper alpha value."))

    # reset chunks on every user-input change
    chunks_reset()

    # set up expression chunk - lattice graph
    chunks_push(
      expression = quote(
        merged_ds <- dplyr::mutate_if(merged_ds, is.character, as.factor)
      )
    )

    # set up expression chunk - lattice graph
    chunks_push(
      substituteDirect(
        object = quote(
          lattice::splom(merged_ds[, .cols], pch = 16, alpha = .alpha, cex = .cex)
        ),
        frame = list(.cols = cols, .alpha = alpha, .cex = cex)
      )
    )

    p <- chunks_eval()

    chunks_validate_is_ok()

    p
  })

  # show r code
  observeEvent(input$show_rcode, {

    title <- paste0("Scatterplotmatrix of ",
                    paste(get_dataset_prefixed_col_names(col_extract()),
                          collapse = ", "))

    show_rcode_modal(
      title = "R Code for a Scatterplotmatrix",
      rcode = get_rcode(
        datasets = datasets,
        merge_expression = "",
        title = title
      )
    )
  })
}
