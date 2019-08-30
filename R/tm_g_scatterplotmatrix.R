#' Create a scatterplot matrix
#'
#' The module allows to add and remove dataset selectors (data extract inputs) and it will create
#' the scatterplot matrix for a combination of the selected columns in the merge of all such datasets.
#' The available datasets to choose from for each dataset selector is the same and
#' determined by the argument \code{selected}.
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
#' @importFrom stats na.omit
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
#'
#' ADSL <- cadsl
#' ADSL_2 <- cadsl
#' ADSL_3 <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     dataset("ADSL_2", ADSL_2, keys = get_cdisc_keys("ADSL")),
#'     dataset("ADSL_3", ADSL_2, keys = get_cdisc_keys("ADSL")),
#'     code = 'ADSL <- cadsl; ADSL_2 <- cadsl; ADSL_3 <- cadsl',
#'     check = TRUE
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
#'         ),
#'         data_extract_spec(
#'           dataname = "ADSL_3",
#'           select = select_spec(
#'             label = "Select variables:",
#'             choices = colnames(ADSL_3),
#'             selected = c("AGE", "ACTARM", "SEX"),
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
#'             choices = apply(as.matrix(expand.grid(
#'               levels(ADRS$PARAMCD), levels(ADRS$AVISIT))), 1, paste, collapse = " - "),
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
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix on two long datasets with subsets",
#'       selected = data_extract_spec(
#'         dataname = "ADRS",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADRS$PARAMCD),
#'             selected = levels(ADRS$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Select lab:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADRS$AVISIT),
#'             selected = levels(ADRS$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = c("AVAL", "BMRKR1", "BMRKR2", "AGE"),
#'           selected = c("AVAL", "AGE"),
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
      plot_height_output(ns("myplot"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args$selected),
      actionLink(ns("add_selector"), "Add Selector"),
      hr(),
      sliderInput(
        ns("alpha"), "Opacity:", min = 0, max = 1,
        step = .05, value = .5, ticks = FALSE
      ),
      sliderInput(
        ns("cex"), "Points size:", min = 0.2, max = 3,
        step = .05, value = .65, ticks = FALSE
      ),
      plot_height_input(id = ns("myplot"), value = args$plot_height)
    ),
    pre_output = args$pre_output,
    post_output = args$post_output,
    forms = actionButton(ns("show_rcode"), "Show R code", width = "100%")
  )
}

#' HTML id of the UI element that contains the extract input
#' @param idx index or identifier of the data extract UI that contains the input element
#' @return the html id of the element given the idx
extract_ui_id <- function(idx) {
  paste0("extract_ui_", idx)
}

#' HTML id of the extract input contained within another UI
#' @param idx index or identifier of the data extract input element
#' @return the html id of the element given the idx
extract_input_id <- function(idx) {
  paste0("extract_input_", idx)
}

#' HTML id of the element to remove the whole extract UI
#' @param idx index or identifier of the button to remove the data extract ui
#' @return the html id of the element given the idx
remove_extract_ui_id <- function(idx) {
  paste0("remove_extract_ui_", idx)
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

  extract_ui_indices <- reactiveVal(value = NULL)

  # add_ui is called whenever a new ui element should be added, it contains the id of the next ui element to be added
  add_ui <- reactive({
    # index of new data extract UI
    idx <- max(extract_ui_indices(), 0) + 1 # also works when empty
    insertUI(
      selector = paste0("#", session$ns("add_selector")),
      where = "beforeBegin",
      ui = div(
        id = extract_ui_id(idx),
        data_extract_input(
          id = session$ns(paste0("extract_input_", idx)),
          label = paste0("Selector (", idx, ")"),
          data_extract_spec = selected
        ),
        div(
          actionLink(session$ns(remove_extract_ui_id(idx)), "Remove"),
          align = "right"
        )
      )
    )
    extract_ui_indices(c(extract_ui_indices(), idx))

    # bind remove event
    observeEvent(
      input[[remove_extract_ui_id(idx)]],
      {
        # executed within an isolate block
        # need at least one data extract to select from, validate(need) not working here without output element
        if (length(extract_ui_indices()) <= 1) {
          # todo: somehow show in interface
          print("need at least one data extract to select from")
        }
        req(length(extract_ui_indices()) > 1)
        removeUI(selector = paste0("#", extract_ui_id(idx)))
        extract_ui_indices(extract_ui_indices()[extract_ui_indices() != idx])
      }
    )
  })

  observeEvent(input$add_selector, add_ui())
  observeEvent(extract_ui_indices(), ignoreNULL = FALSE, {
    # ignoreNULL = FALSE and is.null(extract_ui_indices() --> only run on startup
    req(is.null(extract_ui_indices()))
    # add two data extracts initially
    add_ui()
    add_ui()
  })

  merged_data <- reactive({
    validate(need(length(extract_ui_indices()) > 0, "Need at least 1 input."))
    data_merge_module(
      datasets = datasets,
      data_extract = replicate(length(extract_ui_indices()), selected, simplify = FALSE),
      input_id = vapply(extract_ui_indices(), function(idx) extract_input_id(idx), character(1))
    )()
  })

  # Insert the plot into a plot_height module
  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  # plot
  output$plot <- renderPlot({
    ANL <- merged_data()$data()
    chunks_reset()

    alpha <- input$alpha
    cex <- input$cex
    cols_names <- unique(unname(do.call(c, merged_data()$columns_source))) #todo: remove names in merge_datasets()
    validate(need(length(cols_names) > 1, "Need at least 2 columns."))

    # ANL <- dplyr::mutate_if(ANL, is.character, as.factor) # should not be done here, but as preprocessing of data #nolintr

    # create plot
    chunks_push(bquote({
      lattice::splom(ANL[, .(cols_names)], pch = 16, alpha = .(alpha), cex = .(cex))
    }))

    safe_chunks_eval()
  })

  # show r code
  observeEvent(input$show_rcode, {
    title <- paste0(
      "Scatterplotmatrix of ",
      paste(merged_data()$cols, collapse = ", ")
    )

    show_rcode_modal(
      title = "R Code for a Scatterplotmatrix",
      rcode = get_rcode(
        datasets = datasets,
        merge_expression = merged_data()$expr,
        title = title
      )
    )
  })
}
