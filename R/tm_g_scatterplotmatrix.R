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
#' @noRd
#'
#' @examples
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ADTE <- radtte(ASL, seed = 1, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))
#'
#' keys(ASL) <- c("STUDYID", "USUBJID")
#' keys(ADTE) <- c("STUDYID", "USUBJID", "PARAMCD")
#'
#'
#' app <- init(
#'   data = cdisc_data(
#'        ASL = ASL,
#'        ADTE = ADTE,
#'        code = 'ASL <- cadsl
#'                ADTE <- radtte(ASL, seed = 1, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))
#'                keys(ASL) <- c("STUDYID", "USUBJID")
#'                keys(ADTE) <- c("STUDYID", "USUBJID", "PARAMCD")
#'                ',
#'        check = FALSE),
#'   modules = root_modules(
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix",
#'       dataname = c("ASL","ADTE"),
#'       selected = data_extract_spec(
#'         dataname = "ADTE",
#'         filter = filter_spec(
#'           vars = c("PARAMCD"),
#'           sep = " - ",
#'           choices = c("OS", "PFS", "EFS"),
#'           selected = "OS",
#'           multiple = TRUE,
#'           label = "Choose endpoint"
#'         ),
#'         columns = columns_spec(
#'           label = "Selected columns",
#'           choices = colnames(ADTE),
#'           selected = if (all(c('AGE', 'SEX') %in% colnames(ADTE))) {
#'             c('AGE', 'SEX')
#'           } else {
#'             colnames(ADTE)[1:2]
#'           },
#'           multiple = TRUE,
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
        plot_height_output(ns("myplot")))
    ),
    encoding = div(
      helpText("Datasets: ", lapply(args$dataname, tags$code)),
      data_extract_input(
        id = ns("select_col"),
        label = "Selected columns",
        data_extract_spec = args$selected
      ),
      sliderInput(ns("alpha"), "Opacity",
                  min = 0, max = 1, step = .05, value = .5, ticks = FALSE),
      sliderInput(ns("cex"), "Point Size",
                  min = 0.2, max = 3, step = .05, value = .65, ticks = FALSE),
      plot_height_input(id = ns("myplot"), value = args$plot_height)
    ),
    pre_output = args$pre_output,
    post_output = args$post_output,
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%")
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
                                    select_col) {
  stopifnot(all(dataname %in% datasets$datanames()))

  # setup to use chunks
  init_chunks()

  # data extraction
  col_extract <- callModule(
    data_extract_module,
    id = "select_col",
    datasets = datasets,
    data_extract_spec = select_col
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
    validate(need(cols, "Please select columns first."))

    # lattice need at least 2 columns for the plot
    validate(need(length(cols) >= 2, "Please select at least two columns."))


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
        merged_dataname = "merged_ds",
        merged_datasets = list(col_extract()),
        title = title,
        description = ""
      )
    )
  })
}
