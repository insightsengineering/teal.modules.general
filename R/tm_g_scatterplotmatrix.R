#' Create a scatterplot matrix
#'
#' @inheritParams teal::module
#' @inheritParams teal::standard_layout
#' @param dataname Name of datasets used to generate the regression plot (just used for labeling).
#' @param select_col (\code{list}) Output of \code{teal.devel::data_extract_spec}
#'  to define the plotting variables from an incoming dataset with filtering and selecting.
#' @param plot_height (\code{numeric}) A vector of length three with \code{c(value, min and max)} for a slider
#'  encoding the plot height.
#'
#' @export
#'
#' @examples
#' # libraries: shiny, magrittr, teal.devel
#' asl <- random.cdisc.data::radsl(N = 600)
#' adte <- random.cdisc.data::radtte(asl, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))
#' teal.devel::keys(adte) <- c("STUDYID", "USUBJID", "PARAMCD")
#' teal.devel::keys(asl) <- c("STUDYID", "USUBJID")
#'
#' adte_filters <- teal.devel::filter_spec(
#'   vars = c("PARAMCD"), #'  only key variables are allowed
#'   sep = " - ",
#'   choices = c("OS", "PFS", "EFS"),
#'   selected = "OS",
#'   multiple = TRUE, #'  if multiple, then a spread is needed
#'   label = "Choose endpoint"
#' )
#'
#' adte_prep <- teal.devel::data_extract_spec(
#'   dataname = "ADTE",
#'   filter = adte_filters,
#'   columns = teal.devel::columns_spec(
#'     choices = colnames(adte),
#'     selected = colnames(adte)[1:2],
#'     multiple = TRUE,
#'     fixed = FALSE, #'  Whether the user can select the item
#'     label = "" #'  Label the column select dropdown (optional)
#'   )
#' )
#'
#' app <- teal::init(
#'   data = teal.devel::cdisc_data(
#'     ASL = asl,
#'     ADTE = adte,
#'     code = "",
#'     check = FALSE),
#'   modules = teal::root_modules(
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix",
#'       dataname = c("ASL","ADTE"),
#'       select_col = list(adte_prep)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
tm_g_scatterplotmatrix <- function(
  label = "Scatterplot matrix",
  dataname,
  select_col,
  plot_height = c(600, 200, 2000),
  pre_output = NULL,
  post_output = NULL) {

  args <- as.list(environment())

  teal::module(
    label = label,
    server = srv_g_scatterplotmatrix,
    ui = ui_g_scatterplotmatrix,
    ui_args = args,
    server_args = list(select_col = select_col, dataname = dataname),
    filters = "all"
  )
}


ui_g_scatterplotmatrix <- function(id, ...) {
  args <- list(...)

  ns <- NS(id)
  teal::standard_layout(
    output = white_small_well(
      tags$div(
        plot_height_output(ns("myplot")))
    ),
    encoding = div(
      helpText("Datasets: ", lapply(args$dataname, tags$code)),
      data_extract_input(
        id = ns("select_col"),
        label = "Selected columns",
        data_extract_spec = args$select_col
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

#' @importFrom lattice splom
#' @importFrom methods substituteDirect
srv_g_scatterplotmatrix <- function(input, output, session, datasets, dataname, select_col) {

  # checks
  stopifnot(is.list(select_col))

  # setup to use chunks
  use_chunks(session)

  # data extraction
  col_extract <- callModule(data_extract_module,
                            id = "select_col",
                            datasets = datasets,
                            data_extract_spec = select_col
  )

  # set plot output
  callModule(plot_with_height,
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
    validate(need(cols,
                  "Please select columns first."))

    # lattice need at least 2 columns for the plot
    validate(need(length(cols) >= 2,
                  "Please select at least two columns."))


    # check that data are available
    validate(need(nrow(merged_ds) > 0,
                  "There are zero observations in the (filtered) dataset."))

    # check proper input values
    validate(need(cex, "Need a proper cex value."))

    # check proper input values
    validate(need(alpha, "Need a proper alpha value."))

    # reset chunks on every user-input change
    renew_chunk_environment(envir = environment())
    renew_chunks()

    # set up expression chunk - lattice graph
    set_chunk(
      expression = quote(
        merged_ds <- dplyr::mutate_if(merged_ds, is.character, as.factor)
      )
    )

    # set up expression chunk - lattice graph
    set_chunk("plt",
              substituteDirect(
                object = quote(
                  splom(merged_ds[, .cols], pch = 16, alpha = .alpha, cex = .cex)
                ),
                frame = list(.cols = cols, .alpha = alpha, .cex = cex)
              )
    )

    # evaluate chunk
    eval_remaining()

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
        dataname = dataname,
        merged_dataname = "merged_ds",
        merged_datasets = list(col_extract()),
        title = title,
        description = "",
        libraries = c("random.cdisc.data"),
        git_pkgs = list(roche = c("NEST/teal", "NEST/random.cdisc.data", "NEST/teal.devel",
                                  "NEST/teal.modules.general"))
      )
    )
  })

}
