#' Scatterplot matrix
#'
#' @inheritParams teal::module
#' @inheritParams teal::standard_layout
#' @param label Name of the module.
#' @param dataname Name of dataset used to generate table.
#' @param select_col Selected columns to display.
#' @param plot_height If scalar then the plot will have a fixed height. If a
#'   slider should be presented to adjust the plot height dynamically then it
#'   can be a vector of length three with \code{c(value, min and max)}.
#'
#' @export
#'
#' @examples
#' asl <- radsl(N = 600)
#' adte <- radtte(asl, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))
#' keys(adte) <- c("STUDYID", "USUBJID", "PARAMCD")
#' keys(asl) <- c("STUDYID", "USUBJID")
#'
#' adte_prep <- data_extract_spec(
#'   dataname = "ADTE",
#'   columns = columns_spec(
#'     choices = c("*no columns selected*" = "", colnames(adte)),
#'     selected = "",
#'     multiple = TRUE,
#'     fixed = FALSE, #'  Whether the user can select the item
#'     label = "" #'  Label the column select dropdown (optional)
#'   )
#' )
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     ASL = asl,
#'     ADTE = adte,
#'     code = "",
#'     check = FALSE),
#'   modules = root_modules(
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
    filters = "all" #dataname
  )
}


#' @import teal
#' @importFrom teal.devel white_small_well
#' @importFrom teal.devel plot_height_output
#' @importFrom teal.devel plot_height_input
ui_g_scatterplotmatrix <- function(id, ...) {
  args <- list(...)

  ns <- NS(id)
  teal::standard_layout(
    output = teal.devel::white_small_well(
      tags$div(
        tags$div(
          teal.devel::white_small_well(
            teal.devel::plot_height_output(ns("myplot")))
        )
      )
    ),
    encoding = div(
      # helpText("Dataset:", tags$code(args$dataname)),
      helpText("Datasets: ", args$dataname %>% lapply(., tags$code)),
      # uiOutput(ns("select_ui")),
      data_extract_input(
        id = ns("select_col"),
        label = "Selected columns",
        data_extract_spec = args$select_col
      ),
      sliderInput(ns("bins"), "Number of bins in histograms", min = 1, max = 100,
                  value = 30, round = TRUE, step = 3),
      plot_height_input(id = ns("myplot"), value = args$plot_height)
    )#,
    # pre_output = args$pre_output,
    # post_output = args$post_output
  )
}

#' @importFrom teal.devel as.global
# TO DO: import GGally ?? (only requireNamespace should be enough -> discuss)
# TO DO: no columns selected? keep it as an option?
# TO DO: check for cardinality - different column names in validate(need()) than in selectInput: ADTE.USUBJID vs USUBJID
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

  # reactive handling of user inputs to produce a scatterplotmatrix
  reprod_plot <- reactive({
    # get number of bins for histograms
    bins <- input$bins

    # get selected columns
    cols <- get_dataset_prefixed_col_names(col_extract())

    # merge datasets
    merged_ds <- merge_datasets(list(col_extract()))

    # check columns selected
    validate(need(cols,
                  "Please select columns first."))

    # check that data are available
    validate(need(nrow(merged_ds) > 0,
                  "There are zero observations in the (filtered) dataset."))

    # check for high cardinality (factor variables with high number of levels take long time to be plotted ...)
    len_levels <- vapply(merged_ds[, cols, drop = F], function(x) {
      if (is.character(x)) {length(levels(as.factor(x)))} else {0}
    }, 1)
    ggpairs_thresh <- as.list(args(GGally::ggpairs))[["cardinality_threshold"]]
    invalid_cols <- names(len_levels)[len_levels > ggpairs_thresh]
    validate(need(identical(invalid_cols, character(0)),
                  paste0("There is/are column(-s) with high number of levels (> ",
                         ggpairs_thresh,
                         "), please remove it/them: ",
                         paste(invalid_cols, collapse = ", "))))

    # reset chunks on every user-input change
    renew_chunk_environment(envir = environment())
    renew_chunks()

    # set up expression chunk
    set_chunk(
      expression = quote(
        expr = GGally::ggpairs(
          data = merged_ds,
          columns = .cols,
          diag = list(continuous = GGally::wrap(funcVal = "barDiag", bins = .bins)))) %>%
        substituteDirect(list(.cols = cols, .bins = bins))
    )

    # evaluate chunk
    eval_remaining()

  })

  # set plot output
  callModule(plot_with_height,
             id = "myplot",
             plot_height = reactive(input$myplot),
             plot_id = session$ns("plot"))

  # plot
  output$plot <- renderPlot({
    reprod_plot()
  })

}





