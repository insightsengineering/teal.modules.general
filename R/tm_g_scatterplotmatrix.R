#' Scatterplot matrix
#'
#' @inheritParams teal::module
#' @inheritParams teal::standard_layout
#' @param label Name of the module.
#' @param dataname Name of dataset used to generate table. 
#' @param plot_height If scalar then the plot will have a fixed height. If a
#'   slider should be presented to adjust the plot height dynamically then it
#'   can be a vector of length three with \code{c(value, min and max)}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' N <- 100
#' ASL <- data.frame(
#'   USUBJID = paste("id", seq_len(N), sep = "-"),
#'   STUDYID = "study1",
#'   F1 = factor(sample(paste0("facet_1_", c("A", "B")), N, TRUE)),
#'   F2 = factor(sample(paste0("facet_2_", c("a", "b", "c")), N, TRUE)),
#'   cont = rnorm(N),
#'   disc = factor(sample(letters[1:5], N, TRUE)),
#'   cont2 = runif(N),
#'   disc2 = factor(sample(LETTERS[1:5], N, TRUE))
#' )
#'
#' ASL$cont3 <- ASL$cont * 3 + 2 + rnorm(nrow(ASL), sd = .3)
#'
#' attr(ASL, "source") <- "# ASL is random data"
#'
#' x <- teal::init(
#'   data = list(ASL = ASL),
#'   modules = teal::root_modules(
#'     tm_g_scatterplotmatrix(
#'       dataname = "ASL",
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#' }
#'
tm_g_scatterplotmatrix <- function(
    label = "Scatterplot matrix",
    dataname,
    plot_height = c(600, 200, 2000),
    pre_output = NULL,
    post_output = NULL) {
    
    args <- as.list(environment())
    
    teal::module(
        label = label,
        server = srv_g_scatterplotmatrix,
        ui = ui_g_scatterplotmatrix,
        ui_args = args,
        server_args = list(dataname = dataname),
        filters = dataname
    )
}


#' @import teal
#' @importFrom teal.devel white_small_well
ui_g_scatterplotmatrix <- function(id, ...) {
    a <- list(...)
    
    ns <- NS(id)
    
    teal::standard_layout(
        output = teal.devel::white_small_well(
            tags$div(
                tags$div(teal.devel::white_small_well(uiOutput(ns("plot_ui")))
                )
            )
        ),
        encoding = div(
            helpText("Dataset:", tags$code(a$dataname)),
            uiOutput(ns("select_ui")),
            teal::optionalSliderInputValMinMax(ns("plot_height"), "Plot height", a$plot_height, ticks = FALSE)
        )
    )
}

#' @importFrom teal.devel as.global
# TO DO: import GGally (only requireNamespace should be enough -> discuss)
srv_g_scatterplotmatrix <- function(input, output, session, datasets, dataname) {
    
    # get filtered data
    anl_filtered <- reactive(datasets$get_data(dataname, reactive = TRUE, filtered = TRUE))
    
    # UI for selectInput that is based on filtered data
    output$select_ui <- renderUI({
        req(anl_filtered())
        teal::optionalSelectInput(
            session$ns("matrix_cols"), "Columns displayed in matrix",
            choices = colnames(anl_filtered()), selected = NULL, multiple = T
        )
    })
    
    # UI for plot
    output$plot_ui <- renderUI({
        plot_height <- input$plot_height
        validate(need(plot_height, "need valid plot height"))
        plotOutput(session$ns("plot"), height = plot_height)
    })
    
    # plot itself
    output$plot <- renderPlot({
        # check that columns are selected
        validate(need(input$matrix_cols, 
                      "Please select columns first."))
        
        # check that data are available
        validate(need(nrow(anl_filtered()) > 0, 
                      "There are zero observations in the (filtered) dataset."))
        
        # check for high cardinality (factor variables with high number of levels take long time to be plotted ...)
        len_levels <- vapply(anl_filtered()[, input$matrix_cols, drop = F], function(x) length(levels(x)), 1)
        ggpairs_thresh <- as.list(args(GGally::ggpairs))[['cardinality_threshold']]
        invalid_cols <- names(len_levels)[len_levels > ggpairs_thresh]
        validate(need(identical(invalid_cols, character(0)), 
                      paste0('There is/are column(-s) with high number of levels (> ', 
                             ggpairs_thresh, 
                             '), please remove it/them: ', 
                             paste(invalid_cols, collapse = ", "))))
        
        # plot
        GGally::ggpairs(anl_filtered(), 
                        columns = input$matrix_cols, 
                        diag = list(continuous = 'barDiag'))
    })
    
}





