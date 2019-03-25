#' Scatterplot and Regression Model
#'
#' @import teal.devel
#'
#' @param dataname name of datasets used to generate the regression plot (just used for labeling)
#' @param regressor (\code{\link{teal.devel}{DataExtractSpec}}) Output of \code{teal.devel::data_extract_spec}
#'  to define the regressor variable from an incoming dataset with filtering and selecting.
#' @param response (\code{\link{teal.devel}{DataExtractSpec}}) Output of \code{teal.devel::data_extract_spec}
#'  to define the response variable from an incoming dataset with filtering and selecting.
#' @param plot_height (\code{numeric}) a vector of length three with \code{c(value, min and max)} for a slider
#'  encoding the plot height.
#'
#' @export
#' @inheritParams teal::module
#' @inheritParams teal::standard_layout
#'
#' @examples
#'
#' library(random.cdisc.data)
#' asl <- radsl(N = 600)
#'
#' adte <- radtte(asl, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))
#'
#' adte_filters <- filter_spec(
#'   vars = c("PARAMCD"), #'  only key variables are allowed
#'   sep = " - ",
#'   choices = c("OS", "PFS", "EFS"),
#'   selected = "OS",
#'   multiple = TRUE, #'  if multiple, then a spread is needed
#'   label = "Choose endpoint"
#' )
#'
#'
#' adte_extracted_regressor <- data_extract_spec(
#'   dataname = "ADTE",
#'   filter = adte_filters,
#'   columns = columns_spec(
#'     choices = c("AVAL", "BMRKR1", "SITEID"),
#'     selected = c("AVAL"),
#'     multiple = TRUE,
#'     fixed = FALSE, #'  Whether the user can select the item (optional)
#'     label = "Column" #'  Label the column select dropdown (optional)
#'   )
#' )
#'
#' adte_extracted_response <- data_extract_spec(
#'   dataname = "ADTE",
#'   filter = adte_filters,
#'   columns = columns_spec(
#'     choices = c("AVAL", "BMRKR1"),
#'     selected = c("AVAL"),
#'     multiple = FALSE,
#'     fixed = FALSE, #'  Whether the user can select the item
#'     label = "" #'  Label the column select dropdown (optional)
#'   )
#' )
#'
#' asl_extracted <- data_extract_spec(
#'   dataname = "ASL",
#'   columns = columns_spec(
#'     choices = c("SEX", "AGE"),
#'     selected = c("AGE"),
#'     multiple = TRUE,
#'     fixed = FALSE
#'   )
#' )
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'     ASL = data_for_teal(
#'       asl,
#'       keys = c("USUBJID", "STUDYID"),
#'       source = "radsl(N = 600)"
#'     ),
#'     ADTE = data_for_teal(
#'       adte,
#'       keys = c("USUBJID", "STUDYID", "PARAMCD"),
#'       source = "radaette(radsl(N = 600))"
#'     )
#'   ),
#'   modules = root_modules(
#'     teal.modules.general::tm_g_regression(
#'       label = "Regression",
#'       dataname = c("ASL", "ADTE"),
#'       response = list(adte_extracted_response),
#'       regressor = list(
#'         adte_extracted_regressor,
#'         asl_extracted
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_regression <- function(
                            label = "Regression Analysis",
                            dataname,
                            regressor,
                            response,
                            plot_height = c(600, 200, 2000),
                            pre_output = NULL,
                            post_output = NULL) {

  # Error prevention
  stopifnot(!is.null(dataname))
  stopifnot(dataname != "")

  args <- as.list(environment())

  teal::module(
    label = label,
    server = srv_g_regression,
    ui = ui_g_regression,
    ui_args = args,
    server_args = list(regressor = regressor, response = response),
    filters = "all"
  )
}



#' @import teal
ui_g_regression <- function(id, ...) {
  arguments <- list(...)

  ns <- NS(id)

  standard_layout(
    output = teal.devel::white_small_well(
      tags$div(
        # This shall be wrapped in a teal::plot
        plot_height_output(id = ns("myplot")),
        tags$div(verbatimTextOutput(ns("text")))
      )
    ),
    encoding = div(
      helpText("Datasets", paste(unlist(arguments$dataname))),
      data_extract_input(
        id = ns("regressor"),
        label = "Regressor Variable",
        data_extract_spec = arguments$regressor
      ),
      data_extract_input(
        id = ns("response"),
        label = "Response Variable",
        data_extract_spec = arguments$response
      ),
      radioButtons(ns("plot_type"),
        label = "Plot Type",
        choices = c(
          "Response vs Regressor",
          "Residuals vs Fitted",
          "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
          "Cook's dist vs Leverage h[ii]/(1 - h[ii]"
        ),
        selected = "Response vs Regressor"
      ),
      # This shall be wrapped in a teal::plot
      plot_height_input(id = ns("myplot"), value = arguments$plot_height)
    )
  )
}

#' @importFrom graphics plot abline
#' @importFrom methods is
srv_g_regression <- function(input, output, session, datasets, response, regressor) {
  stopifnot(is.list(response))
  stopifnot(is.list(regressor))

  # data_extract_module, "response",
  # dataname AND filtering (yes/no) AND Names(Filtering-selected) AND Names(Columns-Selected)

  regressor_data <- callModule(data_extract_module,
    id = "regressor",
    datasets = datasets,
    data_extract_spec = regressor
  )
  response_data <- callModule(data_extract_module,
    id = "response",
    datasets = datasets,
    data_extract_spec = response
  )

  fit_cl <- reactive({
    call("lm",
      formula = as.formula(
        paste(get_dataset_prefixed_col_names(response_data()),
          paste(get_dataset_prefixed_col_names(regressor_data()),
            collapse = " + "
          ),
          sep = " ~ "
        )
      ),
      data = as.name("merged_dataset")
    )
  })

  fit <- reactive({
    merged_dataset <- merge_datasets(regressor_data(), response_data())
    fit_cl <- fit_cl()

    validate(
      need(nrow(merged_dataset) >= 10, paste("need at lease 10 observations, currently have only",
              nrow(merged_dataset)))
    )

    attr(fit_cl[[2]], ".Environment") <- environment()

    fit <- eval(fit_cl)
    fit
  })

  output$plot <- renderPlot({
    fit <- fit()
    plot_type <- input$plot_type
    if (plot_type == "Response vs Regressor") {
      if (ncol(fit$model) > 1) {
        plot(fit$model[, 2:1])
      } else {        
        # Relabel the data for X vs X plot
        plot_data <- data.frame(fit$model[, 1], fit$model[, 1])
        names(plot_data) <- rep(names(fit$model), 2)
        plot(plot_data)
      }
      abline(fit)
    } else {
      i <- which(plot_type == c(
        "Residuals vs Fitted",
        "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
        "Cook's dist vs Leverage h[ii]/(1 - h[ii]"
      ))

      plot(fit, which = i, id.n = NULL)
    }
  })

  # Insert the plot into a plot_height module from teal.devel
  callModule(plot_with_height, id = "myplot", plot_height = reactive(input$myplot), plot_id = session$ns("plot"))

  output$text <- renderPrint({
    fit <- fit()

    validate(need(methods::is(fit, "lm"), "there seem to problems fitting the model"))

    summary(fit)
  })
}
