#' Scatterplot and Regression Model
#'
#'
#' @param dataname name of datasets used to generate the regression plot (just used for labeling)
#' @param regressor (\code{list}) Output of \code{teal.devel::data_extract_spec}
#'  to define the regressor variable from an incoming dataset with filtering and selecting.
#' @param response (\code{list}) Output of \code{teal.devel::data_extract_spec}
#'  to define the response variable from an incoming dataset with filtering and selecting.
#' @param plot_height (\code{numeric}) a vector of length three with \code{c(value, min and max)} for a slider
#'  encoding the plot height.
#'
#' @export
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#'
#' @examples
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' asl <- radsl(seed = 1)
#' adte <- radtte(asl, seed = 1, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))
#' keys(adte) <- c("STUDYID", "USUBJID", "PARAMCD")
#' keys(asl) <- c("STUDYID", "USUBJID")
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
#'app <- teal::init(
#'   data = cdisc_data(
#'        ASL = asl,
#'        ADTE = adte,
#'        code = 'asl <- radsl(seed = 1)
#'                adte <- radtte(asl, seed = 1, event.descr = c("STUDYID", "USUBJID", "PARAMCD"))
#'                keys(asl) <- c("USUBJID", "STUDYID")
#'                keys(adte) <- c("STUDYID", "USUBJID", "PARAMCD")',
#'        check = FALSE),
#'    modules = teal::root_modules(
#'        tm_g_regression(
#'          label = "Regression",
#'          dataname = c("ASL","ADTE"),
#'          response = list(adte_extracted_response),
#'          regressor = list(
#'            adte_extracted_regressor,
#'            asl_extracted
#'          )
#'        )
#'    )
#')
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_regression <- function(label = "Regression Analysis",
                            dataname,
                            regressor,
                            response,
                            plot_height = c(600, 200, 2000),
                            pre_output = NULL,
                            post_output = NULL) {

  # Error prevention
  stopifnot(!is.null(dataname))
  stopifnot(dataname != "")

  # No check necessary for regressor and response, as checked in data_extract_input

  # Send ui args
  args <- as.list(environment())

  teal::module(
    label = label,
    server = srv_g_regression,
    ui = ui_g_regression,
    ui_args = args,
    server_args = list(regressor = regressor, response = response, dataname = dataname),
    filters = "all"
  )
}



#' @importFrom teal.devel data_extract_input plot_height_output standard_layout
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
      helpText("Datasets: ", lapply(arguments$dataname, tags$code)),
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
          "Cook's dist vs Leverage h[ii]/(1 - h[ii])"
        ),
        selected = "Response vs Regressor"
      ),
      # This shall be wrapped in a teal::plot
      plot_height_input(id = ns("myplot"), value = arguments$plot_height)
    ),
    pre_output = arguments$pre_output,
    post_output = arguments$post_output,
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%")
  )
}

#' @importFrom graphics plot abline
#' @importFrom methods is substituteDirect
#' @importFrom teal.devel %<chunk% %<chunk_env% %substitute% data_extract_module eval_remaining
#' @importFrom teal.devel get_envir_chunks get_rcode renew_chunk_environment renew_chunks set_chunk use_chunks
#' @importFrom teal.devel get_dataset_prefixed_col_names merge_datasets plot_with_height plot_height_input
#' @importFrom teal.devel get_rcode show_rcode_modal validate_has_data
srv_g_regression <- function(input, output, session, datasets, dataname, response, regressor) {
  stopifnot(is.list(response))
  stopifnot(is.list(regressor))

  use_chunks(session)

  # Data Extraction
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

  fit <- reactive({

    response_var <- get_dataset_prefixed_col_names(response_data())
    regressor_var <- get_dataset_prefixed_col_names(regressor_data())
    merged_dataset <- merge_datasets(list(response_data(), regressor_data()))
    validate_has_data(merged_dataset, 10)

    renew_chunk_environment(envir = environment())
    renew_chunks()

    form <- form %<chunk_env%
        as.formula(
            paste(response_var,
            paste(regressor_var,
                collapse = " + "
            ),
            sep = " ~ "
        ))

    set_chunk(
        expression = quote(fit <- lm(form, data = merged_dataset)) %>% substituteDirect(list(form = form))
    )

    summary %<chunk%
        quote(summary <- summary(fit))

    eval_remaining()

    if (input$plot_type == "Response vs Regressor") {
      fit <- get_envir_chunks()$fit

      if (ncol(fit$model) > 1) {
        validate(need(dim(fit$model)[2] < 3, "Response vs Regressor is not provided for >2 Regressors"))
        plot %<chunk%
            plot(fit$model[, 2:1])
      } else {
        plot %<chunk% {
          plot_data <- data.frame(fit$model[, 1], fit$model[, 1])
          names(plot_data) <- rep(names(fit$model), 2)
          plot <- plot(plot_data)
          abline(merged_dataset)
        }
      }
    } else {
      i <- which(input$plot_type == c(
                  "Residuals vs Fitted",
                  "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
                  "Cook's dist vs Leverage h[ii]/(1 - h[ii])"
              ))
      plot %<chunk%
          plot(fit, which = i, id.n = NULL) %substitute% list(i = i)
    }
  })

  output$plot <- renderPlot({
    fit()
    eval_remaining()
  })

  # Insert the plot into a plot_height module from teal.devel
  callModule(plot_with_height,
      id = "myplot",
      plot_height = reactive(input$myplot),
      plot_id = session$ns("plot"))

  output$text <- renderPrint({
    fit()
    return(get_envir_chunks()$summary)
  })

  observeEvent(input$show_rcode, {

    title <- paste0("RegressionPlot of ",  get_dataset_prefixed_col_names(response_data()), " ~ ",
        get_dataset_prefixed_col_names(regressor_data()))

    teal.devel::show_rcode_modal(
      title = "R Code for a Regression Plot",
      rcode = get_rcode(
        datasets = datasets,
        dataname = dataname,
        merged_dataname = "merged_dataset",
        merged_datasets = list(response_data(), regressor_data()),
        title = title
      )
    )
  })
}
