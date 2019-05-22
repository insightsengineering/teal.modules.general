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
#' ASL <- radsl(seed = 1)
#' keys(ASL) <- c("STUDYID", "USUBJID")
#'
#'
#' asl_extracted <- data_extract_spec(
#'   dataname = "ASL",
#'   columns = columns_spec(
#'     choices = c("SEX", "AGE", "BMRKR1", "BMRKR2"),
#'     selected = c("AGE"),
#'     multiple = TRUE,
#'     fixed = FALSE
#'   )
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     code = 'ASL <- radsl(seed = 1); keys(ASL) <- c("STUDYID", "USUBJID")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_regression(
#'       label = "Regression",
#'       dataname = c("ASL"),
#'       response = list(asl_extracted),
#'       regressor = list(asl_extracted)
#'     )
#'   )
#' )
#'
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

  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  stopifnot(is.list(response))
  stopifnot(is.list(regressor))
  stopifnot(is_numeric_vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])


  # No check necessary for regressor and response, as checked in data_extract_input

  # Send ui args
  args <- as.list(environment())

  module(
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
      radioButtons(
        ns("plot_type"),
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
  stopifnot(all(dataname %in% datasets$datanames()))

  use_chunks(session)

  # Data Extraction
  regressor_data <- callModule(
    data_extract_module,
    id = "regressor",
    datasets = datasets,
    data_extract_spec = regressor
  )
  response_data <- callModule(
    data_extract_module,
    id = "response",
    datasets = datasets,
    data_extract_spec = response
  )

  fit <- reactive({

    response_var <- get_dataset_prefixed_col_names(response_data())
    regressor_var <- get_dataset_prefixed_col_names(regressor_data())
    merged_dataset <- merge_datasets(list(response_data(), regressor_data()))
    validate_has_data(merged_dataset, 10)

    input$plot_type

    renew_chunk_environment(envir = environment())
    renew_chunks()

    form <- form %<chunk_env%
      as.formula(
        paste(
          response_var,
          paste(
            regressor_var,
            collapse = " + "
          ),
          sep = " ~ "
        )
      )

    set_chunk(
      expression = quote(fit <- lm(form, data = merged_dataset)) %>% substituteDirect(list(form = form))
    )

    eval_remaining()


  })

  plot_reactive <- reactive({

    fit()

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
    plot_reactive()
    eval_remaining()
  })

  # Insert the plot into a plot_height module from teal.devel
  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  output$text <- renderPrint({
    fit()
    set_chunk(id = "summary", expression = quote(summary(fit)))
    eval_remaining()
  })

  observeEvent(input$show_rcode, {
    title <- paste0(
      "RegressionPlot of ",  get_dataset_prefixed_col_names(response_data()), " ~ ",
      get_dataset_prefixed_col_names(regressor_data())
    )

    show_rcode_modal(
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
