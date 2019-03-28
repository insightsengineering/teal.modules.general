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
#'     tm_g_regression(
#'       label = "Regression",
#'       dataname = c("ASL", "ADTE"),
#'       response = list(adte_extracted_response),
#'       regressor = list(
#'         asl_extracted,
#'         adte_extracted_regressor
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
      helpText("Datasets: ", arguments$dataname %>% lapply(., tags$code)),
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
    ),
    pre_output = arguments$pre_output,
    post_output = arguments$post_output,
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%")
  )
}

#' @importFrom graphics plot abline
#' @importFrom methods is
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

  data_to_merge <- reactive(list(response_data(), regressor_data()))

  set_chunks <- reactive({

    set_chunk("formula", rlang::expr(
      paste(response_var,
        paste(regressor_var,
          collapse = " + "
        ),
        sep = " ~ "
      )
    ), vars = list(
      dataname = "",
      response_var = get_dataset_prefixed_col_names(response_data()),
      regressor_var = get_dataset_prefixed_col_names(regressor_data())
    ))

    merged_dataset <- merge_datasets(data_to_merge())

    validate_has_data(merged_dataset, 10)

    set_chunk("lm",
      # Please note rlang::expr ignores the left hand side of the "<-"
      rlang::expr(fit <- lm(as.formula(form), data = dataset)),
      vars = list(
        dataset = merged_dataset,
        dataname = "merged_dataset",
        form = eval_chunk("formula")
      )
    )

    set_chunk("summary",
        my_expr <- rlang::expr(
            summary(dataset)
        ),
        vars = list(
            dataset = eval_chunk("lm"),
            dataname = get_lhs_chunk("lm")
        )
    )

    set_chunk("plot",
        plot_expression(eval_chunk("lm"), input$plot_type),
        vars = list(
            dataset = eval_chunk("lm"),
            dataname = get_lhs_chunk("lm"),
            i = which(input$plot_type == c(
                    "Residuals vs Fitted",
                    "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
                    "Cook's dist vs Leverage h[ii]/(1 - h[ii]"
                ))
        )
    )
  })

  output$plot <- renderPlot({
    set_chunks() # Re-act to changes in linear model
    eval_chunk("plot")
  })

  # Insert the plot into a plot_height module from teal.devel
  callModule(plot_with_height, id = "myplot", plot_height = reactive(input$myplot), plot_id = session$ns("plot"))

  output$text <- renderPrint({
    set_chunks()
    summary <- eval_chunk("summary")
    summary$call <- paste0("summary(lm(as.formula(", eval_chunk("formula"), "), data = dataset))")
    return(summary)
  })

  observeEvent(input$show_rcode, {

    title <- paste("RegressionPlot of ",  get_dataset_prefixed_col_names(response_data()), " ~ ",
        get_dataset_prefixed_col_names(regressor_data()))

    teal.devel::show_rcode_modal(
      title = "R Code for a Regression Plot",
      rcode = get_rcode(
        datasets = datasets,
        dataname = dataname,
        merged_dataname = "merged_dataset",
        merged_datasets = data_to_merge(),
        title = title,
        description = "",
        libraries = c(),
        git_pkgs = list(roche = c("NEST/teal", "NEST/teal.devel", "NEST/teal.modules.general")),
        selected_chunk_ids = c("lm", "summary", "plot")
      )
    )
  })
}

plot_expression <- function(fit, plot_type) {
  if (plot_type == "Response vs Regressor") {
    if (ncol(fit$model) > 1) {
      validate(need(dim(fit$model)[2] < 3, "Response vs Regressor is not provided for >2 Regressors"))
      rlang::expr(plot(dataset$model[, 2:1]))
    } else {
      rlang::expr({
        plot_data <- data.frame(dataset$model[, 1], dataset$model[, 1])
        # Relabel the data for X vs X plot
        names(plot_data) <- rep(names(dataset$model), 2)
        plot(plot_data)
        abline(dataset)
      })
    }
  } else {
    rlang::expr(plot(dataset, which = i, id.n = NULL))
  }
}
