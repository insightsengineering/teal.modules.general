#' Scatterplot and Regression Model
#'
#'
#' @param dataname name of datasets used to generate the regression plot (just used for labeling)
#' @param regressor (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'  regressor variable from an incoming dataset with filtering and selecting.
#' @param response (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'  response variable from an incoming dataset with filtering and selecting.
#' @param plot_height (\code{numeric}) a vector of length three with \code{c(value, min and max)} for a slider
#'  encoding the plot height.
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#'
#' @noRd
#'
#' @examples
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ALB <- cadlb
#'
#' keys(ASL) <- c("STUDYID", "USUBJID")
#' keys(ALB) <- c("STUDYID", "USUBJID", "PARAMCD")
#'
#' asl_extracted <- data_extract_spec(
#'   dataname = "ASL",
#'   columns = columns_spec(
#'     choices = c("BMRKR1", "BMRKR2"),
#'     selected = c("BMRKR1"),
#'     multiple = FALSE,
#'     fixed = FALSE
#'   )
#' )
#' asl_extracted_regressor <- data_extract_spec(
#'   dataname = "ASL",
#'   columns = columns_spec(
#'     choices = c("SEX", "AGE", "BMRKR1", "BMRKR2"),
#'     selected = c("BMRKR1"),
#'     multiple = TRUE,
#'     fixed = FALSE
#'   )
#' )
#'
#' alb_filters <- filter_spec(
#'   vars = c("PARAMCD", "AVISIT"),
#'   sep = " - ",
#'   choices = paste0(unique(ALB$PARAMCD), " - WEEK 4 DAY 29"),
#'   selected = paste0(unique(ALB$PARAMCD), " - WEEK 4 DAY 29")[1],
#'   multiple = FALSE,
#'   label = "Choose endpoint"
#' )
#'
#' alb_extracted <- data_extract_spec(
#'   dataname = "ALB",
#'   filter = alb_filters,
#'   columns = columns_spec(
#'     choices = c("AVAL"),
#'     selected = "AVAL",
#'     multiple = FALSE,
#'     fixed = FALSE,
#'     label = "variable"
#'   )
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ALB = ALB,
#'     code = 'ASL <- cadsl
#'             ALB <- cadlb
#'             keys(ASL) <- c("STUDYID", "USUBJID")
#'             keys(ALB) <- c("STUDYID", "USUBJID", "PARAMCD")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_regression(
#'       label = "Regression",
#'       dataname = c("ASL", "ALB"),
#'       response = alb_extracted,
#'       regressor = asl_extracted
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
  stopifnot(is.class.list("data_extract_spec")(response) || is(response, "data_extract_spec"))
  if (is.class.list("data_extract_spec")(response)) {
    stop_if_not(list(all(vapply(response, function(x) !isTRUE(x$columns$multiple), logical(1))),
                     "Response variable should not allow multiple selection"))
  } else if (is(response, "data_extract_spec")) {
    stop_if_not(list(!isTRUE(response$columns$multiple),
                     "Response variable should not allow multiple selection"))
  }
  stopifnot(is.class.list("data_extract_spec")(regressor) || is(regressor, "data_extract_spec"))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
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


ui_g_regression <- function(id, ...) {
  arguments <- list(...)

  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      tags$div(
        # This shall be wrapped in a teal::plot
        plot_height_output(id = ns("myplot")),
        tags$div(verbatimTextOutput(ns("text")))
      )
    ),
    encoding = div(
      helpText("Datasets: ", lapply(arguments$dataname, tags$code)),
      data_extract_input(
        id = ns("response"),
        label = "Response Variable",
        data_extract_spec = arguments$response
      ),
      data_extract_input(
        id = ns("regressor"),
        label = "Regressor Variable",
        data_extract_spec = arguments$regressor
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
#' @importFrom magrittr %>%
#' @importFrom methods is substituteDirect
#' @importFrom stats as.formula
srv_g_regression <- function(input, output, session, datasets, dataname, response, regressor) {
  stopifnot(all(dataname %in% datasets$datanames()))

  init_chunks()

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

    input$plot_type

    response_var <- get_dataset_prefixed_col_names(response_data())
    validate(need(length(response_var) == 1, "Response variable should be of lenght one."))
    regressor_var <- get_dataset_prefixed_col_names(regressor_data())
    merged_dataset <- merge_datasets(list(response_data(), regressor_data()))
    validate_has_data(merged_dataset, 10)

    form <- stats::as.formula(
      paste(
        response_var,
        paste(
          regressor_var,
          collapse = " + "
        ),
        sep = " ~ "
      )
    )

    chunks_reset()

    chunks_push(
      expression = quote(fit <- lm(form, data = merged_dataset)) %>% substituteDirect(list(form = form))
    )

    chunks_eval()
  })

  output$plot <- renderPlot({

    fit()

    if (input$plot_type == "Response vs Regressor") {
      fit <- chunks_get_var("fit")

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
        "Normal Q-Q",
        "Scale-Location",
        "Cook's distance",
        "Residuals vs Leverage",
        "Cook's dist vs Leverage h[ii]/(1 - h[ii])"
      ))
      expr <- bquote(plot(fit, which = .(i), id.n = NULL))
      plot %<chunk%
        expr
    }

    p <- chunks_eval()

    chunks_validate_is_ok()

    p
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
    chunks_push(expression = quote(summary(fit)), id = "summary")
    chunks_eval()
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
        merged_dataname = "merged_dataset",
        merged_datasets = list(response_data(), regressor_data()),
        title = title
      )
    )
  })
}
