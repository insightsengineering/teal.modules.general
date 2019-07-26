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
#' @export
#'
#' @examples
#' library(random.cdisc.data)
#' library(tern)
#'
#'
#' # datasets: same wide
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' keys(ASL) <- c("STUDYID", "USUBJID")
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     code = 'ASL <- cadsl
#'             keys(ASL) <- c("STUDYID", "USUBJID")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_regression(
#'       label = "Regression",
#'       dataname = c("ASL", "ALB"),
#'       response = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = c("BMRKR1", "BMRKR2"),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "variable"
#'         )
#'       ),
#'       regressor = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = c("AGE", "SEX", "RACE"),
#'           selected = c("AGE"),
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
#'
#' # datasets: different wide
#'
#' library(random.cdisc.data)
#' library(tern)
#' library(dplyr)
#'
#' ASL <- cadsl
#' ASL <- mutate_at(ASL,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = funs(as.factor(.))) %>% select("ARM", "ACTARM", "ACTARMCD",
#'  "SEX", "STRATA1", "AGE", "USUBJID", "STUDYID", "STRATA2")
#' keys(ASL) <- c("STUDYID", "USUBJID")
#'
#'
#' ASL_2 <- mutate_at(cadsl,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = funs(as.factor(.))) %>% select("ACTARM", "AGE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
#' keys(ASL_2) <- c("STUDYID", "USUBJID")
#'
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ASL_2 = ASL_2,
#'     code = 'ASL <- cadsl
#'             ASL <- mutate_at(ASL,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = funs(as.factor(.))) %>% select("ARM", "ACTARM", "ACTARMCD",
#'                      "SEX", "STRATA1", "AGE", "USUBJID", "STUDYID", "STRATA2")
#'             ASL_2 <- mutate_at(cadsl,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = funs(as.factor(.))) %>% select("ACTARM", "AGE", "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
#'             keys(ASL) <- keys(ASL_2) <- c("STUDYID", "USUBJID")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_regression(
#'       label = "Regression",
#'       dataname = c("ASL", "ASL_2"),
#'       response = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'          label = "Select variable",
#'           choices = c("AGE", "SEX", "STRATA1", "RACE"),
#'           selected = c("AGE"),
#'           multiple = FALSE
#'         )),
#'       regressor = data_extract_spec(
#'         dataname = "ASL_2",
#'         columns = columns_spec(
#'           label = "Select variables",
#'           choices = c("COUNTRY", "AGE", "RACE"),
#'           selected = c("AGE", "COUNTRY", "RACE"),
#'           multiple = TRUE
#'         ))
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: same long
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ADLB <- cadlb
#'
#' keys(ASL) <-  c("STUDYID", "USUBJID")
#' keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ADLB = ADLB,
#'     code = 'ASL <- cadsl
#'             ADLB <- cadlb
#'             keys(ASL) <-  c("STUDYID", "USUBJID")
#'             keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_regression(
#'       label = "Regression",
#'       dataname = c("ASL", "ADLB"),
#'       response = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAM",
#'             choices = levels(ADLB$PARAM),
#'             selected = levels(ADLB$PARAM)[c(1,2)],
#'             multiple = TRUE,
#'             label = "Choose measurement"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = TRUE,
#'             label = "Choose visit"
#'           )
#'         ),
#'         columns = columns_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "variable"
#'         )
#'       ),
#'       regressor = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAM",
#'             choices = levels(ADLB$PARAM),
#'             selected = levels(ADLB$PARAM)[1],
#'             multiple = FALSE,
#'             label = "Choose measurement"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Choose visit"
#'           )
#'         ),
#'         columns = columns_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           label = "variable"
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
#' # datasets: wide and long
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ADLB <- cadlb
#'
#' keys(ASL) <- c("STUDYID", "USUBJID")
#' keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#' app <- init(
#'  data = cdisc_data(
#'    ASL = ASL,
#'    ADLB = ADLB,
#'    code = 'ASL <- cadsl
#'            ADLB <- cadlb
#'            keys(ASL) <- c("STUDYID", "USUBJID")
#'            keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'    check = FALSE),
#'  modules = root_modules(
#'    tm_g_regression(
#'      label = "Regression",
#'      dataname = c("ASL", "ADLB"),
#'      response = data_extract_spec(
#'        dataname = "ADLB",
#'        filter = list(
#'          filter_spec(
#'            vars = "PARAM",
#'           choices = levels(ADLB$PARAM),
#'            selected = levels(ADLB$PARAM)[1],
#'            multiple = FALSE,
#'            label = "Choose measurement"
#'          ),
#'          filter_spec(
#'            vars = "AVISIT",
#'            choices = levels(ADLB$AVISIT),
#'            selected = levels(ADLB$AVISIT)[1],
#'            multiple = FALSE,
#'            label = "Choose visit"
#'          )
#'        ),
#'        columns = columns_spec(
#'          choices = "AVAL",
#'          selected = "AVAL",
#'          multiple = FALSE,
#'          fixed = FALSE,
#'          label = "variable"
#'        )
#'     ),
#'      regressor = data_extract_spec(
#'        dataname = "ASL",
#'        columns = columns_spec(
#'          choices = c("BMRKR1", "BMRKR2"),
#'          selected = c("BMRKR1", "BMRKR2"),
#'          multiple = TRUE,
#'          fixed = FALSE
#'       )
#'      )
#'    )
#' )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: wide, long, long
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ADLB <- cadlb
#' ADRS <- cadrs
#'
#' keys(ASL) <- c("STUDYID", "USUBJID")
#' keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#' keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ADLB = ADLB,
#'     code = 'ASL <- cadsl
#'             ADLB <- cadlb
#'             keys(ASL) <- c("STUDYID", "USUBJID")
#'             keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'             keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_regression(
#'       label = "Regression",
#'       dataname = c("ASL", "ADLB", "ADRS"),
#'       response = list(data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAM",
#'             choices = levels(ADLB$PARAM),
#'             selected = levels(ADLB$PARAM)[1],
#'             multiple = FALSE,
#'             label = "Choose measurement"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Choose visit"
#'           )
#'         ),
#'         columns = columns_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "variable"
#'         )
#'       ),
#'       data_extract_spec(
#'         dataname = "ADRS",
#'         filter = list(
#'           filter_spec(
#'             vars = "ARMCD",
#'             choices = levels(ADRS$ARMCD),
#'             selected = levels(ADRS$ARMCD)[1],
#'             multiple = FALSE,
#'             label = "Choose ARM"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADRS$AVISIT),
#'             selected = levels(ADRS$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Choose visit"
#'           )
#'         ),
#'         columns = columns_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )),
#'       regressor = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = c("BMRKR1", "BMRKR2"),
#'           selected = c("BMRKR1"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: multiple long datasets
#' library(random.cdisc.data)
#'
#' ASL <- cadsl
#' ADRS <- cadrs
#' ADTTE <- cadtte
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ADRS = ADRS,
#'     ADTTE = ADTTE,
#'     code = "ASL <- cadsl; ADRS <- cadrs; ADTTE <- cadtte",
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_regression(
#'       label = "Regression Analysis on two long datasets",
#'       dataname = c("ASL", "ADRS", "ADTTE"),
#'       response = data_extract_spec(
#'         dataname = "ADTTE",
#'         columns = columns_spec(
#'           choices = c("AVAL", "CNSR"),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         ),
#'         filter = filter_spec(
#'           vars = c("PARAMCD"),
#'           choices = unique(ADTTE$PARAMCD),
#'           selected = "OS",
#'           multiple = FALSE,
#'           label = "ADTTE filter"
#'         )
#'       ),
#'       regressor = list(
#'         data_extract_spec(
#'           dataname = "ADRS",
#'           columns = columns_spec(
#'             choices = names(ADRS),
#'             selected = "AVAL",
#'             multiple = TRUE,
#'             fixed = FALSE
#'           ),
#'           filter = filter_spec(
#'             vars = c("PARAMCD", "AVISIT"),
#'             choices = apply(expand.grid(unique(ADRS$PARAMCD), unique(ADRS$AVISIT)), 1, paste, collapse = " - "),
#'             selected = "OVRINV - Screening",
#'             multiple = TRUE,
#'             label = "ADRS filter"
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ASL",
#'           columns = columns_spec(
#'             choices = c("BMRKR1", "BMRKR2"),
#'             selected = c("BMRKR1"),
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
tm_g_regression <- function(label = "Regression Analysis",
                            dataname,
                            regressor,
                            response,
                            plot_height = c(600, 200, 2000),
                            pre_output = NULL,
                            post_output = NULL) {
  if (!is.class.list("data_extract_spec")(regressor)) {
    regressor <- list(regressor)
  }
  if (!is.class.list("data_extract_spec")(response)) {
    response <- list(response)
  }

  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  stopifnot(is.class.list("data_extract_spec")(response))
  stop_if_not(list(all(vapply(response, function(x) !isTRUE(x$columns$multiple), logical(1))),
                   "Response variable should not allow multiple selection"))
  stopifnot(is.class.list("data_extract_spec")(regressor))
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
        merge_expression = "",
        title = title
      )
    )
  })
}
