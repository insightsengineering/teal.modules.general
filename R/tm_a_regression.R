#' @include utils.R
NULL

#' Scatterplot and Regression Model
#'
#' Scatterplot and Regression Model
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#' @param regressor (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'  Regressor variables from an incoming dataset with filtering and selecting.
#' @param response (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'  Response variables from an incoming dataset with filtering and selecting.
#' @param default_plot_type optional, (\code{numeric}) Defaults to Response vs Regressor.
#' \itemize{
#'  \item{1 }{Response vs Regressor}
#'  \item{2 }{Residuals vs Fitted}
#'  \item{3 }{Normal Q-Q}
#'  \item{4 }{Scale-Location}
#'  \item{5 }{Cooks distance}
#'  \item{6 }{Residuals vs Leverage}
#'  \item{7 }{Cooks dist vs Leverage}
#' }
#'
#' @note For more examples, please see the vignette "Using regression plots" via
#'   \code{vignette("using-regression-plots", package = "teal.modules.general")}.
#' @export
#'
#' @examples
#' # Regression graphs from selected response variable (BMRKR1) and
#' # selected regressors (AGE)
#' library(random.cdisc.data)
#'
#' ADSL <- radsl(cached = TRUE)
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- radsl(cached = TRUE)",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_a_regression(
#'       label = "Regression",
#'       response = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = "BMRKR1",
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       ),
#'       regressor = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = variable_choices(ADSL, c("AGE", "SEX", "RACE")),
#'           selected = "AGE",
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
tm_a_regression <- function(label = "Regression Analysis",
                            regressor,
                            response,
                            plot_height = c(600, 200, 2000),
                            pre_output = NULL,
                            post_output = NULL,
                            default_plot_type = 1
                            ) {
  if (!is_class_list("data_extract_spec")(regressor)) {
    regressor <- list(regressor)
  }
  if (!is_class_list("data_extract_spec")(response)) {
    response <- list(response)
  }

  stopifnot(is_character_single(label))
  stopifnot(is_class_list("data_extract_spec")(response))
  stop_if_not(list(all(vapply(response, function(x) !isTRUE(x$select$multiple), logical(1))),
                   "Response variable should not allow multiple selection"))
  stopifnot(is_class_list("data_extract_spec")(regressor))
  stopifnot(is_numeric_vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])
  # No check necessary for regressor and response, as checked in data_extract_input

  # Send ui args
  args <- as.list(environment())

  data_extract_list <- list(
    regressor = regressor,
    response = response
  )

  module(
    label = label,
    server = srv_a_regression,
    ui = ui_a_regression,
    ui_args = args,
    server_args = data_extract_list,
    filters = get_extract_datanames(data_extract_list)
  )
}


ui_a_regression <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)

  plot_choices <- c(
      "Response vs Regressor",
      "Residuals vs Fitted",
      "Normal Q-Q", "Scale-Location", "Cook's distance", "Residuals vs Leverage",
      "Cook's dist vs Leverage h[ii]/(1 - h[ii])"
  )

  standard_layout(
    output = white_small_well(
      tags$div(
        plot_height_output(id = ns("myplot")),
        tags$div(verbatimTextOutput(ns("text")))
      )
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("response", "regressor")]),
      data_extract_input(
        id = ns("response"),
        label = "Response variable",
        data_extract_spec = args$response
      ),
      data_extract_input(
        id = ns("regressor"),
        label = "Regressor variables",
        data_extract_spec = args$regressor
      ),
      radioButtons(
        ns("plot_type"),
        label = "Plot type:",
        choices = plot_choices,
        selected = plot_choices[args$default_plot_type]
      ),
      plot_height_input(id = ns("myplot"), value = args$plot_height)
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}


#' @importFrom graphics plot abline
#' @importFrom magrittr %>%
#' @importFrom methods is substituteDirect
#' @importFrom stats as.formula
srv_a_regression <- function(input, output, session, datasets, response, regressor) {
  init_chunks()

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(response, regressor),
    input_id = c("response", "regressor")
  )

  # Insert the plot into a plot_height module from teal.devel
  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  # sets chunk object and populates it with data merge call and fit expression
  fit <- reactive({
    chunks_reset()
    chunks_push_data_merge(merged_data())

    ANL <- chunks_get_var("ANL") # nolint
    validate_has_data(ANL, 10)

    response_var <- as.vector(merged_data()$columns_source$response)
    regressor_var <- as.vector(merged_data()$columns_source$regressor)

    # validation
    validate(need(length(regressor_var) > 0, "At least one regressor should be selected."))
    validate(need(length(response_var) == 1, "Response variable should be of length one."))
    validate(need(is.numeric(ANL[response_var][[1]]), "Response variable should be numeric."))
    if (input$plot_type == "Response vs Regressor") {
      validate(need(length(regressor_var) == 1, "Response vs Regressor is only provided for exactly one regressor"))
    }

    validate_has_data(ANL[, c(response_var, regressor_var)], 10, complete = TRUE)

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

    chunks_push(
      expression = quote(fit <- lm(form, data = ANL)) %>% substituteDirect(list(form = form))
    )
    chunks_safe_eval()
    return(form)
  })

  output$plot <- renderPlot({
    fit()

    if (input$plot_type == "Response vs Regressor") {
      fit <- chunks_get_var("fit") # chunk already evaluated
      response_var <- as.vector(merged_data()$columns_source$response)
      regressor_var <- as.vector(merged_data()$columns_source$regressor)
      ANL <- chunks_get_var("ANL") # nolint

      if (ncol(fit$model) > 1) {
        stopifnot(ncol(fit$model) == 2)
        chunks_push(bquote({
          plot(fit$model[, 2:1],
                               main = "Response vs Regressor",
                               xlab = .(varname_w_label(regressor_var, ANL)),
                               ylab = .(varname_w_label(response_var, ANL)))
        }))
        if (is.numeric(ANL[[regressor_var]])) {
          chunks_push(quote(abline(fit, col = "red", lwd = 2L)))
        }
      } else {
        ANL <- chunks_get_var("ANL") # nolint
        chunks_push(quote({
          plot_data <- data.frame(fit$model[, 1], fit$model[, 1])
          names(plot_data) <- rep(names(fit$model), 2)
          plot <- plot(plot_data, main = "Response vs Regressor")
          abline(ANL)
        }))
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
      chunks_push(bquote(plot(fit, which = .(i), id.n = NULL)))
    }

    chunks_safe_eval()
  })

  output$text <- renderPrint({
    fit()
    chunks_push(expression = quote(summary(fit)), id = "summary")
    chunks_safe_eval()
  })

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(response, regressor)),
    modal_title = "R code for the regression plot",
    code_header = paste0(
      "Regression plot of ",
      format(fit())
    )
  )
}
