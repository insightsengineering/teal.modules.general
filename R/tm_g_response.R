#' Response Plots
#'
#'
#' @param dataname (\code{character}) Name of dataset used to generate the response plot
#' @param response (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Which variable to use as the response. You can define one fixed column by using the
#'   setting \code{fixed = TRUE} inside the \code{select_spec}.
#' @param x (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Which variable to use on the X-axis of the response plot. Allow the user to select multiple
#'   columns from the \code{data} allowed in teal. Just allow single columns by \code{multiple = FALSE}.
#' @param row_facet optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Which data columns to use for faceting rows.  Just allow single columns by \code{multiple = FALSE}.
#' @param col_facet optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Which data to use for faceting columns. Just allow single columns by \code{multiple = FALSE}.
#' @param coord_flip (\code{logical}) Whether to flip coordinates
#' @param freq (\code{logical}) Display frequency (\code{TRUE}) or density (\code{FALSE}).
#' @param plot_height (\code{numeric}) Vector of length three with \code{c(value, min and max)}.
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @export
#' @examples
#' # datasets: same wide
#' # Response plot with selected response (BMRKR1) and selected x variable (RACE)
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- cadsl",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_response(
#'       label = "Response Plots",
#'       dataname = c("ADSL"),
#'       response = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("BMRKR1", "BMRKR2"),
#'           selected = "BMRKR1",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("AGE", "SEX", "RACE"),
#'           selected = "RACE",
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
#' # datasets: different wide
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ADSL <- cadsl
#' ADSL <- mutate_at(ADSL,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = list(~as.factor(.))) %>% select("ARM", "ACTARM", "ACTARMCD",
#'  "SEX", "STRATA1", "AGE", "USUBJID", "STUDYID", "STRATA2")
#'
#' ADSL_2 <- mutate_at(cadsl,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = list(~as.factor(.))) %>% select("ACTARM", "AGE", "STRATA2", "COUNTRY",
#'                  "USUBJID", "STUDYID")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     dataset("ADSL_2", ADSL_2),
#'     code = 'ADSL <- cadsl
#'             ADSL <- mutate_at(ADSL,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = list(~as.factor(.))) %>% select("ARM", "ACTARM", "ACTARMCD",
#'                      "SEX", "BMRKR1", "AGE", "USUBJID", "STUDYID", "BMRKR2")
#'             ADSL_2 <- mutate_at(cadsl,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = list(~as.factor(.))) %>% select("ACTARM", "AGE", "STRATA2",
#'                  "COUNTRY", "USUBJID", "STUDYID")',
#'     check = FALSE #TODO
#'   ),
#'   modules = root_modules(
#'     tm_g_response(
#'       label = "Response Plots",
#'       dataname = c("ADSL", "ADSL_2"),
#'       response = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("BMRKR1", "BMRKR2"),
#'           selected = c("BMRKR1"),
#'           multiple = FALSE
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "ADSL_2",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("COUNTRY", "AGE", "RACE"),
#'           selected = "COUNTRY",
#'           multiple = FALSE
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
#' # Response plot of different parameters from ADRS or ADLB datasets
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#' ADLB <- cadlb
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = "ADSL <- cadsl; ADRS <- cadrs; ADLB <- cadlb",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_response(
#'       label = "Response Plot on two long datasets",
#'       dataname = c("ADSL", "ADRS", "ADLB"),
#'       response = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             label = "Select parameter:",
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE
#'           ),
#'           filter_spec(
#'             label = "Select visit:",
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE
#'           )
#'         ),
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("AVAL", "AVALC"),
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "ADRS",
#'         filter = list(
#'           filter_spec(
#'             label = "Select parameter:",
#'             vars = "PARAMCD",
#'             choices = levels(ADRS$PARAMCD),
#'             selected = levels(ADRS$PARAMCD)[1],
#'             multiple = FALSE
#'           ),
#'           filter_spec(
#'             label = "Select visit:",
#'             vars = "AVISIT",
#'             choices = levels(ADRS$AVISIT),
#'             selected = levels(ADRS$AVISIT)[1],
#'             multiple = FALSE
#'           )
#'         ),
#'         select = select_spec(
#'           choices = "AVALC",
#'           selected = "AVALC",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       ),
#'       row_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("SEX", "AGE"),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("SEX", "AGE"),
#'           selected = NULL,
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
#' # datasets: wide and long
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADLB <- cadlb
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = "ADSL <- cadsl; ADLB <- cadlb",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_response(
#'       label = "Response Plots",
#'       dataname = c("ADSL", "ADLB"),
#'       response = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAM",
#'             choices = levels(ADLB$PARAM),
#'             selected = levels(ADLB$PARAM)[1],
#'             multiple = FALSE,
#'             label = "Select measurement:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
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
#' # datasets: same long dataset
#' # Examine response with respect to chosen explanatory variable, split by PARAMCD and AVISIT
#'
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     code = "ADSL <- cadsl; ADRS <- cadrs",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_response(
#'       dataname = "ADRS",
#'       response = data_extract_spec(
#'         dataname = "ADRS",
#'         select = select_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = TRUE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "ADRS",
#'         select = select_spec(
#'           choices = c("ARMCD", "ACTARMCD"),
#'           selected = "ARMCD",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       row_facet = data_extract_spec(
#'         dataname = "ADRS",,
#'         select = select_spec(
#'           choices = "PARAMCD",
#'           selected = "PARAMCD",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ADRS",
#'         select = select_spec(
#'           choices = "AVISIT",
#'           selected = "AVISIT",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#'
#' # datasets: different subsets of long dataset
#' # Examine lab values with respect to chosen explanatory variable, split by PARAMCD and AVISIT
#'
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADLB <- cadlb
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = "ADSL <- cadsl; ADLB <- cadlb",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_response(
#'       dataname = "ADLB",
#'       response = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Select lab:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[2],
#'             multiple = FALSE,
#'             label = "Select lab:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       ),
#'       row_facet = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Select lab:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = c("SEX", "RACE"),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select Variable"
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Select lab:"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Select visit:"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = c("SEX", "RACE"),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_response <- function(label = "Response Plot",
                          dataname,
                          response,
                          x,
                          row_facet = NULL,
                          col_facet = NULL,
                          coord_flip = TRUE,
                          freq = FALSE,
                          plot_height = c(600, 400, 5000),
                          pre_output = NULL,
                          post_output = NULL) {
  if (!is.class.list("data_extract_spec")(response)) {
    response <- list(response)
  }
  if (!is.class.list("data_extract_spec")(x)) {
    x <- list(x)
  }
  if (!is.class.list("data_extract_spec")(row_facet)) {
    row_facet <- list_or_null(row_facet)
  }
  if (!is.class.list("data_extract_spec")(col_facet)) {
    col_facet <- list_or_null(col_facet)
  }

  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  # No empty columns allowed for Response Var
  # No multiple Response variables allowed
  stopifnot(is.class.list("data_extract_spec")(response))
  # todo: this check should go into data_extract or at least create a function for reusability (add an argument allow_empty_values)
  # todo: also refactor this in tm_g_response.R to use this function
  stop_if_not(list(all(vapply(response, function(x) !("" %in% x$select$choices), logical(1))),
                   "'response' should not allow empty values"))
  stop_if_not(list(all(vapply(response, function(x) !(x$select$multiple), logical(1))),
                   "'response' should not allow multiple selection"))
  # No empty columns allowed for X-Var
  # No multiple X variables allowed
  stopifnot(is.class.list("data_extract_spec")(x))
  stop_if_not(list(all(vapply(x, function(x) !("" %in% x$select$choices), logical(1))),
                   "'x' should not allow empty values"))
  stop_if_not(list(all(vapply(x, function(x) !(x$select$multiple), logical(1))),
                   "'x' should not allow multiple selection"))
  stopifnot(is.null(row_facet) || is.class.list("data_extract_spec")(row_facet))
  stopifnot(is.null(col_facet) || is.class.list("data_extract_spec")(col_facet))
  stopifnot(is.logical.single(coord_flip))
  stopifnot(is.logical.single(freq))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])


  args <- as.list(environment())

  module(
    label = label,
    server = function(input, output, session, datasets, ...) return(NULL),
    ui = ui_g_response,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      response = response,
      x = x,
      row_facet = row_facet,
      col_facet = col_facet
    ),
    filters = "all"
  )
}

ui_g_response <- function(id, ...) {
  arguments <- list(...)

  ns <- NS(id)

  standard_layout(
    output = white_small_well(
      plot_height_output(id = ns("myplot"))
    ),
    encoding = div(
        helpText("Datasets: ", lapply(arguments$dataname, tags$code)),

      data_extract_input(
        id = ns("response"),
        label = "Response variable",
        data_extract_spec = arguments$response
      ),
      data_extract_input(
        id = ns("x"),
        label = "X variable",
        data_extract_spec = arguments$x
      ),
      if (!is.null(arguments$row_facet)) {
        data_extract_input(
          id = ns("row_facet"),
          label = "Row facetting",
          data_extract_spec = arguments$row_facet
        )
      },
      if (!is.null(arguments$col_facet)) {
        data_extract_input(
          id = ns("col_facet"),
          label = "Column facetting",
          data_extract_spec = arguments$col_facet
        )
      },
      radioButtons(
        ns("freq"),
        NULL,
        choices = c("frequency", "density"),
        selected = ifelse(arguments$freq, "frequency", "density"),
        inline = TRUE
      ),
      checkboxInput(ns("coord_flip"), "Swap axes", value = arguments$coord_flip),
      plot_height_input(id = ns("myplot"), value = arguments$plot_height)
    ),
    forms = actionButton(ns("show_rcode"), "Show R code", width = "100%"),
    pre_output = arguments$pre_output,
    post_output = arguments$post_output
  )
}

#' @importFrom forcats fct_rev
#' @importFrom magrittr %>%
#' @importFrom methods substituteDirect
srv_g_response <- function(input,
                           output,
                           session,
                           datasets,
                           dataname,
                           response,
                           x,
                           row_facet,
                           col_facet) {
  stopifnot(all(dataname %in% datasets$datanames()))

  init_chunks()

  # Data Extraction
  response_data <- callModule(data_extract_module,
                              id = "response",
                              datasets = datasets,
                              data_extract_spec = response
  )
  x_data <- callModule(data_extract_module,
                          id = "x",
                          datasets = datasets,
                          data_extract_spec = x
  )
  row_facet_data <- callModule(data_extract_module,
                                   id = "row_facet",
                                   datasets = datasets,
                                   data_extract_spec = row_facet
  )
  col_facet_data <- callModule(data_extract_module,
                                   id = "col_facet",
                                   datasets = datasets,
                                   data_extract_spec = col_facet
  )

  data_reactive <- reactive({

    merge_datasets(
      list(
        x_data(),
        row_facet_data(),
        col_facet_data(),
        response_data()
      )
    )

  })

  # Insert the plot into a plot_height module from teal.devel
  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("plot"), height = plot_height)
  })

  output$plot <- renderPlot({
    resp_var <- get_dataset_prefixed_col_names(response_data())
    x <- get_dataset_prefixed_col_names(x_data())
    row_facet_name <- get_dataset_prefixed_col_names(row_facet_data())
    col_facet_name <- get_dataset_prefixed_col_names(col_facet_data())

    validate(need(resp_var != "", "Please define a valid column for the response variable"))
    validate(need(x != "", "Please define a valid column for the X-variable"))

    freq <- input$freq == "frequency"
    swap_axes <- input$coord_flip

    arg_position <- if (freq) "stack" else "fill" # nolint
    cl_arg_x <- if (is.null(x)) {
      1
    } else {
      tmp_cl <- if (length(x) == 1) {
        as.name(x)
      } else {
        tmp <- call_fun_dots("interaction", x)
        tmp[["sep"]] <- " x "
        tmp
      }

      if (swap_axes) {
        bquote(forcats::fct_rev(.(tmp_cl)))
      } else {
        tmp_cl
      }
    }

    anl <- data_reactive()

    validate_has_data(anl, 10)

    validate(
      need(is.factor(anl[[resp_var]]), "Please select a factor variable as the name.")
    )

    plot_call <- bquote(
      anl %>%
        ggplot() +
        aes(x = .(cl_arg_x)) +
        geom_bar(aes(fill = .(as.name(resp_var))), position = .(arg_position))
    )

    if (!freq) {
      if (swap_axes) {
        tmp_cl1 <- quote(xlab(label)) %>%
          substituteDirect(list(label = tmp_cl %>%
                                  deparse()))
        tmp_cl2 <- quote(expand_limits(y = c(0, 1.4)))
      } else {
        tmp_cl1 <- quote(geom_text(stat = "count", aes(label = ..count.., vjust = -1), position = "fill")) # nolint
        tmp_cl2 <- quote(expand_limits(y = c(0, 1.2)))
      }

      plot_call <- call("+", call("+", plot_call, tmp_cl1), tmp_cl2)
    } else {
      # Change Y-Axis Label in case of Swap
      tmp_cl1 <- quote(xlab(label)) %>%
        substituteDirect(list(label = tmp_cl %>%
                                deparse()))
      plot_call <- call("+", plot_call, tmp_cl1)
    }

    if (swap_axes) {
      plot_call <- call("+", plot_call, quote(coord_flip()))
    }

    facet_cl <- facet_ggplot_call(row_facet_name, col_facet_name)

    if (!is.null(facet_cl)) {
      plot_call <- call("+", plot_call, facet_cl)
    }

    chunks_reset()

    chunks_push(expression = plot_call, id = "plotCall")

    p <- chunks_eval()

    chunks_validate_is_ok()

    p
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Response Plot",
      rcode = get_rcode(
        datasets = datasets,
        merge_expression = "",
        title = "Response Plot"
      )
    )
  })
}
