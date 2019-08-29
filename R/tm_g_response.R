#' Response Plots
#'
#'
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
#' # bug: lapply(selector_list, check_selector) this function throw an error if some
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
#'       response = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("BMRKR2", "COUNTRY"),
#'           selected = "BMRKR2",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("SEX", "RACE"),
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
#' ADSL <- select(ADSL, "ARM", "ACTARM", "ACTARMCD", "SEX",
#'   "AGE", "USUBJID", "STUDYID", "BMRKR1", "BMRKR2")
#' ADSL_2 <- select(cadsl, "ACTARM", "AGE", "RACE",
#'   "STRATA2", "COUNTRY", "USUBJID", "STUDYID")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     dataset("ADSL_2", ADSL_2, keys = get_cdisc_keys("ADSL")),
#'     code = 'ADSL <- cadsl
#'             ADSL <- select(ADSL, "ARM", "ACTARM", "ACTARMCD", "SEX",
#'               "AGE", "USUBJID", "STUDYID", "BMRKR1", "BMRKR2")
#'             ADSL_2 <- select(cadsl, "ACTARM", "AGE", "RACE",
#'               "STRATA2", "COUNTRY", "USUBJID", "STUDYID")',
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_response(
#'       label = "Response Plots",
#'       response = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("BMRKR2", "SEX"),
#'           selected = c("BMRKR2"),
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
#' library(dplyr)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#' ADLB <- mutate(cadlb, ABLFL2 = as.factor(ABLFL2))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = "ADSL <- cadsl; ADRS <- cadrs; ADLB <- mutate(cadlb, ABLFL2 = as.factor(ABLFL2))",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_response(
#'       label = "Response Plot on two long datasets",
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
#'           choices = c("BEP01FL"),
#'           selected = "BEP01FL",
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
#'           choices = "STRATA2",
#'           selected = "STRATA2",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       ),
#'       row_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("SEX"),
#'           selected = NULL,
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = c("SEX", "COUNTRY"),
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
#' library(dplyr)
#'
#' ADSL <- cadsl
#' ADLB <- mutate(cadlb, ABLFL2 = as.factor(ABLFL2))
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = "ADSL <- cadsl; ADLB <- mutate(cadlb, ABLFL2 = as.factor(ABLFL2))",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_response(
#'       label = "Response Plots",
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
#'           choices = "BEP01FL",
#'           selected = "BEP01FL",
#'           multiple = FALSE,
#'           fixed = FALSE,
#'           label = "Select variable:"
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           choices = c("BMRKR2", "COUNTRY"),
#'           selected = c("BMRKR2"),
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
#'       response = data_extract_spec(
#'         dataname = "ADRS",
#'         select = select_spec(
#'           choices = "BMRKR2",
#'           selected = "BMRKR2",
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
#'       response = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Select lab:"
#'         ),
#'         select = select_spec(
#'           choices = "BEP01FL",
#'           selected = "BEP01FL",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[2],
#'             multiple = FALSE,
#'             label = "Select lab:"
#'         ),
#'         select = select_spec(
#'           choices = "AVISIT",
#'           selected = "AVISIT",
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
    server = srv_g_response,
    ui = ui_g_response,
    ui_args = args,
    server_args = list(
      response = response,
      x = x,
      row_facet = row_facet,
      col_facet = col_facet
    ),
    filters = "all"
  )
}

ui_g_response <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)

  standard_layout(
    output = white_small_well(
      plot_height_output(id = ns("myplot"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("response", "x", "row_facet", "col_facet")]),
      data_extract_input(
        id = ns("response"),
        label = "Response variable",
        data_extract_spec = args$response
      ),
      data_extract_input(
        id = ns("x"),
        label = "X variable",
        data_extract_spec = args$x
      ),
      if (!is.null(args$row_facet)) {
        data_extract_input(
          id = ns("row_facet"),
          label = "Row facetting",
          data_extract_spec = args$row_facet
        )
      },
      if (!is.null(args$col_facet)) {
        data_extract_input(
          id = ns("col_facet"),
          label = "Column facetting",
          data_extract_spec = args$col_facet
        )
      },
      radioButtons(
        ns("freq"),
        NULL,
        choices = c("frequency", "density"),
        selected = ifelse(args$freq, "frequency", "density"),
        inline = TRUE
      ),
      checkboxInput(ns("coord_flip"), "Swap axes", value = args$coord_flip),
      plot_height_input(id = ns("myplot"), value = args$plot_height)
    ),
    forms = actionButton(ns("show_rcode"), "Show R code", width = "100%"),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

#' @importFrom forcats fct_rev
#' @importFrom magrittr %>%
#' @importFrom methods substituteDirect
#' @importFrom utils.nest is.character.empty
srv_g_response <- function(input,
                           output,
                           session,
                           datasets,
                           response,
                           x,
                           row_facet,
                           col_facet) {
  init_chunks(session)

  data_extract <- list(response, x, row_facet, col_facet)
  names(data_extract) <- c("response", "x", "row_facet", "col_facet")
  data_extract <- data_extract[!vapply(data_extract, is.null, logical(1))]

  merged_data <- data_merge_module(
      datasets = datasets,
      data_extract = data_extract,
      input_id = names(data_extract)
  )

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

      ANL <- merged_data()$data() # nolint
      chunks_reset()

      resp_var <- merged_data()$columns_source$response
      x <- merged_data()$columns_source$x

      row_facet_name <- merged_data()$columns_source$col_facet
      col_facet_name <- merged_data()$columns_source$row_facet

      validate(need(!is.character.empty(x), "Please define a valid column for the response variable"))
      validate(need(!is.character.empty(x), "Please define a valid column for the X-variable"))


      freq <- input$freq == "frequency"
      swap_axes <- input$coord_flip

      arg_position <- if (freq) "stack" else "fill" # nolint
      cl_arg_x <- if (is.character.empty(x)) {
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

      validate_has_data(ANL, 10)

      validate(
          need(is.factor(ANL[[resp_var]]), "Please select a factor variable as the response.")
      )
      validate(
          need(is.factor(ANL[[x]]), "Please select a factor variable as the X-Variable.")
      )

      plot_call <- bquote(
          ANL %>%
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

      if(!is.character.empty(row_facet_name) || !is.character.empty(col_facet_name)) {
        facet_cl <- facet_ggplot_call(row_facet_name, col_facet_name)
        if (!is.null(facet_cl)) {
          plot_call <- call("+", plot_call, facet_cl)
        }
      }


      chunks_push(expression = plot_call, id = "plotCall")

      p <- chunks_eval()

      chunks_validate_is_ok()

      p
    })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "R Code for a Scatterplotmatrix",
      rcode = get_rcode(
        datasets = datasets,
        merge_expression = merged_data()$expr,
        title = "",
        description = ""
      )
    )
  })
}
