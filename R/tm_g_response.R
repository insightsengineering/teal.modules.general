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
#' @param rotate_xaxis_labels (\code{logical}) Wheater to rotate plot X axis labels
#' @param freq (\code{logical}) Display frequency (\code{TRUE}) or density (\code{FALSE}).
#' @param plot_height (\code{numeric}) Vector of length three with \code{c(value, min and max)}.
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @export
#' @examples
#' # Response plot with selected response (BMRKR1) and selected x variable (RACE)
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
#'     tm_g_response(
#'       label = "Response Plots",
#'       response = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL, c("BMRKR2", "COUNTRY")),
#'           selected = "BMRKR2",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL, c("SEX", "RACE")),
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
tm_g_response <- function(label = "Response Plot",
                          response,
                          x,
                          row_facet = NULL,
                          col_facet = NULL,
                          coord_flip = TRUE,
                          rotate_xaxis_labels = FALSE,
                          freq = FALSE,
                          plot_height = c(600, 400, 5000),
                          pre_output = NULL,
                          post_output = NULL) {
  if (!is_class_list("data_extract_spec")(response)) {
    response <- list(response)
  }
  if (!is_class_list("data_extract_spec")(x)) {
    x <- list(x)
  }
  if (!is_class_list("data_extract_spec")(row_facet)) {
    row_facet <- list_or_null(row_facet)
  }
  if (!is_class_list("data_extract_spec")(col_facet)) {
    col_facet <- list_or_null(col_facet)
  }

  stopifnot(is_character_single(label))
  stopifnot(is_class_list("data_extract_spec")(response))
  stop_if_not(list(all(vapply(response, function(x) !("" %in% x$select$choices), logical(1))),
                   "'response' should not allow empty values"))
  stop_if_not(list(all(vapply(response, function(x) !(x$select$multiple), logical(1))),
                   "'response' should not allow multiple selection"))
  stopifnot(is_class_list("data_extract_spec")(x))
  stop_if_not(list(all(vapply(x, function(x) !("" %in% x$select$choices), logical(1))),
                   "'x' should not allow empty values"))
  stop_if_not(list(all(vapply(x, function(x) !(x$select$multiple), logical(1))),
                   "'x' should not allow multiple selection"))
  stopifnot(is.null(row_facet) || is_class_list("data_extract_spec")(row_facet))
  stopifnot(is.null(col_facet) || is_class_list("data_extract_spec")(col_facet))
  stopifnot(is_logical_single(coord_flip))
  stopifnot(is_logical_single(rotate_xaxis_labels))
  stopifnot(is_logical_single(freq))
  stopifnot(is_numeric_vector(plot_height) && length(plot_height) == 3)
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
      radioGroupButtons(
        inputId = ns("freq"),
        label = NULL,
        choices = c("frequency", "density"),
        selected = ifelse(args$freq, "frequency", "density"),
        justified = TRUE
      ),
      plot_height_input(id = ns("myplot"), value = args$plot_height),
      panel_group(
        panel_item(
          title = "Plot settings",
          checkboxInput(ns("coord_flip"), "Swap axes", value = args$coord_flip),
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = args$rotate_xaxis_labels)
        )
      )
    ),
    forms = actionButton(ns("show_rcode"), "Show R code", width = "100%"),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

#' @importFrom forcats fct_rev
#' @importFrom magrittr %>%
#' @importFrom methods substituteDirect
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
    validate_has_data(ANL, 3)
    chunks_reset()

    resp_var <- unname(merged_data()$columns_source$response)
    x <- unname(merged_data()$columns_source$x)

    row_facet_name <- unname(if_empty(merged_data()$columns_source$col_facet, character(0)))
    col_facet_name <- unname(if_empty(merged_data()$columns_source$row_facet, character(0)))


    validate(
      need(!identical(resp_var, character(0)), "Please define a valid column for the response variable"),
      need(!identical(x, character(0)), "Please define a valid column for the X-variable"),
      need(length(resp_var) == 1, "Please define a column for Response variable"),
      need(length(x) == 1, "Please define a column for X variable"),
      need(is.factor(ANL[[resp_var]]), "Please select a factor variable as the response."),
      need(is.factor(ANL[[x]]), "Please select a factor variable as the X-Variable.")
    )
    validate_has_data(ANL, 10)

    freq <- input$freq == "frequency"
    swap_axes <- input$coord_flip
    rotate_xaxis_labels <- input$rotate_xaxis_labels

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

    plot_call <- bquote(
      ANL %>%
        ggplot() +
        aes(x = .(cl_arg_x)) +
        geom_bar(aes(fill = .(as.name(resp_var))), position = .(arg_position)) +
        xlab(.(paste0(attr(ANL[[x]], "label"), " [", x, "]"))) +
        ylab(.(paste0("Proportion of ", attr(ANL[[resp_var]], "label"), " [", resp_var, "]")))
    )

    if (!freq) {
      plot_call <- if (swap_axes) {
        bquote(
          .(plot_call) +
            expand_limits(y = c(0, 1.05)) +
            geom_text(stat = "count", aes(label = ..count..), hjust = "left", position = "fill"))

      } else {
        bquote(
          .(plot_call) +
            expand_limits(y = c(0, 1.05)) +
            geom_text(stat = "count", aes(label = ..count..), vjust = -1, position = "fill"))
      }
    }

    if (swap_axes) {
      plot_call <- bquote(.(plot_call) + coord_flip())
    }

    if (rotate_xaxis_labels) {
      plot_call <- bquote(.(plot_call) + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
    }

    facet_cl <- facet_ggplot_call(row_facet_name, col_facet_name)

    if (!is.null(facet_cl)) {
      plot_call <- bquote(.(plot_call) + .(facet_cl))
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
