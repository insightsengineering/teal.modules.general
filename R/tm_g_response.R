

#' Response Plots
#'
#' @inheritParams teal::module
#' @inheritParams teal::standard_layout
#' @export
#'
#' @param dataname name of dataset used to generate table
#' @param response (\code{list} of \code{data_extract_spec}) Which variable to use as Response
#' @param xvar (\code{list} of \code{data_extract_spec}) Which variable to use as X
#' @param row_facet_var (\code{list} of \code{data_extract_spec}) Which variable to use for faceting rows
#' @param col_facet_var (\code{list} of \code{data_extract_spec}) Which variable to use for faceting columns
#' @param coord_flip (\code{logical}) Whether to flip coordinates
#' @param freq (\code{logical}) Display frequency
#' @param plot_height if scalar then the plot will have a fixed height. If a
#'   slider should be presented to adjust the plot height dynamically then it
#'   can be a vector of length three with \code{c(value, min and max)}.
#' @examples
#'
#' library(random.cdisc.data)
#' library(magrittr)
#'
#' asl <- radsl()
#' keys(asl) <- c("USUBJID", "STUDYID")
#' ars <- radrs(asl)
#' keys(ars) <- c("USUBJID", "STUDYID")
#'
#' ars_filters <- filter_spec(
#'     vars = c("PARAMCD"),
#'     sep = " - ",
#'     choices = unique(ars$PARAMCD),
#'     selected = unique(ars$PARAMCD)[1],
#'     multiple = FALSE,
#'     label = "Choose endpoint and Censor"
#' )
#' ars_extracted_response <- data_extract_spec(
#'     dataname = "ARS",
#'     filter = ars_filters,
#'     columns = columns_spec(
#'         choices = c("AVALC"),
#'         selected = c("AVALC"),
#'         multiple = FALSE,
#'         fixed = TRUE,
#'         label = "variable"
#'     )
#' )
#'
#' asl_extracted <- data_extract_spec(
#'     dataname = "ASL",
#'     columns = columns_spec(
#'         choices = names(asl),
#'         selected = c("RACE"),
#'         multiple = FALSE,
#'         fixed = FALSE
#'     )
#' )
#' asl_extracted_row <- data_extract_spec(
#'     dataname = "ASL",
#'     columns = columns_spec(
#'         choices = c("","SEX", "AGE"),
#'         selected = "",
#'         multiple = FALSE,
#'         fixed = FALSE
#'     )
#' )
#' asl_extracted_col <- data_extract_spec(
#'     dataname = "ASL",
#'     columns = columns_spec(
#'         choices = c("", "SEX", "AGE"),
#'         selected = "",
#'         multiple = FALSE,
#'         fixed = FALSE
#'     )
#' )
#'
#'
#' app <- teal::init(
#'   data = cdisc_data(
#'       ASL = asl,
#'       ARS = ars,
#'       code = "",
#'       check = FALSE),
#'   modules = root_modules(
#'     tm_g_response(
#'       dataname = "ARS",
#'       response = list(ars_extracted_response),
#'       xvar = list(asl_extracted),
#'       row_facet_var = list(asl_extracted_row),
#'       col_facet_var = list(asl_extracted_col)
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#' ## as ggplot only
#' library(ggplot2)
#' library(dplyr)
#' library(forcats)
#'
#' anl_filtered <- inner_join(ars, asl)
#'
#' anl_filtered <- anl %>%
#'   filter(PARAMCD == "BESRSPI") %>%
#'   mutate(ALL = factor(rep("Response", n())))
#'
#' anl_filtered %>%
#'   ggplot() +
#'   aes(x = ALL) +
#'   geom_bar(aes(fill = AVALC))
#'
#'
#' anl_filtered %>%
#'   ggplot() +
#'   aes(x = SEX) +
#'   geom_bar(aes(fill = AVALC))
#'
#' anl_filtered %>%
#'   ggplot() +
#'   aes(x = SEX) +
#'   geom_bar(aes(fill = AVALC)) +
#'   facet_grid(cols = vars(ARM))
#'
#'
#' anl_filtered %>%
#'   ggplot() +
#'   aes(x = SEX) +
#'   geom_bar(aes(fill = AVALC), position = "fill") +
#'   facet_grid(cols = vars(ARM)) +
#'   ylab("Distribution") +
#'   coord_flip()
#'
#'
#' anl_filtered %>%
#'   ggplot() +
#'   aes(x = SEX) +
#'   geom_bar(aes(fill = AVALC), position = "fill") +
#'   geom_text(stat = "count", aes(label = ..count.., vjust = -1), position = "fill") +
#'   expand_limits(y = c(0, 1.2)) +
#'   facet_grid(cols = vars(ARM)) +
#'   ylab("Distribution")
#'
#' anl_filtered %>%
#'   ggplot() +
#'   aes(x = forcats::fct_rev(SEX)) +
#'   xlab("SEX") +
#'   geom_bar(aes(fill = AVALC), position = "fill") +
#'   geom_text(stat = "count", aes(label = paste(" ", ..count..), hjust = 0), position = "fill") +
#'   scale_y_continuous(limits = c(0, 1.4)) +
#'   facet_grid(cols = vars(ARM)) +
#'   ylab("Distribution") +
#'   coord_flip()
#'
#' @import teal.devel
#' @importFrom forcats fct_rev
tm_g_response <- function(
                          label = "Response Plot",
                          dataname,
                          response,
                          xvar = NULL,
                          row_facet_var = NULL,
                          col_facet_var = NULL,
                          coord_flip = FALSE,
                          freq = FALSE,
                          plot_height = c(600, 400, 5000),
                          pre_output = NULL,
                          post_output = NULL) {
  dataname != "asl" || stop("currently does not work with ASL data")

  args <- as.list(environment())

  teal::module(
    label = label,
    server = srv_g_response,
    ui = ui_g_response,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      response = response,
      xvar = xvar,
      row_facet_var = row_facet_var,
      col_facet_var = col_facet_var
    ),
    filters = dataname
  )
}

#' @import teal
#' @importFrom teal.devel white_small_well plot_height_output
ui_g_response <- function(id, ...) {
  arguments <- list(...)

  ns <- NS(id)

  standard_layout(
    output = teal.devel::white_small_well(
      plot_height_output(id = ns("myplot"))
    ),
    encoding = div(
      helpText("Dataset:", tags$code(arguments$dataname)),

      data_extract_input(
        id = ns("response"),
        label = "Response Variable",
        data_extract_spec = arguments$response
      ),
      data_extract_input(
        id = ns("xvar"),
        label = "X Variable",
        data_extract_spec = arguments$xvar
      ),
      data_extract_input(
        id = ns("row_facet_var"),
        label = "Row facetting Variables ",
        data_extract_spec = arguments$row_facet_var
      ),
      data_extract_input(
        id = ns("col_facet_var"),
        label = "Column facetting Variables",
        data_extract_spec = arguments$col_facet_var
      ),

      radioButtons(ns("freq"), NULL,
        choices = c("frequency", "density"),
        selected = ifelse(arguments$freq, "frequency", "density"), inline = TRUE
      ),
      checkboxInput(ns("coord_flip"), "swap axes", value = arguments$coord_flip),
      # This shall be wrapped in a teal::plot
      plot_height_input(id = ns("myplot"), value = arguments$plot_height)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = arguments$pre_output,
    post_output = arguments$post_output
  )
}


#' @importFrom teal no_selected_as_NULL
#' @import teal.devel
srv_g_response <- function(
                           input,
                           output,
                           session,
                           datasets,
                           dataname,
                           response,
                           xvar,
                           row_facet_var,
                           col_facet_var) {
  stopifnot(is.list(response))
  stopifnot(is.list(xvar))
  stopifnot(is.list(row_facet_var))
  stopifnot(is.list(col_facet_var))

  use_chunks(session)

  # Data Extraction
  response_data <- callModule(data_extract_module,
    id = "response",
    datasets = datasets,
    data_extract_spec = response
  )
  xvar_data <- callModule(data_extract_module,
    id = "xvar",
    datasets = datasets,
    data_extract_spec = xvar
  )
  row_facet_var_data <- callModule(data_extract_module,
    id = "row_facet_var",
    datasets = datasets,
    data_extract_spec = row_facet_var
  )
  col_facet_var_data <- callModule(data_extract_module,
    id = "col_facet_var",
    datasets = datasets,
    data_extract_spec = col_facet_var
  )

  data_reactive <- reactive({
    merge_datasets(
      list(
        xvar_data(),
        row_facet_var_data(),
        col_facet_var_data(),
        response_data()
      )
    )
  })

  # Insert the plot into a plot_height module from teal.devel
  callModule(plot_with_height,
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

  plot_call <- reactive({
    resp_var <- get_dataset_prefixed_col_names(response_data())
    xvar <- get_dataset_prefixed_col_names(xvar_data())
    row_facet_var_name <- get_dataset_prefixed_col_names(row_facet_var_data())
    col_facet_var_name <- get_dataset_prefixed_col_names(col_facet_var_data())

    freq <- input$freq == "frequency"
    swap_axes <- input$coord_flip

    arg_position <- if (freq) "stack" else "fill" # nolint
    cl_arg_x <- if (is.null(xvar)) {
      1
    } else {
      tmp_cl <- if (length(xvar) == 1) {
        as.name(xvar)
      } else {
        tmp <- call_fun_dots("interaction", xvar)
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
        need(is.factor(anl[[resp_var]]),"Please select a factor variable as the name."))

    plot_call <- bquote(
      anl %>%
        ggplot() +
        aes(x = .(cl_arg_x)) +
        geom_bar(aes(fill = .(as.name(resp_var))), position = .(arg_position))
    )

    if (!freq) {
      if (swap_axes) {
        tmp_cl1 <- quote(xlab(label)) %>% substituteDirect(list(label = tmp_cl %>% deparse))
        tmp_cl2 <- quote(expand_limits(y = c(0, 1.4)))
      } else {
        tmp_cl1 <- quote(geom_text(stat = "count", aes(label = ..count.., vjust = -1), position = "fill")) # nolint
        tmp_cl2 <- quote(expand_limits(y = c(0, 1.2)))
      }

      plot_call <- call("+", call("+", plot_call, tmp_cl1), tmp_cl2)
    }else{
      # Change Y-Axis Label in case of Swap
      tmp_cl1 <- quote(xlab(label)) %>% substituteDirect(list(label = tmp_cl %>% deparse))
      plot_call <- call("+", plot_call, tmp_cl1)

    }

    if (swap_axes) {
      plot_call <- call("+", plot_call, quote(coord_flip()))
    }

    facet_cl <- g_facet_cl(row_facet_var_name, col_facet_var_name)

    if (!is.null(facet_cl)) plot_call <- call("+", plot_call, facet_cl)

    renew_chunk_environment(envir = environment())
    renew_chunks()

    set_chunk("plotCall", plot_call)
  })

  output$plot <- renderPlot({
    plot_call()
    eval_remaining()
  })

  observeEvent(input$show_rcode, {
    teal.devel::show_rcode_modal(
      title = "Response Plot",
      rcode = get_rcode(
        datasets = datasets,
        dataname = dataname,
        merged_dataname = "anl",
        merged_datasets = list(
          response_data(),
          xvar_data(),
          row_facet_var_data(),
          col_facet_var_data()
        ),
        title = "Response Plot"
      )
    )
  })
}
