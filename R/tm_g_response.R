#' Response Plots
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @export
#'
#' @param dataname (\code{character}) Name of dataset used to generate the response plot
#' @param response (\code{list} of \code{data_extract_spec}) Which variable to use as the
#'   response. You can define one fixed column by using the setting \code{fixed = TRUE} inside
#'   the \code{column_spec}.
#' @param xvar (\code{list} of \code{data_extract_spec}) Which variable to use on the
#'   X-axis of the response plot. Allow the user to select multiple columns from the \code{data} allowed
#'   in teal. Just allow single columns by \code{multiple = FALSE}.
#' @param row_facet_var (\code{list} of \code{data_extract_spec}) Which data columns
#'   to use for faceting rows.  Just allow single columns by \code{multiple = FALSE}.
#' @param col_facet_var (\code{list} of \code{data_extract_spec}) Which data to use for faceting columns.
#'   Just allow single columns by \code{multiple = FALSE}.
#' @param coord_flip (\code{logical}) Whether to flip coordinates
#' @param freq (\code{logical}) Display frequency (\code{TRUE}) or density (\code{FALSE}).
#' @param plot_height (\code{numeric}) Vector of length three with \code{c(value, min and max)}.
#' @examples
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- radsl(seed = 1)
#' ARS <- radrs(ASL, seed = 1)
#' keys(ASL) <- c("USUBJID", "STUDYID")
#' keys(ARS) <- c("USUBJID", "STUDYID", "PARAMCD")
#'
#' ars_filters <- filter_spec(
#'   vars = c("PARAMCD"),
#'   sep = " - ",
#'   choices = unique(ARS$PARAMCD),
#'   selected = unique(ARS$PARAMCD)[1],
#'   multiple = FALSE,
#'   label = "Choose endpoint"
#' )
#'
#' ars_extracted_response <- data_extract_spec(
#'   dataname = "ARS",
#'   filter = ars_filters,
#'   columns = columns_spec(
#'     choices = c("AVALC"),
#'     selected = c("AVALC"),
#'     multiple = FALSE,
#'     fixed = TRUE,
#'     label = "variable"
#'   )
#' )
#'
#' asl_extracted <- data_extract_spec(
#'   dataname = "ASL",
#'   columns = columns_spec(
#'     choices = names(ASL),
#'     selected = c("RACE"),
#'     multiple = FALSE,
#'     fixed = FALSE
#'   )
#' )
#' asl_extracted_row <- data_extract_spec(
#'   dataname = "ASL",
#'   columns = columns_spec(
#'     choices = c("", "SEX", "AGE"),
#'     selected = "",
#'     multiple = FALSE,
#'     fixed = FALSE
#'   )
#' )
#' asl_extracted_col <- data_extract_spec(
#'   dataname = "ASL",
#'   columns = columns_spec(
#'     choices = c("", "SEX", "AGE"),
#'     selected = "",
#'     multiple = FALSE,
#'     fixed = FALSE
#'   )
#' )
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ARS = ARS,
#'     code = 'ASL <- radsl(seed = 1)
#'            ARS <- radrs(ASL, seed = 1)
#'            keys(ASL) <- c("USUBJID", "STUDYID")
#'            keys(ARS) <- c("USUBJID", "STUDYID")',
#'      check = FALSE),
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
#' library(dplyr)
#' library(forcats)
#'
#' ANL <- suppressWarnings(inner_join(ARS, ASL))
#'
#' ANL_FILTERED <- ANL %>%
#'   dplyr::filter(PARAMCD == "BESRSPI") %>% # strict call of filter
#'   mutate(ALL = factor(rep("Response", n())))
#'
#' ANL_FILTERED %>%
#'   ggplot() +
#'   aes(x = ALL) +
#'   geom_bar(aes(fill = AVALC))
#'
#'
#' ANL_FILTERED %>%
#'   ggplot() +
#'   aes(x = SEX) +
#'   geom_bar(aes(fill = AVALC))
#'
#' ANL_FILTERED %>%
#'   ggplot() +
#'   aes(x = SEX) +
#'   geom_bar(aes(fill = AVALC)) +
#'   facet_grid(cols = vars(ARM))
#'
#'
#' ANL_FILTERED %>%
#'   ggplot() +
#'   aes(x = SEX) +
#'   geom_bar(aes(fill = AVALC), position = "fill") +
#'   facet_grid(cols = vars(ARM)) +
#'   ylab("Distribution") +
#'   coord_flip()
#'
#'
#' ANL_FILTERED %>%
#'   ggplot() +
#'   aes(x = SEX) +
#'   geom_bar(aes(fill = AVALC), position = "fill") +
#'   geom_text(stat = "count", aes(label = ..count.., vjust = -1), position = "fill") +
#'   expand_limits(y = c(0, 1.2)) +
#'   facet_grid(cols = vars(ARM)) +
#'   ylab("Distribution")
#'
#' ANL_FILTERED %>%
#'   ggplot() +
#'   aes(x = fct_rev(SEX)) +
#'   xlab("SEX") +
#'   geom_bar(aes(fill = AVALC), position = "fill") +
#'   geom_text(stat = "count", aes(label = paste(" ", ..count..), hjust = 0), position = "fill") +
#'   scale_y_continuous(limits = c(0, 1.4)) +
#'   facet_grid(cols = vars(ARM)) +
#'   ylab("Distribution") +
#'   coord_flip()
tm_g_response <- function(label = "Response Plot",
                          dataname,
                          response,
                          xvar = NULL,
                          row_facet_var = NULL,
                          col_facet_var = NULL,
                          coord_flip = TRUE,
                          freq = FALSE,
                          plot_height = c(600, 400, 5000),
                          pre_output = NULL,
                          post_output = NULL) {
  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  stop_if_not(list(toupper(dataname) != "ASL", "currently does not work with ASL data"))
  stopifnot(is.list(response))
  # No empty columns allowed for Response Var
  # No multiple Response variables allowed
  lapply(response, function(ds_extract){
        stopifnot(!("" %in% ds_extract$columns$choices))
        stopifnot(!ds_extract$columns$multiple)
      }
  )
  stopifnot(is.list(xvar))
  # No empty columns allowed for X-Var
  # No multiple X variables allowed
  lapply(xvar, function(ds_extract){
        stopifnot(!("" %in% ds_extract$columns$choices))
        stopifnot(!ds_extract$columns$multiple)
      }
  )
  stopifnot(is.null(row_facet_var) || is.list(row_facet_var))
  stopifnot(is.null(col_facet_var) || is.list(col_facet_var))
  stopifnot(is.logical.single(coord_flip))
  stopifnot(is.logical.single(freq))
  stopifnot(is_numeric_vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])


  args <- as.list(environment())

  module(
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
#' @importFrom teal.devel white_small_well plot_height_input plot_height_output standard_layout
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
      plot_height_input(id = ns("myplot"), value = arguments$plot_height)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = arguments$pre_output,
    post_output = arguments$post_output
  )
}

#' @importFrom teal.devel data_extract_module get_dataset_prefixed_col_names merge_datasets plot_with_height
#' @importFrom teal.devel get_rcode show_rcode_modal
#' @importFrom forcats fct_rev
#' @importFrom methods substituteDirect
srv_g_response <- function(input,
                           output,
                           session,
                           datasets,
                           dataname,
                           response,
                           xvar,
                           row_facet_var,
                           col_facet_var) {
  stopifnot(all(dataname %in% datasets$datanames()))

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

  # create a plot call from data inserted
  plot_call <- reactive({
    resp_var <- get_dataset_prefixed_col_names(response_data())
    xvar <- get_dataset_prefixed_col_names(xvar_data())
    row_facet_var_name <- get_dataset_prefixed_col_names(row_facet_var_data())
    col_facet_var_name <- get_dataset_prefixed_col_names(col_facet_var_data())

    validate(need(resp_var != "", "Please define a valid column for the response variable"))
    validate(need(xvar != "", "Please define a valid column for the X-variable"))

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

    facet_cl <- facet_ggplot_call(row_facet_var_name, col_facet_var_name)

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
    show_rcode_modal(
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
