#' Response Plots
#'
#'
#' @param dataname (\code{character}) Name of dataset used to generate the response plot
#' @param response (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Which variable to use as the response. You can define one fixed column by using the
#'   setting \code{fixed = TRUE} inside the \code{column_spec}.
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
#'
#' @noRd
#'
#' @examples
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ARS <- cadrs
#' keys(ASL) <- c("USUBJID", "STUDYID")
#' keys(ARS) <- c("USUBJID", "STUDYID", "PARAMCD")
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     ARS = ARS,
#'     code = 'ASL <- cadsl
#'            ARS <- cadrs
#'            keys(ASL) <- c("USUBJID", "STUDYID")
#'            keys(ARS) <- c("USUBJID", "STUDYID", "PARAMCD")',
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_response_dummy(
#'       dataname = "ARS",
#'       response = data_extract_spec(
#'         dataname = "ARS",
#'         filter = filter_spec(
#'           vars = c("PARAMCD"),
#'           sep = " - ",
#'           choices = unique(ARS$PARAMCD),
#'           selected = unique(ARS$PARAMCD)[1],
#'           multiple = FALSE,
#'           label = "Choose endpoint"
#'         ),
#'         columns = columns_spec(
#'           choices = c("AVALC"),
#'           selected = c("AVALC"),
#'           multiple = FALSE,
#'           fixed = TRUE,
#'           label = "variable"
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = names(ASL),
#'           selected = c("RACE"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       row_facet = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = c("", "SEX", "AGE"),
#'           selected = "",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ASL",
#'         columns = columns_spec(
#'           choices = c("", "SEX", "AGE"),
#'           selected = "",
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
#' ANL_FILTERED %>%
#'   ggplot() +
#'   aes(x = SEX) +
#'   geom_bar(aes(fill = AVALC), position = "fill") +
#'   facet_grid(cols = vars(ARM)) +
#'   ylab("Distribution") +
#'   coord_flip()
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
tm_g_response_dummy <- function(label = "Response Plot",
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
  stop_if_not(list(toupper(dataname) != "ASL", "currently does not work with ASL data"))
  # No empty columns allowed for Response Var
  # No multiple Response variables allowed
  stopifnot(is.class.list("data_extract_spec")(response))
  # todo: this check should go into data_extract or at least create a function for reusability (add an argument allow_empty_values)
  # todo: also refactor this in tm_g_response.R to use this function
  stop_if_not(list(all(vapply(response, function(x) !("" %in% x$columns$choices), logical(1))),
                   "'response' should not allow empty values"))
  stop_if_not(list(all(vapply(response, function(x) !(x$columns$multiple), logical(1))),
                   "'response' should not allow multiple selection"))
  # No empty columns allowed for X-Var
  # No multiple X variables allowed
  stopifnot(is.class.list("data_extract_spec")(x))
  stop_if_not(list(all(vapply(x, function(x) !("" %in% x$columns$choices), logical(1))),
                   "'x' should not allow empty values"))
  stop_if_not(list(all(vapply(x, function(x) !(x$columns$multiple), logical(1))),
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
    filters = dataname
  )
}

ui_g_response_dummy <- function(id, ...) {
  arguments <- list(...)

  ns <- NS(id)

  standard_layout(
    output = white_small_well(
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
        id = ns("x"),
        label = "X Variable",
        data_extract_spec = arguments$x
      ),
      data_extract_input(
        id = ns("row_facet"),
        label = "Row facetting Variables ",
        data_extract_spec = arguments$row_facet
      ),
      data_extract_input(
        id = ns("col_facet"),
        label = "Column facetting Variables",
        data_extract_spec = arguments$col_facet
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

