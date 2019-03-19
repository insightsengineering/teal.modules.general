

#' Response Plots
#'
#' @inheritParams teal::module
#' @inheritParams teal::standard_layout
#' @export
#'
#' @param dataname name of dataset used to generate table
#' @param endpoint (\code{choices_selected}) Which variable to use as EndPoint
#' @param resp_var (\code{choices_selected}) Which variable to use as Response
#' @param x_var (\code{choices_selected}) Which variable to use as X
#' @param row_facet_var (\code{choices_selected}) Which variable to use for faceting rows
#' @param col_facet_var (\code{choices_selected}) Which variable to use for faceting columns
#' @param coord_flip (\code{logical}) Whether to flip coordinates
#' @param freq (\code{logical}) Display frequency
#' @param plot_height if scalar then the plot will have a fixed height. If a
#'   slider should be presented to adjust the plot height dynamically then it
#'   can be a vector of length three with \code{c(value, min and max)}.
#' @param code_data_processing (\code{character}) Code that was used to pre-process the data

#' @examples
#'
#' library(random.cdisc.data)
#'
#' asl <- radsl()
#' ars <- radrs(asl)
#' attr(asl, "source") <- "random.cdisc.data::radsl(seed = 1)"
#' attr(ars, "source") <- 'subset(random.cdisc.data::radrs(asl, seed = 1), AVISIT == "Follow Up")'
#'
#' x <- teal::init(
#'   data = list(ASL = asl, ARS = ars),
#'   modules = root_modules(
#'     tm_g_response(
#'       dataname = "ARS",
#'       endpoint = choices_selected(ars$PARAMCD),
#'       resp_var = choices_selected("AVALC"),
#'       x_var = choices_selected(names(asl), "SEX"),
#'       row_facet_var = choices_selected(names(asl), NULL),
#'       col_facet_var = choices_selected(names(asl), NULL)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(x$ui, x$server)
#'
#' ## as ggplot only
#' library(ggplot2)
#' library(dplyr)
#' library(forcats)
#'
#' anl <- inner_join(ars, asl)
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
#'   aes(x = fct_rev(SEX)) +
#'   geom_bar(aes(fill = AVALC), position = "fill") +
#'   geom_text(stat = "count", aes(label = paste(" ", ..count..), hjust = 0), position = "fill") +
#'   scale_y_continuous(limits = c(0, 1.4)) +
#'   facet_grid(cols = vars(ARM)) +
#'   ylab("Distribution") +
#'   coord_flip()
#' }
#'
#' @importFrom teal add_no_selected_choices
tm_g_response <- function(
  label = "Response Plot",
  dataname,
  endpoint,
  resp_var,
  x_var = NULL,
  row_facet_var = NULL,
  col_facet_var = NULL,
  coord_flip = TRUE,
  freq = FALSE,
  plot_height = c(600, 400, 5000),
  pre_output = NULL,
  post_output = NULL,
  code_data_processing = NULL) {
  x_var <- teal::add_no_selected_choices(x_var, TRUE)
  row_facet_var <- teal::add_no_selected_choices(row_facet_var, TRUE)
  col_facet_var <- teal::add_no_selected_choices(col_facet_var, TRUE)

  stopifnot(is.choices_selected(endpoint))
  stopifnot(is.choices_selected(resp_var))
  stopifnot(is.choices_selected(x_var))
  stopifnot(is.choices_selected(row_facet_var))
  stopifnot(is.choices_selected(col_facet_var))
  dataname != "asl" || stop("currently does not work with ASL data")


  args <- as.list(environment())

  teal::module(
    label = label,
    server = srv_g_response,
    ui = ui_g_response,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      code_data_processing = code_data_processing
    ),
    filters = dataname
  )
}

#' @import teal
#' @importFrom teal.devel white_small_well
ui_g_response <- function(id, ...) {
  a <- list(...)

  ns <- NS(id)

  standard_layout(
    output = teal.devel::white_small_well(uiOutput(ns("plot_ui"))),
    encoding = div(
      helpText("Dataset:", tags$code(a$dataname)),

      optionalSelectInput(ns("endpoint"), "Endpoint (PARAMCD)", a$endpoint$choices, a$endpoint$selected),
      optionalSelectInput(ns("resp_var"), "Response Variable", a$resp_var$choices, a$resp_var$selected),
      optionalSelectInput(ns("x_var"), "X Variable",
                          a$x_var$choices, a$x_var$selected,
                          multiple = TRUE, label_help = helpText("from ASL")
      ),

      optionalSelectInput(ns("row_facet_var"), "Row facetting Variables",
                          a$row_facet_var$choices, a$row_facet_var$selected,
                          multiple = TRUE
      ),
      optionalSelectInput(ns("col_facet_var"), "Column facetting Variables",
                          a$col_facet_var$choices, a$col_facet_var$selected,
                          multiple = TRUE
      ),

      radioButtons(ns("freq"), NULL,
                   choices = c("frequency", "density"),
                   selected = ifelse(a$freq, "frequency", "density"), inline = TRUE
      ),
      checkboxInput(ns("coord_flip"), "swap axes", value = TRUE),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}


#' @importFrom teal no_selected_as_NULL
#' @importFrom teal.devel get_rcode_header
#' @importFrom forcats fct_rev
srv_g_response <- function(
  input,
  output,
  session,
  datasets,
  dataname,
  code_data_processing) {

  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("plot"), height = plot_height)
  })

  ANL_head <- head(datasets$get_data(dataname, filtered = FALSE, reactive = FALSE)) # nolint
  ASL <- datasets$get_data("ASL", filtered = FALSE, reactive = FALSE) # nolint

  plot_call <- reactive({
    endpoint <- input$endpoint # nolint
    resp_var <- input$resp_var # nolint
    x_var <- input$x_var
    freq <- input$freq == "frequency"
    row_facet_var <- input$row_facet_var
    col_facet_var <- input$col_facet_var
    swap_axes <- input$coord_flip

    # call global as.global(ANL_head, ASL, resp_var, x_var, freq, row_facet_var, col_facet_var, swap_axes, endpoint)

    x_var <- teal::no_selected_as_NULL(x_var)
    row_facet_var <- teal::no_selected_as_NULL(row_facet_var)
    col_facet_var <- teal::no_selected_as_NULL(col_facet_var)

    cl_anl <- bquote(.(as.name(paste0(dataname, "_FILTERED"))) %>%
                dplyr::filter(PARAMCD %in% .(endpoint)))

    asl_vars <- c(x_var, row_facet_var, col_facet_var)
    if (!is.null(asl_vars)) {
      asl_vars <- unique(c("USUBJID", "STUDYID", asl_vars))
      cl_anl <- call(
        "%>%", cl_anl,
        call("left_join", bquote(ASL[, .(asl_vars), drop = FALSE]))
      )
    }

    arg_position <- if (freq) "stack" else "fill" # nolint
    cl_arg_x <- if (is.null(x_var)) {
      1
    } else {
      tmp_cl <- if (length(x_var) == 1) {
        as.name(x_var)
      } else {
        tmp <- call_fun_dots("interaction", x_var)
        tmp[["sep"]] <- " x "
        tmp
      }

      if (swap_axes) {
        bquote(fct_rev(.(tmp_cl)))
      } else {
        tmp_cl
      }
    }

    plot_call <- bquote(
      .(cl_anl) %>%
        ggplot() +
        aes(x = .(cl_arg_x)) +
        geom_bar(aes(fill = .(as.name(resp_var))), position = .(arg_position))
    )

    if (!freq) {
      if (swap_axes) {
        tmp_cl1 <- quote(geom_text(
          stat = "count", aes(label = paste(" ", ..count..), hjust = 0), # nolint
          position = "fill"
        ))
        tmp_cl2 <- quote(expand_limits(y = c(0, 1.4)))
      } else {
        tmp_cl1 <- quote(geom_text(stat = "count", aes(label = ..count.., vjust = -1), position = "fill")) # nolint
        tmp_cl2 <- quote(expand_limits(y = c(0, 1.2)))
      }

      plot_call <- call("+", call("+", plot_call, tmp_cl1), tmp_cl2)
    }

    if (swap_axes) {
      plot_call <- call("+", plot_call, quote(coord_flip()))
    }

    facet_cl <- g_facet_cl(row_facet_var, col_facet_var)

    if (!is.null(facet_cl)) plot_call <- call("+", plot_call, facet_cl)

    plot_call
  })

  output$plot <- renderPlot({
    anl_filtered <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)

    plot_call <- plot_call()

    p <- try(eval(
      plot_call,
      list2env(setNames(list(anl_filtered, emptyenv()), c(paste0(dataname, "_FILTERED"), "parent")))
    )) # try and eval

    if (is(p, "try-error")) {
      validate(need(FALSE, p))
    } else {
      p
    }
  })

  observeEvent(input$show_rcode, {
    header <- teal.devel::get_rcode_header(
      title = "Bivariate and Univariate Plot",
      datanames = dataname,
      datasets = datasets,
      code_data_processing,
      packages = c("ggplot2", "forcats", "dplyr")
    )

    str_rcode <- paste(c(
      "",
      header,
      "",
      deparse(plot_call(), width.cutoff = 60)
    ), collapse = "\n")

    showModal(modalDialog(
      title = "R Code for the Current Plot",
      tags$pre(tags$code(class = "R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
}
