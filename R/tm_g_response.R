

#' Response Plots
#'
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' library(random.cdisc.data)
#'
#' ASL <- radsl()
#' ARS <- radrs(ASL)
#' attr(ASL, "source") <- "# asl import"
#' attr(ARS, "source") <- "# ars import"
#'
#' x <- teal::init(
#'   data = list(ASL = ASL, ARS = ARS),
#'   modules = root_modules(
#'     tm_g_response(
#'       dataname = "ARS",
#'       endpoint = select_choices(ARS$PARAMCD),
#'       resp_var = select_choices("AVAL"),
#'       x_var = NULL,
#'       facet_var = NULL,
#'       row_facet_var = NULL,
#'       col_facet_var = NULL
#'     )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#'
#'
#' ## as ggplot only
#' library(ggplot2)
#' library(dplyr)
#' library(forcats)
#'
#' ANL <- inner_join(ASL, ARS)
#'
#' ANL_filtered <- ANL %>% filter(PARAMCD == "BESRSPI") %>%
#'    mutate(ALL = factor(rep("Response", n())))
#'
#' ANL_filtered %>% ggplot() +
#'   aes(x = ALL) +
#'   geom_bar(aes(fill = AVALC))
#'
#'
#' ANL_filtered %>% ggplot() +
#'   aes(x = SEX) +
#'   geom_bar(aes(fill = AVALC))
#'
#' ANL_filtered %>% ggplot() +
#'   aes(x = SEX) +
#'   geom_bar(aes(fill = AVALC)) +
#'   facet_grid(cols = vars(ARM))
#'
#'
#' ANL_filtered %>% ggplot() +
#'   aes(x = SEX) +
#'   geom_bar(aes(fill = AVALC), position =  "fill") +
#'   facet_grid(cols = vars(ARM)) +
#'   ylab("Distribution") +
#'   coord_flip()
#'
#'
#' ANL_filtered %>% ggplot() +
#'   aes(x = SEX) +
#'   geom_bar(aes(fill = AVALC), position =  "fill") +
#'   geom_text(stat='count', aes(label=..count.., vjust = -1), position =  "fill") +
#'   #scale_y_continuous(limits = c(0, 1.2)) +
#'   facet_grid(cols = vars(ARM)) +
#'   ylab("Distribution")
#'
#' ANL_filtered %>% ggplot() +
#'   aes(x = fct_rev(SEX)) +
#'   geom_bar(aes(fill = AVALC), position =  "fill") +
#'   geom_text(stat='count', aes(label=..count.., hjust = -0.3), position =  "fill") +
#'   scale_y_continuous(limits = c(0, 1.4)) +
#'   facet_grid(cols = vars(ARM)) +
#'   ylab("Distribution") +
#'   coord_flip()
#'
#'
#' }
#'
tm_g_response <- function(
  label = "Association",
  dataname,
  endpoint,
  resp_var,
  x_var = NULL,
  facet_var = NULL,
  row_facet_var = NULL,
  col_facet_var = NULL,
  coord_flip = TRUE,
  freq = FALSE,
  plot_height = c(600, 400, 5000),
  pre_output = NULL,
  post_output = NULL,
  code_data_processing = NULL
) {

  args <- as.list(environment())

  if (is.null(x_var)) x_var <- select_choices(NULL)
  if (is.null(facet_var)) facet_var <- select_choices(NULL)
  if (is.null(row_facet_var)) row_facet_var <- select_choices(NULL)
  if (is.null(col_facet_var)) col_facet_var <- select_choices(NULL)

  stopifnot(is.select_choices(endpoint))
  stopifnot(is.select_choices(resp_var))
  stopifnot(is.select_choices(x_var))
  stopifnot(is.select_choices(facet_var))
  stopifnot(is.select_choices(row_facet_var))
  stopifnot(is.select_choices(col_facet_var))


  module(
    label = label,
    server = srv_g_response,
    ui = ui_g_response,
    ui_args = args,
    server_args = list(dataname = dataname,
                       code_data_processing = code_data_processing),
    filters = dataname
  )

}



ui_g_response <- function(id, ...) {

  a <- list(...)

  ns <- NS(id)

  standard_layout(
    output = whiteSmallWell(uiOutput(ns("plot_ui"))),
    encoding = div(
      helpText("Dataset:", tags$code(a$dataname)),

      optionalSelectInput(ns("endpoint"), "Endpoint (PARAMCD)", a$endpoint$choices, a$endpoint$selected),
      optionalSelectInput(ns("resp_var"), "Response Variable", a$resp_var$choices, a$resp_var$selected),
      optionalSelectInput(ns("x_var"), "X Variable", a$x_var$choices, a$x_var$selected),


      optionalSelectInput(ns("row_facet_var"), "Row facetting Variables", a$row_facet_var$choices, a$row_facet_var$selected, multiple = TRUE),
      optionalSelectInput(ns("col_facet_var"), "Column facetting Variables", a$col_facet_var$choices, a$col_facet_var$selected, multiple = TRUE),

      radioButtons(ns("freq"), NULL, choices = c("frequency", "density"), selected = ifelse(a$freq, "frequency", "density"), inline = TRUE),
      checkboxInput(ns("coord_flip"), "swap axes", value = TRUE),
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", a$plot_height, ticks = FALSE)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )

}



srv_g_response <- function(input,
                            output,
                            session,
                            datasets,
                            dataname,
                            code_data_processing) {


  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("plot"), height=plot_height)
  })

  ANL_head <- head(datasets$get_data(dataname, filtered = FALSE, reactive = FALSE))

  output$plot <- renderPlot({

    ANL_filtered <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)

    resp_var <- input$resp_var
    x_var <- input$x_var
    freq <- input$freq == "frequency"
    row_facet_var <- input$row_facet_var
    col_facet_var <- input$col_facet_var
    swap_axes <- input$coord_flip


    as.global(ANL_filtered, resp_var, x_var, freq, row_facet_var, col_facet_var, swap_axes)

    plot(1,1)
  })

}
