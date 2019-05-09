#' Cross table based on rtables
#'
#' @inheritDotParams teal.devel::standard_layout -output -encoding -forms
#' @param label (\code{character}) Label of the app in the teal menu
#' @param dataname (\code{character}) Name of the dataset used in the teal app. Just a single
#'   dataset is allowed!
#' @param x_var \code{\link[teal]{choices_selected}} object with all available
#'   choices with preselected option for variable X
#' @param y_var \code{\link[teal]{choices_selected}} object with all available
#'   choices with preselected option for variable Y
#' @export
#'
#' @author wolfs25 waddella
#'
#' @examples
#' \dontrun{
#' library(random.cdisc.data)
#' library(tern)
#'
#' asl <- radsl(seed = 1)
#' keys(asl) <- c('USUBJID', 'STUDYID')
#'
#' x <- teal::init(
#'   data = cdisc_data(
#'     ASL = asl,
#'     code = "library(random.cdisc.data)
#'            asl <- radsl(seed = 1)
#'            keys(asl) <- c('USUBJID', 'STUDYID')",
#'     check = TRUE
#'   ),
#'   modules = teal::root_modules(
#'     tm_t_percentage_cross_table(
#'       label = "Cross Table",
#'       dataname = "ASL",
#'       x_var = choices_selected(c("STRATA1"), "STRATA1"),
#'       y_var = choices_selected(c("STRATA2"), "STRATA2")
#'     )
#'   )
#' )
#' shinyApp(x$ui, x$server)
#' }
tm_t_percentage_cross_table <- function(
                                        label = "Cross Table",
                                        dataname,
                                        x_var,
                                        y_var, ...) {
  stopifnot(is.character(dataname))
  stopifnot(length(dataname) == 1)
  stopifnot(is.choices_selected(x_var))
  stopifnot(is.choices_selected(y_var))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_percentage_cross_table,
    ui = ui_percentage_cross_table,
    ui_args = args,
    server_args = list(dataname = dataname, label = label),
    filters = dataname,
    ...
  )
}

#' @importFrom teal.devel optionalSelectInput standard_layout white_small_well
ui_percentage_cross_table <- function(id, ...) {
  a <- list(...)

  ns <- NS(id)

  standard_layout(
    output = white_small_well(uiOutput(ns("table"))),
    encoding = div(
      helpText("Dataset:", tags$code(a$dataname)),
      optionalSelectInput(ns("x_var"), "x var", a$x_var$choices, a$x_var$selected),
      optionalSelectInput(ns("y_var"), "y var", a$y_var$choices, a$y_var$selected)
    ),
    forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @importFrom teal.devel get_rcode show_rcode_modal
#' @importFrom teal.devel eval_remaining renew_chunks renew_chunk_environment set_chunk set_chunk
#' @importFrom rtables rrowl rtablel as_html
#' @importFrom stats addmargins
srv_percentage_cross_table <- function(input, output, session, datasets, dataname, label) {
  use_chunks(session)

  table_code <- reactive({
    anl_f <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)

    x_var <- input$x_var
    y_var <- input$y_var

    validate(need(anl_f, "data missing"))
    validate_has_data(anl_f, 10)

    validate(need(x_var, "selected x_var does not exist"))
    validate(need(y_var, "selected y_var does not exist"))

    data_name <- paste0(dataname, "_FILTERED")
    assign(data_name, anl_f)

    # Set chunks

    renew_chunk_environment(envir = environment())
    renew_chunks()

    set_chunk(expression = bquote(data_table <-
      addmargins(table(.(as.name(data_name))[[.(x_var)]], .(as.name(data_name))[[.(y_var)]]))))

    set_chunk(expression = quote(perc_table <- data_table / data_table[nrow(data_table), ncol(data_table)]))

    set_chunk(
      expression =
        quote(add_row <- function(i, x, p) {
          rrowl(rownames(x)[i], Map(function(xii, pii) c(xii, pii), x[i, ], p[i, ]))
        })
    )
    set_chunk(expression = quote(rows <- lapply(1:nrow(data_table), add_row, x = data_table, p = perc_table)))
    set_chunk(expression = quote(rtablel(header = colnames(data_table), rows, format = "xx (xx.xx%)")))
  })

  output$table <- renderUI({
    table_code()
    as_html(eval_remaining())
  })

  observeEvent(input$show_rcode, {
    teal.devel::show_rcode_modal(
      title = "Cross Table",
      rcode = get_rcode(
        datasets = datasets,
        dataname = dataname,
        title = label
      )
    )
  })
}
