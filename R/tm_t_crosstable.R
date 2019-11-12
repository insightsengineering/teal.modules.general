#' Create a simple cross-table
#'
#' @inheritDotParams teal.devel::standard_layout -output -encoding -forms
#' @param label (\code{character}) Label of the app in the teal menu
#' @param x (\code{choices_selected}) object with all available
#'   choices with pre-selected option for variable X
#' @param y (\code{choices_selected}) object with all available
#'   choices with pre-selected option for variable Y
#' @param show_percentage whether to show percentages (relevant only when \code{x} is a \code{factor})
#' @param show_total whether to show total column
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#'
#' @export
#'
#' @author wolfs25 waddella
#'
#' @examples
#' # Percentage cross table of variables from ADSL dataset
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
#'     tm_t_crosstable(
#'       label = "Cross Table",
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL, c("COUNTRY", "STUDYID")),
#'           selected = "COUNTRY",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL, c("SEX","RACE")),
#'           selected = "SEX",
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
tm_t_crosstable <- function(label = "Cross Table",
                           x,
                           y,
                           show_percentage = TRUE,
                           show_total = TRUE,
                           pre_output = NULL,
                           post_output = NULL) {
  stopifnot(is_character_single(label))
  stopifnot(is_class_list("data_extract_spec")(x) || is(x, "data_extract_spec"))
  stopifnot(is_class_list("data_extract_spec")(y) || is(y, "data_extract_spec"))
  if (is_class_list("data_extract_spec")(x)) {
    stop_if_not(list(all(vapply(x, function(x) !isTRUE(x$select$multiple), logical(1))),
                     "x variable should not allow multiple selection"))
  } else if (is(x, "data_extract_spec")) {
    stop_if_not(list(!isTRUE(x$select$multiple),
                     "x variable should not allow multiple selection"))
  }
  if (is_class_list("data_extract_spec")(y)) {
    stop_if_not(list(all(vapply(y, function(x) !isTRUE(x$select$multiple), logical(1))),
                     "y variable should not allow multiple selection"))
  } else if (is(y, "data_extract_spec")) {
    stop_if_not(list(!isTRUE(y$select$multiple),
                     "y variable should not allow multiple selection"))
  }
  stopifnot(is_logical_single(show_percentage))
  stopifnot(is_logical_single(show_total))
  args <- as.list(environment())

  module(
    label = label,
    server = srv_t_crosstable,
    ui = ui_t_crosstable,
    ui_args = args,
    server_args = list(label = label, x = x, y = y),
    filters = "all"
  )
}

ui_t_crosstable <- function(id, datasets, x, y, show_percentage, show_total, pre_output, post_output, ...) {
  ns <- NS(id)

  standard_layout(
    output = white_small_well(
      textOutput(ns("title")),
      tags$br(),
      uiOutput(ns("table"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(list(x, y)),
      data_extract_input(ns("x"), label = "Row values", x),
      tags$hr(),
      data_extract_input(ns("y"), label = "Column values", y),
      tags$hr(),
      panel_group(
        panel_item(
          title = "Table settings",
          checkboxInput(ns("show_percentage"), "Show percentage", value = show_percentage),
          checkboxInput(ns("show_total"), "Show total column", value = show_total)
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = pre_output,
    post_output = post_output
  )
}

#' @importFrom tern t_summary
#' @importFrom rtables rrowl rtablel as_html
srv_t_crosstable <- function(input, output, session, datasets, label, x, y) {
  init_chunks(session)

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(x, y),
    input_id = c("x", "y")
  )

  create_table <- reactive({
    ANL <- merged_data()$data() # nolint
    validate_has_data(ANL, 3)
    chunks_reset()

    x_name <- unname(merged_data()$columns_source$x)
    y_name <- unname(merged_data()$columns_source$y)

    validate(need(!is_character_empty(x_name), "Please define column for x variable that is not empty."))
    validate(need(!is_character_empty(y_name), "Please define column for y variable that is not empty."))

    plot_title <- paste(
      "Cross-Table of",
      paste(attr(ANL[[x_name]], "label"), paste0("[", x_name, "]")),
      "(rows)", "vs.",
      paste(attr(ANL[[y_name]], "label"), paste0("[", y_name, "]")),
      "(columns)"
    )
    chunks_push(bquote({
      title <- .(plot_title)
      print(title)

      tbl <- tern::t_summary(
        ANL[[.(x_name)]],
        col_by = ANL[[.(y_name)]],
        total = .(if (input$show_total) "Sum" else NULL),
        denominator = .(if (input$show_percentage) "n" else "omit")
      )
      tbl
    }))

    chunks_safe_eval()
  })

  output$title <- renderText({
    create_table()
    chunks_get_var("title")
  })

  output$table <- renderUI({
    as_html(create_table())
  })

  show_r_code_title <- reactive(
    paste(
      "Cross-Table of",
      merged_data()$columns_source$x,
      "vs.",
      merged_data()$columns_source$y
    )
  )

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    merge_expression = merged_data()$expr,
    modal_title = show_r_code_title(),
    code_header = show_r_code_title()
  )
}
