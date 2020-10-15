#' Create a simple cross-table
#' @md
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param x (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'  Object with all available choices with pre-selected option for variable X - row values.
#'  \code{data_extract_spec} must not allow multiple selection in this case.
#' @param y (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'  Object with all available choices with pre-selected option for variable Y - column values
#'  \code{data_extract_spec} must not allow multiple selection in this case.
#' @param show_percentage optional, (`logical`) Whether to show percentages
#'   (relevant only when `x` is a `factor`). Defaults to `TRUE`.
#' @param show_total optional, (`logical`) Whether to show total column. Defaults to `TRUE`.
#'
#' @note For more examples, please see the vignette "Using cross table" via
#'   `vignette("using-cross-table", package = "teal.modules.general")`.
#'
#' @export
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
#'           choices = variable_choices(ADSL, c("SEX", "RACE")),
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
  stop_if_not(
    is_character_single(label),
    is_class_list("data_extract_spec")(x) || is(x, "data_extract_spec"),
    is_class_list("data_extract_spec")(y) || is(y, "data_extract_spec"),
    is_logical_single(show_percentage),
    is_logical_single(show_total),
    list(
      (is(x, "data_extract_spec") && !isTRUE(x$select$multiple)) ||
        (is_class_list("data_extract_spec")(x) && all(vapply(x, function(xx) !isTRUE(xx$select$multiple), logical(1)))),
      "x variable should not allow multiple selection"
    ),
    list(
      (is(y, "data_extract_spec") && !isTRUE(y$select$multiple)) ||
        (is_class_list("data_extract_spec")(y) && all(vapply(y, function(yy) !isTRUE(yy$select$multiple), logical(1)))),
      "y variable should not allow multiple selection"
    )
  )

  ui_args <- as.list(environment())

  server_args <- list(
    label = label,
    show_percentage = show_percentage,
    x = x,
    y = y
  )

  module(
    label = label,
    server = srv_t_crosstable,
    ui = ui_t_crosstable,
    ui_args = ui_args,
    server_args = server_args,
    filters = get_extract_datanames(list(x = x, y = y))
  )
}

ui_t_crosstable <- function(id, datasets, x, y, show_total, pre_output, post_output, ...) {
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
          uiOutput(ns("show_percentage_ui")),
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
#' @importFrom rtables as_html
srv_t_crosstable <- function(input, output, session, datasets, label, show_percentage, x, y) {
  ns <- session$ns
  init_chunks()

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = list(x, y),
    input_id = c("x", "y")
  )

  output$show_percentage_ui <- renderUI({
    ANL <- merged_data()$chunks$get("ANL") # nolint
    x_name <- merged_data()$columns_source$x

    if (inherits(ANL[[x_name]], "factor")) {
      checkboxInput(ns("show_percentage"), "Show percentage", value = show_percentage)
    }
  })

  vals <- reactiveValues(show_percentage = show_percentage)

  create_table <- reactive({
    chunks_reset()
    chunks_push_data_merge(merged_data())

    ANL <- chunks_get_var("ANL") # nolint

    # As this is a summary
    validate_has_data(ANL, 3)

    x_name <- as.vector(merged_data()$columns_source$x)
    y_name <- as.vector(merged_data()$columns_source$y)

    validate(need(!is_character_empty(x_name), "Please define column for x variable that is not empty."))
    validate(need(!is_character_empty(y_name), "Please define column for y variable that is not empty."))

    validate_has_data(ANL[, c(x_name, y_name)], 3, complete = TRUE, allow_inf = FALSE)


    supported_types <- c("numeric", "integer", "factor", "character", "logical")
    validate(need(
      class(ANL[[x_name]]) %in% supported_types,
      "Selected x-variable has an unsupported data type."
    ))
    validate(need(
      class(ANL[[y_name]]) %in% setdiff(supported_types, "character"),
      "Selected y-variable has an unsupported data type."
    ))

    plot_title <- paste(
      "Cross-Table of",
      varname_w_label(x_name, ANL),
      "(rows)", "vs.",
      varname_w_label(y_name, ANL),
      "(columns)"
    )

    observeEvent(
      input$show_percentage,
      vals$show_percentage <- input$show_percentage
    )

    chunks_push(bquote({
      title <- .(plot_title)
      print(title)

      tbl <- tern::t_summary(
        ANL[[.(x_name)]],
        col_by = ANL[[.(y_name)]],
        total = .(if (input$show_total) "Total" else NULL),
        denominator = .(if (inherits(ANL[[x_name]], "factor") && vals$show_percentage) "n" else "omit")
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
    datanames = get_extract_datanames(list(x, y)),
    modal_title = show_r_code_title(),
    code_header = show_r_code_title()
  )
}
