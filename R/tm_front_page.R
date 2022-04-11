#' Front page module
#'
#' @description This `teal` module creates a simple front page for your application
#'
#' @inheritParams teal::module
#' @param bold_header_text `string` text to be shown in bold at the top of the module
#' @param header_text `string` text to be shown (not in bold) at the top of the module
#' @param tables `named list of dataframes` tables to be shown in the module
#' @param footnotes `character` a vector of text to be shown at the bottom of the module, the first
#'   word of each element is shown in bold.
#' @param show_metadata `logical` should the metadata of the datasets be available on the module?
#' @return A `teal` module to be used in `teal` applications
#' @export
#' @examples
#' library(scda)
#'
#' table_1 <- data.frame(Info = c("A", "B"), Text = c("A", "B"))
#' table_2 <- data.frame(`Column 1` = c("C", "D"), `Column 2` = c(5.5, 6.6), `Column 3` = c("A", "B"))
#' table_3 <- data.frame(Info = c("E", "F"), Text = c("G", "H"))
#'
#' table_input <- list(
#'   "Table 1" = table_1,
#'   "Table 2" = table_2,
#'   "Table 3" = table_3
#' )
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL,
#'       code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl",
#'       metadata = list("Author" = "NEST team", "data_source" = "synthetic data")
#'     ),
#'     check = TRUE
#'   ),
#'   modules = modules(
#'     tm_front_page(
#'       bold_header_text = "Important information here",
#'       header_text = "Other information added here",
#'       tables = table_input,
#'       footnotes = c("X is the first footnote", "Y is the second footnote"),
#'       show_metadata = TRUE
#'     )
#'   ),
#'   header = tags$h1("Sample Application"),
#'   footer = tags$p("Application footer"),
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_front_page <- function(label = "Front page",
                          bold_header_text = NULL,
                          header_text = NULL,
                          tables = list(),
                          footnotes = character(0),
                          show_metadata = FALSE) {
  checkmate::assert_string(label)
  checkmate::assert_string(bold_header_text, null.ok = TRUE)
  checkmate::assert_string(header_text, null.ok = TRUE)
  checkmate::assert_list(tables, types = "data.frame", names = "named", any.missing = FALSE)
  checkmate::assert_character(footnotes, min.len = 0, any.missing = FALSE)
  checkmate::assert_flag(show_metadata)

  logger::log_info("Initializing tm_front_page")
  args <- as.list(environment())

  module(
    label = label,
    server = srv_front_page,
    ui = ui_front_page,
    ui_args = args,
    server_args = list(tables = tables),
    filters = NULL
  )
}

ui_front_page <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)

  header_tags <- list()
  table_tags <- list()
  footnote_tags <- list()

  if (!is.null(args$bold_header_text)) {
    header_tags <- c(
      header_tags,
      list(tags$strong(args$bold_header_text), br())
    )
  }

  if (!is.null(args$header_text)) {
    header_tags <- c(
      header_tags,
      list(p(args$header_text), br())
    )
  }

  if (length(args$tables) > 0) {
    table_names <- names(args$tables)
    table_tags <- c(lapply(seq_along(table_names), function(idx) {
      list(
        tags$strong(table_names[idx]),
        tableOutput(ns(paste0("table_", idx)))
      )
    }))
  }

  if (length(args$footnotes) > 0) {
    footnotes <- strsplit(args$footnotes, "\\s+")
    footnote_tags <- lapply(footnotes, function(note) {
      list(
        HTML(paste("<b>", note[1], "</b>", paste(tail(note, -1), collapse = " "))),
        br()
      )
    })
  }

  tagList(
    header_tags,
    table_tags,
    footnote_tags,
    if (args$show_metadata) actionButton(ns("metadata_button"), "Show metadata")
  )
}

srv_front_page <- function(id, datasets, tables) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    lapply(seq_along(tables), function(idx) {
      output[[paste0("table_", idx)]] <- renderTable(tables[[idx]], bordered = TRUE)
    })

    observeEvent(
      input$metadata_button, showModal(
        modalDialog(
          title = "Metadata",
          dataTableOutput(ns("metadata_table")),
          size = "l"
        )
      )
    )

    output$metadata_table <- renderDataTable({
      output <- lapply(
        datasets$datanames(),
        function(dataname) {
          single_dataset_metadata <- datasets$get_metadata(dataname)
          if (is.null(single_dataset_metadata)) {
            return(data.frame(Dataset = character(0), Name = character(0), Value = character(0)))
          }
          return(data.frame(
            Dataset = dataname,
            Name = names(single_dataset_metadata),
            Value = unlist(unname(single_dataset_metadata))
          ))
        }
      )
      output <- do.call(rbind, output)
      validate(need(nrow(output) > 0, "The data has no associated metadata"))
      output
    })
  })
}
