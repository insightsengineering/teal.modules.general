#' Front page module
#'
#' @description This `teal` module creates a simple front page for your application
#'
#' @inheritParams teal::module
#' @param header_text `character vector` text to be shown at the top of the module, for each
#'   element, if named the name is shown first in bold as a header followed by the value. The first
#'   element's header is displayed larger than the others
#' @param tables `named list of dataframes` tables to be shown in the module
#' @param additional_tags `shiny.tag.list` additional shiny tags to be included after the table,
#'   for example to include an image, `tagList(tags$img(src = "image.png"))`
#' @param footnotes `character vector` text to be shown at the bottom of the module, for each
#'   element, if named the name is shown first in bold, followed by the value
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
#'       header_text = c("Important information" = "It can go here.", "Other information" = "Can go here."),
#'       tables = table_input,
#'       footnotes = c("X" = "is the first footnote", "Y is the second footnote"),
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
                          header_text = character(0),
                          tables = list(),
                          additional_tags = tagList(),
                          footnotes = character(0),
                          show_metadata = FALSE) {
  checkmate::assert_string(label)
  checkmate::assert_character(header_text, min.len = 0, any.missing = FALSE)
  checkmate::assert_list(tables, types = "data.frame", names = "named", any.missing = FALSE)
  checkmate::assert_class(additional_tags, classes = "shiny.tag.list")
  checkmate::assert_character(footnotes, min.len = 0, any.missing = FALSE)
  checkmate::assert_flag(show_metadata)

  logger::log_info("Initializing tm_front_page")
  args <- as.list(environment())

  module(
    label = label,
    server = srv_front_page,
    ui = ui_front_page,
    ui_args = args,
    server_args = list(tables = tables, show_metadata = show_metadata),
    filters = NULL
  )
}

ui_front_page <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)

  header_tags <- list()
  table_tags <- list()
  footnote_tags <- list()

  get_header_tags <- function(header_text, p_text, header_tag = tags$h4) {
    tagList(
      if (!is.null(header_text) && nchar(header_text) > 0) header_tag(header_text),
      tags$p(p_text)
    )
  }

  if (length(args$header_text) > 0) {
    header_tags <- get_header_tags(names(args$header_text[1]), args$header_text[1], header_tag = tags$h3)
    header_tags <- c(
      header_tags, mapply(get_header_tags, tail(names(args$header_text), -1), tail(args$header_text, -1))
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
    bold_texts <- if (is.null(names(args$footnotes))) rep("", length(args$footnotes)) else names(args$footnotes)
    footnote_tags <- mapply(function(bold_text, value) {
      list(
        HTML(paste("<b>", bold_text, "</b>", value)),
        br()
      )
    }, bold_text = bold_texts, value = args$footnotes)
  }

  tagList(
    header_tags,
    table_tags,
    args$additional_tags,
    footnote_tags,
    if (args$show_metadata) actionButton(ns("metadata_button"), "Show metadata")
  )
}

srv_front_page <- function(id, datasets, tables, show_metadata) {
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

    if (show_metadata) {

      metadata_data_frame <- reactive({
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
              Value = unlist(lapply(single_dataset_metadata, as.character))
            ))
          }
        )
        do.call(rbind, output)
      })

      output$metadata_table <- renderDataTable({
        validate(need(nrow(metadata_data_frame()) > 0, "The data has no associated metadata"))
        metadata_data_frame()
      })
    }
  })
}
