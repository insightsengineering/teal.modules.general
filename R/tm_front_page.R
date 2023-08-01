#' Front page module
#'
#' @description This `teal` module creates a simple front page for `teal` applications
#'
#' @inheritParams teal::module
#' @param header_text `character vector` text to be shown at the top of the module, for each
#'   element, if named the name is shown first in bold as a header followed by the value. The first
#'   element's header is displayed larger than the others
#' @param tables `named list of dataframes` tables to be shown in the module
#' @param additional_tags `shiny.tag.list` or `html` additional shiny tags or `html` to be included after the table,
#'   for example to include an image, `tagList(tags$img(src = "image.png"))` or to include further `html`,
#'   `HTML("html text here")`
#' @param footnotes `character vector` text to be shown at the bottom of the module, for each
#'   element, if named the name is shown first in bold, followed by the value
#' @param show_metadata `logical` should the metadata of the datasets be available on the module?
#' @return A `teal` module to be used in `teal` applications
#' @export
#' @examples
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
#' ADSL <- teal.modules.general::rADSL
#' app <- teal::init(
#'   data = teal.data::cdisc_data(
#'     teal.data::cdisc_dataset("ADSL", ADSL,
#'       code = "ADSL <- teal.modules.general::rADSL",
#'       metadata = list("Author" = "NEST team", "data_source" = "synthetic data")
#'     ),
#'     check = TRUE
#'   ),
#'   modules = teal::modules(
#'     teal.modules.general::tm_front_page(
#'       header_text = c(
#'         "Important information" = "It can go here.",
#'         "Other information" = "Can go here."
#'       ),
#'       tables = table_input,
#'       additional_tags = HTML("Additional HTML or shiny tags go here <br>"),
#'       footnotes = c("X" = "is the first footnote", "Y is the second footnote"),
#'       show_metadata = TRUE
#'     )
#'   ),
#'   header = tags$h1("Sample Application"),
#'   footer = tags$p("Application footer"),
#' )
#' if (interactive()) {
#'   shinyApp(app$ui, app$server)
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
  checkmate::assert_multi_class(additional_tags, classes = c("shiny.tag.list", "html"))
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
    datanames = if (show_metadata) "all" else NULL
  )
}

ui_front_page <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)

  tagList(
    include_css_files("custom"),
    tags$div(
      id = "front_page_content",
      class = "ml-8",
      tags$div(
        id = "front_page_headers",
        get_header_tags(args$header_text)
      ),
      tags$div(
        id = "front_page_tables",
        class = "ml-4",
        get_table_tags(args$tables, ns)
      ),
      tags$div(
        id = "front_page_custom_html",
        class = "my-4",
        args$additional_tags
      ),
      if (args$show_metadata) {
        tags$div(
          id = "front_page_metabutton",
          class = "m-4",
          actionButton(ns("metadata_button"), "Show metadata")
        )
      },
      tags$footer(
        class = ".small",
        get_footer_tags(args$footnotes)
      )
    )
  )
}

get_header_tags <- function(header_text) {
  if (length(header_text) == 0) {
    return(list())
  }

  get_single_header_tags <- function(header_text, p_text, header_tag = tags$h4) {
    tagList(
      tags$div(
        if (!is.null(header_text) && nchar(header_text) > 0) header_tag(header_text),
        tags$p(p_text)
      )
    )
  }

  header_tags <- get_single_header_tags(names(header_text[1]), header_text[1], header_tag = tags$h3)
  c(header_tags, mapply(get_single_header_tags, utils::tail(names(header_text), -1), utils::tail(header_text, -1)))
}

get_table_tags <- function(tables, ns) {
  if (length(tables) == 0) {
    return(list())
  }
  table_tags <- c(lapply(seq_along(tables), function(idx) {
    list(
      tableOutput(ns(paste0("table_", idx)))
    )
  }))
  return(table_tags)
}

get_footer_tags <- function(footnotes) {
  if (length(footnotes) == 0) {
    return(list())
  }
  bold_texts <- if (is.null(names(footnotes))) rep("", length(footnotes)) else names(footnotes)
  footnote_tags <- mapply(function(bold_text, value) {
    list(
      tags$div(
        tags$b(bold_text),
        value,
        tags$br()
      )
    )
  }, bold_text = bold_texts, value = footnotes)
}

srv_front_page <- function(id, data, tables, show_metadata) {
  checkmate::assert_class(data, "tdata")
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    lapply(seq_along(tables), function(idx) {
      output[[paste0("table_", idx)]] <- renderTable(
        tables[[idx]],
        bordered = TRUE,
        caption = names(tables)[idx],
        caption.placement = "top"
      )
    })

    if (show_metadata) {
      observeEvent(
        input$metadata_button, showModal(
          modalDialog(
            title = "Metadata",
            dataTableOutput(ns("metadata_table")),
            size = "l",
            easyClose = TRUE
          )
        )
      )

      metadata_data_frame <- reactive({
        convert_metadata_to_dataframe(
          lapply(names(data), function(dataname) get_metadata(data, dataname)),
          names(data)
        )
      })

      output$metadata_table <- renderDataTable({
        validate(need(nrow(metadata_data_frame()) > 0, "The data has no associated metadata"))
        metadata_data_frame()
      })
    }
  })
}

# take a list of metadata, one item per dataset (raw_metadata each element from datasets$get_metadata())
# and the corresponding datanames and output a data.frame with columns {Dataset, Name, Value}.
# which are, the Dataset the metadata came from, the metadata's name and value
convert_metadata_to_dataframe <- function(raw_metadata, datanames) {
  output <- mapply(function(metadata, dataname) {
    if (is.null(metadata)) {
      return(data.frame(Dataset = character(0), Name = character(0), Value = character(0)))
    }
    return(data.frame(
      Dataset = dataname,
      Name = names(metadata),
      Value = unname(unlist(lapply(metadata, as.character)))
    ))
  }, raw_metadata, datanames, SIMPLIFY = FALSE)
  do.call(rbind, output)
}
