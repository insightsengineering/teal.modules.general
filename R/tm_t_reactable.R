#' @export
tm_t_reactables <- function(label = "Table",
                            datanames = "all",
                            colnames = list(),
                            transformators = list(),
                            decorators = list(),
                            ...) {
  module(
    label = label,
    ui = ui_t_reactables,
    server = srv_t_reactables,
    ui_args = list(decorators = decorators),
    server_args = c(
      list(datanames = datanames, colnames = colnames, decorators = decorators),
      rlang::list2(...)
    ),
    datanames = datanames,
    transformators = transformators
  )
}

ui_t_reactables <- function(id, decorators = list()) {
  ns <- NS(id)
  uiOutput(ns("subtables"), container = bslib::page_fluid)
}

srv_t_reactables <- function(id, data, filter_panel_api, datanames, colnames = list(), decorators = list(), ...) {
  moduleServer(id, function(input, output, session) {
    # todo: this to the function .validate_datanames
    datanames_r <- .validate_datanames(datanames = datanames, data = data)
    colnames_r <- reactive({
      req(datanames_r())
      sapply(datanames_r(), function(dataname) {
        if (length(colnames[[dataname]])) {
          colnames()[[dataname]]
        } else {
          colnames(isolate(data())[[dataname]])
        }
      })
    })

    datalabels_r <- reactive({
      req(datanames_r())
      sapply(datanames_r(), function(dataname) {
        datalabel <- attr(isolate(data())[[dataname]], "label")
        if (length(datalabel)) datalabel else dataname
      })
    })

    # todo: re-render only if datanames changes
    output$subtables <- renderUI({
      logger::log_debug("srv_t_reactables@1 render subtables")
      if (length(datanames_r()) == 0) {
        return(NULL)
      }
      div(
        do.call(
          bslib::accordion,
          c(
            list(id = session$ns("reactables")),
            lapply(
              datanames_r(),
              function(dataname) {
                bslib::accordion_panel(
                  title = datalabels_r()[dataname],
                  ui_t_reactable(session$ns(dataname))
                )
              }
            )
          )
        )
      )
    })

    called_datanames <- reactiveVal()
    observeEvent(datanames_r(), {
      lapply(
        setdiff(datanames_r(), called_datanames()), # call module only once per dataname
        function(dataname) {
          srv_t_reactable(
            dataname,
            data = data,
            dataname = dataname,
            filter_panel_api = filter_panel_api,
            colnames = colnames[[dataname]],
            ...
          )
        }
      )
      called_datanames(union(called_datanames(), datanames_r()))
    })
  })
}

ui_t_reactable <- function(id) {
  ns <- NS(id)
  
  input <- shinyWidgets::pickerInput(
    ns("colnames"),
    label = NULL,
    choices = NULL,
    selected = NULL,
    multiple = TRUE,
    width = "100%",
    options = shinyWidgets::pickerOptions(
      actionsBox = TRUE,
      `show-subtext` = TRUE,
      countSelectedText = TRUE,
      liveSearch = TRUE
    )
  )
  
  # input <- actionButton(ns("show_select_colnames"), "Nothing selected", class = "rounded-pill btn-sm primary") |>
  #   bslib::popover(input)
  bslib::page_fluid(
    input,
    reactable::reactableOutput(ns("table"))
  )
}

srv_t_reactable <- function(id, data, filter_panel_api, dataname, colnames, decorators, ...) {
  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_t_reactable initializing for dataname: { dataname }")
    dataname_reactable <- sprintf("%s_reactable", dataname)

    dataset_labels <- reactive({
      req(data())
      teal.data::col_labels(data()[[dataname]], fill = TRUE)
    })

    cols_choices <- reactiveVal()
    cols_selected <- reactiveVal()
    observeEvent(dataset_labels(), {
      req(dataset_labels())
      choices <- if (length(colnames)) {
        colnames
      } else {
        names(dataset_labels())
      }
      labels_choices <- dataset_labels()[choices]
      cols_choices_new <- setNames(choices, labels_choices)
      if (!identical(cols_choices_new, cols_choices())) {
        logger::log_debug("srv_t_reactable@1 update column choices")
        shinyWidgets::updatePickerInput(
          inputId = "colnames",
          choices = cols_choices_new,
          selected = cols_choices_new
        )
        cols_choices(cols_choices_new)
        cols_selected(cols_choices_new)
      }
    })
    observeEvent(input$colnames_open, `if`(!isTruthy(input$colnames_open), cols_selected(input$colnames)))
    observeEvent(cols_selected(), {
      updateActionButton(
        inputId = "show_select_colnames", 
        label = paste(substring(toString(cols_selected()), 1, 100), "...")
      )
    })

    select_call <- reactive({
      req(cols_selected())
      substitute(
        lhs <- rhs,
        list(
          lhs = str2lang(dataname),
          rhs = as.call(
            c(
              list(name = str2lang("dplyr::select"), .data = str2lang(dataname)),
              lapply(cols_selected(), str2lang)
            )
          )
        )
      )
    })
    reactable_call <- reactive({
      req(cols_selected(), data())
      default_args <- list(
        #columns = .make_reactable_columns_call(data()[[dataname]][cols_selected()]),
        resizable = TRUE,
        onClick = "select",
        defaultPageSize = 10,
        rowClass = JS("
          function(rowInfo) {
              if (rowInfo.selected) {
                return 'selected-row';
              }
          }
        ")
      )
      args <- modifyList(default_args, rlang::list2(...))

      substitute(
        lhs <- rhs,
        list(
          lhs = str2lang(dataname_reactable),
          rhs = .make_reactable_call(dataname = dataname, args = args)
        )
      )
    })

    table_q <- reactive({
      req(reactable_call(), select_call())
      print(reactable_call())
      data() |>
        eval_code(select_call()) |>
        eval_code(reactable_call())
    })
    output$table <- reactable::renderReactable({
      logger::log_debug("srv_t_reactable@2 render table for dataset { dataname }")
      table_q()[[dataname_reactable]]
    })

    table_selected_q <- reactive({
      selected_row <- reactable::getReactableState("table", "selected")
      if (!is.null(selected_row)) {
        within(
          table_q(),
          selected_row = selected_row,
          dataname_selected = str2lang(sprintf("%s_selected", dataname)),
          dataname = str2lang(dataname),
          expr = {
            dataname_selected <- dataname[selected_row, ]
          }
        )
      } else {
        table_q()
      }
    })
    table_selected_q
  })
}

.make_reactable_call <- function(dataname, args) {
  args <- modifyList(
    list(
      data = str2lang(dataname),
      defaultColDef = quote(
        colDef(
          cell = function(value) {
            is_url <- is.character(value) && any(
              grepl(
                "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)",
                x = head(value),
                perl = TRUE
              )
            )
            if (is_url) {
              if (!is.na(value) && !is.null(value) && value != "") {
                htmltools::tags$a(href = value, target = "_blank", "Link")
              } else {
                "N/A"
              }
            } else {
              value
            }
          }
        )
      )
    ),
    args
  )
  as.call(c(list(name = quote(reactable)), args))
}

#' Makes `reactable::colDef` call containing:
#' name = <column label attribute>
#' cell = <url formatter>
#' Arguments of [reactable::colDef()] are specified only if necessary
#' @param dataset (`data.frame`)
#' @return named list of `colDef` calls
#' @keywords internal
.make_reactable_columns_call <- function(dataset) {
  checkmate::assert_data_frame(dataset)
  args <- lapply(
    seq_along(dataset),
    function(i) {
      column <- dataset[[i]]
      label <- attr(column, "label")
      is_labelled <- length(label) == 1 && !is.na(label) && !identical(label, "")
      is_url <- is.character(column) && any(
        grepl(
          "https?:\\/\\/(www\\.)?[-a-zA-Z0-9@:%._\\+~#=]{1,256}\\.[a-zA-Z0-9()]{1,6}\\b([-a-zA-Z0-9()@:%_\\+.~#?&//=]*)",
          x = head(column),
          perl = TRUE
        )
      )
      # todo: move url formatter to the defaultColDef
      width <- max(nchar(head(as.character(column), 100))) * 9
      args <- c(
        if (!is.na(width) && width > 100 && !is_url) list(width = width),
        if (is_labelled) list(name = label)
      )

      if (length(args)) {
        as.call(c(list(name = "colDef"), args))
      }
    }
  )
  names(args) <- names(dataset)
  args <- Filter(length, args)
  if (length(args)) {
    as.call(c(list("list"), args))
  }
}

.name_to_id <- function(name) {
  gsub("[[:space:][:punct:]]+", "_", x = tolower(name))
}

.validate_datanames <- function(datanames, data, class = "data.frame") {
  all_datanames_r <- reactive({
    req(data())
    names(
      Filter(
        function(dataset) inherits(dataset, class), 
        as.list(data())
      )
    )
  })
  
  this_datanames_r <- reactive({
    if (is.reactive(datanames)) {
      datanames()
    } else {
      datanames
    }
  })
  
  datanames_r <- reactiveVal()
  
  observeEvent(all_datanames_r(), {
    new_datanames <- if (identical(this_datanames_r(), "all")) {
      all_datanames_r()
    } else {
      intersect(this_datanames_r(), all_datanames_r())
    }
    if (!identical(new_datanames, datanames_r())) {
      datanames_r(new_datanames)
    }
  })
  datanames_r
}
