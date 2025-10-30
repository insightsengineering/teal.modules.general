#' `teal` module: Reactable
#'
#' Wrapper module on [reactable::reactable()]
#'
#' @inheritParams teal::module
#' @inheritParams shared_params
#' @param reactable_args (`list`) any argument of [reactable::reactable()].
#' @export
tm_t_reactables <- function(label = "Table",
                            datanames = "all",
                            colnames = list(),
                            transformators = list(),
                            decorators = list(),
                            reactable_args = list()) {
  module(
    label = label,
    ui = ui_t_reactables,
    server = srv_t_reactables,
    ui_args = list(decorators = decorators),
    server_args = list(
      datanames = datanames,
      colnames = colnames,
      reactable_args = reactable_args,
      decorators = decorators
    ),
    datanames = datanames,
    transformators = transformators
  )
}

ui_t_reactables <- function(id, decorators = list()) {
  ns <- NS(id)
  uiOutput(ns("subtables"), container = div)
}

srv_t_reactables <- function(
  id, data, filter_panel_api, datanames,
  colnames = list(), decorators = list(), reactable_args = list()
) {
  moduleServer(id, function(input, output, session) {
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

    output$subtables <- renderUI({
      logger::log_debug("srv_t_reactables@1 render subtables")
      if (length(datanames_r()) == 0) {
        return(NULL)
      }
      div(
        htmltools::htmlDependency(
          name = "teal-modules-general-reactable",
          version = utils::packageVersion("teal.modules.general"),
          package = "teal.modules.general",
          src = "css",
          stylesheet = "reactable.css"
        ),
        do.call(
          bslib::accordion,
          c(
            list(id = session$ns("reactables"), class = "teal-modules-general reactable-accordion"),
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
            reactable_args = reactable_args
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
      liveSearch = TRUE,
      container = "body"
    )
  )

  # input <- actionButton(ns("show_select_colnames"), "Nothing selected", class = "rounded-pill btn-sm primary") |>
  #   bslib::popover(input)
  bslib::page_fluid(
    input,
    bslib::card(
      class = "teal-modules-general reactable-card",
      full_screen = TRUE,
      reactable::reactableOutput(ns("table"))
    )
  )
}

srv_t_reactable <- function(id, data, filter_panel_api, dataname, colnames, decorators, reactable_args = list()) {
  moduleServer(id, function(input, output, session) {
    logger::log_debug("srv_t_reactable initializing for dataname: { dataname }")
    dataname_reactable <- sprintf("%s_reactable", dataname)

    dataset_labels <- reactive({
      req(data())
      teal.data::col_labels(data()[[dataname]], fill = TRUE)
    })

    reactable_args_r <- if (is.reactive(reactable_args)) reactable_args else reactive(reactable_args)

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
      cols_choices_new <- stats::setNames(choices, labels_choices)
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

    table_q <- reactive({
      req(cols_selected())
      select_call <- as.call(
        c(
          list(name = str2lang("dplyr::select"), .data = str2lang(dataname)),
          lapply(unname(cols_selected()), str2lang)
        )
      )

      reactable_call <- .make_reactable_call(
        dataset = data()[[dataname]][cols_selected()],
        dataname = dataname,
        args = reactable_args_r()
      )

      data() |>
        within(lhs <- rhs, lhs = str2lang(dataname), rhs = select_call) |>
        within(lhs <- rhs, lhs = str2lang(dataname_reactable), rhs = reactable_call)
    })
    output$table <- reactable::renderReactable({
      logger::log_debug("srv_t_reactable@2 render table for dataset { dataname }")
      table_q()[[dataname_reactable]]
    })

    # todo: add select -> show children table
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

.make_reactable_call <- function(dataset, dataname, args) {
  columns <- .make_reactable_columns_call(dataset = dataset, col_defs = args$columns)
  call_args <- utils::modifyList(
    list(columns = columns, onClick = "select", selection = "multiple"),
    args[!names(args) %in% "columns"]
  )
  as.call(
    c(
      list(
        name = quote(reactable::reactable),
        data = str2lang(dataname)
      ),
      call_args
    )
  )
}

#' Makes `reactable::colDef` call containing:
#' name = <column label attribute>
#' cell = <url formatter>
#' Arguments of [reactable::colDef()] are specified only if necessary
#' @param dataset (`data.frame`)
#' @return named list of `colDef` calls
#' @keywords internal
.make_reactable_columns_call <- function(dataset, col_defs) {
  checkmate::assert_data_frame(dataset)
  args <- lapply(
    colnames(dataset),
    function(i) {
      column <- dataset[[i]]
      label <- attr(column, "label")
      is_labelled <- length(label) == 1 && !is.na(label) && !identical(label, "")
      default_col_def <- if (is_labelled) list(name = label) else list()
      col_def_override <- if (!is.null(col_defs[[i]])) col_defs[[i]] else list()
      col_def_args <- utils::modifyList(default_col_def, col_def_override)
      if (length(col_def_args)) {
        as.call(
          c(
            list(quote(colDef)),
            col_def_args
          )
        )
      }
    }
  )
  names(args) <- names(dataset)
  Filter(length, args)
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
