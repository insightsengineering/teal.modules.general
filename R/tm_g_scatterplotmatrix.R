#' Create a scatterplot matrix
#'

#' The available datasets to choose from for each dataset selector is the same and
#' determined by the argument \code{variables}.
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @inheritParams tm_g_scatterplot
#'
#' @param variables (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'  Plotting variables from an incoming dataset with filtering and selecting.
#' @export
#' @importFrom stats na.omit
#' @importFrom shinyjs show hide
#' @examples
#' # Scatterplot matrix of variables from ADSL dataset
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
#'     tm_g_scatterplotmatrix(
#'       label = "Scatterplot matrix",
#'       variables = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = variable_choices(ADSL),
#'           selected = c("AGE", "RACE", "SEX"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_scatterplotmatrix <- function(label = "Scatterplot matrix",
                                   variables,
                                   plot_height = c(600, 200, 2000),
                                   pre_output = NULL,
                                   post_output = NULL) {
  if (!is_class_list("data_extract_spec")(variables)) {
    variables <- list(variables)
  }

  stopifnot(is_character_single(label))
  stopifnot(is_class_list("data_extract_spec")(variables))
  stopifnot(is_numeric_vector(plot_height) && (length(plot_height) == 3 || length(plot_height) == 1))
  stopifnot(`if`(length(plot_height) == 3, plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3], TRUE))
  stopifnot(all(plot_height >= 0))

  args <- as.list(environment())
  module(
    label = label,
    server = srv_g_scatterplotmatrix,
    ui = ui_g_scatterplotmatrix,
    ui_args = args,
    server_args = list(variables = variables),
    filters = get_extract_datanames(variables)
  )
}


ui_g_scatterplotmatrix <- function(id, ...) {
  args <- list(...)
  ns <- NS(id)
  standard_layout(
    output = white_small_well(plot_height_output(ns("myplot"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args$variables),
      lapply(get_extract_datanames(args$variables), function(dataname) {
        div(actionLink(ns(sprintf("add_%s_selector", dataname)),
                   sprintf("Add %s selector", dataname)))
      }),
      hr(),
      plot_height_input(id = ns("myplot"), value = args$plot_height),
      panel_group(
        panel_item(
          title = "Plot settings",
          sliderInput(
            ns("alpha"), "Opacity:", min = 0, max = 1,
            step = .05, value = .5, ticks = FALSE
          ),
          sliderInput(
            ns("cex"), "Points size:", min = 0.2, max = 3,
            step = .05, value = .65, ticks = FALSE
          ),
          checkboxInput(ns("cor"), "Add Correlation", value = FALSE),
          radioButtons(ns("cor_method"), "Select Correlation Method",
                       choiceNames = c("Pearson", "Kendall", "Spearman"),
                       choiceValues = c("pearson", "kendall", "spearman"),
                       inline = TRUE),
          checkboxInput(ns("cor_na_omit"), "Omit Missing Values",
                        value = TRUE)
          )
        )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}


#' @importFrom magrittr %>%
#' @importFrom dplyr mutate_if
#' @importFrom lattice splom panel.splom panel.text current.panel.limits
#' @importFrom methods substituteDirect
#' @importFrom stats cor.test
srv_g_scatterplotmatrix <- function(input,
                                    output,
                                    session,
                                    datasets,
                                    variables) {

  init_chunks()

  # rv to keep all current data_extract which should be in UI
  # idx - id of the last data_extract (incremented by clicking add selector)
  rv <- reactiveValues(idx = length(variables),
                       data_extracts = setNames(variables, seq_along(variables)))

  for (dataname in get_extract_datanames(variables)) {
    observe({
      possible  <- possible_extract_selectors(data_extracts = variables, dataname = dataname) # nolint
      current   <- current_extract_selectors(input = input) # nolint
      available <- setdiff(possible, current)

      # adding selector possible only if :
      # - there are any unique filter combination left from specific dataset
      if (length(available) == 0) {
        shinyjs::hide(sprintf("add_%s_selector", dataname))
      } else {
        shinyjs::show(sprintf("add_%s_selector", dataname))
      }
    })


    observeEvent(input[[sprintf("add_%s_selector", dataname)]], {
      # increment id of data extract
      rv$idx <- rv$idx + 1

      # new data_extract in encoding panel shouldn't be duplicated - find next different extract
      possible  <- possible_extract_selectors(variables, dataname) # nolint
      current   <- current_extract_selectors(input) # nolint
      available <- setdiff(possible, current)
      if (length(available) > 0) {
        to_add <- available[[1]]

        # - here we modify current data_extracts passed as argument to app, changing only filters
        # - modified variable data has to match selected dataname, and needs to have the same set of
        #   filters (because the same dataset can have multiple data_extracts with different set of
        #   filters)
        new_data_extract <- variables[
          sapply(variables, `[[`, "dataname") == to_add$dataname &
          sapply(variables, function(x) length(x$filter)) == length(to_add$filter)
        ][1]
        if (!is.null(new_data_extract[[1]]$filter)) {
          for (i in seq_along(to_add$filter)) {
            new_data_extract[[1]]$filter[[i]]$selected <- as.list(
              setNames(
                to_add$filter[[i]],
                to_add$filter[[i]]
              )
            )
          }

        }
      } else {
        return(NULL)
      }

      # add new data_extract to list od data_extracts with the unique idx
      rv$data_extracts <- append(rv$data_extracts, setNames(new_data_extract, rv$idx))
    })
  }

  observeEvent(rv$data_extracts, {
    # if some rv$data_extract are not yet created in shiny
    # - create ui element and remove observer.
    validate(need(length(rv$data_extracts) > 0, "Need at least 1 input."))

    queue <- selectors_waiting(idx = names(rv$data_extracts), input = input) # nolint
    if (length(queue) == 0) return(NULL)
    for (idx in queue) {
      add_selector(input = input,
                   output = output,
                   session = session,
                   idx = idx,
                   label = paste0("Selector (", idx, ")"),
                   data_extract = rv$data_extracts[idx])
      remove_selector(input, output, session, rv, idx) # nolint
    }
  })

  merged_data <- reactive({
    queue <- selectors_waiting(idx = names(rv$data_extracts), input = input) # nolint
    req(length(queue) == 0) # all selector are rendered

    data_merge_module(
      datasets = datasets,
      data_extract = unname(rv$data_extracts),
      input_id = selector_id(idx = names(rv$data_extracts))
    )()
  })

  # Insert the plot into a plot_height module
  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  # plot
  output$plot <- renderPlot({
    chunks_reset()
    chunks_push_data_merge(merged_data())

    validate_has_data(chunks_get_var("ANL"), 3)

    alpha <- input$alpha # nolint
    cex <- input$cex # nolint
    add_cor <- input$cor # nolint
    cor_method <- input$cor_method # nolint
    cor_na_omit <- input$cor_na_omit # nolint

    cor_na_action <- if (cor_na_omit) {
      "na.omit"
    } else {
      "na.fail"
    }

    cols_names <- unique(unname(do.call(c, merged_data()$columns_source)))
    validate(need(length(cols_names) > 1, "Need at least 2 columns."))

    # create plot
    if (add_cor) {
      shinyjs::show("cor_method")
      shinyjs::show("cor_use")
      shinyjs::show("cor_na_omit")

      chunks_push(bquote({
        lattice::splom(ANL[, .(cols_names)] %>% droplevels(),
                       panel = function(x, y, ...) {
                         lattice::panel.splom(x = x, y = y, ...)
                         cpl <- lattice::current.panel.limits()
                         lattice::panel.text(mean(cpl$xlim),
                                             mean(cpl$ylim),
                                             get_scatterplotmatrix_stats(x, y, .f = cor.test,
                                                                         .f_args = list(method = .(cor_method),
                                                                                        na.action = .(cor_na_action))),
                                             alpha = 0.6,
                                             fontsize = 18,
                                             fontface = "bold")
                       },
                       pch = 16,
                       alpha = .(alpha),
                       cex = .(cex))
      }))
    } else {
      shinyjs::hide("cor_method")
      shinyjs::hide("cor_use")
      shinyjs::hide("cor_na_omit")
      chunks_push(bquote({
        lattice::splom(ANL[, .(cols_names)] %>% droplevels(), pch = 16, alpha = .(alpha), cex = .(cex))
      }))
    }
    chunks_safe_eval()
  })
  # show r code
  observeEvent(input$show_rcode, {
    title <- paste0(
      "Scatterplotmatrix of ",
      paste(merged_data()$cols, collapse = ", ")
    )

    show_rcode_modal(
      title = "R Code for a Scatterplotmatrix",
      rcode = get_rcode(
        datasets = datasets,
        title = title
      )
    )
  })

  show_r_code_title <- reactive(
    paste0(
      "Scatterplotmatrix of ",
      paste(unlist(merged_data()$columns_source), collapse = ", ")
    )
  )

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    modal_title = show_r_code_title(),
    code_header = show_r_code_title()
  )
}

#' Add data extract input to ui
#'
#' Add data extract input to ui
#' @param input shiny object
#' @param output shiny object
#' @param session shiny object
#' @param idx id of input
#' @param label label to be attached to data extract input
#' @param data_extract \code{data_extract_spect} to be rendered in ui
add_selector <- function(input, output, session, idx, label, data_extract) {
  insertUI(
    selector = paste0("#", session$ns(sprintf("add_%s_selector", data_extract[[1]]$dataname))),
    where = "beforeBegin",
    ui = div(
      id = extract_ui_id(idx, session),
      data_extract_input(
        id = selector_id(idx, session),
        label = label,
        data_extract_spec = data_extract
      ),
      div(actionLink(extract_remove_id(idx, session), label = "Remove"),
          align = "right")
    ),
    session = session
  )
}

#' Removes data extract input from ui
#'
#' Removes data extract input from DOM and set input reactive value to \code{NULL}
#' @param input shiny object
#' @param output shiny object
#' @param session shiny object
#' @param rv reactive object containing \code{data_extracts} slot
#' @param idx id of input
remove_selector <- function(input, output, session, rv, idx) {
  observeEvent(input[[extract_remove_id(idx)]], {

    if (length(rv$data_extracts) <= 1) {
      cat("need at least one data extract to select from\n")
    }
    req(length(rv$data_extracts) > 1)

    removeUI(selector = paste0("#", extract_ui_id(idx, session)),
             multiple = TRUE,
             immediate = TRUE,
             session = session)


    inputs_to_remove <- get_extract_active_inputs(selector_id(idx), input) # nolint

    for (i in inputs_to_remove) {
      shinyjs::runjs(sprintf("Shiny.setInputValue('%s', null);", session$ns(i)))
    }

    rv$data_extracts <- rv$data_extracts[names(rv$data_extracts) != idx]
  })
}

#' HTML id of the UI element that contains the extract input
#'
#' HTML id of the UI element that contains the extract input
#' @param idx index or identifier of the data extract UI that contains the input element
#' @param session shiny session object
#' @return the html id of the element given the \code{idx}
extract_ui_id <- function(idx, session) {
  if (missing(session)) {
    paste0("extract_ui_", idx)
  } else {
    session$ns(paste0("extract_ui_", idx))
  }
}

#' HTML id of the extract input contained within another UI
#'
#' HTML id of the extract input contained within another UI
#' @param idx index of the selector
#' @param session shiny session object
#' @return the html id of the element given the \code{idx}
selector_id <- function(idx, session) {
  if (missing(session)) {
    paste0("selector_", idx)
  } else {
    session$ns(paste0("selector_", idx))
  }
}

#' HTML id of the element to remove the whole extract UI
#'
#' HTML id of the element to remove the whole extract UI
#' @param idx index of the selector
#' @param session shiny session object
#' @return the html id of the element given the \code{idx}
extract_remove_id <- function(idx, session) {
  if (missing(session)) {
    paste0("extract_remove_", idx)
  } else {
    session$ns(paste0("extract_remove_", idx))
  }
}

#' Get list of non-empty shiny inputs
#'
#' Get list of non-empty shiny inputs
#' @param input shiny input object
#' @param inverse \code{logical} \code{TRUE} for non-active input names
#' @return names of the inputs which are not \code{NULL}
get_active_inputs <- function(input, inverse = FALSE) {
  res <- vapply(reactiveValuesToList(input), utils.nest::is_empty, logical(1), USE.NAMES = TRUE)
  if (inverse) {
    names(res[res])
  } else {
    names(res[!res])
  }

}

#' Index of the UI in rendering queue
#'
#' Index of the UI in rendering queue
#' @param idx index of the selector
#' @param input shiny input object
#' @param inverse if opposite search should be applied
#' @return id which are not rendered yet (found in shiny input names)
#' or not-found if \code{inverse = TRUE}
selectors_waiting <- function(idx, input, inverse = FALSE) {
  input_names <- get_active_inputs(input) # nolint
  ui_ids <- selector_id(idx = idx) # nolint

  which_rendered <- vapply(ui_ids, function(x) {
    any(grepl(x, input_names))
  }, FUN.VALUE = logical(1), USE.NAMES = FALSE)

  if (inverse) {
    idx[which_rendered]
  } else {
    idx[!which_rendered]
  }
}

#' Active inputs
#'
#' @param idx pattern to be found in names of the input object
#' @param input shiny input object
#' @param inverse \code{logical = TRUE} for non-active inputs
#' @return vector of input names matching id pattern
get_extract_active_inputs <- function(idx, input, inverse = FALSE) {
  input_names <- get_active_inputs(input) # nolint
  grep(idx, input_names, value = TRUE, invert = inverse)
}

#' Filter choices from \code{data_extract_spec}
#'
#' Creates combination of filter choices from \code{data_extract_spec}. Each choice is being
#' converted into separate list with \code{dataname} and \code{filter} elements
#' @param x \code{data_extract_spec} object
#' @return lists with one element per choice alongside with dataname
extract_filters_choices <- function(x) {
  stopifnot(is(x, "data_extract_spec"))

  filters_comb <- expand.grid(
    lapply(x[["filter"]], function(xx) {
      unname(xx[["choices"]])
    })
  )
  if (length(filters_comb) == 0) {
    list(list(dataname = x[["dataname"]]))
  } else {
    apply(filters_comb, 1, function(xx) {
      list(
        dataname = x[["dataname"]],
        filter = unname(
          lapply(xx,
                 function(xxx) filter <- unname(xxx)
          )
        )
      )
    })
  }
}

#' Possible data extracts
#'
#' Possible combination of dataname and selected filters from available \code{data_extract_spec}.
#' @param data_extracts list of \code{data_extract_spec}
#' @param dataname name of dataset
#' @return list of possible data extract spec - only dataname and selected filter
possible_extract_selectors <- function(data_extracts, dataname) {
  data_extracts <- data_extracts[sapply(data_extracts, `[[`, "dataname") == dataname]
  filters <- list()
  for (d in data_extracts) {
    filters <- append(filters, extract_filters_choices(d)) # nolint
  }

  return(unique(filters))
}

#' Current data extract selectors
#'
#' List of unique data extract selectors basing on the dataname and applied filters.
#'
#' @param input shiny object
#' @return list of selectors defined by dataname and selected filter
#' @importFrom stringr str_extract
current_extract_selectors <- function(input) {
  inputs <- get_active_inputs(input) # nolint

  filter_inputs <- grep("selector_[0-9]+-dataset_[[:alnum:]]+_singleextract-filter", inputs, value = TRUE)
  filter_ds <- unique(gsub("^.+-dataset_([[:alnum:]]+)_singleextract-filter.*$", "\\1", filter_inputs))
  filter_selectors <- unique(stringr::str_extract(filter_inputs, "selector_[0-9]+"))

  select_inputs <- grep("selector_[0-9]+-dataset_[[:alnum:]]+_singleextract-select", inputs, value = TRUE)
  select_ds <- unique(gsub("^.+-dataset_([[:alnum:]]+)_singleextract-select$", "\\1", select_inputs))

  non_filter_ds <- setdiff(select_ds, filter_ds)

  if (length(c(select_ds, filter_ds)) == 0) return(NULL)

  if (length(filter_selectors) > 0) {
    filtered_selectors <- lapply(filter_selectors, function(selector) {
      filters <- sort(grep(selector, filter_inputs, value = TRUE))
      dataname <- gsub("^.+-dataset_([[:alnum:]]+)_singleextract-filter.*$", "\\1", filters)[1]

      list(
        dataname = dataname,
        filter = lapply(filters, function(f) input[[f]])
      )
    })

  } else {
    filtered_selectors <- list()
  }

  if (length(non_filter_ds) > 0) {
    non_filtered_selectors <- lapply(non_filter_ds, function(x) list(dataname = x))
  } else {
    non_filtered_selectors <- list()
  }


  selectors <- append(filtered_selectors, non_filtered_selectors)

  return(unique(selectors))
}

#' Get stats for x-y pairs in scatterplot matrix
#' @description uses cor.test per default for all numerical input variables and converts results
#'  to character vector. Could be extended if different stats for different variable
#'  types are needed. Meant to be called from \code{lattice::panel.text}.
#' @param x \code{numeric}
#' @param y \code{numeric}
#' @param .f \code{function}, function that accepts x and y as formula input \code{~ x + y}.
#' Default \code{cor.test}
#' @param .f_args \code{list} of arguments to be passed to \code{.f}
#' @param round_stat \code{integer}
#' @param round_pval \code{integer}
#' @details presently we need to use a formula input for \code{cor.test} because
#' \code{na.fail} only gets evaluated when a formula is passed (see below).
#' \preformatted{
#' x = c(1,3,5,7,NA)
#' y = c(3,6,7,8,1)
#' cor.test(x, y, na.action = "na.fail")
#' cor.test(~ x + y,  na.action = "na.fail")
#' }
#' @return \code{character} with stats. For \code{cor.test} correlation coefficient and p-value.
#' @export
#' @examples
#' set.seed(1)
#' x <- runif(25, 0, 1)
#' y <- runif(25, 0, 1)
#' x[c(3,10,18)] <- NA
#'
#' get_scatterplotmatrix_stats(x, y, .f = cor.test, .f_args = list(method = "pearson"))
#' get_scatterplotmatrix_stats(x, y, .f = cor.test, .f_args = list(method = "pearson",
#'   na.action = na.fail))
get_scatterplotmatrix_stats <- function(x, y,
                                        .f = cor.test,
                                        .f_args = list(),
                                        round_stat = 2,
                                        round_pval = 4) {
  if (is.numeric(x) && is.numeric(y)) {

    stat <- tryCatch(do.call(.f, c(list(~ x + y), .f_args)),
              error = function(e) NA)

    if (anyNA(stat)) {
      return("NA")
    } else if (all(c("estimate", "p.value") %in% names(stat))) {
      return(paste(c(paste0(names(stat$estimate), ":", round(stat$estimate, round_stat)),
                     paste0("P:", round(stat$p.value, round_pval))),
                   collapse = "\n"))
    } else {
      stop("function not supported")
    }

  } else {
    if ("method" %in% names(.f_args)) {
      if (.f_args$method == "pearson") {
        return("cor:-")
      }
      if (.f_args$method == "kendall") {
        return("tau:-")
      }
      if (.f_args$method == "spearman") {
        return("rho:-")
      }
    }
    return("-")
  }

}
