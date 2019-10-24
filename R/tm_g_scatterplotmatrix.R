#' Create a scatterplot matrix
#'
#' The module allows to add and remove dataset selectors (data extract inputs) and it will create
#' the scatterplot matrix for a combination of the selected columns in the merge of all such datasets.
#' The available datasets to choose from for each dataset selector is the same and
#' determined by the argument \code{variables}.
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param variables (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'  Plotting variables from an incoming dataset with filtering and selecting.
#' @param plot_height (\code{numeric}) A vector of length three with \code{c(value, min and max)} for a slider
#'  encoding the plot height.
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @export
#' @importFrom stats na.omit
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
  stopifnot(is_numeric_vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_scatterplotmatrix,
    ui = ui_g_scatterplotmatrix,
    ui_args = args,
    server_args = list(variables = variables),
    filters = "all"
  )
}


ui_g_scatterplotmatrix <- function(id, ...) {
  args <- list(...)

  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      plot_height_output(ns("myplot"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args$variables),
      actionLink(ns("add_selector"), "Add Selector"),
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
          )
        )
      )
    ),
    pre_output = args$pre_output,
    post_output = args$post_output,
    forms = actionButton(ns("show_rcode"), "Show R code", width = "100%")
  )
}

#' HTML id of the UI element that contains the extract input
#' @param idx index or identifier of the data extract UI that contains the input element
#' @param session shiny session object
#' @return the html id of the element given the \code{idx}
extract_ui_id <- function(idx, session) {
  session$ns(paste0("extract_ui_", idx))
}

#' HTML id of the extract input contained within another UI
#' @param idx index or identifier of the data extract input element
#' @param session shiny session object
#' @return the html id of the element given the \code{idx}
extract_input_id <- function(idx, session) {
  session$ns(paste0("extract_input_", idx))
}

#' HTML id of the element to remove the whole extract UI
#' @param idx index or identifier of the button to remove the data extract ui
#' @param session shiny session object
#' @return the html id of the element given the \code{idx}
extract_remove_id <- function(idx, session) {
  session$ns(paste0("extract_remove_", idx))
}


#' @importFrom dplyr mutate_if
#' @importFrom lattice splom
#' @importFrom methods substituteDirect
srv_g_scatterplotmatrix <- function(input,
                                    output,
                                    session,
                                    datasets,
                                    variables) {

  init_chunks(session)

  extract_ui_indices <- reactiveVal(value = NULL)

  add_ui <- reactive({
    # index of new data extract UI
    idx <- max(extract_ui_indices(), 0) + 1
    insertUI(
      selector = paste0("#", session$ns("add_selector")),
      where = "beforeBegin",
      ui = div(
        id = extract_ui_id(idx, session),
        data_extract_input(
          id = session$ns(extract_input_id(idx, session)),
          label = paste0("Selector (", idx, ")"),
          data_extract_spec = variables
        ),
        div(
          actionLink(session$ns(extract_remove_id(idx, session)), "Remove"),
          align = "right"
        )
      )
    )
    extract_ui_indices(c(extract_ui_indices(), idx))

    # bind remove event
    observeEvent(input[[isolate(extract_remove_id(idx, session))]], {
      # please use the same condition in the "once" argument
      if (length(extract_ui_indices()) <= 1) {
        print("need at least one data extract to select from")
      }
      req(length(extract_ui_indices()) > 1)
      removeUI(selector = paste0("#", extract_ui_id(idx, session)))
      extract_ui_indices(extract_ui_indices()[extract_ui_indices() != idx])
    }, once = !(length(isolate(extract_ui_indices())) <= 1), ignoreInit = TRUE)
  })

  # add two data extracts initially
  init_observe <- observe({
    add_ui()
    add_ui()
  })
  init_observe$suspend()

  observeEvent(input$add_selector, add_ui())

  merged_data <- reactive({
    validate(need(length(extract_ui_indices()) > 0, "Need at least 1 input."))
    data_merge_module(
      datasets = datasets,
      data_extract = replicate(length(extract_ui_indices()), variables, simplify = FALSE),
      input_id = vapply(extract_ui_indices(), function(idx) extract_input_id(idx, session), character(1))
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
    ANL <- merged_data()$data() # nolint
    validate_has_data(ANL, 3)
    chunks_reset()

    alpha <- input$alpha # nolint
    cex <- input$cex # nolint
    cols_names <- unique(unname(do.call(c, merged_data()$columns_source)))
    validate(need(length(cols_names) > 1, "Need at least 2 columns."))

    # create plot
    chunks_push(bquote({
      lattice::splom(ANL[, .(cols_names)], pch = 16, alpha = .(alpha), cex = .(cex))
    }))

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
        merge_expression = merged_data()$expr,
        title = title
      )
    )
  })
}
