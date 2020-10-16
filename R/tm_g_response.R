#' Response Plots
#' @md
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @inheritParams shared_params
#' @param response (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Which variable to use as the response. You can define one fixed column by using the
#'   setting `fixed = TRUE` inside the `select_spec`.
#'  `data_extract_spec` must not allow multiple selection in this case.
#' @param x (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Which variable to use on the X-axis of the response plot. Allow the user to select multiple
#'   columns from the `data` allowed in teal.
#'  `data_extract_spec` must not allow multiple selection in this case.
#' @param row_facet optional, (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Which data columns to use for faceting rows.
#' @param col_facet optional, (`data_extract_spec` or `list` of multiple `data_extract_spec`)
#'   Which data to use for faceting columns.
#' @param coord_flip optional, (`logical`) Whether to flip coordinates between `x` and `response`.
#' @param count_labels optional, (`logical`) Whether to show count labels.
#'   Defaults to `TRUE`.
#' @param freq optional, (`logical`) Whether to display frequency (`TRUE`) or density (`FALSE`).
#'   Defaults to density (`FALSE`).
#' @param ggtheme optional, (`character`) `ggplot` Theme to be used by default.
#'   All themes can be chosen by the user. Defaults to `gray`.
#'
#' @note For more examples, please see the vignette "Using response plot" via
#'   \code{vignette("using-response-plot", package = "teal.modules.general")}.
#' @export
#' @examples
#' # Response plot with selected response (BMRKR1) and selected x variable (RACE)
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
#'     tm_g_response(
#'       label = "Response Plots",
#'       response = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL, c("BMRKR2", "COUNTRY")),
#'           selected = "BMRKR2",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = variable_choices(ADSL, c("SEX", "RACE")),
#'           selected = "RACE",
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
tm_g_response <- function(label = "Response Plot",
                          response,
                          x,
                          row_facet = NULL,
                          col_facet = NULL,
                          coord_flip = TRUE,
                          count_labels = TRUE,
                          rotate_xaxis_labels = FALSE,
                          freq = FALSE,
                          plot_height = c(600, 400, 5000),
                          plot_width = NULL,
                          ggtheme = c(
                            "gray", "bw", "linedraw", "light", "dark", "minimal",
                            "classic", "void", "test"
                          ),
                          pre_output = NULL,
                          post_output = NULL) {
  if (!is_class_list("data_extract_spec")(response)) {
    response <- list(response)
  }
  if (!is_class_list("data_extract_spec")(x)) {
    x <- list(x)
  }
  if (!is_class_list("data_extract_spec")(row_facet)) {
    row_facet <- list_or_null(row_facet)
  }
  if (!is_class_list("data_extract_spec")(col_facet)) {
    col_facet <- list_or_null(col_facet)
  }

  ggtheme <- match.arg(ggtheme)

  stop_if_not(
    is.null(row_facet) || is_class_list("data_extract_spec")(row_facet),
    is.null(col_facet) || is_class_list("data_extract_spec")(col_facet),
    is_character_single(label),
    is_class_list("data_extract_spec")(response),
    is_class_list("data_extract_spec")(x),
    is_logical_single(coord_flip),
    is_logical_single(count_labels),
    is_logical_single(rotate_xaxis_labels),
    is_logical_single(freq),
    is_character_single(ggtheme),
    list(
      all(vapply(response, function(x) !("" %in% x$select$choices), logical(1))),
      "'response' should not allow empty values"),
    list(
      all(vapply(response, function(x) !(x$select$multiple), logical(1))),
      "'response' should not allow multiple selection"),
    list(
      all(vapply(x, function(x) !("" %in% x$select$choices), logical(1))),
      "'x' should not allow empty values"),
    list(
      all(vapply(x, function(x) !(x$select$multiple), logical(1))),
      "'x' should not allow multiple selection")
    )

  check_slider_input(plot_height, allow_null = FALSE)
  check_slider_input(plot_width)

  args <- as.list(environment())

  data_extract_list <- list(
    response = response,
    x = x,
    row_facet = row_facet,
    col_facet = col_facet
  )

  module(
    label = label,
    server = srv_g_response,
    ui = ui_g_response,
    ui_args = args,
    server_args = c(data_extract_list, list(plot_height = plot_height, plot_width = plot_width)),
    filters = get_extract_datanames(data_extract_list)
  )
}

ui_g_response <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)

  standard_layout(
    output = white_small_well(
      plot_with_settings_ui(id = ns("myplot"), height = args$plot_height, width = args$plot_width)
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("response", "x", "row_facet", "col_facet")]),
      data_extract_input(
        id = ns("response"),
        label = "Response variable",
        data_extract_spec = args$response
      ),
      data_extract_input(
        id = ns("x"),
        label = "X variable",
        data_extract_spec = args$x
      ),
      if (!is.null(args$row_facet)) {
        data_extract_input(
          id = ns("row_facet"),
          label = "Row facetting",
          data_extract_spec = args$row_facet
        )
      },
      if (!is.null(args$col_facet)) {
        data_extract_input(
          id = ns("col_facet"),
          label = "Column facetting",
          data_extract_spec = args$col_facet
        )
      },
      radioGroupButtons(
        inputId = ns("freq"),
        label = NULL,
        choices = c("frequency", "density"),
        selected = ifelse(args$freq, "frequency", "density"),
        justified = TRUE
      ),
      panel_group(
        panel_item(
          title = "Plot settings",
          checkboxInput(ns("count_labels"), "Add count labels", value = args$count_labels),
          checkboxInput(ns("coord_flip"), "Swap axes", value = args$coord_flip),
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = args$rotate_xaxis_labels),
          optionalSelectInput(
            inputId = ns("ggtheme"),
            label = "Theme (by ggplot):",
            choices = c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test"),
            selected = args$ggtheme,
            multiple = FALSE
          )
        )
      )
    ),
    forms = get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

#' @importFrom dplyr mutate summarise group_by_at vars n
#' @importFrom forcats fct_rev
#' @importFrom magrittr %>%
srv_g_response <- function(input,
                           output,
                           session,
                           datasets,
                           response,
                           x,
                           row_facet,
                           col_facet,
                           plot_height,
                           plot_width) {
  init_chunks()
  data_extract <- list(response, x, row_facet, col_facet)
  names(data_extract) <- c("response", "x", "row_facet", "col_facet")
  data_extract <- data_extract[!vapply(data_extract, is.null, logical(1))]

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = data_extract,
    input_id = names(data_extract)
  )

  plot_r <- reactive({
    chunks_reset()
    chunks_push_data_merge(merged_data())

    ANL <- chunks_get_var("ANL") # nolint
    validate_has_data(ANL, 10)

    resp_var <- as.vector(merged_data()$columns_source$response)
    x <- as.vector(merged_data()$columns_source$x)

    row_facet_name <- as.vector(if_empty(merged_data()$columns_source$row_facet, character(0)))
    col_facet_name <- as.vector(if_empty(merged_data()$columns_source$col_facet, character(0)))

    validate(need(!identical(resp_var, character(0)), "Please define a valid column for the response variable"))
    validate(need(!identical(x, character(0)), "Please define a valid column for the X-variable"))
    validate(need(length(resp_var) == 1, "Please define a column for Response variable"))
    validate(need(length(x) == 1, "Please define a column for X variable"))
    validate(need(is.factor(ANL[[resp_var]]), "Please select a factor variable as the response."))
    validate(need(is.factor(ANL[[x]]), "Please select a factor variable as the X-Variable."))


    validate_has_data(ANL[, c(resp_var, x)], 10, complete = TRUE, allow_inf = FALSE)

    freq <- input$freq == "frequency"
    swap_axes <- input$coord_flip
    counts <- input$count_labels
    rotate_xaxis_labels <- input$rotate_xaxis_labels
    ggtheme <- input$ggtheme

    validate(need(!is.null(ggtheme), "Please select a theme."))

    arg_position <- if (freq) "stack" else "fill" # nolint

    rowf <- if (is_empty(row_facet_name)) NULL else as.name(row_facet_name) #nolint
    colf <- if (is_empty(col_facet_name)) NULL else as.name(col_facet_name) #nolint
    resp_cl <- as.name(resp_var) #nolint
    x_cl <- as.name(x) #nolint

    if (swap_axes) {

    chunks_push(expression = bquote({
      ANL[[.(x)]] <- with(ANL, forcats::fct_rev(.(x_cl)))
    }))

    }

    chunks_push(expression = bquote({
      ANL[[.(resp_var)]] <- factor(ANL[[.(resp_var)]])
    }))
    # nolint start
    # rowf and colf will be a NULL if not set by a user
    chunks_push(expression = bquote({
      ANL2 <- ANL %>%
        dplyr::group_by_at(
          dplyr::vars(
          .(x_cl),
          .(resp_cl),
          .(rowf),
          .(colf)
          )
        ) %>%
        dplyr::summarise(ns = dplyr::n()) %>%
        dplyr::group_by_at(
          dplyr::vars(
          .(x_cl),
          .(rowf),
          .(colf)
          )
        ) %>%
        dplyr::mutate(
          sums = sum(ns),
          percent = round(ns / sums * 100, 1)
        )
    }))


    chunks_push(expression = bquote({
      ANL3 <- ANL %>%
        dplyr::group_by_at(
          dplyr::vars(
          .(x_cl),
          .(rowf),
          .(colf)
        )
        ) %>%
        dplyr::summarise(ns = dplyr::n())
    }))
    # nolint end
    plot_call <- bquote(
      ANL2 %>%
        ggplot() +
        aes(x = .(x_cl), y = ns) +
        geom_bar(aes(fill = .(resp_cl)), stat = "identity", position = .(arg_position)) +
        xlab(.(varname_w_label(x, ANL))) +
        ylab(.(varname_w_label(resp_var, ANL, prefix = "Proportion of "))) +
        .(as.call(parse(text = paste0("theme_", ggtheme))))
    )

    plot_call <- bquote(.(plot_call) +
      labs(fill = .(varname_w_label(resp_var, ANL))) +
      theme(legend.position = "bottom"))

    if (!freq) plot_call <- bquote(.(plot_call) + expand_limits(y = c(0, 1.1)))

    if (counts) {
      plot_call <-
        bquote(.(plot_call) +
          geom_text(
            data = ANL2,
            aes(label = ns, x = .(x_cl), y = ns, group = .(resp_cl)),
            col = "white",
            vjust = "middle",
            hjust = "middle",
            position = .(if (!freq) quote(position_fill(0.5)) else quote(position_stack(0.5))))  +
          geom_text(
            data = ANL3, aes(label = ns, x = .(x_cl), y = .(if (!freq) 1.1 else as.name("ns"))),
            hjust = .(if (swap_axes) "left" else "middle"),
            vjust = .(if (swap_axes) "middle" else -1),
            position = .(if (!freq) "fill" else "stack")
          )
        )
    }

    if (swap_axes) {
      plot_call <- bquote(.(plot_call) + coord_flip())
    }

    if (rotate_xaxis_labels) {
      plot_call <- bquote(.(plot_call) + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
    }

    facet_cl <- facet_ggplot_call(row_facet_name, col_facet_name)

    if (!is.null(facet_cl)) {
      plot_call <- bquote(.(plot_call) + .(facet_cl))
    }

    plot_call <- bquote(p <- .(plot_call))

    chunks_push(expression = plot_call, id = "plotCall")

    # explicitly calling print on the plot inside the chunk evaluates
    # the ggplot call and therefore catches errors
    plot_print_call <- quote(print(p))
    chunks_push(plot_print_call)

    chunks_safe_eval()
  })

  # Insert the plot into a plot_with_settings module from teal.devel
  callModule(
    plot_with_settings_srv,
    id = "myplot",
    plot_r = plot_r,
    height = plot_height,
    width = plot_width
  )

  callModule(
    get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = get_extract_datanames(list(response, x, row_facet, col_facet)),
    modal_title = "R Code for Response Plot"
  )
}
