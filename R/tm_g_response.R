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
#'
#' @note For more examples, please see the vignette "Using response plot" via
#'   \code{vignette("using-response-plot", package = "teal.modules.general")}.
#' @export
#' @examples
#' # Response plot with selected response (BMRKR1) and selected x variable (RACE)
#' library(scda)
#'
#' ADSL <- synthetic_cdisc_data("latest")$adsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL, code = "ADSL <- synthetic_cdisc_data(\"latest\")$adsl"),
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
                          coord_flip = FALSE,
                          count_labels = TRUE,
                          rotate_xaxis_labels = FALSE,
                          freq = FALSE,
                          plot_height = c(600, 400, 5000),
                          plot_width = NULL,
                          ggtheme = gg_themes,
                          ggplot2_args = teal.devel::ggplot2_args(),
                          pre_output = NULL,
                          post_output = NULL) {
  logger::log_info("Initializing tm_g_response")
  if (inherits(response, "data_extract_spec")) response <- list(response)
  if (inherits(x, "data_extract_spec")) x <- list(x)
  if (inherits(row_facet, "data_extract_spec")) row_facet <- list(row_facet)
  if (inherits(col_facet, "data_extract_spec")) col_facet <- list(col_facet)
  checkmate::assert_string(label)
  ggtheme <- match.arg(ggtheme)
  checkmate::assert_list(response, types = "data_extract_spec")
  if (!all(vapply(response, function(x) !("" %in% x$select$choices), logical(1)))) {
    stop("'response' should not allow empty values")
  }
  if (!all(vapply(response, function(x) !x$select$multiple, logical(1)))) {
    stop("'response' should not allow multiple selection")
  }
  checkmate::assert_list(x, types = "data_extract_spec")
  if (!all(vapply(x, function(x) !("" %in% x$select$choices), logical(1)))) {
    stop("'x' should not allow empty values")
  }
  if (!all(vapply(x, function(x) !x$select$multiple, logical(1)))) {
    stop("'x' should not allow multiple selection")
  }
  checkmate::assert_list(row_facet, types = "data_extract_spec", null.ok = TRUE)
  checkmate::assert_list(col_facet, types = "data_extract_spec", null.ok = TRUE)
  checkmate::assert_flag(coord_flip)
  checkmate::assert_flag(count_labels)
  checkmate::assert_flag(rotate_xaxis_labels)
  checkmate::assert_flag(freq)
  checkmate::assert_numeric(plot_height, len = 3, any.missing = FALSE, finite = TRUE)
  checkmate::assert_numeric(plot_height[1], lower = plot_height[2], upper = plot_height[3], .var.name = "plot_height")
  checkmate::assert_numeric(plot_width, len = 3, any.missing = FALSE, null.ok = TRUE, finite = TRUE)
  checkmate::assert_numeric(
    plot_width[1],
    lower = plot_width[2], upper = plot_width[3], null.ok = TRUE, .var.name = "plot_width"
  )

  checkmate::assert_class(ggplot2_args, "ggplot2_args")

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
    server_args = c(
      data_extract_list,
      list(plot_height = plot_height, plot_width = plot_width, ggplot2_args = ggplot2_args)
    ),
    filters = teal.devel::get_extract_datanames(data_extract_list)
  )
}

ui_g_response <- function(id, ...) {
  ns <- NS(id)
  args <- list(...)
  is_single_dataset_value <- teal.devel::is_single_dataset(args$response, args$x, args$row_facet, args$col_facet)

  teal.devel::standard_layout(
    output = teal.devel::white_small_well(
      teal.devel::plot_with_settings_ui(id = ns("myplot"))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      teal.devel::datanames_input(args[c("response", "x", "row_facet", "col_facet")]),
      teal.devel::data_extract_ui(
        id = ns("response"),
        label = "Response variable",
        data_extract_spec = args$response,
        is_single_dataset = is_single_dataset_value
      ),
      teal.devel::data_extract_ui(
        id = ns("x"),
        label = "X variable",
        data_extract_spec = args$x,
        is_single_dataset = is_single_dataset_value
      ),
      if (!is.null(args$row_facet)) {
        teal.devel::data_extract_ui(
          id = ns("row_facet"),
          label = "Row facetting",
          data_extract_spec = args$row_facet,
          is_single_dataset = is_single_dataset_value
        )
      },
      if (!is.null(args$col_facet)) {
        teal.devel::data_extract_ui(
          id = ns("col_facet"),
          label = "Column facetting",
          data_extract_spec = args$col_facet,
          is_single_dataset = is_single_dataset_value
        )
      },
      shinyWidgets::radioGroupButtons(
        inputId = ns("freq"),
        label = NULL,
        choices = c("frequency", "density"),
        selected = ifelse(args$freq, "frequency", "density"),
        justified = TRUE
      ),
      teal.devel::panel_group(
        teal.devel::panel_item(
          title = "Plot settings",
          checkboxInput(ns("count_labels"), "Add count labels", value = args$count_labels),
          checkboxInput(ns("coord_flip"), "Swap axes", value = args$coord_flip),
          checkboxInput(ns("rotate_xaxis_labels"), "Rotate X axis labels", value = args$rotate_xaxis_labels),
          optionalSelectInput(
            inputId = ns("ggtheme"),
            label = "Theme (by ggplot):",
            choices = gg_themes,
            selected = args$ggtheme,
            multiple = FALSE
          )
        )
      )
    ),
    forms = teal.devel::get_rcode_ui(ns("rcode")),
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}

srv_g_response <- function(input,
                           output,
                           session,
                           datasets,
                           response,
                           x,
                           row_facet,
                           col_facet,
                           plot_height,
                           plot_width,
                           ggplot2_args) {
  teal.devel::init_chunks()
  data_extract <- list(response, x, row_facet, col_facet)
  names(data_extract) <- c("response", "x", "row_facet", "col_facet")
  data_extract <- data_extract[!vapply(data_extract, is.null, logical(1))]

  merged_data <- teal.devel::data_merge_module(
    datasets = datasets,
    data_extract = data_extract
  )

  plot_r <- reactive({
    teal.devel::chunks_reset()
    teal.devel::chunks_push_data_merge(merged_data())

    ANL <- teal.devel::chunks_get_var("ANL") # nolint
    teal.devel::validate_has_data(ANL, 10)

    resp_var <- as.vector(merged_data()$columns_source$response)
    x <- as.vector(merged_data()$columns_source$x)

    row_facet_name <- if (length(merged_data()$columns_source$row_facet) == 0) {
      character(0)
    } else {
      as.vector(merged_data()$columns_source$row_facet)
    }
    col_facet_name <- if (length(merged_data()$columns_source$col_facet) == 0) {
      character(0)
    } else {
      as.vector(merged_data()$columns_source$col_facet)
    }
    validate(need(!identical(resp_var, character(0)), "Please define a valid column for the response variable"))
    validate(need(!identical(x, character(0)), "Please define a valid column for the X-variable"))
    validate(need(length(resp_var) == 1, "Please define a column for Response variable"))
    validate(need(length(x) == 1, "Please define a column for X variable"))
    validate(need(is.factor(ANL[[resp_var]]), "Please select a factor variable as the response."))
    validate(need(is.factor(ANL[[x]]), "Please select a factor variable as the X-Variable."))


    teal.devel::validate_has_data(ANL[, c(resp_var, x)], 10, complete = TRUE, allow_inf = FALSE)

    freq <- input$freq == "frequency"
    swap_axes <- input$coord_flip
    counts <- input$count_labels
    rotate_xaxis_labels <- input$rotate_xaxis_labels
    ggtheme <- input$ggtheme

    validate(need(!is.null(ggtheme), "Please select a theme."))

    arg_position <- if (freq) "stack" else "fill" # nolint

    rowf <- if (length(row_facet_name) == 0) NULL else as.name(row_facet_name) # nolint
    colf <- if (length(col_facet_name) == 0) NULL else as.name(col_facet_name) # nolint
    resp_cl <- as.name(resp_var) # nolint
    x_cl <- as.name(x) # nolint

    if (swap_axes) {
      teal.devel::chunks_push(expression = substitute(
        expr = ANL[[x]] <- with(ANL, forcats::fct_rev(x_cl)), # nolint
        env = list(x = x, x_cl = x_cl)
      ))
    }

    teal.devel::chunks_push(expression = substitute(
      expr = ANL[[resp_var]] <- factor(ANL[[resp_var]]), # nolint
      env = list(resp_var = resp_var)
    ))
    # nolint start
    # rowf and colf will be a NULL if not set by a user
    teal.devel::chunks_push(expression = substitute(
      expr = ANL2 <- ANL %>%
        dplyr::group_by_at(dplyr::vars(x_cl, resp_cl, rowf, colf)) %>%
        dplyr::summarise(ns = dplyr::n()) %>%
        dplyr::group_by_at(dplyr::vars(x_cl, rowf, colf)) %>%
        dplyr::mutate(sums = sum(ns), percent = round(ns / sums * 100, 1)),
      env = list(x_cl = x_cl, resp_cl = resp_cl, rowf = rowf, colf = colf)
    ))

    teal.devel::chunks_push(expression = substitute(
      expr = ANL3 <- ANL %>%
        dplyr::group_by_at(dplyr::vars(x_cl, rowf, colf)) %>%
        dplyr::summarise(ns = dplyr::n()),
      env = list(x_cl = x_cl, rowf = rowf, colf = colf)
    ))
    # nolint end

    plot_call <- substitute(
      expr =
        ggplot(ANL2, aes(x = x_cl, y = ns)) +
          geom_bar(aes(fill = resp_cl), stat = "identity", position = arg_position),
      env = list(
        x_cl = x_cl,
        resp_cl = resp_cl,
        arg_position = arg_position
      )
    )

    if (!freq) plot_call <- substitute(plot_call + expand_limits(y = c(0, 1.1)), env = list(plot_call = plot_call))

    if (counts) {
      plot_call <- substitute(
        expr = plot_call +
          geom_text(
            data = ANL2,
            aes(label = ns, x = x_cl, y = ns, group = resp_cl),
            col = "white",
            vjust = "middle",
            hjust = "middle",
            position = position_anl2_value
          ) +
          geom_text(
            data = ANL3, aes(label = ns, x = x_cl, y = anl3_y),
            hjust = hjust_value,
            vjust = vjust_value,
            position = position_anl3_value
          ),
        env = list(
          plot_call = plot_call,
          x_cl = x_cl,
          resp_cl = resp_cl,
          hjust_value = if (swap_axes) "left" else "middle",
          vjust_value = if (swap_axes) "middle" else -1,
          position_anl2_value = if (!freq) quote(position_fill(0.5)) else quote(position_stack(0.5)),
          anl3_y = if (!freq) 1.1 else as.name("ns"),
          position_anl3_value = if (!freq) "fill" else "stack"
        )
      )
    }

    if (swap_axes) {
      plot_call <- substitute(plot_call + coord_flip(), env = list(plot_call = plot_call))
    }

    facet_cl <- facet_ggplot_call(row_facet_name, col_facet_name)

    if (!is.null(facet_cl)) {
      plot_call <- substitute(expr = plot_call + facet_cl, env = list(plot_call = plot_call, facet_cl = facet_cl))
    }

    dev_ggplot2_args <- teal.devel::ggplot2_args(
      labs = list(
        x = varname_w_label(x, ANL),
        y = varname_w_label(resp_var, ANL, prefix = "Proportion of "),
        fill = varname_w_label(resp_var, ANL)
      ),
      theme = list(legend.position = "bottom")
    )

    if (rotate_xaxis_labels) {
      dev_ggplot2_args$theme[["axis.text.x"]] <- quote(element_text(angle = 45, hjust = 1)) # nolint
    }

    all_ggplot2_args <- teal.devel::resolve_ggplot2_args(
      user_plot = ggplot2_args,
      module_plot = dev_ggplot2_args
    )

    parsed_ggplot2_args <- teal.devel::parse_ggplot2_args(
      all_ggplot2_args,
      ggtheme = ggtheme
    )

    plot_call <- substitute(expr = {
      p <- plot_call + labs + ggthemes + themes
      print(p)
    }, env = list(
      plot_call = plot_call,
      labs = parsed_ggplot2_args$labs,
      themes = parsed_ggplot2_args$theme,
      ggthemes = parsed_ggplot2_args$ggtheme
    ))

    teal.devel::chunks_push(expression = plot_call, id = "plotCall")

    teal.devel::chunks_safe_eval()
  })

  # Insert the plot into a plot_with_settings module from teal.devel
  callModule(
    teal.devel::plot_with_settings_srv,
    id = "myplot",
    plot_r = plot_r,
    height = plot_height,
    width = plot_width
  )

  callModule(
    teal.devel::get_rcode_srv,
    id = "rcode",
    datasets = datasets,
    datanames = teal.devel::get_extract_datanames(list(response, x, row_facet, col_facet)),
    modal_title = "R Code for Response Plot"
  )
}
