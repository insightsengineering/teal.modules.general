#' @include utils.R
NULL

#' Univariate and bivariate visualizations.
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param label (\code{character}) Label of the module
#' @param x (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable name selected to plot along the x-axis by default. Variable can be numeric, factor or character.
#'   No empty selections are allowed!
#' @param y (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable name selected to plot along the y-axis by default. Variable can be numeric, factor or character.
#' @param use_density (\code{logical}) value for whether density (\code{TRUE}) is plotted or frequency (\code{FALSE})
#' @param row_facet (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable for row facetting
#' @param col_facet (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable for col facetting
#' @param expert_settings (\code{logical}) Whether coloring, filling and size should be chosen
#'   by the user
#' @param colour optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable selection for the colouring inside the expert settings
#' @param fill optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable selection for the filling inside the expert settings
#' @param size optional, (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   Variable selection for the size of \code{geom_point} plots inside the expert settings
#' @param free_x_scales (\code{logical}) If X scaling shall be changeable
#' @param free_y_scales (\code{logical}) If Y scaling shall be changeable
#' @param plot_height (\code{numeric}) \code{c(value, min and max)} of plot height slider
#' @param with_show_r_code (\code{logical}) Whether show R code button shall be shown
#' @param ggtheme (\code{character}) ggplot theme to be used by default. All themes can be chosen by the user.
#'
#'
#'
#' @details
#' This is a general module to visualize 1 & 2 dimensional data.
#'
#' @importFrom methods is
#' @export
#'
#'
#' @examples
#' # Bivariate plot of selected variable (AGE) against selected (SEX)
#' library(random.cdisc.data)
#' ADSL <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- cadsl",
#'     check = TRUE
#'   ),
#'   modules = root_modules(
#'     tm_g_bivariate(
#'       x = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = names(ADSL),
#'           selected = "AGE",
#'           fixed = FALSE
#'         )
#'       ),
#'       y = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variable:",
#'           choices = names(ADSL),
#'           selected = "SEX",
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )
#'       ),
#'       row_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = names(ADSL),
#'           selected = NULL,
#'           multiple = TRUE,
#'           fixed = FALSE
#'         )
#'       ),
#'       col_facet = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select variables:",
#'           choices = names(ADSL),
#'           selected = NULL,
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
tm_g_bivariate <- function(label = "Bivariate Plots",
                           x,
                           y,
                           row_facet,
                           col_facet,
                           colour = NULL,
                           fill = NULL,
                           size = NULL,
                           use_density = FALSE,
                           expert_settings = TRUE,
                           free_x_scales = FALSE,
                           free_y_scales = FALSE,
                           plot_height = c(600, 200, 2000),
                           ggtheme = c("grey", "gray", "bw", "linedraw", "light", "dark", "minimal",
                                       "classic", "void", "test"),
                           with_show_r_code = TRUE,
                           pre_output = NULL,
                           post_output = NULL) {
  stopifnot(is.character.single(label))
  stopifnot(is.class.list("data_extract_spec")(x) || is(x, "data_extract_spec"))
  stopifnot(is.class.list("data_extract_spec")(y) || is(y, "data_extract_spec"))
  stopifnot(is.class.list("data_extract_spec")(row_facet) || is(row_facet, "data_extract_spec"))
  stopifnot(is.class.list("data_extract_spec")(col_facet) || is(col_facet, "data_extract_spec"))
  stopifnot(is.null(colour) || is.class.list("data_extract_spec")(colour) || is(colour, "data_extract_spec"))
  stopifnot(is.null(fill) || is.class.list("data_extract_spec")(fill) || is(fill, "data_extract_spec"))
  stopifnot(is.null(size) || is.class.list("data_extract_spec")(size) || is(size, "data_extract_spec"))
  if (is.class.list("data_extract_spec")(x)) {
    stop_if_not(list(all(vapply(x, function(xx) !isTRUE(xx$select$multiple), logical(1))),
                     "x variable should not allow multiple selection"))
  } else if (is(x, "data_extract_spec")) {
    stop_if_not(list(!isTRUE(x$select$multiple),
                     "x variable should not allow multiple selection"))
  }
  if (is.class.list("data_extract_spec")(y)) {
    stop_if_not(list(all(vapply(y, function(x) !isTRUE(x$select$multiple), logical(1))),
                     "y variable should not allow multiple selection"))
  } else if (is(y, "data_extract_spec")) {
    stop_if_not(list(!isTRUE(y$select$multiple),
                     "y variable should not allow multiple selection"))
  }
  stopifnot(is.logical.single(use_density))
  stopifnot(is.logical.single(expert_settings))
  stopifnot(is.logical.single(free_x_scales))
  stopifnot(is.logical.single(free_y_scales))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])
  stopifnot(is.logical.single(with_show_r_code))
  ggtheme <- match.arg(ggtheme)
  stopifnot(is.character.single(ggtheme))

  if (expert_settings) {
    if (is.null(colour)) {
      colour <- `if`(inherits(x, "list"), x, list(x))
      colour[[1]]$select <- select_spec(choices = colour[[1]]$select$choices, selected = NULL)
    }
    if (is.null(fill)) {
      fill <- `if`(inherits(x, "list"), x, list(x))
      fill[[1]]$select <- select_spec(choices = fill[[1]]$select$choices, selected = NULL)
    }
    if (is.null(size)) {
      size <- `if`(inherits(x, "list"), x, list(x))
      size[[1]]$select <- select_spec(choices = size[[1]]$select$choices, selected = NULL)
    }
  }

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_bivariate,
    ui = ui_g_bivariate,
    ui_args = args,
    server_args = list(
      x = x,
      y = y,
      row_facet = row_facet,
      col_facet = col_facet,
      expert_settings = expert_settings,
      colour = colour,
      fill = fill,
      size = size
    ),
    filters = "all"
  )
}


#' @importFrom shinyWidgets radioGroupButtons switchInput
ui_g_bivariate <- function(id, ...) {
  args <- list(...)

  ns <- NS(id)
  standard_layout(
    output = white_small_well(
      tags$div(plot_height_output(id = ns("myplot")))
    ),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      datanames_input(args[c("x", "y", "row_facet", "col_facet", "colour", "fill", "size")]),
      data_extract_input(
        id = ns("x"),
        label = "X variable",
        data_extract_spec = args$x
      ),
      data_extract_input(
        id = ns("y"),
        label = "Y variable",
        data_extract_spec = args$y
      ),
      radioGroupButtons(
        inputId = ns("use_density"),
        label = NULL,
        choices = c("frequency", "density"),
        selected = ifelse(args$use_density, "density", "frequency"),
        justified = TRUE
      ),
      div(
        class = "data-extract-box",
        tags$label("Facetting"),
        switchInput(inputId = ns("facetting"), value = TRUE, size = "mini"),
        conditionalPanel(
          condition = paste0("input['", ns("facetting"), "']"),
          div(
            data_extract_input(
              id = ns("row_facet"),
              label = "Row facetting variable",
              data_extract_spec = args$row_facet
            ),
            data_extract_input(
              id = ns("col_facet"),
              label = "Column facetting variable",
              data_extract_spec = args$col_facet
            ),
            checkboxInput(ns("free_x_scales"), "free x scales", value = args$free_x_scales),
            checkboxInput(ns("free_y_scales"), "free y scales", value = args$free_y_scales)
          )
        )
      ),
      if (args$expert_settings) {
        # Put a grey border around the expert settings
        div(
          class = "data-extract-box",
          tags$label("Expert settings"),
          switchInput(inputId = ns("expert"), value = FALSE, size = "mini"),
          conditionalPanel(
            condition = paste0("input['", ns("expert"), "']"),
            div(
              data_extract_input(
                id = ns("colour"),
                label = "Colour by variable",
                data_extract_spec = args$colour
              ),
              data_extract_input(
                id = ns("fill"),
                label = "Fill colour by variable",
                data_extract_spec = args$fill
              ),
              data_extract_input(
                id = ns("size"),
                label = "Size of points by variable (only if x and y are numeric)",
                data_extract_spec = args$size
              )
            )
          )
        )
      },
      plot_height_input(id = ns("myplot"), value = args$plot_height),
      optionalSelectInput(
        inputId = ns("ggtheme"),
        label = "Theme (by ggplot):",
        choices = c("grey", "gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test"),
        selected = args$ggtheme,
        multiple = FALSE
      )
    ),


    forms = if (args$with_show_r_code) {
      actionButton(ns("show_rcode"), "Show R code", width = "100%")
    } else {
      NULL
    },
    pre_output = args$pre_output,
    post_output = args$post_output
  )
}


#' @importFrom magrittr %>%
#' @importFrom methods is
srv_g_bivariate <- function(input,
                            output,
                            session,
                            datasets,
                            x,
                            y,
                            row_facet,
                            col_facet,
                            expert_settings = FALSE,
                            colour,
                            fill,
                            size) {
  init_chunks(session)
  data_extract <- if (expert_settings) {
    setNames(
      list(x, y, row_facet, col_facet, colour, fill, size),
      c("x", "y", "row_facet", "col_facet", "colour", "fill", "size")
    )
  } else {
    setNames(
      list(x, y, row_facet, col_facet),
      c("x", "y", "row_facet", "col_facet")
    )
  }

  merged_data <- data_merge_module(
    datasets = datasets,
    data_extract = data_extract,
    input_id = names(data_extract)
  )

  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  output$plot <- renderPlot({
    ANL <- merged_data()$data()
    chunks_reset()

    x_name <- merged_data()$columns_source$x
    y_name <- merged_data()$columns_source$y
    row_facet_name <- merged_data()$columns_source$row_facet
    col_facet_name <- merged_data()$columns_source$col_facet
    colour_name <- merged_data()$columns_source$colour
    fill_name <- merged_data()$columns_source$fill
    size_name <- merged_data()$columns_source$size

    use_density <- input$use_density == "density"
    free_x_scales <- input$free_x_scales
    free_y_scales <- input$free_y_scales
    ggtheme <- input$ggtheme

    validate(
      need(!is.character.empty(x_name) || !is.empty(y_name),
           "x-variable and y-variable isn't correcly specified. At least one should be valid."))

    cl <- bivariate_plot_call(
      data_name = "ANL",
      x = x_name,
      y = y_name,
      x_class = if (!is.character.empty(x_name)) class(ANL[[x_name]]) else "NULL",
      y_class = if (!is.character.empty(y_name)) class(ANL[[y_name]]) else "NULL",
      freq = !use_density
    )

    if (input$facetting) {
      facet_cl <- facet_ggplot_call(row_facet_name, col_facet_name, free_x_scales, free_y_scales)

      if (!is.null(facet_cl)) {
        cl <- call("+", cl, facet_cl)
      }
    }
    expert_cl <- NULL
    if (expert_settings) {
      if (input$expert) {
        expert_cl <- expert_ggplot_call(
          colour = colour_name, fill = fill_name, size = size_name,
          is_point = any(grepl("geom_point", cl %>% deparse()))
        )
      }
      if (!is.null(expert_cl)) {
        cl <- call("+", cl, expert_cl)
      }
    }

    if (!is.null(ggtheme)) {
      cl <- call("+", cl, as.call(parse(text = paste0("theme_", ggtheme))))
    }

    chunks_push(expression = cl, id = "plotCall")
    chunks_safe_eval()
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "R Code for a Bivariate plot",
      rcode = get_rcode(
        datasets = datasets,
        merge_expression = merged_data()$expr,
        title = "",
        description = ""
      )
    )
  })
}


#' Get Substituted ggplot call
#'
#' @noRd
#'
#' @examples
#'
#' bivariate_plot_call("ANL", "BAGE", "RACE", "numeric", "factor")
#' bivariate_plot_call("ANL", "BAGE", NULL, "numeric", "NULL")
bivariate_plot_call <- function(data_name,
                                x = character(0),
                                y = character(0),
                                x_class,
                                y_class,
                                freq = TRUE) {
  cl <- bivariate_ggplot_call(x_class = x_class, y_class = y_class, freq = freq)

  if (is.character.empty(x)) {
    x <- "-"
  }
  if (is.character.empty(y)) {
    y <- "-"
  }

  cl_plot <- substitute_q(cl, list(
    .ggplotcall = bquote(ggplot(.(as.name(data_name)))),
    .x = if (is.call(x)) x else as.name(x),
    .y = if (is.call(y)) y else as.name(y)
  ))

  cl_plot
}

substitute_q <- function(x, env) {
  stopifnot(is.language(x))
  call <- substitute(substitute(x, env), list(x = x))
  eval(call)
}


#' Create ggplot part of plot call
#'
#' Due to the type of the x and y variable the plot type is chosen
#'
#' @noRd
#'
#' @examples
#' bivariate_ggplot_call("numeric", "NULL")
#' bivariate_ggplot_call("numeric", "NULL", freq = FALSE)
#'
#' bivariate_ggplot_call("NULL", "numeric")
#' bivariate_ggplot_call("NULL", "numeric", freq = FALSE)
#'
#' bivariate_ggplot_call("NULL", "factor")
#' bivariate_ggplot_call("NULL", "factor", freq = FALSE)
#'
#' bivariate_ggplot_call("factor", "NULL")
#' bivariate_ggplot_call("factor", "NULL", freq = FALSE)
#'
#' bivariate_ggplot_call("numeric", "numeric")
#' bivariate_ggplot_call("numeric", "factor")
#' bivariate_ggplot_call("factor", "numeric")
#' bivariate_ggplot_call("factor", "factor")
bivariate_ggplot_call <- function(x_class = c("NULL", "numeric", "integer", "factor", "character", "logical"),
                                  y_class = c("NULL", "numeric", "integer", "factor", "character", "logical"),
                                  freq = TRUE) {
  x_class <- match.arg(x_class)
  y_class <- match.arg(y_class)

  if (x_class %in% c("character", "logical")) {
    x_class <- "factor"
  }
  if (x_class %in% c("integer")) {
    x_class <- "numeric"
  }
  if (y_class %in% c("character", "logical")) {
    y_class <- "factor"
  }
  if (y_class %in% c("integer")) {
    y_class <- "numeric"
  }

  if (all(c(x_class, y_class) == "NULL")) {
    stop("either x or y is required")
  }

  # Single data plots
  if (x_class == "numeric" && y_class == "NULL") {
    if (freq) {
      quote(.ggplotcall + aes(x = .x) + geom_histogram() + ylab("Frequency"))
    } else {
      quote(.ggplotcall + aes(x = .x) + geom_histogram(aes(y = ..density..)) + ylab("Density")) # nolint
    }
  } else if (x_class == "NULL" && y_class == "numeric") {
    if (freq) {
      quote(.ggplotcall + aes(x = .y) + geom_histogram() + ylab("Frequency") + coord_flip())
    } else {
      quote(.ggplotcall + aes(x = .y) + geom_histogram(aes(y = ..density..)) + ylab("Density") + coord_flip()) # nolint
    }
  } else if (x_class == "factor" && y_class == "NULL") {
    if (freq) {
      quote(.ggplotcall + aes(x = .x) + geom_bar() + ylab("Frequency"))
    } else {
      quote(.ggplotcall + aes(x = .x) + geom_bar(aes(y = ..prop.., group = 1)) + ylab("Proportion")) # nolint
    }
  } else if (x_class == "NULL" && y_class == "factor") {
    if (freq) {
      quote(.ggplotcall + aes(x = .y, fill = factor(.fill)) + geom_bar() + ylab("Frequency") + coord_flip()) # nolint
    } else {
      quote(.ggplotcall + aes(x = .y) + geom_bar(aes(y = ..prop.., group = 1)) + # nolint
              ylab("Proportion") + coord_flip())
    }

    # Numeric Plots
  } else if (x_class == "numeric" && y_class == "numeric") {
    quote(.ggplotcall + aes(x = .x, y = .y) + geom_point())
  } else if (x_class == "numeric" && y_class == "factor") {
    quote(.ggplotcall + aes(x = .y, y = .x) + geom_boxplot() + coord_flip())
  } else if (x_class == "factor" && y_class == "numeric") {
    quote(.ggplotcall + aes(x = .x, y = .y) + geom_boxplot())

    # Factor and character plots
  } else if (x_class == "factor" && y_class == "factor") {
    quote(.ggplotcall + geom_mosaic(aes(x = product(.x), fill = .y), na.rm = TRUE))
  } else {
    stop("x y type combination not allowed")
  }
}


#' Create facet call
#'
#' @noRd
#'
#' @examples
#'
#' facet_ggplot_call(LETTERS[1:3])
#' facet_ggplot_call(NULL, LETTERS[23:26])
#' facet_ggplot_call(LETTERS[1:3], LETTERS[23:26])
facet_ggplot_call <- function(row_facet = character(0),
                              col_facet = character(0),
                              free_x_scales = FALSE,
                              free_y_scales = FALSE) {
  scales <- if (free_x_scales && free_y_scales) {
    "free"
  } else if (free_x_scales) {
    "free_x"
  } else if (free_y_scales) {
    "free_y"
  } else {
    "fixed"
  }

  if (is.character.empty(row_facet) && is.character.empty(col_facet)) {
    NULL
  } else if (!is.character.empty(row_facet) && !is.character.empty(col_facet)) {
    call("facet_grid",
         rows = call_fun_dots("vars", row_facet),
         cols = call_fun_dots("vars", col_facet),
         scales = scales
    )
  } else if (is.character.empty(row_facet) && !is.character.empty(col_facet)) {
    call("facet_grid", cols = call_fun_dots("vars", col_facet), scales = scales)

  } else if (!is.character.empty(row_facet) && is.character.empty(col_facet)) {
    call("facet_grid", rows = call_fun_dots("vars", row_facet), scales = scales)
  }
}

expert_ggplot_call <- function(colour,
                               fill,
                               size,
                               is_point = FALSE) {

  if (!is.character.empty(colour) && !is.character.empty(fill) &&
      is_point && !is.character.empty(size)) {
    bquote(aes(
      colour = .(as.name(colour)),
      fill = .(as.name(fill)),
      size = .(as.name(size))
    ))
  } else if (!is.character.empty(colour) && !is.character.empty(fill) &&
             (!is_point || is.character.empty(size))) {
    bquote(aes(
      colour = .(as.name(colour)),
      fill = .(as.name(fill))
    ))
  } else if (!is.character.empty(colour) && is.character.empty(fill) &&
             (!is_point || is.character.empty(size))) {
    bquote(aes(
      colour = .(as.name(colour))
    ))
  } else if (is.character.empty(colour) && !is.character.empty(fill) &&
             (!is_point || is.character.empty(size))) {
    bquote(aes(
      fill = .(as.name(fill))
    ))
  } else if (is.character.empty(colour) && is.character.empty(fill) &&
             is_point && !is.character.empty(size)) {
    bquote(aes(
      size = .(as.name(size))
    ))
  } else if (!is.character.empty(colour) && is.character.empty(fill) &&
             is_point && !is.character.empty(size)) {
    bquote(aes(
      colour = .(as.name(colour)),
      size = .(as.name(size))
    ))
  } else if (is.character.empty(colour) && !is.character.empty(fill) &&
             is_point && !is.character.empty(size)) {
    bquote(aes(
      fill = .(as.name(fill)),
      size = .(as.name(size))
    ))
  } else {
    NULL
  }
}
