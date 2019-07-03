#' Univariate and bivariate visualizations.
#'
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param label (\code{character}) Label of the module
#' @param dataname (\code{character}) name of datasets used to generate the bivariate plot. You need
#'   to name all datasets used in the available \code{data_extract_spec}
#' @param xvar (\code{list} of \code{data_extract_spec}) Variable name selected to plot along the x-axis by default.
#'  Variable can be numeric, factor or character. No empty selections are allowed!
#' @param yvar (\code{list} of \code{data_extract_spec}) Variable name selected to plot along the y-axis by default.
#'  Variable can be numeric, factor or character.
#' @param use_density (\code{logical}) value for whether density (\code{TRUE}) is plotted or frequency (\code{FALSE})
#' @param row_facet_var (\code{list} of \code{data_extract_spec}) variable for row facetting
#' @param col_facet_var (\code{list} of \code{data_extract_spec}) variable for col facetting
#' @param expert_settings (\code{logical}) Whether coloring, filling and size should be chosen
#'   by the user
#' @param colour_var (\code{list} of \code{data_extract_spec}) Variable selection for the colouring
#'   inside the expert settings
#' @param fill_var (\code{list} of \code{data_extract_spec}) Variable selection for the filling
#'   inside the expert settings
#' @param size_var (\code{list} of \code{data_extract_spec}) Variable selection for the size of \code{geom_point}
#'   plots inside the expert settings
#' @param free_x_scales (\code{logical}) If X scaling shall be changeable
#' @param free_y_scales (\code{logical}) If Y scaling shall be changeable
#' @param plot_height (\code{numeric}) \code{c(value, min and max)} of plot height slider
#' @param with_show_r_code (\code{logical}) Whether show R code button shall be shown
#' @param ggtheme (\code{character}) ggplot theme to be used by default. All themes can be chosen by the user.
#'
#' @details
#' This is a general module to visualize 1 & 2 dimensional data.
#'
#' @importFrom methods is
#'
#' @noRd
#'
#' @examples
#' library(random.cdisc.data)
#' library(tern)
#'
#' ASL <- cadsl
#' ARS <- cadrs
#'
#' keys(ASL) <- c("STUDYID", "USUBJID")
#' keys(ARS) <- c("STUDYID", "USUBJID", "PARAMCD")
#'
#' ars_filters <- filter_spec(
#'     vars = c("PARAMCD"),
#'     sep = " - ",
#'     choices = c("BESRSPI", "INVET"),
#'     selected = "BESRSPI",
#'     multiple = FALSE,
#'     label = "Choose endpoint"
#' )
#' ars_extracted_response <- data_extract_spec(
#'     dataname = "ARS",
#'     filter = ars_filters,
#'     columns = columns_spec(
#'         choices = c("","AVAL", "AVALC"),
#'         selected = "AVALC",
#'         multiple = FALSE,
#'         fixed = FALSE,
#'         label = "variable"
#'     )
#' )
#' asl_extracted <- data_extract_spec(
#'     dataname = "ASL",
#'     columns = columns_spec(
#'         choices = c(base::setdiff(names(ASL), keys(ASL))), # strict call of setdiff
#'         selected = c("AGE"),
#'         multiple = FALSE,
#'         fixed = FALSE,
#'         label = "variable"
#'     )
#' )
#' asl_extracted_row <- data_extract_spec(
#'     dataname = "ASL",
#'     columns = columns_spec(
#'         choices = c("","SEX", "RACE"),
#'         selected = "",
#'         multiple = TRUE,
#'         fixed = FALSE,
#'         label = "variable"
#'     )
#' )
#'
#' app <- init(
#'  data = cdisc_data(
#'    ASL = ASL,
#'    ARS = ARS,
#'    code = 'ASL <- cadsl
#'           ARS <- cadrs
#'           keys(ASL) <- c("STUDYID", "USUBJID")
#'           keys(ARS) <- c("STUDYID", "USUBJID", "PARAMCD")',
#'    check = FALSE),
#'  modules = root_modules(
#'    tm_g_bivariate(
#'      dataname = c("ASL","ARS"),
#'      xvar = list(asl_extracted),
#'      yvar = list(ars_extracted_response),
#'      use_density = FALSE,
#'      row_facet_var = list(asl_extracted_row),
#'      col_facet_var = list(asl_extracted_row),
#'      expert_settings = TRUE,
#'      plot_height = c(600, 200, 2000),
#'      ggtheme = "grey"
#'    )
#'  )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_bivariate <- function(label = "Bivariate Plots",
                           dataname,
                           xvar,
                           yvar,
                           use_density = FALSE,
                           row_facet_var,
                           col_facet_var,
                           expert_settings = TRUE,
                           colour_var = list(),
                           fill_var = list(),
                           size_var = list(),
                           free_x_scales = FALSE,
                           free_y_scales = FALSE,
                           plot_height = c(600, 200, 2000),
                           ggtheme = "minimal",
                           with_show_r_code = TRUE,
                           pre_output = NULL,
                           post_output = NULL
) {
  stopifnot(is.character.single(label))
  stopifnot(is.character.vector(dataname))
  stopifnot(is.list(xvar))
  # No empty columns allowed for X-Var
  # No multiple X variables allowed
  lapply(xvar, function(ds_extract) {
    stopifnot(is(ds_extract, "data_extract_spec"))
    stopifnot(!("" %in% ds_extract$columns$choices))
    stopifnot(!ds_extract$columns$multiple)
  })
  stopifnot(is.list(yvar))
  stopifnot(is.logical.single(use_density))
  stopifnot(is.list(row_facet_var))
  stopifnot(is.list(col_facet_var))
  stopifnot(is.logical.single(expert_settings))
  stopifnot(is.list(colour_var))
  stopifnot(is.list(fill_var))
  stopifnot(is.list(size_var))
  stopifnot(is.logical.single(free_x_scales))
  stopifnot(is.logical.single(free_y_scales))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])
  stopifnot(is.character.single(ggtheme))
  stopifnot(is.logical.single(with_show_r_code))

  if (expert_settings) {
    if (length(colour_var) == 0) {
      colour_var <- xvar
      colour_var[[1]]$columns$selected <- ""
      colour_var[[1]]$columns$choices <- c("", colour_var[[1]]$columns$choices)
    }
    if (length(fill_var) == 0) {
      fill_var <- xvar
      fill_var[[1]]$columns$selected <- ""
      fill_var[[1]]$columns$choices <- c("", fill_var[[1]]$columns$choices)
    }
    if (length(size_var) == 0) {
      size_var <- xvar
      size_var[[1]]$columns$selected <- ""
      size_var[[1]]$columns$selected <- ""
      size_var[[1]]$columns$choices <- c("", size_var[[1]]$columns$choices)
    }
  }

  args <- as.list(environment())

  module(
    label = label,
    server = srv_g_bivariate,
    ui = ui_g_bivariate,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      xvar = xvar,
      yvar = yvar,
      row_facet_var = row_facet_var,
      col_facet_var = col_facet_var,
      expert_settings = expert_settings,
      colour_var = colour_var,
      fill_var = fill_var,
      size_var = size_var
    ),
    filters = "all"
  )
}


#' @importFrom shinyWidgets switchInput
ui_g_bivariate <- function(id, ...) {
  a <- list(...)

  # Set default values for expert settings in case those were not given
  if (is.null(a$colour_var) || length(a$colour_var) == 0) {
    a[["colour_var"]] <- a$xvar
  }
  if (is.null(a$fill_var) || length(a$fill_var) == 0) {
    a[["fill_var"]] <- a$xvar
  }
  if (is.null(a$size_var) || length(a$size_var) == 0) {
    a[["size_var"]] <- a$xvar
  }

  ns <- NS(id)

  standard_layout(
    output = white_small_well(plot_height_output(id = ns("myplot"))),
    encoding = div(
      helpText("Dataset:", tags$code(a$dataname)),
      data_extract_input(
        id = ns("xvar"),
        label = "X Variable",
        data_extract_spec = a$xvar
      ),
      data_extract_input(
        id = ns("yvar"),
        label = "Y Variable",
        data_extract_spec = a$yvar
      ),
      radioButtons(
        inputId = ns("use_density"),
        label = NULL,
        choices = c("frequency", "density"),
        selected = ifelse(a$use_density, "density", "frequency"),
        inline = TRUE
      ),
      div(
        style = "border: 1px solid #e3e3e3; border-radius: 5px; padding: 0.6em; margin-left: -0.6em",
        tags$label("Facetting:"),
        switchInput(inputId = ns("facetting"), value = FALSE, size = "small"),
        conditionalPanel(
          condition = paste0("input['", ns("facetting"), "']"),
          ui_facetting(ns, a$row_facet_var, a$col_facet_var, a$free_x_scales, a$free_y_scales)
        )
      ),
      if (a$expert_settings) {
        # Put a grey border around the expert settings
        div(
          style = "border: 1px solid #e3e3e3; border-radius: 5px; padding: 0.6em; margin-left: -0.6em",
          tags$label("Expert settings:"),
          switchInput(inputId = ns("expert"), value = FALSE, size = "small"),
          conditionalPanel(
            condition = paste0("input['", ns("expert"), "']"),
            ui_expert(ns, a$colour_var, a$fill_var, a$size_var)
          )
        )
      },
      plot_height_input(id = ns("myplot"), value = a$plot_height),
      optionalSelectInput(
        inputId = ns("ggtheme"),
        label = "Theme (by ggplot)",
        choices = c("grey", "gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test"),
        selected = a$ggtheme,
        multiple = FALSE
      )
    ),
    forms = if (a$with_show_r_code) actionButton(ns("show_rcode"), "Show R Code", width = "100%") else NULL,
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

ui_facetting <- function(ns, row_facet_var_spec, col_facet_var_spec, free_x_scales, free_y_scales) {
  div(
    data_extract_input(
      id = ns("row_facet_var"),
      label = "Row facetting Variable",
      data_extract_spec = row_facet_var_spec
    ),
    data_extract_input(
      id = ns("col_facet_var"),
      label = "Column facetting Variable",
      data_extract_spec = col_facet_var_spec
    ),
    checkboxInput(ns("free_x_scales"), "free x scales", value = free_x_scales),
    checkboxInput(ns("free_y_scales"), "free y scales", value = free_y_scales)
  )
}

ui_expert <- function(ns, colour_var_spec, fill_var_spec, size_var_spec) {
  div(
    data_extract_input(
      id = ns("colour_var"),
      label = "Colour by variable",
      data_extract_spec = colour_var_spec
    ),
    data_extract_input(
      id = ns("fill_var"),
      label = "Fill colour by variable",
      data_extract_spec = fill_var_spec
    ),
    data_extract_input(
      id = ns("size_var"),
      label = "Size of points by variable (only if x and y are numeric)",
      data_extract_spec = size_var_spec
    )
  )
}


#' @importFrom magrittr %>%
#' @importFrom methods is
#' @importFrom tern keys
srv_g_bivariate <- function(input,
                            output,
                            session,
                            datasets,
                            dataname,
                            xvar = xvar,
                            yvar = yvar,
                            row_facet_var = row_facet_var,
                            col_facet_var = col_facet_var,
                            expert_settings = FALSE,
                            colour_var = colour_var,
                            fill_var = fill_var,
                            size_var = size_var) {
  stopifnot(all(dataname %in% datasets$datanames()))

  use_chunks()

  # Data Extraction
  xvar_data <- callModule(data_extract_module,
                          id = "xvar",
                          datasets = datasets,
                          data_extract_spec = xvar
  )
  yvar_data <- callModule(data_extract_module,
                          id = "yvar",
                          datasets = datasets,
                          data_extract_spec = yvar
  )
  row_facet_var_data <- callModule(data_extract_module,
                                   id = "row_facet_var",
                                   datasets = datasets,
                                   data_extract_spec = row_facet_var
  )
  col_facet_var_data <- callModule(data_extract_module,
                                   id = "col_facet_var",
                                   datasets = datasets,
                                   data_extract_spec = col_facet_var
  )

  if (expert_settings) {
    colour_var_data <- callModule(data_extract_module,
                                  id = "colour_var",
                                  datasets = datasets,
                                  data_extract_spec = colour_var
    )
    fill_var_data <- callModule(data_extract_module,
                                id = "fill_var",
                                datasets = datasets,
                                data_extract_spec = fill_var
    )
    size_var_data <- callModule(data_extract_module,
                                id = "size_var",
                                datasets = datasets,
                                data_extract_spec = size_var
    )
  }

  # Merging data ::: Preparation
  data_to_merge <- function(do_expert) {
    standard_data <- list(
      xvar_data(),
      yvar_data(),
      row_facet_var_data(),
      col_facet_var_data()
    )
    expert_data <- list()
    if (do_expert) {
      expert_data <- list(
        colour_var_data(),
        fill_var_data(),
        size_var_data()
      )
    }
    all_data <- append(standard_data, expert_data)
    return(all_data)
  }

  # Merging data ::: Execution
  data_reactive <- reactive({
    merge_datasets(
      data_to_merge(expert_settings && input$expert)
    )
  })

  # Access variables ::: Pre-checks
  variable_reactive <- reactive({
    anl <- data_reactive()
    xvar_name <- get_dataset_prefixed_col_names(xvar_data())
    yvar_name <- get_dataset_prefixed_col_names(yvar_data())

    validate(need(!(!is.null(yvar_name) && yvar_name %in% keys(yvar_data())),
                  "Please do not select key variables inside data"))
    validate(need(!(!is.null(xvar_name) && xvar_name %in% keys(yvar_data())),
                  "Please do not select key variables inside data"))

    if (input$facetting) {
      row_facet_var_name <- get_dataset_prefixed_col_names(row_facet_var_data())
      col_facet_var_name <- get_dataset_prefixed_col_names(col_facet_var_data())

      validate(need(!(!is.null(col_facet_var_name) &&
                        col_facet_var_name %in% keys(col_facet_var_data())),
                    "Please do not select key variables inside data"))
      validate(need(!(!is.null(row_facet_var_name) &&
                        row_facet_var_name %in% keys(row_facet_var_data())),
                    "Please do not select key variables inside data"))

      if (!is.null(col_facet_var_name) && !is.null(row_facet_var_name)) {
        validate(need(
          length(intersect(row_facet_var_name, col_facet_var_name)) == 0,
          "x and y facet variables cannot overlap"
        ))
      }
    }

    if (expert_settings) {
      if (input$expert) {
        colour_var_name <- get_dataset_prefixed_col_names(colour_var_data())
        fill_var_name <- get_dataset_prefixed_col_names(fill_var_data())
        size_var_name <- get_dataset_prefixed_col_names(size_var_data())
        validate(need(!(!is.null(colour_var_name) && colour_var_name %in% keys(colour_var_data())),
                      "Please do not select key variables inside data"))
        validate(need(!(!is.null(fill_var_name) && fill_var_name %in% keys(fill_var_data())),
                      "Please do not select key variables inside data"))
        validate(need(!(!is.null(size_var_name) && size_var_name %in% keys(size_var_data())),
                      "Please do not select key variables inside data"))
      }
    }
    use_density <- input$use_density == "density"
    free_x_scales <- input$free_x_scales
    free_y_scales <- input$free_y_scales

    validate_has_data(anl, 10)
    validate(need(!is.null(xvar), "Please define a valid column for the X-variable"))

    return(environment())
  })

  # Insert the plot into a plot_height module from teal.devel
  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  output$plot <- renderPlot({

    validate(need(is.environment(variable_reactive()), "Error in your variable selection"))

    # Copy all variables over from variable_reactive
    for (n in ls(variable_reactive(), all.names = TRUE)) {
      assign(n, get(n, variable_reactive()), environment())
    }

    cl <- bivariate_plot_call(
      data_name = "anl",
      xvar = xvar_name,
      yvar = yvar_name,
      x_class = class(anl[[xvar_name]]),
      y_class = if (!is.null(yvar_name)) class(anl[[yvar_name]]) else NULL,
      freq = !use_density
    )

    if (input$facetting) {
      facet_cl <- facet_ggplot_call(row_facet_var_name, col_facet_var_name, free_x_scales, free_y_scales)

      if (!is.null(facet_cl)) {
        cl <- call("+", cl, facet_cl)
      }
    }

    expert_cl <- NULL
    if (expert_settings) {
      if (input$expert) {
        expert_cl <- expert_ggplot_call(
          colour_var = colour_var_name, fill_var = fill_var_name, size_var = size_var_name,
          is_point = any(grepl("geom_point", cl %>% deparse()))
        )
      }
      if (!is.null(expert_cl)) {
        cl <- call("+", cl, expert_cl)
      }
    }

    ggtheme <- input$ggtheme
    if (!is.null(ggtheme)) {
      cl <- call("+", cl, as.call(parse(text = paste0("theme_", ggtheme))))
    }

    reset_chunks()

    set_chunk(expression = cl, id = "plotCall")

    p <- eval_chunks()

    if (is(p, "try-error")) {
      validate(need(FALSE, p))
    } else {
      p
    }
  })


  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "Bivariate Plot",
      rcode = get_rcode(
        datasets = datasets,
        merged_dataname = "anl",
        merged_datasets = data_to_merge(expert_settings && input$expert),
        title = "Bivariate Plot"
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
                                xvar,
                                yvar = NULL,
                                x_class,
                                y_class,
                                freq = TRUE,
                                col_var = NULL,
                                x_facet = NULL,
                                y_facet = NULL) {
  cl <- bivariate_ggplot_call(x_class = x_class, y_class = y_class, freq = freq)

  if (is.null(xvar)) {
    xvar <- "-"
  }
  if (is.null(yvar)) {
    yvar <- "-"
  }

  cl_plot <- substitute_q(cl, list(
    .ggplotcall = bquote(ggplot(.(as.name(data_name)))),
    .xvar = if (is.call(xvar)) xvar else as.name(xvar),
    .yvar = if (is.call(yvar)) yvar else as.name(yvar)
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
      quote(.ggplotcall + aes(x = .xvar) + geom_histogram() + ylab("Frequency"))
    } else {
      quote(.ggplotcall + aes(x = .xvar) + geom_histogram(aes(y = ..density..)) + ylab("Density")) # nolint
    }
  } else if (x_class == "NULL" && y_class == "numeric") {
    if (freq) {
      quote(.ggplotcall + aes(x = .yvar) + geom_histogram() + ylab("Frequency") + coord_flip())
    } else {
      quote(.ggplotcall + aes(x = .yvar) + geom_histogram(aes(y = ..density..)) + ylab("Density") + coord_flip()) # nolint
    }
  } else if (x_class == "factor" && y_class == "NULL") {
    if (freq) {
      quote(.ggplotcall + aes(x = .xvar) + geom_bar() + ylab("Frequency"))
    } else {
      quote(.ggplotcall + aes(x = .xvar) + geom_bar(aes(y = ..prop.., group = 1)) + ylab("Proportion")) # nolint
    }
  } else if (x_class == "NULL" && y_class == "factor") {
    if (freq) {
      quote(.ggplotcall + aes(x = .yvar, fill = factor(.fill_var)) + geom_bar() + ylab("Frequency") + coord_flip()) # nolint
    } else {
      quote(.ggplotcall + aes(x = .yvar) + geom_bar(aes(y = ..prop.., group = 1)) + # nolint
              ylab("Proportion") + coord_flip())
    }

    # Numeric Plots
  } else if (x_class == "numeric" && y_class == "numeric") {
    quote(.ggplotcall + aes(x = .xvar, y = .yvar) + geom_point())
  } else if (x_class == "numeric" && y_class == "factor") {
    quote(.ggplotcall + aes(x = .yvar, y = .xvar) + geom_boxplot() + coord_flip())
  } else if (x_class == "factor" && y_class == "numeric") {
    quote(.ggplotcall + aes(x = .xvar, y = .yvar) + geom_boxplot())

    # Factor and character plots
  } else if (x_class == "factor" && y_class == "factor") {
    quote(.ggplotcall + geom_mosaic(aes(x = product(.xvar), fill = .yvar), na.rm = TRUE))
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
facet_ggplot_call <- function(row_facet_var = NULL,
                              col_facet_var = NULL,
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

  if (is.null(row_facet_var) && is.null(col_facet_var)) {
    NULL
  } else if (!is.null(row_facet_var) && is.null(col_facet_var)) {
    call("facet_grid", rows = call_fun_dots("vars", row_facet_var), scales = scales)
  } else if (is.null(row_facet_var) && !is.null(col_facet_var)) {
    call("facet_grid", cols = call_fun_dots("vars", col_facet_var), scales = scales)
  } else {
    call("facet_grid",
         rows = call_fun_dots("vars", row_facet_var),
         cols = call_fun_dots("vars", col_facet_var),
         scales = scales
    )
  }
}

expert_ggplot_call <- function(colour_var, fill_var, size_var, is_point = FALSE) {
  if (!is.null(colour_var) && !is.null(fill_var) && is_point && !is.null(size_var)) {
    bquote(aes(
      colour = .(as.name(colour_var)),
      fill = .(as.name(fill_var)),
      size = .(as.name(size_var))
    ))
  } else if (!is.null(colour_var) && !is.null(fill_var) && (!is_point || is.null(size_var))) {
    bquote(aes(
      colour = .(as.name(colour_var)),
      fill = .(as.name(fill_var))
    ))
  } else if (!is.null(colour_var) && is.null(fill_var) && (!is_point || is.null(size_var))) {
    bquote(aes(
      colour = .(as.name(colour_var))
    ))
  } else if (is.null(colour_var) && !is.null(fill_var) && (!is_point || is.null(size_var))) {
    bquote(aes(
      fill = .(as.name(fill_var))
    ))
  } else if (is.null(colour_var) && is.null(fill_var) && is_point && !is.null(size_var)) {
    bquote(aes(
      size = .(as.name(size_var))
    ))
  } else if (!is.null(colour_var) && is.null(fill_var) && is_point && !is.null(size_var)) {
    bquote(aes(
      colour = .(as.name(colour_var)),
      size = .(as.name(size_var))
    ))
  } else if (is.null(colour_var) && !is.null(fill_var) && is_point && !is.null(size_var)) {
    bquote(aes(
      fill = .(as.name(fill_var)),
      size = .(as.name(size_var))
    ))
  } else {
    NULL
  }
}
