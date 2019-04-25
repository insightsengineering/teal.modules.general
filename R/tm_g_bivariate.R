#' Univariate and bivariate visualizations.
#'
#'
#' @inheritParams teal::module
#' @inheritParams teal::standard_layout
#' @param dataname name of dataset used to generate table
#' @param label (\code{character}) Label of the module
#' @param xvar variable name selected to plot along the x-axis by default. Variable can be numeric,
#'  factor or character.
#' @param yvar variable name selected to plot along the y-axis by default. Variable can be numeric,
#'  factor or character.
#' @param use_density boolean value for whether density is plotted
#' @param row_facet_var variable for x facets
#' @param col_facet_var variable for y facets
#' @param plot_height if scalar then the plot will have a fixed height. If a
#'   slider should be presented to adjust the plot height dynamically then it
#'   can be a vector of length three with \code{c(value, min and max)}
#'
#' @param color_byvar (\code{character}) Which variable to color by
#' @param free_x_scales (\code{logical}) If X scaling shall be changeable
#' @param free_y_scales (\code{logical}) If Y scaling shall be changeable
#' @param with_show_r_code (\code{logical}) Whether show R code button shall be show
#' @param code_data_processing (\code{character}) Code that was used to pre-process the data
#'
#' @import ggplot2
#' @import ggmosaic
#'
#' @details
#' This is a general module to visualize 1 & 2 dimensional data.
#'
#' @export
#'
#' @examples
#'
#' library(random.cdisc.data)
#' asl  <- radsl(seed = 1)
#'
#' attr(asl, "source") <- "random.cdisc.data::radsl(seed = 1)"
#'
#' x <- teal::init(
#'   data = list(ASL = asl),
#'   modules = root_modules(
#'     tm_g_bivariate(
#'       dataname = "ASL",
#'       xvar = choices_selected(names(asl), "BMRKR1"),
#'       yvar = choices_selected(names(asl), "AGE"),
#'       use_density = FALSE,
#'       color_byvar = choices_selected("STUDYID"),
#'       row_facet_var = choices_selected(c("SEX", "RACE"), NULL),
#'       col_facet_var = choices_selected(c("SEX", "RACE"), NULL),
#'       plot_height = c(600, 200, 2000)
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(x$ui, x$server)
#' }
#' @importFrom teal add_no_selected_choices
#' @import teal.devel
#' @importFrom methods is
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
    pre_output = NULL,
    post_output = NULL,
    with_show_r_code = TRUE,
    code_data_processing = NULL) {
  stopifnot(is.list(xvar))
  stopifnot(is.list(yvar))
  stopifnot(is.list(row_facet_var))
  stopifnot(is.list(col_facet_var))
  stopifnot(is.list(colour_var))
  stopifnot(is.list(fill_var))
  stopifnot(is.list(size_var))

  # No empty columns allowed for X-Var
  # No multiple X variables allowed
  lapply(xvar, function(ds_extract){
        stopifnot(is(ds_extract,"data_extract_spec"))
        stopifnot(!("" %in% ds_extract$columns$choices))
        stopifnot(!ds_extract$columns$multiple)
      }
  )

  stopifnot(is.logical(with_show_r_code))
  stopifnot(is.logical(free_x_scales))
  stopifnot(is.logical(free_y_scales))
  stopifnot(is.logical(use_density))

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

  teal::module(
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


#' @import teal
#' @import teal.devel
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
          radioButtons(ns("use_density"), NULL,
            choices = c("frequency", "density"),
            selected = ifelse(a$use_density, "density", "frequency"), inline = TRUE
          ),
          data_extract_input(
            id = ns("yvar"),
            label = "Y Variable",
            data_extract_spec = a$yvar
          ),
          div(style="border: 1px solid #e3e3e3; border-radius: 5px; padding: 0.6em; margin-left: -0.6em",
              tags$label("Facetting:"),
              switchInput(inputId = ns("facetting"), value = FALSE, size = "small"),
              conditionalPanel(
                  condition = paste0("input['",ns("facetting"), "']"),
                  ui_facetting(ns, a$row_facet_var, a$col_facet_var, a$free_x_scales, a$free_y_scales)
              )
          ),
          if (a$expert_settings) {
            # Put a grey border around the expert settings
            div(style="border: 1px solid #e3e3e3; border-radius: 5px; padding: 0.6em; margin-left: -0.6em",
              tags$label("Expert settings:"),
              switchInput(inputId = ns("expert"), value = FALSE, size = "small"),
              conditionalPanel(
                condition = paste0("input['",ns("expert"), "']"),
                ui_expert(ns, a$colour_var, a$fill_var, a$size_var)
              )
            )
          },
          optionalSelectInput(
              inputId = ns("ggtheme"), # nolint
              label = "Theme (by ggplot)",
              choices = c("grey", "gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void", "test"),
              selected = a$ggtheme,
              multiple = FALSE
          ),
          plot_height_input(id = ns("myplot"), value = a$plot_height)
      ),
      forms = if (a$with_show_r_code) actionButton(ns("show_rcode"), "Show R Code", width = "100%") else NULL,
      pre_output = a$pre_output,
      post_output = a$post_output
  )
}

#' @import shiny
#' @import teal.devel
ui_facetting <- function(ns, row_facet_var_spec, col_facet_var_spec, free_x_scales, free_y_scales){
  div(data_extract_input(
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
#' @import shiny
#' @import teal.devel
ui_expert <- function(ns, colour_var_spec, fill_var_spec, size_var_spec){
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

#' @importFrom methods is
#' @importFrom teal.devel get_rcode_header
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
    size_var = size_var
) {

  stopifnot(length(datasets$datanames()) == length(dataname))
  stopifnot(is.list(xvar))
  stopifnot(is.list(yvar))
  stopifnot(is.list(row_facet_var))
  stopifnot(is.list(col_facet_var))
  stopifnot(is.list(colour_var))
  stopifnot(is.list(fill_var))
  stopifnot(is.list(size_var))

  use_chunks(session)

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

  # Merging data
  data_to_merge <- function(do_expert) {
    standard_data <- list( xvar_data(),
        yvar_data(),
        row_facet_var_data(),
        col_facet_var_data())
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

  data_reactive <- reactive({
     merge_datasets(
      data_to_merge(expert_settings && input$expert)
     )
  })

  variable_reactive <- reactive({
      anl <- data_reactive()
      xvar_name <- get_dataset_prefixed_col_names(xvar_data())
      yvar_name <- get_dataset_prefixed_col_names(yvar_data())
      if (input$facetting) {
        row_facet_var_name <- get_dataset_prefixed_col_names(row_facet_var_data())
        col_facet_var_name <- get_dataset_prefixed_col_names(col_facet_var_data())

        if (!is.null(col_facet_var_name) && !is.null(row_facet_var_name)) {
          validate(need(length(intersect(row_facet_var_name, col_facet_var_name)) == 0,
                  "x and y facet variables cannot overlap"))
        }
      }

      if (expert_settings) {
       if (input$expert){
        colour_var_name <- get_dataset_prefixed_col_names(colour_var_data())
        fill_var_name <- get_dataset_prefixed_col_names(fill_var_data())
        size_var_name <- get_dataset_prefixed_col_names(size_var_data())
       }
      }
      use_density <- input$use_density == "density"
      free_x_scales <- input$free_x_scales
      free_y_scales <- input$free_y_scales

      validate_has_data(anl, 10)
      validate(need(!is.null(xvar), "Please define a valid column for the X-variable"))

      return(environment())
  })

  plot_call <- reactive({

        for(n in ls(variable_reactive(), all.names=TRUE)) assign(n, get(n, variable_reactive()), environment())

        cl <- g_bp_cl(
            data_name = "anl",
            xvar = xvar_name,
            yvar = yvar_name,
            x_class = class(anl[[xvar_name]]),
            y_class = if(!is.null(yvar_name)) class(anl[[yvar_name]]) else NULL,
            freq = !use_density)

        if (input$facetting) {
          facet_cl <- g_facet_cl(row_facet_var_name, col_facet_var_name, free_x_scales, free_y_scales)

          if (!is.null(facet_cl)) {
            cl <- call("+", cl, facet_cl)
          }
        }

        expert_cl <- NULL
        if (expert_settings){
          if (input$expert) {
            expert_cl <- g_expert_cl(colour_var = colour_var_name, fill_var = fill_var_name, size_var = size_var_name,
              is_point = any(grepl("geom_point", cl %>% deparse()))
            )
          }
          if (!is.null(expert_cl)) {
            cl <- call("+", cl, expert_cl)
          }
        }

        ggtheme <- input$ggtheme
        if(!is.null(ggtheme)){
          cl <- call("+", cl,  as.call(parse(text = paste0("theme_", ggtheme))))
        }

        renew_chunk_environment(envir = environment())
        renew_chunks()

        set_chunk("plotCall", cl)

      })

  # Insert the plot into a plot_height module from teal.devel
  callModule(plot_with_height,
      id = "myplot",
      plot_height = reactive(input$myplot),
      plot_id = session$ns("plot")
  )
  output$plot <- renderPlot({
        plot_call()

        p <- eval_remaining()

        if (is(p, "try-error")) {
          validate(need(FALSE, p))
        } else {
          p
        }
      })


  observeEvent(input$show_rcode, {
        teal.devel::show_rcode_modal(
            title = "Bivariate Plot",
            rcode = get_rcode(
                datasets = datasets,
                dataname = dataname,
                merged_dataname = "anl",
                merged_datasets = data_to_merge(expert_settings && input$expert),
                title = "Bivariate Plot"
            )
        )
      })
}


#' Create facet call
#'
#' @noRd
#'
#' @examples
#'
#' g_facet_cl(LETTERS[1:3])
#' g_facet_cl(NULL, LETTERS[23:26])
#' g_facet_cl(LETTERS[1:3], LETTERS[23:26])
g_facet_cl <- function(row_facet_var = NULL,
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



#' Bivariate Plot
#'
#'
#' @noRd
#'
#' @examples
#' # need to temporary export in order to test
#' c1 <- rnorm(100)
#' c2 <- rnorm(100)
#' d1 <- factor(sample(letters[1:5], 100, TRUE), levels = letters[1:5])
#' d2 <- factor(sample(LETTERS[24:26], 100, TRUE), levels = LETTERS[24:26])
#'
#' g_bp(x = c1)
#' g_bp(x = c1, freq = FALSE)
#' g_bp(y = c1)
#' g_bp(y = c1, freq = FALSE)
#' g_bp(x = d1)
#' g_bp(x = d1, freq = FALSE)
#' g_bp(y = d1)
#' g_bp(y = d1, freq = FALSE)
#'
#'
#' g_bp(x = c1, y = c2)
#' g_bp(x = c1, y = d1)
#' g_bp(x = d1, y = c1)
#' g_bp(x = d1, y = d2) ## no color
#'
#'
#'
#'
#' x <- d1
#' library(scales)
#' ggplot() + aes(x = x) + geom_bar(aes(y = ..prop.., group = 1))
g_bp <- function(x = NULL, y = NULL, freq = TRUE) {
  df <- if (!is.null(x) && !is.null(y)) {
        data.frame(x = x, y = y)
      } else if (is.null(x) && !is.null(y)) {
        data.frame(y = y)
      } else if (!is.null(x) && is.null(y)) {
        data.frame(x = x)
      } else {
        stop("not both variables can be null")
      }

  eval(g_bp_cl("df", "x", "y", class(x), class(y), freq = freq))
}


#' Get Substituted ggplot call
#'
#' @noRd
#'
#' @examples
#'
#' g_bp_cl("ANL", "BAGE", "RACE", "numeric", "factor")
#' g_bp_cl("ANL", "BAGE", NULL, "numeric", "NULL")
g_bp_cl <- function(data_name,
    xvar,
    yvar = NULL,
    x_class,
    y_class,
    freq = TRUE,
    col_var = NULL,
    x_facet = NULL,
    y_facet = NULL
) {

  cl <- aes_geom_call(x_class = x_class, y_class = y_class, freq = freq)

  if (is.null(xvar)) {
    xvar <- "-"
  }
  if (is.null(yvar)) {
    yvar <- "-"
  }

  cl_plot <- substitute_q(cl, list(
          .gg = bquote(ggplot(.(as.name(data_name)))),
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

#' Create ggplot part of call
#'
#' @noRd
#'
#' @examples
#' aes_geom_call("numeric", "NULL")
#' aes_geom_call("numeric", "NULL", freq = FALSE)
#'
#' aes_geom_call("NULL", "numeric")
#' aes_geom_call("NULL", "numeric", freq = FALSE)
#'
#' aes_geom_call("NULL", "factor")
#' aes_geom_call("NULL", "factor", freq = FALSE)
#'
#' aes_geom_call("factor", "NULL")
#' aes_geom_call("factor", "NULL", freq = FALSE)
#'
#' aes_geom_call("numeric", "numeric")
#' aes_geom_call("numeric", "factor")
#' aes_geom_call("factor", "numeric")
#' aes_geom_call("factor", "factor")
aes_geom_call <- function(x_class = c("NULL", "numeric", "factor", "character", "logical"),
    y_class = c("NULL", "numeric", "factor", "character", "logical"),
    freq = TRUE) {
  x_class <- match.arg(x_class)
  y_class <- match.arg(y_class)

  if (x_class %in% c("character", "logical")) {
    x_class <- "factor"
  }
  if (y_class %in% c("character", "logical")) {
    y_class <- "factor"
  }

  if (all(c(x_class, y_class) == "NULL")) {
    stop("either x or y is required")
  }

  if (x_class == "numeric" && y_class == "NULL") {
    if (freq) {
      quote(.gg + aes(x = .xvar) + geom_histogram() + ylab("Frequency"))
    } else {
      quote(.gg + aes(x = .xvar) + geom_histogram(aes(y = ..density..)) + ylab("Density")) # nolint
    }
  } else if (x_class == "NULL" && y_class == "numeric") {
    if (freq) {
      quote(.gg + aes(x = .yvar) + geom_histogram() + ylab("Frequency") + coord_flip())
    } else {
      quote(.gg + aes(x = .yvar) + geom_histogram(aes(y = ..density..)) + ylab("Density") + coord_flip()) # nolint
    }
  } else if (x_class == "factor" && y_class == "NULL") {
    if (freq) {
      quote(.gg + aes(x = .xvar) + geom_bar() + ylab("Frequency"))
    } else {
      quote(.gg + aes(x = .xvar) + geom_bar(aes(y = ..prop.., group = 1)) + ylab("Proportion")) # nolint
    }
  } else if (x_class == "NULL" && y_class == "factor") {
    if (freq) {
      quote(.gg + aes(x = .yvar, fill = factor(.fill_var)) + geom_bar() + ylab("Frequency") + coord_flip()) # nolint
    } else {
      quote(.gg + aes(x = .yvar) + geom_bar(aes(y = ..prop.., group = 1)) + # nolint
              ylab("Proportion") + coord_flip())
    }
  } else if (x_class == "numeric" && y_class == "numeric") {
    quote(.gg + aes(x = .xvar, y = .yvar) + geom_point())
  } else if (x_class == "numeric" && y_class == "factor") {
    quote(.gg + aes(x = .yvar, y = .xvar) + geom_boxplot() + coord_flip())
  } else if (x_class == "factor" && y_class == "numeric") {
    quote(.gg + aes(x = .xvar, y = .yvar) + geom_boxplot())
  } else if (x_class == "factor" && y_class == "factor") {
    quote(.gg + geom_mosaic(aes(x = product(.xvar), fill = .yvar), na.rm = TRUE))
  } else {
    stop("x y type combination not allowed")
  }
}

label_over_name <- function(x, name) {
  if (is.null(x)) {
    name
  } else {
    x
  }
}

g_expert_cl <- function(colour_var, fill_var, size_var, is_point = FALSE){
  if (!is.null(colour_var) && !is.null(fill_var) && is_point && !is.null(size_var)) {
    bquote(aes(
            colour = .(as.name(colour_var)),
            fill = .(as.name(fill_var)),
            size = .(as.name(size_var))
        ))
  } else if (!is.null(colour_var) && !is.null(fill_var) && (!is_point || is.null(size_var)) ) {
    bquote(aes(
            colour = .(as.name(colour_var)),
            fill = .(as.name(fill_var))
        ))
  } else if(!is.null(colour_var) && is.null(fill_var) && (!is_point || is.null(size_var)) ){
    bquote(aes(
            colour = .(as.name(colour_var))
        ))
  } else if(is.null(colour_var) && !is.null(fill_var) && (!is_point || is.null(size_var)) ){
    bquote(aes(
            fill = .(as.name(fill_var))
        ))
  } else if(is.null(colour_var) && is.null(fill_var) && is_point && !is.null(size_var) ){
    bquote(aes(
            size = .(as.name(size_var))
    ))
  } else if(!is.null(colour_var) && is.null(fill_var) && is_point && !is.null(size_var) ){
    bquote(aes(
            colour = .(as.name(colour_var)),
            size = .(as.name(size_var))
        ))
  } else if(is.null(colour_var) && !is.null(fill_var) && is_point && !is.null(size_var) ){
    bquote(aes(
            fill = .(as.name(fill_var)),
            size = .(as.name(size_var))
        ))
  } else {
    NULL
  }
}
