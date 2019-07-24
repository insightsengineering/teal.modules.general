#' Create a Venn Diagram Plot with 2 groups
#'
#' @param x boolean has biomarker or not
#' @param y boolean has biomarker of not
#'
#' @return  list with absolute and percentage cross table
#' @noRd
#' @examples
#'
#' x = c(rep(T, 5), rep(F, 3), rep(T, 4), rep(F, 9))
#' y = c(rep(T, 5), rep(F, 3), rep(F, 4), rep(T, 9))
#' table(x, y)
#' y <- teal.modules.general:::venn2(x, y, "X", "Y")
#' plot(y)
#'
#'
#' # if too few then table is plotted
#' x = c(F, F, F, F, T, T)
#' y = c(F, T, T, T, F, F)
#' table(x, y)
#' y <- teal.modules.general:::venn2(x, y, "X", "Y")
#' plot(y)
venn2 <- function(x, y, xlab, ylab) {
  if (length(x) <= 0) {
    stop("lenght of x must be > 0")
  }

  if (missing(xlab)) {
    xlab <- deparse(substitute(x))
  }
  if (missing(ylab)) {
    ylab <- deparse(substitute(y))
  }

  if (length(x) != length(y)) {
    stop("x and y need to be of the same length")
  }
  if (!is.logical(x) || !is.logical(y)) {
    stop("x and y need to be boolean")
  }

  # what to do with NA?
  sel <- !is.na(x) & !is.na(y)

  x <- factor(x, levels = c(FALSE, TRUE))[sel]
  y <- factor(y, levels = c(FALSE, TRUE))[sel]

  abs <- table(x, y)
  per <- abs / length(x)

  structure(list(absolute = abs, perentage = per, xlab = xlab, ylab = ylab), class = "venn2")
}


#' plot venn2 object
#'
#' @param x an object returned by \code{\link{venn2}}
#'
#' @noRd
#'
#' @importFrom graphics plot
#' @importFrom stats uniroot
#' @import grid
#' @export
plot.venn2 <- function(x, ...) {

  abs <- x$absolute
  per <- apply(x$perentage, c(1, 2), function(xi) round(xi * 100, 1))

  if (!all(dim(abs) == c(2, 2))) {
    stop("dimension of x$absolute is not 2x2")
  }


  label_x1_y1 <- paste0(abs[1, 1], "\n(", per[1, 1], "%)")
  label_x_y <- paste0(abs[2, 2], "\n(", per[2, 2], "%)")
  label_x1_y <- paste0(abs[1, 2], "\n(", per[1, 2], "%)")
  label_x_y1 <- paste0(abs[2, 1], "\n(", per[2, 1], "%)")


  plot_grob <- if (any(abs <= 2)) {
    ## plot a table

    labels <- c(label_x1_y1, label_x_y1, label_x_y, label_x1_y)
    w2 <- .5 * do.call(max, lapply(labels, stringWidth))
    h2 <- .5 * do.call(max, lapply(labels, stringHeight))

    u1 <- unit(1, "lines")

    gTree(
      children = gList(
        textGrob(label_x1_y1,
                 x = unit(0.5, "npc") - (w2 + u1),
                 y = unit(0.5, "npc") + (h2 + u1)),
        textGrob(label_x_y1,
                 x = unit(0.5, "npc") - (w2 + u1),
                 y = unit(0.5, "npc") - (h2 + u1)),
        textGrob(label_x1_y,
                 x = unit(0.5, "npc") + (w2 + u1),
                 y = unit(0.5, "npc") + (h2 + u1)),
        textGrob(label_x_y,
                 x = unit(0.5, "npc") + (w2 + u1),
                 y = unit(0.5, "npc") - (h2 + u1)),
        textGrob(x$xlab,
                 y = unit(.5, "npc") + 2 * (h2 + u1),
                 gp = gpar(fontface = "bold")),
        textGrob(x$ylab,
                 x = unit(.5, "npc") - 2 * (w2 + u1),
                 gp = gpar(fontface = "bold"), rot = 90)
      )
    )


  } else {
    ## plot venn diagram

    # solve for radius of circles using area

    ax <- sqrt((abs[2, 1] + abs[2, 2]) / pi) #radius of 1st circle
    ay <- sqrt((abs[1, 2] + abs[2, 2]) / pi) #radius of 2nd circle

    #solve for d, the distance between the 2 centers of the cicles

    d_solve <- uniroot(function(d) ay^2 * acos((d^2 + ay^2 - ax^2) / (2 * d * ay))
                       + ax^2 * acos((d^2 + ax^2 - ay^2) / (2 * d * ax))
                       - 1 / 2 * sqrt((- d + ay + ax) * (d + ay - ax) * (d - ay + ax) * (d + ay + ax)) - abs[2, 2],
                       lower = abs(ax - ay) + 1e-9, upper = ax + ay - 1e-9, tol = 1e-9)$root

    # solve for a (the cord connecting the cusps of the lens)

    a <- 1 / d_solve * sqrt((-d_solve + ay + ax) * (d_solve + ay - ax) * (d_solve - ay + ax) * (d_solve + ay + ax))

    # find dx and dy using pythagorean theorm
    # dx and dy are distances from center of cusp to the respective centers of the circles
    # sacle d and r to viewport width, making 2x diameter 90% width of viewport

    min_side <- unit(1, "snpc")

    dx_num <- sqrt(ax^2 - (a / 2)^2)
    dy_num <- sqrt(ay^2 - (a / 2)^2)

    dx <- dx_num / (2 * (ax + ay)) * min_side
    dy <- dy_num / (2 * (ax + ay)) * min_side

    rx <- ax / (2 * (ax + ay)) * min_side
    ry <- ay / (2 * (ax + ay)) * min_side

    gTree(
      children = gList(
        #draw circles
        circleGrob(x = unit(0.5, "npc") - dx, y = unit(0.5, "npc"), r = rx,
                   gp = gpar(fill = "thistle", alpha = .4)),
        circleGrob(x = unit(0.5, "npc") + dy, y = unit(0.5, "npc"), r = ry,
                   gp = gpar(fill = "orange", alpha = .4)),
        # add labels
        textGrob(
          x$xlab,
          x = unit(0.5, "npc") - dx - 1.2 * cos(pi / 4) * rx,
          y = unit(0.5, "npc") - 1.2 * sin(pi / 4) * rx,
          just = c("right", "center")
        ),
        textGrob(
          x$ylab,
          x = unit(0.5, "npc") + dy + 1.2 * cos(pi / 4) * ry,
          y = unit(0.5, "npc") - 1.2 * sin(pi / 4) * ry,
          just = c("left", "center")
        ),
        textGrob(
          label_x1_y1,
          x = unit(0.5, "npc"),
          y = unit(0.5, "npc") + max(rx, ry) + unit(2, "lines"),
          just = c("center", "center"),
          gp = gpar(lineheight = .9)
        ),
        textGrob(
          label_x_y,
          x = unit(0.5, "npc"),
          y = unit(0.5, "npc"),
          just = c("center", "center"),
          gp = gpar(lineheight = .9)
        ),
        textGrob(
          label_x_y1,
          x = unit(0.5, "npc") - ax / (2 * (ax + ay)) * min_side,
          y = unit(0.5, "npc"),
          just = c("center", "center"),
          gp = gpar(lineheight = .9)
        ),
        textGrob(
          label_x1_y,
          x = unit(0.5, "npc") + ay / (2 * (ax + ay)) * min_side,
          y = unit(0.5, "npc"),
          just = c("center", "center"),
          gp = gpar(lineheight = .9)
        )
      )
    )
  }

  # draw graphic
  grid.newpage()
  pushViewport(plotViewport(margins = c(2, 2, 2, 2))) # add margins
  grid.rect()
  grid.draw(plot_grob)
}


#' venn2 teal module
#'
#'
#' @examples
#' @noRd
#'
#' N <- 100
#' var_biomarkers <- paste0("B", 1:10)
#' sample_bm_data <- lapply(1:10, function(x)sample(c(TRUE, FALSE), N, replace = TRUE))
#' names(sample_bm_data) <- var_biomarkers
#'
#' ASL <- do.call(data.frame, c(
#'   list(USUBJID = paste("ID", 1:N), STUDYID = "1"), sample_bm_data
#' ))
#'
#'
#' app <- init(
#'   data = cdisc_data(
#'     ASL = ASL,
#'     code = '
#'     N <- 100
#'     var_biomarkers <- paste0("B", 1:10)
#'     sample_bm_data <- lapply(1:10, function(x)sample(c(TRUE, FALSE), N, replace = TRUE))
#'     names(sample_bm_data) <- var_biomarkers
#'     ASL <- do.call(data.frame, c(
#'     list(USUBJID = paste("ID", 1:N), STUDYID = "1"), sample_bm_data
#'     ))
#'     ',
#'     check = FALSE),
#'   modules = root_modules(
#'     teal.modules.general:::tm_venn2(
#'       "Venn Diagram", "ASL",
#'       bm1 = data_extract_spec(
#'         dataname = "ASL",
#'         column = columns_spec(
#'           label = "Select Biomarker",
#'           choices = var_biomarkers,
#'           selected = "B1",
#'           fixed = FALSE)
#'       ),
#'       bm2 = data_extract_spec(
#'         dataname = "ASL",
#'         column = columns_spec(
#'           label = "Select Biomarker",
#'           choices = var_biomarkers,
#'           selected = "B2",
#'           fixed = FALSE,)
#'       )
#'     )
#'   )
#' )
#' \dontRun{
#'  shinyApp(app$ui, app$server)
#' }
tm_venn2 <- function(label,
                     dataname,
                     bm1,
                     bm2,
                     plot_height = c(600, 200, 2000),
                     alpha = c(1, 0, 1),
                     pre_output = tags$p("NAs get currently removed"),
                     post_output = NULL) {
  stopifnot(is.character.single(label))
  stopifnot(is.character.single(dataname))
  stopifnot(is.class.list("data_extract_spec")(bm1) || is(bm1, "data_extract_spec"))
  stopifnot(is.class.list("data_extract_spec")(bm2) || is(bm2, "data_extract_spec"))
  if (is.class.list("data_extract_spec")(bm1)) {
    stop_if_not(list(all(vapply(bm1, function(x) !isTRUE(x$columns$multiple), logical(1))),
                     "bm1 variable should not allow multiple selection"))
  } else if (is(bm1, "data_extract_spec")) {
    stop_if_not(list(!isTRUE(bm2$columns$multiple),
                     "bm1 variable should not allow multiple selection"))
  }
  if (is.class.list("data_extract_spec")(bm2)) {
    stop_if_not(list(all(vapply(bm2, function(x) !isTRUE(x$columns$multiple), logical(1))),
                     "bm2 variable should not allow multiple selection"))
  } else if (is(bm2, "data_extract_spec")) {
    stop_if_not(list(!isTRUE(bm2$columns$multiple),
                     "bm2 variable should not allow multiple selection"))
  }
  stopifnot(is.numeric.vector(plot_height) && (length(plot_height) == 3 || length(plot_height) == 1))
  stopifnot(`if`(length(plot_height) == 3, plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3], TRUE))
  stopifnot(is.numeric.vector(alpha) && (length(alpha) == 3 || length(alpha) == 1))
  stopifnot(`if`(length(alpha) == 3, alpha[1] >= alpha[2] && alpha[1] <= alpha[3], TRUE))

  args <- as.list(environment())

  module(
    label = label,
    server = srv_venn2,
    ui = ui_venn2,
    ui_args = args,
    server_args = list(dataname = dataname, bm1 = bm1, bm2 = bm2),
    filters = dataname
  )
}


ui_venn2 <- function(id, ...) {
  arguments <- list(...)
  ns <- NS(id)

  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(arguments$dataname)),
      data_extract_input(
        id = ns("bm1"),
        label = "Biomarker 1",
        data_extract_spec = arguments$bm1
      ),
      data_extract_input(
        id = ns("bm2"),
        label = "Biomarker 2",
        data_extract_spec = arguments$bm2
      ),

      if (all(c(
        length(arguments$plot_height) == 1,
        length(arguments$alpha) == 1
      ))) {
        NULL
      } else {
        tags$label("Plot Settings", class = "text-primary", style = "margin-top: 15px;")
      },
      optionalSliderInputValMinMax(ns("plot_height"), "plot height", arguments$plot_height, ticks = FALSE)
    ),
    pre_output = arguments$pre_output,
    post_output = arguments$post_output
  )
}


srv_venn2 <- function(input, output, session, datasets, dataname, bm1, bm2) {
  stopifnot(all(dataname %in% datasets$datanames()))

  ## dynamic plot height
  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    plotOutput(session$ns("scatterplot"), height = plot_height)
  })

  bm1_data <- callModule(
    data_extract_module,
    id = "bm1",
    datasets = datasets,
    data_extract_spec = bm1
  )
  bm2_data <- callModule(
    data_extract_module,
    id = "bm2",
    datasets = datasets,
    data_extract_spec = bm2
  )

  output$scatterplot <- renderPlot({
    bm1_var <- get_dataset_prefixed_col_names(bm1_data())
    bm2_var <- get_dataset_prefixed_col_names(bm2_data())

    anl <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    validate(need(!is.null(anl) && is.data.frame(anl), "no data left"))
    validate(need(nrow(anl) > 0, "no observations left"))

    #validate(need(bm1_var != bm2_var, "Please choose different Biomarker 1 and 2"))

    merged_dataset <- merge_datasets(list(bm1_data(), bm2_data()))

    x <- try(venn2(merged_dataset[[bm1_var]], merged_dataset[[bm2_var]], bm1_var, bm2_var), silent = TRUE)

    if (is(x, "try-error")) {
      validate(need(FALSE, paste0("could not calculate cross table:\n\n", x)))
    }

    plot(x)
  })
}
