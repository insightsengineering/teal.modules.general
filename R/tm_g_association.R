#' Stack Plots of variables and show association with reference variable
#'
#' @inheritParams teal.devel::standard_layout
#' @inheritParams teal::module
#' @param ref (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   reference variable, must set \code{multiple = FALSE}
#' @param vars (\code{data_extract_spec} or \code{list} of multiple \code{data_extract_spec})
#'   associated variables
#' @param show_association (\code{logical}) wheater show association of \code{vars} with refference variable
#' @param plot_height (\code{numeric}) vector with three elements defining selected, min and max plot height
#' @param with_show_r_code (\code{logical}) Whether show R Code button shall be enabled
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @export
#' @examples
#'
#' # datasets: single wide
#' # Association plot of selected reference variable (SEX)
#' # against other selected variables (BMRKR1)
#' library(random.cdisc.data)
#' ADSL <- cadsl
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     code = "ADSL <- cadsl",
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_association(
#'       ref = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select Variable",
#'           choices = names(ADSL),
#'           selected = "AGE",
#'           fixed = FALSE
#'         )
#'       ),
#'       vars = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select Variables",
#'           choices = names(ADSL),
#'           selected = "BMRKR1",
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
#'
#' # datasets: different wide
#' # Create an association plot with the stratification (STRATA1) as the reference
#' # and the AGE, COUNTRY, RACE as plotted variables
#'
#' library(random.cdisc.data)
#' library(tern)
#' library(dplyr)
#'
#' ADSL <- cadsl
#' ADSL <- mutate_at(ADSL,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = list(~as.factor(.))) %>% select("ARM", "ACTARM", "ACTARMCD",
#'                  "SEX", "STRATA1", "AGE", "USUBJID", "STUDYID", "STRATA2")
#' keys(ADSL) <- c("STUDYID", "USUBJID")
#'
#'
#' ADSL_2 <- mutate_at(cadsl,
#'                  .vars = vars(c("ARM", "ACTARM", "ACTARMCD", "SEX", "STRATA1", "STRATA2")),
#'                  .funs = list(~as.factor(.))) %>% select("ACTARM", "AGE", "STRATA2",
#'                  "COUNTRY", "USUBJID", "STUDYID")
#' keys(ADSL_2) <- c("STUDYID", "USUBJID")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     dataset("ADSL_2", ADSL_2),
#'     code = 'ADSL <- cadsl
#'             keys(ADSL) <- c("STUDYID", "USUBJID")',
#'     check = FALSE),
#'   modules = root_modules(
#'     tm_g_association(
#'       label = "Regression",
#'       ref = data_extract_spec(
#'         dataname = "ADSL",
#'         select = select_spec(
#'           label = "Select Variable",
#'           choices = c("AGE", "SEX", "STRATA1", "RACE"),
#'           selected = c("STRATA1"),
#'           multiple = FALSE,
#'           fixed = FALSE
#'         )),
#'       vars = data_extract_spec(
#'         dataname = "ADSL_2",
#'         select = select_spec(
#'           label = "Select Variables",
#'           choices = c("COUNTRY", "AGE", "RACE"),
#'           selected = c("AGE", "COUNTRY", "RACE"),
#'           multiple = TRUE,
#'           fixed = FALSE
#'         ))
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: multiple long datasets
#' # Association plot of different parameters from ADRS or ADTTE datasets
#' library(random.cdisc.data)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#' ADTTE <- cadtte
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADRS", ADRS),
#'     cdisc_dataset("ADTTE", ADTTE),
#'     code = "ADSL <- cadsl; ADRS <- cadrs; ADTTE <- cadtte",
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_association(
#'       ref = list(
#'         data_extract_spec(
#'           dataname = "ADRS",
#'           select = select_spec(
#'             label = "Select Variable",
#'             choices = c("AGE", "SEX"),
#'             selected = "AGE",
#'             multiple = FALSE,
#'             fixed = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADTTE",
#'           select = select_spec(
#'             label = "Select Variable",
#'             choices = c("AVAL", "CNSR"),
#'             selected = "AVAL",
#'             multiple = FALSE,
#'             fixed = FALSE
#'           )
#'         )
#'       ),
#'       vars = list(
#'         data_extract_spec(
#'           dataname = "ADRS",
#'           select = select_spec(
#'             label = "Select Variables",
#'             choices = names(ADRS),
#'             selected = c("AVAL", "AVALC"),
#'             multiple = TRUE,
#'             fixed = FALSE
#'           ),
#'           filter = filter_spec(
#'             label = "Select Parameter",
#'             vars = "PARAMCD",
#'             choices = unique(ADTTE$PARAMCD),
#'             selected = "OS",
#'             multiple = FALSE
#'           )
#'         ),
#'         data_extract_spec(
#'           dataname = "ADTTE",
#'           filter = filter_spec(
#'             label = "Select Endpoints",
#'             vars = c("PARAMCD", "AVISIT"),
#'             choices = apply(expand.grid(levels(ADRS$PARAMCD),
#'             levels(ADRS$AVISIT)), 1, paste, collapse = " - "),
#'             selected = "OVRINV - Screening",
#'             multiple = TRUE
#'           ),
#'           select = select_spec(
#'             label = "Select Variables",
#'             choices = names(ADTTE),
#'             selected = NULL,
#'             multiple = TRUE,
#'             fixed = FALSE
#'           )
#'         )
#'       )
#'     )
#'   )
#' )
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: wide and long
#' # Create an association plot with the patient AGE (ADSL$AGE) as the reference
#' # and a measurement the response time filtered by visit and response as
#' # the plotted variable
#' library(random.cdisc.data)
#' library(tern)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#'
#' keys(ADSL) <- c("STUDYID", "USUBJID")
#' keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#' app <- init(
#'  data = cdisc_data(
#'    cdisc_dataset("ADSL", ADSL),
#'    cdisc_dataset("ADRS", ADRS),
#'    code = 'ADSL <- cadsl
#'            ADRS <- cadrs
#'            keys(ADSL) <- c("STUDYID", "USUBJID")
#'            keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'    check = FALSE),
#'  modules = root_modules(
#'    tm_g_association(
#'      label = "Association Plots",
#'      ref = data_extract_spec(
#'        dataname = "ADSL",
#'        select = select_spec(
#'          choices = c("SEX", "AGE", "RACE", "COUNTRY"),
#'          selected = c("AGE"),
#'          multiple = FALSE,
#'          fixed = FALSE,
#'          label = "Select Variable"
#'       )
#'      ),
#'      vars = data_extract_spec(
#'        dataname = "ADRS",
#'        filter = list(
#'          filter_spec(
#'            vars = "PARAM",
#'            choices = levels(ADRS$PARAM),
#'            selected = levels(ADRS$PARAM)[1],
#'            multiple = FALSE,
#'            label = "Choose response"
#'          ),
#'          filter_spec(
#'            vars = "AVISIT",
#'            choices = levels(ADRS$AVISIT),
#'            selected = levels(ADRS$AVISIT)[1],
#'            multiple = FALSE,
#'            label = "Choose visit"
#'          )
#'        ),
#'        select = select_spec(
#'          choices = "AVAL",
#'          selected = "AVAL",
#'          multiple = FALSE,
#'          fixed = TRUE,
#'          label = "Selected Variable"
#'        )
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#' # datasets: same long
#' # Examine association between two different variables from ADRS dataset.
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ADSL <- cadsl
#' ADRS <- cadrs
#'
#' keys(ADSL) <- c("STUDYID", "USUBJID")
#' keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#' app <- init(
#'  data = cdisc_data(
#'    cdisc_dataset("ADSL", ADSL),
#'    cdisc_dataset("ADRS", ADRS),
#'    code = 'ADSL <- cadsl
#'            ADRS <- cadrs
#'            keys(ADSL) <- c("STUDYID", "USUBJID")
#'            keys(ADRS) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'    check = FALSE),
#'  modules = root_modules(
#'    tm_g_association(
#'      label = "Association Plots",
#'      ref = data_extract_spec(
#'        dataname = "ADRS",
#'        select = select_spec(
#'          choices = names(ADRS),
#'          selected = "AVAL",
#'          multiple = FALSE,
#'          fixed = FALSE,
#'          label = "Select Variable"
#'       )
#'      ),
#'      vars = data_extract_spec(
#'        dataname = "ADRS",
#'        select = select_spec(
#'          choices = names(ADRS),
#'          selected = "PARAMCD",
#'          multiple = FALSE,
#'          fixed = FALSE,
#'          label = "Select Variable"
#'        )
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
#'
#'
#' # datasets: different subsets of long dataset
#' # Examine association between two different measurements from ALB dataset.
#'
#' library(random.cdisc.data)
#' library(tern)
#'
#' ADSL <- cadsl
#' ADLB <- cadlb
#'
#' keys(ADSL) <- c("STUDYID", "USUBJID")
#' keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")
#'
#' app <- init(
#'   data = cdisc_data(
#'     cdisc_dataset("ADSL", ADSL),
#'     cdisc_dataset("ADLB", ADLB),
#'     code = 'ADSL <- cadsl
#'            ADLB <- cadlb
#'            keys(ADSL) <- c("STUDYID", "USUBJID")
#'            keys(ADLB) <- c("STUDYID", "USUBJID", "PARAMCD", "AVISIT")',
#'     check = FALSE
#'   ),
#'   modules = root_modules(
#'     tm_g_association(
#'       ref = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[1],
#'             multiple = FALSE,
#'             label = "Lab"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Visit"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       ),
#'       vars = data_extract_spec(
#'         dataname = "ADLB",
#'         filter = list(
#'           filter_spec(
#'             vars = "PARAMCD",
#'             choices = levels(ADLB$PARAMCD),
#'             selected = levels(ADLB$PARAMCD)[2:3],
#'             multiple = TRUE,
#'             label = "Lab"
#'           ),
#'           filter_spec(
#'             vars = "AVISIT",
#'             choices = levels(ADLB$AVISIT),
#'             selected = levels(ADLB$AVISIT)[1],
#'             multiple = FALSE,
#'             label = "Visit"
#'           )
#'         ),
#'         select = select_spec(
#'           choices = "AVAL",
#'           selected = "AVAL",
#'           multiple = FALSE,
#'           fixed = TRUE
#'         )
#'       )
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(app$ui, app$server)
#' }
tm_g_association <- function(label = "Association",
                             ref,
                             vars,
                             show_association = TRUE,
                             plot_height = c(600, 400, 5000),
                             pre_output = NULL,
                             post_output = NULL,
                             with_show_r_code = TRUE) {
  if (!is.class.list("data_extract_spec")(ref)) {
    ref <- list(ref)
  }
  if (!is.class.list("data_extract_spec")(vars)) {
    vars <- list(vars)
  }

  stopifnot(is.character.single(label))
  stopifnot(is.class.list("data_extract_spec")(ref))
  stop_if_not(list(all(vapply(ref, function(x) !(x$select$multiple), logical(1))),
                   "'ref' should not allow multiple selection"))
  stopifnot(is.class.list("data_extract_spec")(vars))
  stopifnot(is.logical.single(show_association))
  stopifnot(is.numeric.vector(plot_height) && length(plot_height) == 3)
  stopifnot(plot_height[1] >= plot_height[2] && plot_height[1] <= plot_height[3])
  stopifnot(is.logical(with_show_r_code))

  args <- as.list(environment())

  module(
    label = label,
    server = function(input, output, session, datasets, ...) {
      output$dataname <- renderUI(helpText("Dataset:",tags$code(paste(datasets$datanames(), collapse = ", "))))
      return(NULL)
    },
    ui = ui_tm_g_association,
    ui_args = args,
    server_args = list(
      ref = ref,
      vars = vars
    ),
    filters = "all"
  )
}


ui_tm_g_association <- function(id, ...) {
  ns <- NS(id)
  arguments <- list(...)

  standard_layout(
    output = white_small_well(plot_height_output(id = ns("myplot"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      uiOutput(ns("dataname")),
      data_extract_input(
        id = ns("ref"),
        label = "Reference variable",
        data_extract_spec = arguments$ref
      ),
      data_extract_input(
        id = ns("vars"),
        label = "Associated variables",
        data_extract_spec = arguments$vars
      ),
      checkboxInput(ns("association"),
        "Association with First Variable",
        value = arguments$show_association
      ),
      checkboxInput(ns("show_dist"),
        "Distribution",
        value = FALSE
      ),
      checkboxInput(ns("log_transformation"),
        "Log transformed",
        value = FALSE
      ),
      plot_height_input(id = ns("myplot"), value = arguments$plot_height)
    ),
    forms = if (arguments$with_show_r_code) actionButton(ns("show_rcode"), "Show R Code", width = "100%") else NULL,
    pre_output = arguments$pre_output,
    post_output = arguments$post_output
  )
}


#' @importFrom grid grid.newpage grid.draw
#' @importFrom tern stack_grobs
srv_tm_g_association <- function(input,
                                 output,
                                 session,
                                 datasets,
                                 ref,
                                 vars) {
  dataname <- datasets$datanames()
  init_chunks()


  callModule(
    plot_with_height,
    id = "myplot",
    plot_height = reactive(input$myplot),
    plot_id = session$ns("plot")
  )

  ref_data <- callModule(
    data_extract_module,
    id = "ref",
    datasets = datasets,
    data_extract_spec = ref
  )
  vars_data <- callModule(
    data_extract_module,
    id = "vars",
    datasets = datasets,
    data_extract_spec = vars
  )

  output$plot <- renderPlot({

    ref_name <- get_dataset_prefixed_col_names(ref_data())
    vars_names <- get_dataset_prefixed_col_names(vars_data())
    anl <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    # not working currently because it relies on cols and not on already preprocessed data
    association <- input$association
    show_dist <- input$show_dist
    log_transformation <- input$log_transformation

    anl_f <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, anl_f)

    validate(
      need(nrow(anl_f) > 3, "need at least three rows"),
      need(length(ref) > 0, "need at least one variable selected"),
      need(!(ref %in% vars), "associated variables and reference variable cannot overlap"),
      need(ref %in% names(anl_f), paste("reference variable not found in ", dataname)),
      need(all(vars %in% names(anl_f)), paste("not all selected variables are in ", dataname))
    )

    ref_class <- class(anl_f[[ref]])

    if (ref_class == "numeric" && log_transformation) {
      ref <- call("log", as.name(ref))
    }

    ref_cl <- call(
      "+",
      bivariate_plot_call(anl_name, ref, NULL, ref_class, "NULL", freq = !show_dist),
      quote(theme(panel.background = element_rect(fill = "papayawhip", colour = "papayawhip")))
    )

    ref_class_cov <- if (association) {
      ref_class
    } else {
      "NULL"
    }

    var_cls <- lapply(vars, function(var_i) {
      class_i <- class(anl_f[[var_i]])
      if (class_i == "numeric" && log_transformation) {
        var_i <- call("log", as.name(var_i))
      }

      bivariate_plot_call(anl_name, var_i, ref, class_i, ref_class_cov, freq = !show_dist)
    })


    cl1 <- call("<-", quote(plots), do.call("call", c(list("list", ref_cl), var_cls), quote = TRUE))

    cl2 <- bquote(p <- tern::stack_grobs(grobs = lapply(plots, ggplotGrob)))

    cl <- bquote({.(cl1); .(cl2); grid::grid.newpage(); grid::grid.draw(p)})

    chunks_reset()

    chunks_push(expression = cl, id = "plotCall")

    p <- chunks_eval()

    chunks_validate_is_ok()

    p
  })

  observeEvent(input$show_rcode, {
    show_rcode_modal(
      title = "R Code for the Association Plot",
      rcode = get_rcode(
        datasets = datasets,
        merge_expression = "",
        title = "Association Plot"
      )
    )
  })
}
