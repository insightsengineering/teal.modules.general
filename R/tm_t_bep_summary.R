
#' Summary Table Comparing Analysis Populations
#'
#' Display a summary table comparing analysis populations as a shiny module
#'
#' @inheritParams teal.tern::tm_t_summarize_variables
#' @param bep_var character string containing name of boolean vector for the biomarker evaluable population (BEP)
#'   to compare in the table
#' @param bep_var_choices  character vector with names of additional variables that can
#'   be selected as thebiomarker evaluable population (BEP)
#'
#' @export
#'
#'
#' @examples
#'
#' \dontrun{
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' ASL <- radsl(N = 100) %>%
#'  mutate(BEP = sample(c(TRUE, FALSE), n(), TRUE),
#'         BEP2 = sample(c(TRUE, FALSE), n(), TRUE))
#'
#'
#' x <- teal::init(
#'    data = list(ASL = ASL),
#'    modules = root_modules(
#'      tm_t_bep_summary(
#'         label = "Demographic",
#'         dataname = "ASL",
#'         arm_var = "ARM",
#'         arm_var_choices = c("ARM", "ARMCD"),
#'         summarize_vars = "AGE",
#'         summarize_vars_choices = names(ASL),
#'         bep_var = "BEP",
#'         bep_var_choices = c("BEP", "BEP2")
#'      )
#'   )
#' )
#'
#' shinyApp(x$ui, x$server)
#' }
tm_t_bep_summary <- function(label,
                             dataname,
                             arm_var,
                             arm_var_choices = arm_var,
                             summarize_vars,
                             summarize_vars_choices = summarize_vars,
                             bep_var,
                             bep_var_choices = bep_var,
                             pre_output = NULL,
                             post_output = NULL,
                             code_data_processing = NULL) {

  arm_var_choices <- unique(c("-- no arm --", arm_var_choices))

  args <- as.list(environment())

  module(label = label,
         server = srv_t_bep_summary,
         ui = ui_t_bep_summary,
         ui_args = args,
         server_args = list(dataname = dataname,
                            code_data_processing = code_data_processing),
         filters = dataname)
}

ui_t_bep_summary <- function(id, ...) {

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = whiteSmallWell(uiOutput(ns("table"))),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("bep_var"),
                          "Biomarker-Evaluable Population (BEP) Variable",
                          a$bep_var_choices,
                          a$bep_var,
                          multiple = FALSE),
      radioButtons(ns("all_non_bep_radio_buttons"),
                   "Table Comparison",c("ALL vs BEP","BEP vs Non-BEP")),
      optionalSelectInput(ns("arm_var"),
                          "Arm Variable",
                          a$arm_var_choices,
                          a$arm_var,
                          multiple = FALSE),
      optionalSelectInput(ns("summarize_vars"),
                          "Summarize Variables",
                          a$summarize_vars_choices,
                          a$summarize_vars, multiple = TRUE)
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_t_bep_summary <- function(input,
                              output,
                              session,
                              datasets,
                              dataname,
                              code_data_processing) {


  output$table <- renderUI({

    ANL_f <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    bep_var <- input$bep_var
    arm_var <- input$arm_var
    summarize_vars <- input$summarize_vars
    is_bep_vs_non_bep <- input$all_non_bep_radio_buttons == "BEP vs Non-BEP"

    as.global(ANL_f, bep_var, arm_var, summarize_vars, is_bep_vs_non_bep)

    teal.tern:::validate_has_data(ANL_f, min_nrow = 3)

    validate(need(!is.null(bep_var), "Please provide a BEP variable"))
    validate(need(ANL_f[[bep_var]], "BEP variable does not exist"))
    validate(need(!any(is.na(ANL_f[[bep_var]])), "Please recode NA levels in BEP variable"))
    validate(need(!("" %in% ANL_f[[bep_var]]), "BEP values cannot contain empty strings"))

    validate(need(!is.null(summarize_vars), "Please select a summarize variable"))
    validate(need(all(summarize_vars %in% names(ANL_f)), "Not all variables available"))

    if (arm_var!="-- no arm --") validate(need(ANL_f[[arm_var]], "Arm variable does not exist"))
    validate(need(!("" %in% ANL_f[[arm_var]]), "Arm values cannot contain empty strings"))

    bep <- ANL_f[[bep_var]]
    ANL_select <- ANL_f[, summarize_vars, drop = FALSE]
    arm <- if (arm_var == "-- no arm --") NULL else ANL_f[[arm_var]]

    x <- if (is_bep_vs_non_bep) {
      ## population in table columns does not overlap --  no stacking required

      cb <- factor(ifelse(bep, "BEP", "Non-BEP"), levels = c("BEP", "Non-BEP"))

      list(
        data = ANL_select,
        col_by = if (is.null(arm)) cb else interaction(arm, cb, sep = "\n", lex.order = TRUE),
        total = "All Patients"
      )

    } else {
      ## population in table columns does overlap --  stacking required

      ANL_stacked <- rbind(
        ANL_select,
        ANL_select[bep, ]
      )

      cb2 <- factor(c(rep("ALL", nrow(ANL_f)), rep("BEP", sum(bep))), levels = c("ALL", "BEP"))
      if (!is.null(arm)) arm2 <- factor(c(as.character(arm), as.character(arm[bep])), levels(arm))

      list(
        data = ANL_stacked,
        col_by = if (is.null(arm)) cb2 else interaction( arm2, cb2, sep = "\n", lex.order = TRUE),
        total = NULL
      )

    }

    tbl <- try(t_summary(x$data, x$col_by, total = x$total), silent = TRUE)

    if (is(tbl, "try-error")) tags$p(tbl) else as_html(tbl)

  })

}
