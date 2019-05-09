#' Summary Table Comparing Analysis Populations
#'
#' Display a summary table comparing analysis populations as a shiny module
#' @importFrom teal.devel validate_has_data
#' @inheritParams teal::module
#' @inheritParams teal.devel::standard_layout
#' @param label menue item label of the module in the teal app
#' @param dataname analysis data used in teal module, needs to be available in
#'   the list passed to the \code{data} argument of \code{\link[teal]{init}}.
#'   Note that the data is expected to be in vertical form with the
#'   \code{PARAMCD} variable filtering to one observation per patient.
#' @param arm_var \code{\link[teal]{choices_selected}} object with all available choices and preselected option for
#'   variable names that can be used as \code{arm_var}
#' @param summarize_vars \code{\link[teal]{choices_selected}} object with all available choices and preselected option
#'   for variable names that can be used for summary
#' @param bep_var (\code{choices_selected}) containing name of boolean vector for the biomarker evaluable population
#'   (BEP) to compare in the table
#' @param with_show_r_code (\code{logical}) Whether to include "Show R Code" Button
#' @param code_data_processing (\code{character}) Code that was used to pre-process the data
#'
#' @export
#'
#' @examples
#'
#' library(random.cdisc.data)
#' library(dplyr)
#'
#' asl <- radsl(N = 100) %>%
#'   mutate(
#'     BEP = sample(c(TRUE, FALSE), n(), TRUE),
#'     BEP2 = sample(c(TRUE, FALSE), n(), TRUE)
#'   )
#'
#' x <- teal::init(
#'   data = list(ASL = asl),
#'   modules = root_modules(
#'     tm_t_bep_summary(
#'       label = "Demographic",
#'       dataname = "ASL",
#'       arm_var = choices_selected(c("ARM", "ARMCD"), "ARM"),
#'       summarize_vars = choices_selected(names(asl), "AGE"),
#'       bep_var = choices_selected(c("BEP", "BEP2"), "BEP")
#'     )
#'   )
#' )
#'
#' \dontrun{
#' shinyApp(x$ui, x$server)
#' }
tm_t_bep_summary <- function(label,
                             dataname,
                             arm_var,
                             summarize_vars,
                             bep_var,
                             pre_output = NULL,
                             post_output = NULL,
                             with_show_r_code = FALSE,
                             code_data_processing = NULL) {
  stopifnot(is.choices_selected(arm_var))
  stopifnot(is.choices_selected(summarize_vars))
  stopifnot(is.choices_selected(bep_var))

  if (with_show_r_code) {
    stop("currently show R code feature is not implemented in tm_t_bep_summary")
  }

  arm_var <- teal::add_no_selected_choices(arm_var)

  args <- as.list(environment())

  teal::module(
    label = label,
    server = srv_t_bep_summary,
    ui = ui_t_bep_summary,
    ui_args = args,
    server_args = list(
      dataname = dataname,
      code_data_processing = code_data_processing
    ),
    filters = dataname
  )
}

#' @import teal
#' @importFrom teal.devel optionalSelectInput white_small_well standard_layout
ui_t_bep_summary <- function(id, ...) {
  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = teal.devel::white_small_well(uiOutput(ns("table"))),
    encoding = div(
      tags$label("Encodings", class = "text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("bep_var"),
        "Biomarker-Evaluable Population (BEP) Variable",
        a$bep_var$choices,
        a$bep_var$selected,
        multiple = FALSE
      ),
      radioButtons(
        ns("all_non_bep_radio_buttons"),
        "Table Comparison", c("ALL vs BEP", "BEP vs Non-BEP", "ALL")
      ),
      optionalSelectInput(ns("arm_var"),
        "Arm Variable",
        a$arm_var$choices,
        a$arm_var$selected,
        multiple = FALSE
      ),
      optionalSelectInput(ns("summarize_vars"),
        "Summarize Variables",
        a$summarize_vars$choices,
        a$summarize_vars$selected,
        multiple = TRUE
      )
    ),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

#' @importFrom rtables as_html
#' @importFrom tern t_summary
#' @importFrom teal.devel validate_has_data
srv_t_bep_summary <- function(input,
                              output,
                              session,
                              datasets,
                              dataname,
                              code_data_processing) {
  output$table <- renderUI({
    anl_f <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    bep_var <- input$bep_var
    arm_var <- input$arm_var
    summarize_vars <- input$summarize_vars
    summary_type <- input$all_non_bep_radio_buttons

    # leftover code: as.global(anl_f, bep_var, arm_var, summarize_vars, is_bep_vs_non_bep)

    teal.devel::validate_has_data(anl_f, min_nrow = 3)

    validate(need(!is.null(bep_var), "Please provide a BEP variable"))
    validate(need(is.logical(anl_f[[bep_var]]), "BEP variable does not exist"))
    validate(need(!any(is.na(anl_f[[bep_var]])), "Please recode NA levels in BEP variable"))
    validate(need(!("" %in% anl_f[[bep_var]]), "BEP values cannot contain empty strings"))

    validate(need(!is.null(summarize_vars), "Please select a summarize variable"))
    validate(need(all(summarize_vars %in% names(anl_f)), "Not all variables available"))

    arm_var <- teal::no_selected_as_NULL(arm_var)
    if (!is.null(arm_var)) {
      validate(need(anl_f[[arm_var]], "Arm variable does not exist"))
    }

    bep <- anl_f[[bep_var]]
    anl_select <- anl_f[, summarize_vars, drop = FALSE]
    arm <- if (is.null(arm_var)) {
      NULL
    } else {
      anl_f[[arm_var]]
    }


    all_bep <- all(bep)

    x <- if (summary_type == "ALL" || all_bep || all(!bep)) {
      # normal t_summary without stacking

      cb <- if (is.null(arm)) {
        factor(rep("All Patients", nrow(anl_select)))
      } else if (summary_type != "ALL" && all_bep) {
        tmp <- arm
        levels(tmp) <- paste(levels(tmp), "BEP", sep = "\n")
        tmp
      } else {
        arm
      }

      list(
        data = anl_select,
        col_by = cb,
        total = if (is.null(arm_var)) NULL else "All Patients",
        pop = if (summary_type == "ALL") {
          "ALL"
        } else if (all_bep) {
          "bep"
        } else {
          "non-bep"
        }
      ) # NEXT if() population in tablecolumns does not overlap --  no stacking required
    } else if (summary_type == "BEP vs Non-BEP") {
      cb <- factor(ifelse(bep, "BEP", "Non-BEP"), levels = c("Non-BEP", "BEP"))

      list(
        data = anl_select,
        col_by = if (is.null(arm)) cb else interaction(arm, cb, sep = "\n", lex.order = TRUE),
        total = "All Patients",
        pop = "mixed"
      )
    } else {
      n <- nrow(anl_select) # population in table columns does overlap --  stacking required
      n_bep <- sum(bep)

      anl_stacked <- rbind(
        anl_select, # all x bep
        # all x bep
        anl_select[bep, , drop = FALSE], # nolint
        anl_select # all patients
      )

      # new
      cb_all_bep <- factor(c(rep("ALL", n), rep("BEP", n_bep)), levels = c("ALL", "BEP"))


      cb_all_bep_arm <- if (is.null(arm)) {
        cb_all_bep
      } else {
        arm2 <- unlist(list(arm, arm[bep]), use.names = FALSE) # concatenate factors
        interaction(arm2, cb_all_bep, sep = "\n", lex.order = TRUE)
      }


      list(
        data = anl_stacked,
        col_by = droplevels(unlist(list(cb_all_bep_arm, factor(rep("All Patients", n))), use.names = FALSE)),
        total = NULL,
        pop = "mixed"
      )
    }


    if (!is.factor(x$col_by)) x$col_by <- as.factor(x$col_by)


    n <- table(x$col_by)
    if (any(n == 0)) {

      # this is a special case that will be removed as soon t_summary allows for 0-count columns
      tagList(
        tags$p(
          "Note that the columns ", tags$b(paste(names(n)[n == 0], collapse = ", ")),
          "were removed as they have 0 count"
        ),
        rtables::as_html(tern::t_summary(x$data, droplevels(x$col_by), total = x$total))
      )
    } else {
      tbl <- try(t_summary(x$data, x$col_by, total = x$total), silent = TRUE)

      if (is(tbl, "try-error")) {
        tags$p(tbl)
      } else {
        tbl_html <- rtables::as_html(tbl)

        if (x$pop == "bep") {
          tagList(
            tags$h1("Biomarker Only Population"),
            tbl_html
          )
        } else if (x$pop == "non-bep") {
          tagList(
            tags$h1("Only Non-Biomaker Population left"),
            tbl_html
          )
        } else {
          tbl_html
        }
      }
    }
  })
}
