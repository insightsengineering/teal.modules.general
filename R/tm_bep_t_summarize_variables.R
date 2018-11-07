tm_bep_t_summarize_variables <- function(label,
                                         dataname,
                                         arm_var,
                                         arm_var_choices = arm_var,
                                         summarize_vars,
                                         summarize_vars_choices = summarize_vars,
                                         bep_var,
                                         bep_var_choices = bep_var,
                                         pop_id_var,
                                         pop_id_var_choices = pop_id_var,
                                         pre_output = NULL,
                                         post_output = NULL,
                                         code_data_processing = NULL) {

  print("tm_bep_t_summarize_variables 1")

  args <- as.list(environment())

  module(label = label,
         server = srv_bep_t_summarize_variables,
         ui = ui_bep_t_summarize_variables,
         ui_args = args,
         server_args = list(dataname = dataname,
                            code_data_processing = code_data_processing),
         filters = dataname)
}

ui_bep_t_summarize_variables <- function(id, ...) {

  print("ui_bep_t_summarize_variables 1")

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = whiteSmallWell(uiOutput(ns("table"))),
    encoding = div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("bep_var"),
                          "Biomarker Population",
                          a$bep_var_choices,
                          a$bep_var,
                          multiple = FALSE),
      radioButtons(ns("all_non_bep_radio_buttons"),
                   NULL,c("ALL","NON-BEP")),
      # checkboxInput(ns("all_non_bep_checkbox"),
      #               "ALL or NON-BEP",
      #               value = FALSE),
      optionalSelectInput(ns("arm_var"),
                          "Arm Variable",
                          a$arm_var_choices,
                          a$arm_var,
                          multiple = FALSE),
      # TODO: Check if this is indeed not needed.
      # optionalSelectInput(ns("pop_id_var"),
      #                     "Population Identifier",
      #                     a$pop_id_var_choices,
      #                     a$pop_id_var,
      #                     multiple = TRUE),
      optionalSelectInput(ns("summarize_vars"),
                          "Summarize Variables",
                          a$summarize_vars_choices,
                          a$summarize_vars, multiple = TRUE)
      # checkboxInput(ns("test_p_value"),
      #               "Test for p-value",
      #               value = FALSE),
      # checkboxInput(ns("combined_treatment_arm"),
      #               "Combined treatment arm",
      #               value = FALSE)
    ),
    # Vincent: Commented out the "Show R Code" button for now.
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_bep_t_summarize_variables <- function(input,
                                          output,
                                          session,
                                          datasets,
                                          dataname,
                                          code_data_processing) {

  print("srv_bep_t_summarize_variables 1")

  chunks <- list(analysis = "# Not Calculated")

  output$table <- renderUI({

    print("srv_bep_t_summarize_variables 2")

    ANL_f <- datasets$get_data(dataname, reactive = TRUE, filtered = TRUE)

    ANLLLL <- ANL_f

    as.global(ANLLLL)

    print("srv_bep_t_summarize_variables 3")

    arm_var <- input$arm_var
    if (is.null(arm_var) || arm_var == "")
      arm_var <- NULL
    summarize_vars <- input$summarize_vars
    bep_var <- input$bep_var
    reference_population_all <- FALSE
    reference_population_nonbep <- FALSE
    if (input$all_non_bep_radio_buttons == "ALL")
      reference_population_all <- TRUE
    if (input$all_non_bep_radio_buttons == "NON-BEP")
      reference_population_nonbep <- TRUE

    #test_p_value <- input$test_p_value
    #combined_treatment_arm <- input$combined_treatment_arm

    print("srv_bep_t_summarize_variables 4")

    chunks$analysis <<- "# Not Calculated"

    #validate_has_data(ANL_f, min_nrow = 3)
    validate(need(!is.null(bep_var), "Please provide a BEP variable"))
    validate(need(!is.null(summarize_vars), "Please select a summarize variable"))
    validate(need(all(summarize_vars %in% names(ANL_f)), "Not all variables available"))
    #validate(need(ANL_f[[arm_var]], "Arm variable does not exist"))
    #validate(need(!("" %in% ANL_f[[arm_var]]), "Arm values cannot contain empty strings"))

    print("srv_bep_t_summarize_variables 5")

    data_name <- paste0(dataname, "_FILTERED")

    print("srv_bep_t_summarize_variables 6")

    # Pre-processing here:
    ASL_mod = ANL_f[,c(bep_var,summarize_vars)]
    ASL_mod[bep_var] = as.integer(as.logical(ASL_mod[[bep_var]]))
    ASL_mod = as.data.frame(ASL_mod)

    print("srv_bep_t_summarize_variables 7")

    if (!is.null(arm_var)) {

      print("srv_bep_t_summarize_variables 7 A 1")

      tbl <- NULL
      if (reference_population_all) {

        print("srv_bep_t_summarize_variables 7 A 1: reference_population_all")

        ANL_f <- get_subset_in_long_format(ANL_f,
                                           arm_var,
                                           bep_var,
                                           summarize_vars)

        print("srv_bep_t_summarize_variables 7 A 2")

        print("srv_bep_t_summarize_variables 7 A 3")

        # TODO: For now we simply filter out records with NAs.
        # However NAs should be reported and displayed in the UI to not obfuscate the
        # data.

        if (any(is.na(ANL_f$col_by))) {
          # Only remove those rows where ANL_f$col_by has NAs.
          ANL_f <- na.omit(as.data.table(ANL_f), cols="col_by")
          ANL_f <- as.data.frame(ANL_f)
        }

        as.global(ANL_f)

        assign(data_name, ANL_f)

        print("srv_bep_t_summarize_variables 7 A 4")

        chunks$analysis <<- call(
          "t_summary",
          x = bquote(.(as.name(data_name))[, .(summarize_vars), drop=FALSE]),
          #x = ANL_f,
          #col_by = bquote(as.factor(.(as.name(data_name))[[.(arm_var)]])),
          col_by = ANL_f$col_by,
          total = "Total population",
          #total = "ALL-2",
          useNA = "ifany"
        )
        tbl <- try(eval(chunks$analysis))

        print("srv_bep_t_summarize_variables 7 A 5")

        if (is(tbl, "try-error")) {
          print("tbl error!!!!!!!!!")
          validate(need(FALSE, paste0("could not calculate the table:\n\n", tbl)))
        }

        print("srv_bep_t_summarize_variables 7 A 6")


      }
      if (reference_population_nonbep) {

        print("srv_bep_t_summarize_variables 7 A 6: reference_population_nonbep")

        #bep_var <- "ITTFL"

        ASL_mod = ANL_f[,c(bep_var,summarize_vars,"ACTARMCD")]

        #ASL_mod[bep_var][ASL_mod[bep_var] == "Y"] <- TRUE
        #ASL_mod[bep_var][ASL_mod[bep_var] == "N"] <- TRUE

        ASL_mod[bep_var] = as.integer(as.logical(ASL_mod[[bep_var]]))
        ASL_mod = as.data.frame(ASL_mod)

        ANLffff <- ASL_mod

        as.global(ANLffff)

        ASL_mod <- na.omit(ASL_mod)

        tbl <- rBiomarker::SummaryVars(ASL_mod,
                                       var = summarize_vars,
                                       trt = "ACTARMCD",
                                       bep = bep_var,
                                       bep.name = bep_var,
                                       #itt.name = "ALL",
                                       bep.vs.nonbep = TRUE,
                                       perform.test = FALSE)

      }
      as_html(tbl)
    } else {

      print("srv_bep_t_summarize_variables 7 B 1")

      as.global(ASL_mod)

      tbl <- NULL
      if (reference_population_nonbep) {
        print("srv_bep_t_summarize_variables 7 B 1: reference_population_nonbep")
        test_p_value <- TRUE
        tbl <- rBiomarker::SummaryVars(ASL_mod,
                                       var = summarize_vars,
                                       bep = bep_var,
                                       bep.name = bep_var,
                                       itt.name = "Safety population",
                                       perform.test = test_p_value)

      }

      if (reference_population_all) {
        print("srv_bep_t_summarize_variables 7 B 1: reference_population_all")
        #test_p_value <- TRUE
        tbl <- rBiomarker::SummaryVars(ASL_mod,
                                       var = summarize_vars,
                                       #trt = "ACTARMCD",
                                       bep = bep_var,
                                       bep.name = bep_var,
                                       itt.name = "ALL",
                                       bep.vs.nonbep = FALSE,
                                       perform.test = FALSE)

      }

      as_html(tbl)
    }
  })



  observeEvent(input$show_rcode, {

    header <- get_rcode_header2(title = "Summarize Variables",
                                datanames = dataname,
                                datasets = datasets,
                                code_data_processing)

    str_rcode <- paste(c(
      "",
      header,
      "",
      remove_enclosing_curly_braces(deparse(chunks$analysis,width.cutoff=60))
    ), collapse = "\n")

    # TODO: Dummy result for now, implement properly!
    str_rcode = "Only a dummy place holder for now!"

    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current Demographic Table",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })

}
