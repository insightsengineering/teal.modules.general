create_km_plot <- function(formula_km,
                           formula_coxph,
                           info_coxph,
                           bep,
                           arm_var,
                           title)
{
  exp <- bquote({
    if (!is.null(arm_var)) {

      fit_km <- survfit(formula_km, data = ANL, subset = bep, conf.type = "plain")
      fit_coxph <- coxph(formula_coxph, data = ANL, subset = bep, ties = "exact")
      tbl_km <- t_km(fit_km)
      tbl_coxph <- t_coxph(fit_coxph)
      text_coxph <- paste0(info_coxph, "\n", toString(tbl_coxph, gap = 1))
      coxph_grob <- textGrob(label = text_coxph,
                             x= unit(1, "lines"),
                             y = unit(1, "lines"),
                             just = c("left", "bottom"),
                             gp = gpar(fontfamily = 'mono', fontsize = 8, fontface = "bold"),
                             vp = vpPath("plotArea", "topCurve"))
      grid.newpage()
      p <- g_km(fit_km = fit_km, col = NULL, title = title, draw = FALSE)
      p <-  addTable(p, tbl_km,
                     x = unit(1, "npc") - stringWidth(toString(tbl_km, gap = 1)) - unit(1, "lines"),
                     y = unit(1, "npc") -  unit(1, "lines"),
                     just = c("left", "top"))
      p <- addGrob(p, coxph_grob)
      grid.draw(p)
    } else {
      fit_km <- survfit(formula_km, data = ANL, conf.type = "plain")
      tbl_km <- t_km(fit_km)
      grid.newpage()
      p <- g_km(fit_km = fit_km, col = NULL, title = title, draw = FALSE)
      p <-  addTable(p, tbl_km,
                     x = unit(1, "npc") - stringWidth(toString(tbl_km, gap = 1)) - unit(1, "lines"),
                     y = unit(1, "npc") -  unit(1, "lines"),
                     just = c("left", "top"))
      grid.draw(p)
    }
  })
  exp
}


create_km_plot_backup <- function(formula_km,
                                  formula_coxph,
                                  info_coxph,
                                  bep,
                                  arm_var,
                                  title)
{
  exp <- bquote({
    if (!is.null(arm_var))
    {
      fit_km <- survfit(formula_km, data = ANL, subset = bep, conf.type = "plain")
      fit_coxph <- coxph(formula_coxph, data = ANL, subset = bep, ties = "exact")

      tbl_km <- t_km(fit_km)
      tbl_coxph <- t_coxph(fit_coxph)
      text_coxph <- paste0(info_coxph,
                           "\n",
                           toString(tbl_coxph, gap = 1))
      coxph_grob <- textGrob(label = text_coxph,
                             x= unit(1, "lines"),
                             y = unit(1, "lines"),
                             just = c("left", "bottom"),
                             gp = gpar(fontfamily = 'mono', fontsize = 8, fontface = "bold"),
                             vp = vpPath("plotArea", "topCurve"))
      grid.newpage()
      p <- g_km(fit_km = fit_km, col = NULL, title = title, draw = FALSE)
      p <-  addTable(p, tbl_km,
                     x = unit(1, "npc") - stringWidth(toString(tbl_km, gap = 1)) - unit(1, "lines"),
                     y = unit(1, "npc") -  unit(1, "lines"),
                     just = c("left", "top"))
      p <- addGrob(p, coxph_grob)
      grid.draw(p)
    }
    else
    {
      fit_km <- survfit(formula_km, data = ANL, conf.type = "plain")
      tbl_km <- t_km(fit_km)
      grid.newpage()
      p <- g_km(fit_km = fit_km, col = NULL, title = title, draw = FALSE)
      p <-  addTable(p, tbl_km,
                     x = unit(1, "npc") - stringWidth(toString(tbl_km, gap = 1)) - unit(1, "lines"),
                     y = unit(1, "npc") -  unit(1, "lines"),
                     just = c("left", "top"))
      grid.draw(p)
    }
  })
  exp
}

create_facet_km_plot <- function(formula_km,
                                 formula_coxph,
                                 arm_var)
{
  exp <- bquote({
    grid.newpage()
    pl <-  Map(function(x, label){
      x[[.(arm_var)]] <- factor(x[[.(arm_var)]])
      fit_km <- survfit(formula_km, data = x, conf.type = "plain")
      fit_coxph <- coxph(formula_coxph, data = x, ties = "exact")
      tbl_km <- t_km(fit_km)
      tbl_coxph <- t_coxph(fit_coxph)
      text_coxph <- paste0(info_coxph, "\n", toString(tbl_coxph, gap = 1))
      coxph_grob <- textGrob(label = text_coxph, x= unit(1, "lines"), y = unit(1, "lines"),
                             just = c("left", "bottom"),
                             gp = gpar(fontfamily = 'mono', fontsize = 8, fontface = "bold"),
                             vp = vpPath("plotArea", "topCurve"))
      if (nrow(x) < 5){
        textGrob(paste0("Less than 5 patients in ", label, "group"))
      } else {
        p <- g_km(fit_km = fit_km, col = NULL, title = paste0("Kaplan - Meier Plot for: ", label),
                  xticks = xticks, draw = FALSE)
        p <-  addTable(p, tbl_km,
                       x = unit(1, "npc") - stringWidth(toString(tbl_km, gap = 1)) - unit(1, "lines"),
                       y = unit(1, "npc") -  unit(1, "lines"),
                       just = c("left", "top"))
        p <- addGrob(p, coxph_grob)
      }
    }, dfs, levels(lab))

    grid.draw(gridExtra::arrangeGrob(grobs = pl, ncol = 1) )
  })
  exp
}


tm_bep_km_tern <- function(label,
                           dataname,
                           arm_var,
                           arm_var_choices = arm_var,
                           tte_var,
                           tte_var_choices = tte_var,
                           bep_var,
                           bep_var_choices = bep_var,
                           bmrk_var,
                           bmrk_var_choices = bmrk_var,
                           color_coding_var,
                           color_coding_var_choices = color_coding_var,
                           plot_height = c(1200, 400, 5000),
                           pre_output = NULL,
                           post_output = NULL,
                           code_data_processing = NULL) {

  print("tm_bep_km_tern 1")

  args <- as.list(environment())

  print("tm_bep_km_tern 2")

  module(label = label,
         server = srv_tm_bep_km_tern,
         ui = ui_tm_bep_km_tern,
         ui_args = args,
         server_args = list(dataname = dataname,
                            code_data_processing = code_data_processing),
         filters = dataname)
}

ui_tm_bep_km_tern <- function(id, ...) {

  print("ui_tm_bep_km_tern 1")

  ns <- NS(id)
  a <- list(...)

  standard_layout(
    output = uiOutput(ns("plot_ui")),
    encoding =  div(
      tags$label("Encodings", class="text-primary"),
      helpText("Analysis data:", tags$code(a$dataname)),
      optionalSelectInput(ns("arm_var"),
                          "Arm Variable",
                          a$arm_var_choices,
                          a$arm_var,
                          multiple = FALSE),
      optionalSelectInput(ns("tte_var"),
                          "Time to Event (Endpoint)",
                          a$tte_var_choices,
                          a$tte_var,
                          multiple = FALSE),
      optionalSelectInput(ns("bep_var"),
                          "Biomarker Population",
                          a$bep_var_choices,
                          a$bep_var,
                          multiple = FALSE),
      optionalSelectInput(ns("bmrk_var"),
                          "Biomarker Variable",
                          a$bmrk_var_choices,
                          a$bmrk_var,
                          multiple = FALSE),
      conditionalPanel(
        condition = ns("output.is_numeric"),
        uiOutput(ns("partition_radio_buttons")),
        uiOutput(ns("cutoff_slider")),
        uiOutput(ns("tricatomization_slider"))),
      # uiOutput(ns("cutoff_tick_box")),
      # uiOutput(ns("cutoff_slider")),
      # uiOutput(ns("tricatomization_tick_box")),
      # uiOutput(ns("tricatomization_slider"))
      #checkboxInput(ns("cutoff_tick_box"), "Cutoff", value = FALSE)
      #checkboxInput(ns("cutoff_tick_box"), "Cutoff", value = FALSE),
      # conditionalPanel(
      #  condition = paste0("input['", ns("cutoff_tick_box"), "'] && !input['", ns("tricatomization_tick_box"), "']"),
      #  uiOutput(ns("cutoff_slider"))),
      # # checkboxInput(ns("tricatomization_tick_box"), "Tricatomization", value = FALSE),
      # conditionalPanel(
      #  condition = paste0("input['", ns("tricatomization_tick_box"), "'] && !input['", ns("cutoff_tick_box"), "']"),
      #  uiOutput(ns("tricatomization_slider"))),
      #),
      # optionalSelectInput(ns("color_coding_var"),
      #                     "Color coding",
      #                     a$color_coding_var_choices,
      #                     a$color_coding_var,
      #                     multiple = FALSE),
      optionalSliderInputValMinMax(ns("plot_height"),
                                   "plot height",
                                   a$plot_height,
                                   ticks = FALSE)
    ),
    #forms = actionButton(ns("show_rcode"), "Show R Code", width = "100%"),
    pre_output = a$pre_output,
    post_output = a$post_output
  )
}

srv_tm_bep_km_tern <- function(input,
                               output,
                               session,
                               datasets,
                               dataname,
                               code_data_processing) {

  print("srv_tm_bep_km_tern 1")

  global_tricatomization <- FALSE
  global_cov_var <- reactiveValues(is_numeric = FALSE)

  #chunks <- list(analysis = "# Not Calculated")

  output$plot_ui <- renderUI({
    plot_height <- input$plot_height
    validate(need(plot_height, "need valid plot height"))
    ns <- session$ns
    plotOutput(ns("plot"), height=plot_height)
  })

  observeEvent(input$partition_radio_buttons, {
    if (input$partition_radio_buttons == "Cutoff") {
      global_tricatomization <<- FALSE
      output$cutoff_slider <- renderUI({
        sliderInput(session$ns("cutoff_slider"),
                    "Cutoff",
                    min = global_cov_var$min_val,
                    max = global_cov_var$max_val,
                    value = global_cov_var$mid_val,
                    step = 0.1)
      })

    } else if (input$partition_radio_buttons == "Tricatomization") {
      global_tricatomization <<- TRUE
      output$cutoff_slider <- renderUI({
        sliderInput(session$ns("tricatomization_slider"),
                    "Tricatomization",
                    min = global_cov_var$min_val,
                    max = global_cov_var$max_val,
                    value = c(global_cov_var$mid_val,(global_cov_var$max_val-0.1)),
                    step = 0.1)
      })

    }
  }, ignoreInit = TRUE)

  output$is_numeric <- reactive ({
    if (global_cov_var$is_numeric && !global_tricatomization) {
      output$partition_radio_buttons <- renderUI({
        options <- c("Cutoff", "Tricatomization")
        radioButtons(session$ns("partition_radio_buttons"), NULL, options)
      })
      return(TRUE)
    } else {
      # TODO: This should not be needed!
      #output$partition_radio_buttons <- renderUI({NULL})
      return(FALSE)
    }
  })

  outputOptions(output, "is_numeric", suspendWhenHidden = FALSE)

  cutoff_slider_flag <- FALSE
  tricatomization_flag <- FALSE

  print("srv_tm_bep_km_tern 5")

  chunks <- list(
    vars = "# No Calculated",
    data = "# No Calculated",
    facet = "# No Calculated",
    formula_km = "# No Calculated",
    formula_coxph = "# No Calculated",
    info_coxph = "# No Calculated",
    t_kmplot = "# No Calculated"
  )

  output$plot <- renderPlot({

    ANL_FILTERED <- datasets$get_data(dataname, filtered = TRUE, reactive = TRUE)
    ASL_FILTERED <- datasets$get_data("ASL", reactive = TRUE, filtered = TRUE)

    arm_var = input$arm_var
    if (is.null(arm_var) || arm_var == "")
      arm_var <- NULL
    tte_var = input$tte_var
    if (is.null(tte_var) || tte_var == "")
      tte_var <- NULL
    bep_var = input$bep_var
    if (is.null(bep_var) || bep_var == "")
      bep_var <- NULL
    bmrk_var = input$bmrk_var
    if (is.null(bmrk_var) || bmrk_var == "")
      bmrk_var <- NULL

    cutoff_val = input$cutoff_slider
    if (is.null(cutoff_val) || cutoff_val == "")
      cutoff_val <- NULL

    tricatomization_val = input$tricatomization_slider
    if (is.null(tricatomization_val) || tricatomization_val == "")
      tricatomization_val <- NULL

    if ((is.null(bep_var) && !is.null(bmrk_var)) || (!is.null(bep_var) && is.null(bmrk_var)))
    {
      validate(need(!is.null(bep_var),"Please provide Biomarker Population."))
      validate(need(!is.null(bmrk_var),"Please provide Biomarker Variable."))
    }

    censoring_var <- "CNSR"
    time_to_event_var <- "AVAL"

    cutoff <- NULL
    if (!global_tricatomization) {
      cutoff <- cutoff_val
    } else {
      cutoff <- tricatomization_val
    }

    anl_name <- paste0(dataname, "_FILTERED")
    assign(anl_name, ANL_FILTERED)

    # Get relevant ASL variables.
    asl_vars <- unique(c("USUBJID", "STUDYID", arm_var, bep_var, bmrk_var))

    # Make sure empty inputs are filtered out.
    asl_vars <- asl_vars[asl_vars != ""]

    # Get relevant ATE variables.
    anl_vars <- c("USUBJID","STUDYID","PARAMCD","CNSR","AVAL")

    # Subset ATE based on user input.
    ANL_subset <- subset(ANL_FILTERED, ANL_FILTERED$PARAMCD == tte_var)

    as.global(ANL_subset)

    # TODO: Hack for now, need to investigate where the duplicates come from.
    ANL_subset <- ANL_subset[!ANL_subset$USUBJID=='GO29294-273579-6266',]

    if (any(duplicated(ANL_subset[,c("USUBJID", "STUDYID")])))
      stop("only one row per patient expected")

    asl_p <- ASL_FILTERED[, asl_vars, drop = FALSE]
    anl_p <- ANL_subset[, anl_vars, drop = FALSE]
    merged_data <- merge(x = asl_p,
                         y = anl_p,
                         all.x = FALSE,
                         all.y = FALSE,
                         by = c("USUBJID","STUDYID"))


    global_cov_var$is_numeric <- FALSE
    if (!is.null(bep_var) && !is.null(bmrk_var)) {
      bmrk_var_values <- merged_data[[bmrk_var]]
      if (is.numeric(bmrk_var_values)) {
        global_cov_var$is_numeric <- TRUE
        global_cov_var$min_val = as.numeric(min(bmrk_var_values, na.rm=TRUE))
        global_cov_var$max_val = as.numeric(max(bmrk_var_values, na.rm=TRUE))
        global_cov_var$mid_val = as.numeric((ceiling(global_cov_var$max_val)-floor(global_cov_var$min_val))/2.0)
      } else {
        global_cov_var$is_numeric <- FALSE
      }
    }

    title <- ""
    if (is.null(arm_var) && is.null(bep_var) && is.null(bmrk_var))
      title <- paste0(tte_var," ITT")
    if (is.null(arm_var) && !is.null(bep_var) && is.null(bmrk_var))
      title <- paste0(tte_var," ", bep_var)
    if (!is.null(arm_var) && is.null(bep_var) && is.null(bmrk_var))
      title <- paste0(tte_var," ITT by treatment")
    if (!is.null(arm_var) && !is.null(bep_var) && is.null(bmrk_var))
      title <- paste0(tte_var," ",bep_var," by treatment")
    if (is.null(arm_var) && !is.null(bep_var) && !is.null(bmrk_var))
      title <- paste0(tte_var," ",bep_var," by ",bmrk_var)
    if (!is.null(arm_var) && !is.null(bep_var) && !is.null(bmrk_var))
      title <- paste0(tte_var," ",bep_var," by treatment, by ",bmrk_var)

    tte <- merged_data[[time_to_event_var]]
    cen <- merged_data[[censoring_var]]

    right <- FALSE
    percentile <- FALSE
    trt.level <- NULL
    var.level <- NULL
    conf.type <- "plain"

    bep <- NULL
    if (!is.null(bep_var))
      bep <- GetBEP(merged_data[[bep_var]])

    if (!is.null(arm_var))
      merged_data[[arm_var]] <- GetGrp(merged_data[[arm_var]], trt.level)

    if (!is.null(bmrk_var)) {
      merged_data[[bmrk_var]] <- GetGrp(merged_data[[bmrk_var]],
                                        grp.level = var.level,
                                        cutoff = cutoff,
                                        right = right,
                                        percentile = percentile)
    }

    formula_km <- NULL
    if (is.null(arm_var) && is.null(bmrk_var)) {
      formula_km <- Surv(tte, cen) ~ 1
    } else {
      x <- strata(merged_data[c(arm_var, bmrk_var)], shortlabel = TRUE)
      formula_km <- Surv(tte, cen) ~ x
    }

    formula_coxph <- formula_km
    info_coxph <- "Unstratified Analysis"

    chunks$data <<- bquote({
      ANL <- merged_data
    })

    eval(chunks$data)

    chunks$t_kmplot <<- create_km_plot(formula_km,
                                       formula_coxph,
                                       info_coxph,
                                       bep,
                                       arm_var,
                                       title)
    eval(chunks$t_kmplot)
  })

  observeEvent(input$show_rcode, {

    header <- get_rcode_header2(title = "KM Plot",
                                datanames = dataname,
                                datasets = datasets,
                                code_data_processing)

    str_rcode <- paste(c(
      "",
      header,
      "",
      remove_enclosing_curly_braces(deparse(chunks$analysis, width.cutoff = 60))
    ), collapse = "\n")

    # TODO: Dummy result for now, implement properly!
    str_rcode = "Only a dummy place holder for now!"

    # .log("show R code")
    showModal(modalDialog(
      title = "R Code for the Current KM Plot",
      tags$pre(tags$code(class="R", str_rcode)),
      easyClose = TRUE,
      size = "l"
    ))
  })
}

