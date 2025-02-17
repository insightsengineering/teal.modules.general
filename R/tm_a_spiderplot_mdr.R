tm_a_spiderplot_mdr <- function(label = "Spiderplot",
                            time_var,
                            subject_var,
                            value_var,
                            event_var,
                            resp_cols = c(
                              "subject", "raise_query", "visit_name", "rspdn", "rspd", "rspd_study_day",
                              "orsp", "bma", "bmb", "comnts"
                            ),
                            spep_cols = c(
                              "subject", "visit_name", "visit_date", "form_name", "source_system_url_link", "rspdn", "rspd", 
                              "rspd_study_day", "orsp", "bma", "bmb", "comnts", "asmntdn", "blq", "coldr", "cold_study_day", 
                              "coltm", "coltmu", "lrspep1", "mprte_raw", "mprtec"
                            ),
                            sflc_cols = c(
                              "subject", "visit_name", "visit_date", "form_name", "source_system_url_link", "rspdn", "rspd",
                              "rspd_study_day", "orsp", "bma", "bmb", "comnts", "asmntdn", "blq", "coldr", "cold_study_day",
                              "coltm", "coltmu", "lchfrc", "lchfr_raw", "klchf_raw", "llchf_raw",
                              "klchp_raw", "mprte_raw", "mprtec"
                            ),
                            plot_height = 600) {
  module(
    label = label,
    ui = ui_a_spiderplot_mdr,
    server = srv_a_spiderplot_mdr,
    ui_args = list(height = plot_height),
    server_args = list(
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var,
      resp_cols = resp_cols,
      spep_cols = spep_cols,
      sflc_cols = sflc_cols
    ),
    datanames = "all",
  )
}


ui_a_spiderplot_mdr <- function(id, height) {
  ns <- NS(id)
  tagList(
    ui_p_spiderplot(ns("spiderplot"), height = height),
    div(
      style = "display: flex",
      div(
        style = "width: 50%",
        div(
          class = "simple-card",
          h4("Disease Assessment - SFLC"),
          ui_t_reactable(ns("sflc_listing"))
        ),
        div(
          class = "simple-card",
          h4("Disease Assessment - SPEP"),
          ui_t_reactable(ns("spep_listing"))
        )
      ),
      div(
        class = "simple-card",
        style = "width: 50%",
        h4("Multiple Myeloma Response"),
        ui_t_reactable(ns("all_resp"))
      )
    )
  )
}

srv_a_spiderplot_mdr <- function(id,
                                 data,
                                 time_var,
                                 subject_var,
                                 value_var,
                                 event_var,
                                 resp_cols,
                                 spep_cols,
                                 sflc_cols,
                                 filter_panel_api,
                                 plot_height = 600) {
  moduleServer(id, function(input, output, session) {
    dataname <- "spiderplot_ds"
    recent_resp_selected_q <- srv_p_spiderplot(
      "spiderplot", 
      data = data,
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var,
      table_cols = resp_cols,
      filter_panel_api = filter_panel_api,
      plot_height = plot_height
    )
    
    all_resp_q <- reactive({
      req(nrow(recent_resp_selected_q()[["recent_resp_selected"]]))
      within(
        recent_resp_selected_q(),
        dataname = str2lang(dataname),
        subject_var = str2lang(subject_var),
        subject_var_char = subject_var,
        event_var = str2lang(event_var),
        resp_cols = resp_cols,
        expr = {
          all_resp <- dplyr::filter(
            dataname, 
            event_var == "response_assessment",
            subject_var %in% unique(recent_resp_selected[[subject_var_char]])
          ) |>
            select(all_of(resp_cols))
        }
      )
    })
    
    spep_q <- reactive({
      req(nrow(recent_resp_selected_q()[["recent_resp_selected"]]))
      within(
        recent_resp_selected_q(),
        dataname = str2lang(dataname),
        subject_var = str2lang(subject_var),
        subject_var_char = subject_var,
        event_var = str2lang(event_var),
        spep_cols = spep_cols,
        expr = {
          spep <- dplyr::filter(
            dataname,
            event_var == "Serum M-protein",
            subject_var %in% unique(recent_resp_selected[[subject_var_char]])
          ) |>
            select(all_of(spep_cols))
        }
      )
    })
    
    sflc_q <- reactive({
      req(nrow(recent_resp_selected_q()[["recent_resp_selected"]]))
      within(
        recent_resp_selected_q(),
        dataname = str2lang(dataname),
        subject_var = str2lang(subject_var),
        event_var = str2lang(event_var),
        subject_var_char = subject_var,
        sflc_cols = sflc_cols,
        expr = {
          sflc <- dplyr::filter(
            dataname,
            event_var %in% c(
              "Kappa free light chain quantity",
              "Lambda free light chain quantity",
              "Kappa-Lambda free light chain ratio"
            ),
            subject_var %in% unique(recent_resp_selected[[subject_var_char]])
          ) |>
            select(all_of(sflc_cols))
        }
      )
    })
    
    #todo: show all_resp only if recent_resp is selected  
    all_resp_selected_q <- srv_t_reactable("all_resp", data = all_resp_q, dataname = "all_resp") 
    spep_selected_d <- srv_t_reactable("spep_listing", data = spep_q, dataname = "spep")
    sflc_selected_d <- srv_t_reactable("sflc_listing", data = sflc_q, dataname = "sflc")
    
    all_q <- reactive({
      # all_resp_selected_q could be nothing and `c` won't work because the result is unavailable before clicking subjects in the table
      c(recent_resp_selected_q(), all_resp_selected_q())        
    })
    
    observeEvent(all_q(), {
      "do nothing"
    })
    
    
  })
}


.with_tooltips <- function(...) {
  args <- list(...)
  lapply(args, function(col) {
    col$header <- tags$span(col$name, title = col$name)
    return(col)
  })
}
