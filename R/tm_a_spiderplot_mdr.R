#' @export
tm_a_spiderplot_mdr <- function(label = "Spiderplot",
                                dataname,
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
      dataname = dataname,
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var,
      resp_cols = resp_cols,
      spep_cols = spep_cols,
      sflc_cols = sflc_cols
    ),
    datanames = dataname,
  )
}


ui_a_spiderplot_mdr <- function(id, height) {
  ns <- NS(id)
  tagList(
    
    tagList(
      div(
        style = "display: flex",
        div(
          class = "simple-card",
          style = "width: 50%",
          tagList(
            h4("Most Recent Resp and Best Resp"),
            ui_t_reactable(ns("recent_resp"))
          )
        ),
        div(
          class = "simple-card",
          style = "width: 50%",
          ui_g_spiderplot(ns("spiderplot"), height = height)
        )
      )
    ),
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
                                 dataname,
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
    # todo: plotly_excl_events should be a positive selection or tidyselect 
    #       and exposed as arg
    plotly_excl_events <- c("response_assessment", "latest_response_assessment")
    plotly_data <- reactive({
      req(data())
      within(
        data(), 
        dataname = str2lang(dataname),
        event_var = str2lang(event_var),
        plotly_excl_events = plotly_excl_events,
        expr = spiderplot_data <- dplyr::filter(dataname, !event_var %in% plotly_excl_events)
      )
    })
    plotly_selected_q <- srv_g_spiderplot(
      "spiderplot", 
      data = plotly_data,
      dataname = "spiderplot_data",
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var,
      filter_panel_api = filter_panel_api,
      plot_height = plot_height
    )
    
    recent_resp_q <- reactive({
      req(plotly_selected_q())
      within(
        plotly_selected_q(),
        dataname = str2lang(dataname),
        time_var = str2lang(time_var),
        subject_var = str2lang(subject_var),
        value_var = str2lang(value_var),
        subject_var_char = subject_var,
        event_var = str2lang(event_var),
        recent_resp_event =  "latest_response_assessment",  # todo: whattodo?
        resp_cols = resp_cols,
        expr = {
          brushed_subjects <- dplyr::filter(
            dataname, 
            time_var %in% plotly_brushed_time, 
            value_var %in% plotly_brushed_value
          )[[subject_var_char]]
          recent_resp <- dplyr::filter(
            dataname,
            event_var %in% recent_resp_event,
            subject_var %in% brushed_subjects
          ) |>
            select(all_of(resp_cols))
        }
      )
    })
    
    recent_resp_selected_q <- srv_t_reactable(
      "recent_resp", data = recent_resp_q, dataname = "recent_resp", selection = "single"
    )
    
    # todo: these tables do have the same filters and select. It is just a matter of parametrising 
    #       to named list:
    #       - (table) label
    #       - event_level for filter
    #       - columns
    all_resp_q <- reactive({
      req(nrow(recent_resp_selected_q()[["recent_resp_selected"]]))
      within(
        recent_resp_selected_q(),
        dataname = str2lang(dataname),
        subject_var = str2lang(subject_var),
        subject_var_char = subject_var,
        event_var = str2lang(event_var),
        all_resp_events =  "response_assessment",
        resp_cols = resp_cols,
        expr = {
          all_resp <- dplyr::filter(
            dataname, 
            event_var %in% all_resp_events,
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
        spep_events = "Serum M-protein",
        spep_cols = spep_cols,
        expr = {
          spep <- dplyr::filter(
            dataname,
            event_var %in% spep_events,
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
        subject_var_char = subject_var,
        event_var = str2lang(event_var),
        sflc_events = c(
          "Kappa free light chain quantity",
          "Lambda free light chain quantity",
          "Kappa-Lambda free light chain ratio"
        ),
        sflc_cols = sflc_cols,
        expr = {
          sflc <- dplyr::filter(
            dataname,
            event_var %in% sflc_events,
            subject_var %in% unique(recent_resp_selected[[subject_var_char]])
          ) |>
            select(all_of(sflc_cols))
        }
      )
    })
    
    #todo: show all_resp only if recent_resp is selected  
    all_resp_selected_q <- srv_t_reactable("all_resp", data = all_resp_q, dataname = "all_resp", selection = NULL) 
    spep_selected_d <- srv_t_reactable("spep_listing", data = spep_q, dataname = "spep", selection = NULL)
    sflc_selected_d <- srv_t_reactable("sflc_listing", data = sflc_q, dataname = "sflc", selection = NULL)
    
    all_q <- reactive({
      req(recent_resp_selected_q(), all_resp_selected_q())
      # all_resp_selected_q could be nothing and `c` won't work because the result is unavailable before clicking subjects in the table
      c(recent_resp_selected_q(), all_resp_selected_q())        
    })
    
    observeEvent(all_q(), {
      "do nothing"
    })
    
    
  })
}
