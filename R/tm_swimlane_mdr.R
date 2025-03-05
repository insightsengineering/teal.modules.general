tm_g_swimlane_mdr <- function(label = "Swimlane", 
                              dataname, 
                              time_var, 
                              subject_var, 
                              value_var, 
                              event_var,
                              subtable_labels = c("Multiple Myeloma Response", "Study Tx Listing"),
                              subtable_cols = list(
                                c(
                                  "subject", "visit_name", "visit_date", "form_name", "source_system_url_link",
                                  "rspdn", "rspd", "rspd_study_day", "orsp", "bma", "bmb", "comnts"
                                ),
                                c(
                                  "site_name", "subject", "visit_name", "visit_date", "form_name", "source_system_url_link", "txnam",
                                  "txrec", "txrecrs", "txd_study_day", "date_administered", "cydly", "cydlyrs", "cydlyae", "txdly", 
                                  "txdlyrs", "txdlyae", "txpdos", "txpdosu", "frqdv", "txrte", "txform", "txdmod", "txrmod",
                                  "txdmae", "txad", "txadu", "txd", "txstm", "txstmu", "txed", "txetm", "txetmu", "txtm", "txtmu",
                                  "txed_study_day", "infrt", "infrtu", "tximod", "txirmod", "tximae"
                                )
                              ),
                              value_var_color = c(
                                "DEATH" = "black",
                                "WITHDRAWAL BY SUBJECT" = "grey",
                                "PD (Progressive Disease)" = "red",
                                "SD (Stable Disease)" = "darkorchid4",
                                "MR (Minimal/Minor Response)" = "sienna4",
                                "PR (Partial Response)" = "maroon",
                                "VGPR (Very Good Partial Response)" = "chartreuse4",
                                "CR (Complete Response)" = "#3a41fc",
                                "SCR (Stringent Complete Response)" = "midnightblue",
                                "X Administration Injection" = "goldenrod",
                                "Y Administration Infusion" = "deepskyblue3",
                                "Z Administration Infusion" = "darkorchid"
                              ),
                              # possible markers https://plotly.com/python/marker-style/
                              value_var_symbol = c(
                                "DEATH" = "circle",
                                "WITHDRAWAL BY SUBJECT" = "square",
                                "PD (Progressive Disease)" = "circle",
                                "SD (Stable Disease)" = "square-open",
                                "MR (Minimal/Minor Response)" = "star-open",
                                "PR (Partial Response)" = "star-open",
                                "VGPR (Very Good Partial Response)" = "star-open",
                                "CR (Complete Response)" = "star-open",
                                "SCR (Stringent Complete Response)" = "star-open",
                                "X Administration Injection" = "line-ns",
                                "Y Administration Infusion" = "line-ns",
                                "Z Administration Infusion" = "line-ns"
                              ),
                              plot_height = 700) {
  checkmate::assert_character(subtable_labels)
  checkmate::assert_list(subtable_cols)
  checkmate::assert_character(value_var_color)
  module(
    label = label,
    ui = ui_g_swimlane_mdr,
    server = srv_g_swimlane_mdr,
    datanames = dataname,
    ui_args = list(height = plot_height),
    server_args = list(
      dataname = dataname,
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var,
      value_var_color = value_var_color,
      value_var_symbol = value_var_symbol,
      subtable_labels = subtable_labels,
      subtable_cols = subtable_cols,
      plot_height = plot_height
    )
  )
}

ui_g_swimlane_mdr <- function(id, height) {
  ns <- NS(id)
  tagList(
    fluidRow(
      class = "simple-card",
      h4("Swim Lane - Duration of Tx"),
      ui_g_swimlane(ns("plot"), height = height)
    ),
    fluidRow(
      class = "simple-card",
      ui_t_reactables(ns("subtables"))      
    )

  )
}
srv_g_swimlane_mdr <- function(id, 
                               data, 
                               dataname,
                               time_var,
                               subject_var,
                               value_var,
                               event_var,
                               value_var_color,
                               value_var_symbol,
                               subtable_labels,
                               subtable_cols,
                               filter_panel_api, 
                               plot_height = 600) {
  moduleServer(id, function(input, output, session) {
    plotly_selected_q <- srv_g_swimlane(
      "plot", 
      data = data,
      dataname = dataname,
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var,
      value_var_color = value_var_color,
      value_var_symbol = value_var_symbol,
      filter_panel_api = filter_panel_api
    )
    
    subtable_names <- gsub("[[:space:][:punct:]]+", "_", x = tolower(subtable_labels))
    subtables_q <- reactive({
      req(plotly_selected_q())
      calls <- lapply(seq_along(subtable_names), function(i) {
        substitute(
          list(
            dataname = str2lang(dataname),
            subtable_name = str2lang(subtable_names[i]),
            subtable_label = subtable_labels[i],
            time_var = str2lang(time_var),
            subject_var = str2lang(subject_var),
            col_defs = subtable_cols[[i]]
          ),
          expr = {
            subtable_name <- dataname |>
              dplyr::filter(
                time_var %in% plotly_brushed_time,
                subject_var %in% plotly_brushed_subject
              ) |>
              dplyr::select(dplyr::all_of(col_defs))
            attr(subtable_name, "label") <- subtable_label
          }
        )
      })
      teal.code::eval_code(plotly_selected_q(), as.expression(calls))
    })
    
    srv_t_reactables("subtables", data = subtables_q, datanames = subtable_names)
  })
}
