#' @export
tm_g_swimlane_mdr <- function(label = "Swimlane", 
                              plot_dataname, 
                              time_var, 
                              subject_var, 
                              value_var, 
                              event_var,
                              listing_datanames = character(0),
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
  checkmate::assert_character(value_var_color)
  module(
    label = label,
    ui = ui_g_swimlane_mdr,
    server = srv_g_swimlane_mdr,
    datanames = union(plot_dataname, listing_datanames),
    ui_args = list(height = plot_height),
    server_args = list(
      plot_dataname = plot_dataname,
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var,
      value_var_color = value_var_color,
      value_var_symbol = value_var_symbol,
      listing_datanames = listing_datanames,
      plot_height = plot_height
    )
  )
}

ui_g_swimlane_mdr <- function(id, height) {
  ns <- NS(id)
  tagList(
    div(
      h4("Swim Lane - Duration of Tx"),
      ui_g_swimlane(ns("plot"), height = height)
    ),
    ui_t_reactables(ns("subtables"))
  )
}
srv_g_swimlane_mdr <- function(id, 
                               data, 
                               plot_dataname,
                               time_var,
                               subject_var,
                               value_var,
                               event_var,
                               value_var_color,
                               value_var_symbol,
                               listing_datanames,
                               filter_panel_api, 
                               plot_height = 600) {
  moduleServer(id, function(input, output, session) {
    plotly_selected_q <- srv_g_swimlane(
      "plot", 
      data = data,
      dataname = plot_dataname,
      time_var = time_var,
      subject_var = subject_var,
      value_var = value_var,
      event_var = event_var,
      value_var_color = value_var_color,
      value_var_symbol = value_var_symbol,
      filter_panel_api = filter_panel_api
    )
    
    if (length(listing_datanames)) {
      listings_q <- reactive({
        req(plotly_selected_q())
        calls <- lapply(seq_along(listing_datanames), function(i) {
          listing_name <- listing_names[i]
          listing_label <- attr(plotly_selected_q()[[listing_name]], "label")
          substitute(
            list(
              listing_name = str2lang(listing_name),
              listing_selected = str2lang(sprintf("%s_selected", listing_name)),
              listing_label = listing_label,
              subject_var = str2lang(subject_var)
            ),
            expr = {
              listing_selected <- dplyr::filter(listing_name, subject_var %in% plotly_brushed_subject)
            }
          )
        })
        teal.code::eval_code(plotly_selected_q(), as.expression(calls))
      })
      srv_t_reactables("subtables", data = listings_q, datanames = listing_datanames, layout = "tabs")
    }
  })
}
