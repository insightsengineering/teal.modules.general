tm_swimlane <- function(label = "Swimlane", plot_height = 700) {
  ui <- function(id, height) {
    ns <- NS(id)
    tagList(
      fluidRow(
        class = "simple-card",
        h4("Swim Lane - Duration of Tx"),
        sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, height, width = "100%"),
        plotly::plotlyOutput(ns("plot"), height = "100%")
      ),
      fluidRow(
        column(
          6,
          class = "simple-card",
          tagList(
            h4("Multiple Myeloma Response"),
            reactableOutput(ns("mm_response"))
          )
        ),
        column(
          6,
          class = "simple-card",
          tagList(
            h4("Study Tx Listing"),
            reactableOutput(ns("tx_listing"))
          )
        )
      )
    )
  }
  server <- function(id, data, filter_panel_api, plot_height = 600) {
    moduleServer(id, function(input, output, session) {
      plotly_q <- reactive({
        data() |>
          within(
            {
              swimlane_ds <- swimlane_ds |>
                mutate(subject = forcats::fct_reorder(as.factor(subject), event_study_day, .fun = max)) |>
                mutate(
                  subject = forcats::fct_reorder(as.factor(subject), event_study_day, .fun = max),
                  tooltip = case_when(
                    event_type == "study_drug_administration" ~ paste(
                      "Subject:", subject,
                      "<br>Study Day:", event_study_day,
                      "<br>Administration:", event_result
                    ),
                    event_type == "response_assessment" ~ paste(
                      "Subject:", subject,
                      "<br>Study Day:", event_study_day,
                      "<br>Response Assessment:", event_result
                    ),
                    event_type == "disposition" ~ paste(
                      "Subject:", subject,
                      "<br>Study Day:", event_study_day,
                      "<br>Disposition:", event_result
                    ),
                    TRUE ~ NA_character_
                  )
                )
              
              swimlane_ds <- swimlane_ds |>
                group_by(subject, event_study_day) |>
                mutate(
                  tooltip = paste(unique(tooltip), collapse = "<br>")
                ) |>
                ungroup() |>
                mutate(tooltip = stringr::str_remove_all(tooltip, "<br>Subject: [0-9]+ <br>Study Day: [0-9]+"))
              
              disposition <- swimlane_ds |>
                filter(!is.na(event_study_day)) |>
                filter(event_type == "disposition") |>
                transmute(subject, event_type, catagory = event_result, study_day = event_study_day, tooltip)
              
              response_assessment <- swimlane_ds |>
                filter(!is.na(event_study_day)) |>
                filter(event_type == "response_assessment") |>
                transmute(subject, event_type, catagory = event_result, study_day = event_study_day, tooltip)
              
              study_drug_administration <- swimlane_ds |>
                filter(!is.na(event_study_day)) |>
                filter(event_type == "study_drug_administration") |>
                transmute(subject, event_type, catagory = event_result, study_day = event_study_day, tooltip)
              
              max_subject_day <- swimlane_ds |>
                group_by(subject) |>
                summarise(study_day = max(event_study_day)) |>
                bind_rows(tibble(subject = unique(swimlane_ds$subject), study_day = 0))
              
              p <- plotly::plot_ly(
                source = "swimlane",
                colors = c(
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
                symbols = c(
                  "DEATH" = "circle",
                  "WITHDRAWAL BY SUBJECT" = "square",
                  "PD (Progressive Disease)" = "circle",
                  "SD (Stable Disease)" = "square-open",
                  "MR (Minimal/Minor Response)" = "star-open",
                  "PR (Partial Response)" = "star-open",
                  "VGPR (Very Good Partial Response)" = "star-open",
                  "CR (Complete Response)" = "star-open",
                  "SCR (Stringent Complete Response)" = "star-open",
                  "X Administration Injection" = "line-ns-open",
                  "Y Administration Infusion" = "line-ns-open",
                  "Z Administration Infusion" = "line-ns-open"
                ),
                height = height
              ) |>
                plotly::add_markers(
                  x = ~study_day, y = ~subject, color = ~catagory, symbol = ~catagory,
                  text = ~tooltip,
                  hoverinfo = "text",
                  data = study_drug_administration
                ) |>
                plotly::add_markers(
                  x = ~study_day, y = ~subject, color = ~catagory, symbol = ~catagory,
                  text = ~tooltip,
                  hoverinfo = "text",
                  data = response_assessment
                ) |>
                plotly::add_markers(
                  x = ~study_day, y = ~subject, color = ~catagory, symbol = ~catagory,
                  text = ~tooltip,
                  hoverinfo = "text",
                  data = disposition
                ) |>
                plotly::add_segments(
                  x = ~0, xend = ~study_day, y = ~subject, yend = ~subject,
                  data = max_subject_day,
                  line = list(width = 1, color = "grey"),
                  showlegend = FALSE
                ) |>
                plotly::layout(
                  xaxis = list(title = "Study Day"), yaxis = list(title = "Subject")
                ) |>
                plotly::layout(dragmode = "select") |>
                plotly::config(displaylogo = FALSE)
            },
            height = input$plot_height
          )
      })
      
      output$plot <- plotly::renderPlotly({
        plotly::event_register(
          plotly_q()$p,
          "plotly_selected"
        )
      })
      
      plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "swimlane"))
      
      output$mm_response <- renderReactable({
        swimlane_ds <- data()[["swimlane_ds"]]
        col_defs <- .with_tooltips(
          subject = colDef(name = "Subject"),
          visit_name = colDef(name = "Visit Name", width = 250),
          visit_date = colDef(name = "Visit Date"),
          form_name = colDef(name = "Form Name", width = 250),
          source_system_url_link = colDef(
            name = "Source System URL Link",
            cell = function(value) {
              if (!is.na(value) && !is.null(value) && value != "") {
                htmltools::tags$a(href = value, target = "_blank", "Link")
              } else {
                "N/A"
              }
            }
          ),
          rspdn = colDef(name = "Assessment Performed"),
          rspd = colDef(name = "Response Date"),
          rspd_study_day = colDef(name = "Response Date Study Day"),
          orsp = colDef(name = "Response", width = 250),
          bma = colDef(name = "Best Marrow Aspirate"),
          bmb = colDef(name = "Best Marrow Biopsy"),
          comnts = colDef(name = "Comments")
        )
        mm_response <- swimlane_ds |>
          filter(event_study_day %in% plotly_selected()$x, subject %in% plotly_selected()$y) |>
          select(all_of(names(col_defs)))
        if (nrow(mm_response) == 0) {
          return()
        }
        
        reactable(
          mm_response,
          class = "custom-reactable",
          columns = col_defs,
          defaultPageSize = 10,
          wrap = FALSE,
          searchable = TRUE,
          sortable = TRUE
        )
      })
      
      output$tx_listing <- renderReactable({
        swimlane_ds <- data()[["swimlane_ds"]]
        
        col_defs <- .with_tooltips(
          site_name = colDef(name = "Site Name"),
          subject = colDef(name = "Subject"),
          visit_name = colDef(name = "Visit Name"),
          visit_date = colDef(name = "Visit Date"),
          form_name = colDef(name = "Form Name"),
          source_system_url_link = colDef(
            name = "Source System URL Link",
            cell = function(value) {
              if (!is.na(value) && !is.null(value) && value != "") {
                htmltools::tags$a(href = value, target = "_blank", "Link")
              } else {
                "N/A"
              }
            }
          ),
          txnam = colDef(name = "Study Drug Name"),
          txrec = colDef(name = "Study Drug Administered"),
          txrecrs = colDef(name = "Reason Study Drug Not Admin"),
          txd_study_day = colDef(name = "Date Administered Study Day"),
          date_administered = colDef(name = "Date Administered"),
          cydly = colDef(name = "Cycle Delay"),
          cydlyrs = colDef(name = "Cycle Delay Reason"),
          cydlyae = colDef(name = "Cycle Delay Adverse Event"),
          txdly = colDef(name = "Dose Delay"),
          txdlyrs = colDef(name = "Dose Delay Reason"),
          txdlyae = colDef(name = "AE related to Dose Delay"),
          txpdos = colDef(name = "Planned Dose per Admin"),
          txpdosu = colDef(name = "Planned Dose per Admin Unit"),
          frqdv = colDef(name = "Frequency"),
          txrte = colDef(name = "Route of Administration"),
          txform = colDef(name = "Dose Formulation"),
          txdmod = colDef(name = "Dose Modification"),
          txrmod = colDef(name = "Dose Modification Reason"),
          txdmae = colDef(name = "AE related to Dose Modification"),
          txad = colDef(name = "Total Dose Administered"),
          txadu = colDef(name = "Total Dose Administered Unit"),
          txd = colDef(name = "Date Administered"),
          txstm = colDef(name = "Start Time Administered"),
          txstmu = colDef(name = "Start Time Administered Unknown"),
          txed = colDef(name = "End Date Administered"),
          txetm = colDef(name = "End Time Administered"),
          txetmu = colDef(name = "End Time Administered Unknown"),
          txtm = colDef(name = "Time Administered"),
          txtmu = colDef(name = "Time Administered Unknown"),
          txed_study_day = colDef(name = "End Study Day"),
          infrt = colDef(name = "Infusion Rate"),
          infrtu = colDef(name = "Infusion Rate Unit"),
          tximod = colDef(name = "Infusion Modified?"),
          txirmod = colDef(name = "Reason for Infusion modification"),
          tximae = colDef(name = "AE related to Infusion Modification")
        )
        tx_listing <- swimlane_ds |>
          filter(event_study_day %in% plotly_selected()$x, subject %in% plotly_selected()$y) |>
          select(all_of(names(col_defs)))
        if (nrow(tx_listing) == 0) {
          return()
        }
        
        reactable(
          tx_listing,
          columns = col_defs,
          defaultPageSize = 10,
          wrap = FALSE,
          searchable = TRUE,
          sortable = TRUE
        )
      })
    })
  }
  module(
    label = label,
    ui = ui,
    server = server,
    datanames = "all",
    ui_args = list(height = plot_height)
  )
}