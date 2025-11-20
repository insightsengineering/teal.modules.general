library(teal)
library(DT)
library(labelled)
library(reactable)
pkgload::load_all("~/nest/teal.modules.general")
# Note: Please add the `PATH_TO_DATA` and change the X, Y, and Z Administrations to the actual values in the data

with_tooltips <- function(...) {
  args <- list(...)
  lapply(args, function(col) {
    col$header <- tags$span(col$name, title = col$name)
    return(col)
  })
}

data <- within(teal_data(), {
  library(dplyr)
  library(arrow)
  library(forcats)
  data_path <- "PATH/TO/DATA"

  swimlane_ds <- read_parquet(file.path(data_path, "swimlane_ds.parquet")) |>
    filter(!is.na(event_result), !is.na(event_study_day)) |>
    mutate(subject = forcats::fct_reorder(as.factor(subject), event_study_day, .fun = max))

  spiderplot_ds <- read_parquet(file.path(data_path, "spiderplot_ds.parquet")) |>
    mutate(subject = as.character(subject))

  parent_ds <- bind_rows(
    swimlane_ds |> select(subject, cohrt, txarm),
    spiderplot_ds |> select(subject, cohrt, txarm)
  ) |>
    distinct()
})

join_keys(data) <- join_keys(
  join_key("parent_ds", "swimlane_ds", c("subject", "cohrt", "txarm")),
  join_key("parent_ds", "spiderplot_ds", c("subject", "cohrt", "txarm"))
)

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

              adverse_events <- swimlane_ds |>
                filter(event_type == "adverse_event") |>
                select(subject, event_study_day, event_result, aenum, aeraw, icrsgr, ecrsgr, igrnci, egrnci, aeod_study_day, aerd_study_day) |>
                mutate(
                  initial_grade = coalesce(icrsgr, igrnci),
                  extreme_grade = coalesce(ecrsgr, egrnci),
                  initial_label = case_when(
                    !is.na(icrsgr) ~ "Initial ASTCT Grade",
                    !is.na(igrnci) ~ "Initial NCI CTCAE Grade",
                    TRUE ~ "Initial Grade"
                  ),
                  extreme_label = case_when(
                    !is.na(ecrsgr) ~ "Most Extreme ASTCT Grade",
                    !is.na(egrnci) ~ "Most Extreme NCI CTCAE Grade",
                    TRUE ~ "Most Extreme Grade"
                  )
                ) |>
                mutate(
                  tooltip = sprintf(
                    "Subject: %s<br>Study Day: %d<br>AENUM: %d<br>Event of Interest: %s<br>Primary Adverse Event: %s<br>Onset Study Day: %d<br>End Date Study Day: %d<br>%s: %d<br>%s: %d",
                    subject,
                    event_study_day,
                    aenum,
                    event_result,
                    aeraw,
                    aeod_study_day,
                    aerd_study_day,
                    initial_label,
                    initial_grade,
                    extreme_label,
                    extreme_grade
                  )
                )

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
                  "Z Administration Infusion" = "darkorchid",
                  "Cytokine Release Syndrome" = "#f5a733",
                  "Cytokine Release Syndrome Start" = "#fccf79",
                  "Cytokine Release Syndrome End" = "#f59505",
                  "Infection" = "pink",
                  "Infection Start" = "#f2ced3",
                  "Infection End" = "#d65668"
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
                plotly::add_segments(
                  data = adverse_events,
                  x = ~aeod_study_day,
                  xend = ~aerd_study_day,
                  y = ~subject,
                  yend = ~subject,
                  color = ~event_result,
                  line = list(width = 2),
                  showlegend = TRUE,
                  name = ~event_result,
                  legendgroup = ~event_result,
                  hoverinfo = "none"
                ) |>
                plotly::add_markers(
                  data = adverse_events |> filter(event_study_day == aeod_study_day),
                  x = ~aeod_study_day,
                  y = ~subject,
                  text = ~tooltip,
                  hoverinfo = "text",
                  color = ~ paste0(event_result, " Start"),
                  showlegend = TRUE,
                  legendgroup = ~event_result,
                  marker = list(size = 6, symbol = "arrow-down")
                ) |>
                plotly::add_markers(
                  data = adverse_events |> filter(event_study_day == aerd_study_day),
                  x = ~aerd_study_day,
                  y = ~subject,
                  text = ~tooltip,
                  hoverinfo = "text",
                  color = ~ paste0(event_result, " End"),
                  showlegend = TRUE,
                  legendgroup = ~event_result,
                  marker = list(size = 6, symbol = "arrow-down")
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
        col_defs <- with_tooltips(
          subject = colDef(name = "Subject"),
          raise_query = colDef(
            name = "Raise Query",
            cell = function(value) {
              if (!is.na(value) && !is.null(value) && value != "") {
                htmltools::tags$a(href = value, target = "_blank", "Link")
              } else {
                "N/A"
              }
            }
          ),
          visit_name = colDef(name = "Visit Name"),
          rspdn = colDef(name = "Assessment Performed"),
          rspd = colDef(name = "Response Date"),
          rspd_study_day = colDef(name = "Response Date Study Day"),
          orsp = colDef(name = "Response"),
          bma = colDef(name = "Best Marrow Aspirate"),
          bmb = colDef(name = "Best Marrow Biopsy"),
          comnts = colDef(name = "Comments")
        )
        mm_response <- swimlane_ds |>
          filter(event_study_day %in% plotly_selected()$x, subject %in% plotly_selected()$y, event_type == "response_assessment") |>
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

        col_defs <- with_tooltips(
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

tm_spider <- function(label = "Spiderplot", plot_height = 600) {
  ui <- function(id, height) {
    ns <- NS(id)
    tagList(
      div(
        style = "display: flex; justify-content: center; align-items: center; gap: 30px;",
        div(
          selectInput(NS(id, "event_type"), "Select Y Axis", NULL)
        ),
        div(sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, height))
      ),
      div(
        style = "display: flex",
        div(
          class = "simple-card",
          style = "width: 50%",
          tagList(
            h4("Most Recent Resp and Best Resp"),
            reactableOutput(ns("recent_resp"))
          )
        ),
        div(
          class = "simple-card",
          style = "width: 50%",
          plotly::plotlyOutput(ns("plot"), height = "100%")
        )
      ),
      div(
        style = "display: flex",
        div(
          style = "width: 50%",
          div(
            class = "simple-card",
            h4("Disease Assessment - SFLC"),
            reactableOutput(ns("sflc_listing"))
          ),
          div(
            class = "simple-card",
            h4("Disease Assessment - SPEP"),
            reactableOutput(ns("spep_listing"))
          )
        ),
        div(
          class = "simple-card",
          style = "width: 50%",
          h4("Multiple Myeloma Response"),
          reactableOutput(ns("all_resp"))
        )
      )
    )
  }
  server <- function(id, data, filter_panel_api, plot_height = 600) {
    moduleServer(id, function(input, output, session) {
      spiderplot_ds <- reactive(data()[["spiderplot_ds"]])
      observeEvent(spiderplot_ds(), {
        event_types <- unique(spiderplot_ds()$event_type)
        updateSelectInput(
          inputId = "event_type",
          choices = event_types[!event_types %in% c("response_assessment", "latest_response_assessment")]
        )
      })
      plotly_q <- reactive({
        data() |>
          within(
            {
              y_title <- selected_event
              spiderplot_ds_filtered <- spiderplot_ds |>
                filter(event_type == selected_event)
              ticksuffix <- ifelse(grepl("Change from baseline", selected_event), "%", "")

              p <- plotly::plot_ly(source = "spiderplot", height = height) |>
                plotly::add_markers(
                  x = ~event_study_day, y = ~event_result_num, color = ~subject,
                  data = spiderplot_ds_filtered
                ) |>
                plotly::add_lines(
                  x = ~event_study_day, y = ~event_result_num, color = ~subject,
                  data = spiderplot_ds_filtered,
                  showlegend = FALSE
                ) |>
                plotly::layout(
                  xaxis = list(title = "Collection Date Study Day", zeroline = FALSE),
                  yaxis = list(title = ~y_title, ticksuffix = ticksuffix, separatethousands = TRUE, exponentformat = "none"),
                  title = ~ paste0(paste(strwrap(y_title, width = 50), collapse = "<br>"), " Over Time")
                ) |>
                plotly::layout(dragmode = "select") |>
                plotly::config(displaylogo = FALSE)
            },
            selected_event = input$event_type,
            height = input$plot_height
          )
      })

      output$plot <- plotly::renderPlotly({
        plotly::event_register(
          plotly_q()$p,
          "plotly_selected"
        )
      })

      plotly_selected <- reactive(plotly::event_data("plotly_selected", source = "spiderplot"))


      resp_cols <- with_tooltips(
        subject = colDef(name = "Subject"),
        raise_query = colDef(
          name = "Raise Query",
          cell = function(value) {
            if (!is.na(value) && !is.null(value) && value != "") {
              htmltools::tags$a(href = value, target = "_blank", "Link")
            } else {
              "N/A"
            }
          }
        ),
        visit_name = colDef(name = "Visit Name"),
        rspdn = colDef(name = "Assessment Performed"),
        rspd = colDef(name = "Response Date"),
        rspd_study_day = colDef(name = "Response Date Study Day"),
        orsp = colDef(name = "Response"),
        bma = colDef(name = "Best Marrow Aspirate"),
        bmb = colDef(name = "Best Marrow Biopsy"),
        comnts = colDef(name = "Comments")
      )

      selected_recent_subject <- reactiveVal(NULL)

      plotly_selected_subjects <- reactive({
        data()[["spiderplot_ds"]] |>
          filter(event_study_day %in% plotly_selected()$x, event_result %in% plotly_selected()$y) |>
          pull(subject)
      })

      recent_resp_ds <- reactive({
        data()[["spiderplot_ds"]] |>
          filter(event_type == "latest_response_assessment") |>
          filter(subject %in% plotly_selected_subjects()) |>
          select(all_of(names(resp_cols)))
      })

      output$recent_resp <- renderReactable({
        req(plotly_selected_subjects())

        reactable(
          recent_resp_ds(),
          columns = resp_cols,
          selection = "single",
          onClick = "select",
          defaultPageSize = 15,
          wrap = FALSE,
          rowClass = JS("
            function(rowInfo) {
            console.log(rowInfo);
              if (rowInfo.selected) {
                return 'selected-row';
              }
            }
          ")
        )
      })

      table_selected_subjects <- reactive({
        selected_row <- getReactableState("recent_resp", "selected")
        if (!is.null(selected_row)) {
          recent_resp_ds()[selected_row, ]$subject
        } else {
          unique(recent_resp_ds()$subject)
        }
      })

      all_resp <- reactive({
        data()[["spiderplot_ds"]] |>
          filter(event_type == "response_assessment") |>
          select(all_of(names(resp_cols))) |>
          filter(subject %in% plotly_selected_subjects()) |>
          filter(subject %in% table_selected_subjects())
      })

      output$all_resp <- renderReactable({
        if (nrow(all_resp()) == 0) {
          return()
        }

        reactable(
          all_resp(),
          columns = resp_cols,
          defaultPageSize = 15,
          wrap = FALSE
        )
      })

      spep_cols <- with_tooltips(
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
        rspdn = colDef(name = "Assessment Performed"),
        rspd = colDef(name = "Response Date"),
        rspd_study_day = colDef(name = "Response Date Study Day"),
        orsp = colDef(name = "Response"),
        bma = colDef(name = "Best Marrow Aspirate"),
        bmb = colDef(name = "Best Marrow Biopsy"),
        comnts = colDef(name = "Comments"),
        asmntdn = colDef(name = "Assessment Not Done"),
        blq = colDef(name = "Serum M-protein too small to quantify"),
        coldr = colDef(name = "Collection Date"),
        cold_study_day = colDef(name = "Collection Study Day"),
        coltm = colDef(name = "Collection Time"),
        coltmu = colDef(name = "Collection Time Unknown"),
        lrspep1 = colDef(name = "Another Form added?"),
        mprte_raw = colDef(name = "Serum M-protein"),
        mprtec = colDef(name = "SPEP Serum M-protein detection")
      )

      spep <- reactive({
        data()[["spiderplot_ds"]] |>
          filter(event_type == "Serum M-protein") |>
          filter(subject %in% table_selected_subjects()) |>
          select(all_of(names(spep_cols)))
      })

      output$spep_listing <- renderReactable({
        if (nrow(spep()) == 0) {
          return()
        }

        reactable(
          spep(),
          columns = spep_cols,
          defaultPageSize = 5,
          wrap = FALSE
        )
      })


      sflc_cols <- with_tooltips(
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
        rspdn = colDef(name = "Assessment Performed"),
        rspd = colDef(name = "Response Date"),
        rspd_study_day = colDef(name = "Response Date Study Day"),
        orsp = colDef(name = "Response"),
        bma = colDef(name = "Best Marrow Aspirate"),
        bmb = colDef(name = "Best Marrow Biopsy"),
        comnts = colDef(name = "Comments"),
        asmntdn = colDef(name = "Assessment Not Done"),
        blq = colDef(name = "Serum M-protein too small to quantify"),
        coldr = colDef(name = "Collection Date"),
        cold_study_day = colDef(name = "Collection Study Day"),
        coltm = colDef(name = "Collection Time"),
        coltmu = colDef(name = "Collection Time Unknown"),
        lchfrc = colDef(name = "Presence of Serum free light chains"),
        lchfr_raw = colDef(name = "Serum free light chain results"),
        klchf_raw = colDef(name = "Kappa free light chain results"),
        llchf_raw = colDef(name = "Lambda free light chain results"),
        klchp_raw = colDef(name = "Kappa-Lambda free light chain ratio"),
        mprte_raw = colDef(name = "Serum M-protein"),
        mprtec = colDef(name = "SPEP Serum M-protein detection")
      )

      sflc <- reactive({
        data()[["spiderplot_ds"]] |>
          filter(
            event_type %in% c(
              "Kappa free light chain quantity",
              "Lambda free light chain quantity",
              "Kappa-Lambda free light chain ratio"
            )
          ) |>
          filter(subject %in% table_selected_subjects()) |>
          select(all_of(names(sflc_cols)))
      })

      output$sflc_listing <- renderReactable({
        if (nrow(sflc()) == 0) {
          return()
        }

        reactable(
          sflc(),
          columns = sflc_cols,
          defaultPageSize = 5,
          wrap = FALSE
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

app <- init(
  data = data,
  header = tags$head(tags$style(
    ".simple-card {
      padding: 20px;
      border-radius: 10px;
      border: 1px solid #ddd;
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      background-color: #fff;
    }
    .simple-card h4 {
      text-align: center;
    }
    .selected-row {
      background-color: #d9edf7;
      color: #31708f;
    }
    .custom-reactable.rt-nowrap .rt-th-inner {
      white-space: normal !important;  /* Allow text wrapping */
      text-overflow: unset !important;  /* Disable ellipsis */
      overflow: visible !important;  /* Ensure content is visible and wrapped */
    }"
  )),
  modules = modules(
    tm_swimlane(),
    tm_spider(),
    tm_data_table()
  ),
  filter = teal_slices(
    teal_slice(
      dataname = "parent_ds",
      varname = "subject"
    ),
    teal_slice(
      dataname = "parent_ds",
      varname = "cohrt"
    ),
    teal_slice(
      dataname = "parent_ds",
      varname = "txarm"
    ),
    count_type = "all"
  )
)

shinyApp(app$ui, app$server)
