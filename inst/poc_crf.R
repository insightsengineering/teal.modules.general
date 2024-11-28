pkgload::load_all("teal")
pkgload::load_all("teal.widgets")
pkgload::load_all("teal.modules.general")
library(DT)
library(labelled)

# Note: Please add the `PATH_TO_DATA` and change the X, Y, and Z Administrations to the actual values in the data

data <- within(teal_data(), {
  library(dplyr)
  library(arrow)
  library(forcats)
  data_path <- "PATH_TO_DATA"

  swimlane_ds <- read_parquet(file.path(data_path, "swimlane_ds.parquet")) |>
    filter(!is.na(event_result), !is.na(event_study_day)) |>
    mutate(subject = as.character(subject)) |>
    mutate(
      plot_subject = case_when(
        event_type == "disposition" ~ paste0(subject, " - Disposition"),
        event_type == "response_assessment" ~ paste0(subject, " - Response Assessment"),
        event_type == "study_drug_administration" ~ paste0(subject, " - Drug Administration"),
        TRUE ~ as.character(subject)
      )
    ) |>
    group_by(subject_group = sub(" - .*", "", plot_subject)) |>
    mutate(max_event_day = max(event_study_day)) |>
    ungroup() |>
    mutate(
      plot_subject = forcats::fct_reorder(plot_subject, max_event_day, .fun = max)
    ) |>
    select(-subject_group, -max_event_day)

  spiderplot_ds <- read_parquet(file.path(data_path, "spiderplot_ds.parquet")) |>
    mutate(subject = as.character(subject))
})


swim_plotly_specs <- list(
  list("plotly::add_markers", x = ~study_day, y = ~plot_subject, color = ~catagory, symbol = ~catagory, data = quote(study_drug_administration)),
  list("plotly::add_markers", x = ~study_day, y = ~plot_subject, color = ~catagory, symbol = ~catagory, data = quote(response_assessment)),
  list("plotly::add_markers", x = ~study_day, y = ~plot_subject, color = ~catagory, symbol = ~catagory, data = quote(disposition)),
  list("plotly::add_lines", x = ~study_day, y = ~plot_subject, data = quote(max_subject_day), color = ~plot_subject, line = list(width = 1, color = "grey"), showlegend = FALSE),
  list("plotly::layout", xaxis = list(title = "Study Day"), yaxis = list(title = "Subject"))
)

tm <- teal_transform_module(
  server = function(id, data) {
    reactive({
      data() |>
        within({
          disposition <- swimlane_ds |>
            filter(!is.na(event_study_day)) |>
            filter(event_type == "disposition") |>
            transmute(plot_subject, event_type, catagory = event_result, study_day = event_study_day)

          response_assessment <- swimlane_ds |>
            filter(!is.na(event_study_day)) |>
            filter(event_type == "response_assessment") |>
            transmute(plot_subject, event_type, catagory = event_result, study_day = event_study_day)

          study_drug_administration <- swimlane_ds |>
            filter(!is.na(event_study_day)) |>
            filter(event_type == "study_drug_administration") |>
            transmute(plot_subject, event_type, catagory = event_result, study_day = event_study_day)

          max_subject_day <- swimlane_ds |>
            group_by(plot_subject) |>
            summarise(study_day = max(event_study_day)) |>
            bind_rows(tibble(plot_subject = unique(swimlane_ds$plot_subject), study_day = 0))
        })
    })
  }
)

ui_mod <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6, DTOutput(ns("mm_response"))),
    column(6, DTOutput(ns("tx_listing")))
  )
}

srv_mod <- function(id,
                    data,
                    plotly_selected,
                    filter_panel_api) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    output$test <- renderText({
      print(plotly_selected)
      "It works!"
    })

    output$mm_response <- renderDT({
      select_cols <- c(
        "subject", "visit_name", "visit_date", "form_name", "source_system_url_link",
        "rspdn", "rspd", "rspd_study_day", "orsp", "bma", "bmb", "comnts"
      )
      new_col_names <- setNames(
        select_cols,
        c(
          "Subject", "Visit Name", "Visit Date", "Form Name", "Source System URL Link",
          "Assessment Performed", "Response Date", "Response Date Study Day",
          "Response", "Best Marrow Aspirate", "Best Marrow Biopsy", "Comments"
        )
      )
      swimlane_ds <- data()[["swimlane_ds"]]
      mm_response <- swimlane_ds |>
        filter(event_study_day %in% plotly_selected()$x, plot_subject %in% plotly_selected()$y) |>
        select(all_of(select_cols))
      datatable(mm_response, colnames = new_col_names)
    })

    output$tx_listing <- renderDT({
      select_cols <- c(
        "site_name", "subject", "visit_name", "visit_date", "form_name",
        "source_system_url_link", "txnam", "txrec", "txrecrs", "txd_study_day",
        "date_administered", "cydly", "cydlyrs", "cydlyae", "txdly", "txdlyrs",
        "txdlyae", "txpdos", "txpdosu", "frqdv", "txrte", "txform", "txdmod",
        "txrmod", "txdmae", "txad", "txadu", "txd", "txstm", "txstmu", "txed",
        "txetm", "txetmu", "txtm", "txtmu", "txed_study_day", "infrt", "infrtu",
        "tximod", "txirmod", "tximae"
      )
      new_col_names <- setNames(
        select_cols,
        c(
          "Site Name", "Subject", "Visit Name", "Visit Date", "Form Name", "Source System URL Link",
          "Study Drug Name", "Study Drug Administered", "Reason Study Drug Not Admin",
          "Date Administered Study Day", "Date Administered", "Cycle Delay", "Cycle Delay Reason",
          "Cycle Delay Adverse Event", "Dose Delay", "Dose Delay Reason", "AE related to Dose Delay",
          "Planned Dose per Admin", "Planned Dose per Admin Unit", "Frequency", "Route of Administration",
          "Dose Formulation", "Dose Modification", "Dose Modification Reason",
          "AE related to Dose Modification", "Total Dose Administered", "Total Dose Administered Unit",
          "Date Administered", "Start Time Administered", "Start Time Administered Unknown",
          "End Date Administered", "End Time Administered", "End Time Administered Unknown",
          "Time Administered", "Time Administered Unknown", "End Study Day", "Infusion Rate",
          "Infusion Rate Unit", "Infusion Modified?", "Reason for Infusion modification",
          "AE related to Infusion Modification"
        )
      )
      swimlane_ds <- data()[["swimlane_ds"]]
      tx_listing <- swimlane_ds |>
        filter(event_study_day %in% plotly_selected()$x, plot_subject %in% plotly_selected()$y) |>
        select(all_of(select_cols))
      datatable(tx_listing, colnames = new_col_names)
    })
  })
}

pkgload::load_all("teal.modules.general")

spider_plotly_specs <- list(
  list("plotly::add_markers", x = ~event_study_day, y = ~event_result, color = ~subject, symbol = ~event_type, data = quote(spiderplot_ds)),
  list("plotly::add_lines", x = ~event_study_day, y = ~event_result, data = quote(spiderplot_ds), color = ~subject, showlegend = FALSE)
)

app <- init(
  data = data,
  modules = modules(
    tm_p_swimlane2(
      label = "Swimlane",
      plotly_specs = swim_plotly_specs,
      title = "Swim Lane - Duration of Tx",
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
      transformers = list(tm),
      ui_mod = ui_mod,
      srv_mod = srv_mod
    ),
    tm_p_swimlane2(
      label = "Spiderplot",
      plotly_specs = spider_plotly_specs,
      title = "Swimlane Efficacy Plot"
    ),
    tm_data_table()
  ),
  filter = teal_slices(
    teal_slice(
      dataname = "swimlane_ds",
      varname = "subject"
    ),
    teal_slice(
      dataname = "swimlane_ds",
      varname = "cohrt"
    ),
    teal_slice(
      dataname = "swimlane_ds",
      varname = "txarm"
    )
  )
)

shinyApp(app$ui, app$server)
