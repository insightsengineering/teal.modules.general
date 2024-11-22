pkgload::load_all("teal")
pkgload::load_all("teal.widgets")
pkgload::load_all("teal.modules.general")

# Note: Please add the `PATH_TO_DATA` and change the X, Y, and Z Administrations to the actual values in the data

data <- within(teal_data(), {
  library(dplyr)
  library(arrow)
  library(forcats)
  data_path <- "PATH_TO_DATA"

  swimlane_ds <- read_parquet(file.path(data_path, "swimlane_ds.parquet")) |>
    filter(!is.na(event_result), !is.na(event_study_day)) |>
    mutate(subject = forcats::fct_reorder(as.factor(subject), event_study_day, .fun = max))
  disposition <- swimlane_ds |>
    filter(!is.na(event_study_day)) |>
    filter(event_type == "disposition") |>
    transmute(subject, event_type, catagory = event_result, study_day = event_study_day)

  response_assessment <- swimlane_ds |>
    filter(!is.na(event_study_day)) |>
    filter(event_type == "response_assessment") |>
    transmute(subject, event_type, catagory = event_result, study_day = event_study_day)

  study_drug_administration <- swimlane_ds |>
    filter(!is.na(event_study_day)) |>
    filter(event_type == "study_drug_administration") |>
    transmute(subject, event_type, catagory = event_result, study_day = event_study_day)

  max_subject_day <- swimlane_ds |>
    group_by(subject) |>
    summarise(max_study_day = max(event_study_day))
})

plotly_specs <- list(
  list("plotly::add_markers", x = ~study_day, y = ~subject, color = ~catagory, symbol = ~catagory, data = quote(study_drug_administration)),
  list("plotly::add_markers", x = ~study_day, y = ~subject, color = ~catagory, symbol = ~catagory, data = quote(response_assessment)),
  list("plotly::add_markers", x = ~study_day, y = ~subject, color = ~catagory, symbol = ~catagory, data = quote(disposition)),
  list("plotly::add_bars", x = ~max_study_day, y = ~subject, data = quote(max_subject_day), width = 0.1, marker = list(color = "grey"), showlegend = FALSE)
)

app <- init(
  data = data,
  modules = modules(
    tm_data_table(),
    tm_p_swimlane2(
      label = "Swimlane",
      plotly_specs = plotly_specs,
      title = "Swimlane Efficacy Plot",
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
      )
    )
  )
)

shinyApp(app$ui, app$server)
