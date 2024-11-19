pkgload::load_all("teal")
pkgload::load_all("teal.widgets")
pkgload::load_all("teal.modules.general")

# Example data
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

color_manual <- c(
  "DEATH" = "black",
  "WITHDRAWAL BY SUBJECT" = "grey",
  "PD (Progressive Disease)" = "red",
  "SD (Stable Disease)" = "darkorchid4",
  "MR (Minimal/Minor Response)" = "sienna4",
  "PR (Partial Response)" = "maroon",
  "VGPR (Very Good Partial Response)" = "chartreuse4",
  "CR (Complete Response)" = "#3a41fc",
  "SCR (Stringent Complete Response)" = "midnightblue"
)
shape_manual <- c(
  "DEATH" = 4,
  "WITHDRAWAL BY SUBJECT" = 5,
  "PD (Progressive Disease)" = 8,
  "SD (Stable Disease)" = 5,
  "MR (Minimal/Minor Response)" = 5,
  "PR (Partial Response)" = 5,
  "VGPR (Very Good Partial Response)" = 5,
  "CR (Complete Response)" = 5,
  "SCR (Stringent Complete Response)" = 5
)

app <- init(
  data = data,
  modules = modules(
    tm_data_table(),
    tm_p_swimlane(
      label = "Swimlane",
      geom_specs = list(
        list(
          geom = str2lang("ggplot2::geom_bar"),
          data = quote(max_subject_day),
          mapping = list(y = quote(subject), x = quote(max_study_day)),
          stat = "identity",
          width = 0.1
        ),
        list(
          geom = quote(geom_point),
          data = quote(study_drug_administration),
          mapping = list(
            y = quote(subject), x = quote(study_day), color = quote(catagory), shape = quote(catagory)
          )
        ),
        list(
          geom = quote(geom_point),
          data = quote(disposition),
          mapping = list(
            y = quote(subject), x = quote(study_day), color = quote(catagory), shape = quote(catagory)
          )
        ),
        list(
          geom = quote(geom_point),
          data = quote(response_assessment),
          mapping = list(
            y = quote(subject), x = quote(study_day), color = quote(catagory), shape = quote(catagory)
          )
        ),
        list(
          geom = quote(scale_color_manual),
          values = color_manual,
          breaks = names(color_manual)
        ),
        list(
          geom = quote(scale_shape_manual),
          values = shape_manual,
          breaks = names(shape_manual)
        ),
        list(
          geom = quote(theme_minimal)
        )
      ),
      title = "Swimlane Efficacy Plot"
    )
  )
)

shinyApp(app$ui, app$server)
