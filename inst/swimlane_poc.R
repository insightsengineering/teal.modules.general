pkgload::load_all("teal")
pkgload::load_all("teal.widgets")
pkgload::load_all("teal.modules.general")

# Example data
data <- within(teal_data(), {
  library(dplyr)
  library(tidyr)

  set.seed(123) # Setting a seed for reproducibility
  # Define possible maximum study days
  .possible_end_days <- c(50, 60, 70)

  # Create sample data
  synthetic_data <- tibble(subjid = c(1:15)) |>
    rowwise() |>
    mutate(
      max_study_day = sample(.possible_end_days, 1),
      study_day = list(seq(10, max_study_day, by = 10))
    ) |>
    unnest(study_day) |>
    group_by(subjid) |>
    mutate(
      assigned_drug = sample(c("Drug A", "Drug B"), 1)
    ) |>
    ungroup() |>
    mutate(
      response_type = sample(c("CR", "PR"), n(), replace = TRUE),
      subjid = reorder(as.character(subjid), max_study_day)
    ) |>
    select(-max_study_day)
})

app <- init(
  data = data,
  modules = modules(
    tm_data_table(),
    tm_p_swimlane(
      label = "Swimlane",
      geom_specs = list(
        list(
          geom = str2lang("ggplot2::geom_col"),
          data = quote(synthetic_data),
          mapping = list(y = quote(subjid), x = quote(max(study_day))),
          width = 0.2
        ), # geom_col(data = synthetic_data, mapping = aes(x = subjid, x = max(study_day), width = 0.2)
        list(
          geom = quote(geom_point),
          data = quote(synthetic_data),
          mapping = list(
            y = quote(subjid), x = quote(study_day), color = quote(assigned_drug), shape = quote(assigned_drug)
          )
        ),
        list(
          geom = quote(geom_point),
          data = quote(synthetic_data),
          mapping = list(
            y = quote(subjid), x = quote(study_day), color = quote(response_type), shape = quote(response_type)
          )
        )
      ),
      title = "Swimlane Efficacy Plot"
    )
  )
)

shinyApp(app$ui, app$server)
