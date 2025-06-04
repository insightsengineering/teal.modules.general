pkgload::load_all("teal")
pkgload::load_all("teal.widgets")
pkgload::load_all("teal.modules.general")

# Example data
data <- within(teal_data(), {
  library(dplyr)
  library(tidyr)
  ADSL <- teal.data::rADSL |> mutate(
    EOTSTT2 = case_when(
      !is.na(DCSREAS) ~ DCSREAS,
      TRUE ~ EOTSTT
    )
  )

  ADAE <- teal.data::rADAE
  ADRS <- teal.data::rADRS
})

join_keys(data) <- default_cdisc_join_keys

app <- init(
  data = data,
  modules = modules(
    tm_data_table(),
    tm_p_swimlane(
      label = "Swimlane",
      geom_specs = list(
        list(
          geom = quote(geom_col),
          data = quote(ADSL),
          mapping = list(y = quote(USUBJID), x = quote(EOSDY)),
          width = 0.2
        ), # geom_col(data = synthetic_data, mapping = aes(x = subjid, x = max(study_day), width = 0.2)
        list(
          geom = quote(geom_point),
          data = quote(ADSL),
          mapping = list(
            y = quote(USUBJID), x = quote(EOSDY), color = quote(EOTSTT2), shape = quote(EOTSTT2)
          )
        ),
        list(
          geom = quote(geom_point),
          data = quote(ADRS),
          mapping = list(
            y = quote(USUBJID), x = quote(ADY), color = quote(PARAMCD), shape = quote(PARAMCD)
          )
        ),
        list(
          geom = quote(geom_point),
          data = quote(ADAE),
          mapping = list(
            y = quote(USUBJID), x = quote(ASTDY), color = quote(AETERM), shape = quote(AETERM)
          )
        ),
        list(
          geom = quote(geom_point),
          data = quote(ADAE),
          mapping = list(
            y = quote(USUBJID), x = quote(AENDY), color = quote(AEOUT), shape = quote(AEOUT)
          )
        )
      ),
      title = "Swimlane Efficacy Plot"
    )
  )
)

shinyApp(app$ui, app$server)
