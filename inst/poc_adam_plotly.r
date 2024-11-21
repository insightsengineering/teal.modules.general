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


plotly_specs <- list(
  list("plotly::add_bars", x = ~EOSDY, y = ~USUBJID, data = quote(ADSL)),
  list("plotly::add_markers", x = ~EOSDY, y = ~USUBJID, color = ~EOTSTT2, data = quote(ADSL)),
  list("plotly::add_markers", x = ~ADY, y = ~USUBJID, data = quote(ADRS))
)


app <- init(
  data = data,
  modules = modules(
    tm_data_table(),
    tm_p_swimlane2(
      label = "Swimlane",
      plotly_specs = plotly_specs,
      title = "Swimlane Efficacy Plot"
    )
  )
)

shinyApp(app$ui, app$server)
