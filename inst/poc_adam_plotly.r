library(plotly)
pkgload::load_all("teal.modules.general")

# Example data
data <- within(teal_data(), {
  library(dplyr)
  library(tidyr)
  ADSL <- teal.data::rADSL |> mutate(
    EOTSTT2 = case_when(
      !is.na(DCSREAS) ~ DCSREAS,
      TRUE ~ EOTSTT
    ),
    TRTLEN = as.integer(TRTEDTM - TRTSDTM)
  )

  ADAE <- teal.data::rADAE
  ADRS <- teal.data::rADRS
})

join_keys(data) <- default_cdisc_join_keys


plotly_specs <- list(
  list("plotly::add_bars", x = ~TRTLEN, y = ~USUBJID, color = ~ARM, data = quote(ADSL)),
  list("plotly::add_markers", x = ~ADY, y = ~USUBJID, color = ~AVALC, symbol = ~AVALC, data = quote(ADRS))
)

app <- init(
  data = data,
  modules = modules(
    tm_data_table(),
    tm_p_plotly(
      label = "Swimlane",
      plotly_specs = plotly_specs,
      title = "Swimlane Efficacy Plot"
    )
  ),
  filter = teal_slices(
    teal_slice("ADSL", "AGE", selected = c(20, 25))
  )
)

shinyApp(app$ui, app$server)
