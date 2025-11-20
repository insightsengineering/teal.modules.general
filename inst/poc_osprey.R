pkgload::load_all("teal.modules.general")

data <- within(teal_data(), {
  library(dplyr)
  library(osprey)

  ADSL <- osprey::rADSL |>
    mutate(x_val = as.integer(TRTEDTM - TRTSDTM)) |>
    arrange(x_val) |>
    filter(!is.na(x_val))
  ADRS <- osprey::rADRS |>
    filter(ADY >= 0, USUBJID %in% ADSL$USUBJID)
  reference_lines <- data.frame(x = c(50, 250), xend = c(50, 250), y = min(ADSL$USUBJID), yend = max(ADSL$USUBJID))
})

join_keys(data) <- default_cdisc_join_keys[c("ADSL", "ADRS")]

plotly_specs <- list(
  list(
    "plotly::add_bars",
    data = quote(ADSL),
    x = ~x_val, y = ~USUBJID, color = ~ARM,
    colors = c("A: Drug X" = "#343CFF", "B: Placebo" = "#FF484B", "C: Combination" = "#222222")
  ),
  list(
    "plotly::add_markers",
    data = quote(ADRS),
    x = ~ADY, y = ~USUBJID, symbol = ~AVALC,
    marker = list(
      size = 10,
      color = "#329133"
    )
  ),
  list(
    "plotly::add_segments",
    data = quote(reference_lines),
    x = ~x,
    xend = ~xend,
    y = ~y,
    yend = ~yend,
    line = list(
      color = "#CA0E40",
      width = 2,
      dash = "dash"
    ),
    showlegend = FALSE
  )
)

app <- init(
  data = data,
  filter = teal_slices(
    teal_slice(
      "ADSL",
      "AGE",
      selected = c(24, 25)
    ),
    teal_slice(
      "ADRS",
      "PARAMCD",
      selected = "OVRINV"
    )
  ),
  modules = modules(
    tm_data_table(),
    tm_p_swimlane2(
      label = "Swimlane",
      plotly_specs = plotly_specs,
      title = "Swimlane Efficacy Plot",
      symbols = c("CR" = "circle", "PR" = "triangle-up", "SD" = "diamond-wide", "PD" = "square", "NE" = "x-thin-open")
    )
  )
)

shinyApp(app$ui, app$server)
