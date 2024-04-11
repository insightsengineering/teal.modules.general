# Create a mock data extact spec for tests
mock_data_extract_spec <- function(dataname = "MOCK_DATASET",
                                   select_choices = sample(LETTERS, sample(2:10, 1)),
                                   select_multiple = FALSE) {
  teal.transform::data_extract_spec(
    dataname = dataname,
    select = teal.transform::select_spec(
      choices = select_choices,
      multiple = select_multiple
    )
  )
}

# Based on example
local_app_tm_a_pca <- function() {
  data <- within(teal.data::teal_data(), {
    require(nestcolor)

    USArrests <- USArrests # nolint: object_name.
  })
  datanames(data) <- "USArrests"


  TealAppDriver$new(
    data = data,
    modules = tm_a_pca(
      dat = teal.transform::data_extract_spec(
        dataname = "USArrests",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices(
            data = data[["USArrests"]],
            c("Murder", "Assault", "UrbanPop", "Rape")
          ),
          selected = c("Murder", "Assault"),
          multiple = TRUE
        )
      )
    )
  )
}
