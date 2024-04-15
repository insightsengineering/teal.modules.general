app_driver_tm_g_scatterplotmatrix <- function() {
  data <- simple_cdisc_data()
  data <- within(data, {
    attr(ADSL, "metadata") <- list("Author" = "NEST team", "data_source" = "synthetic data")
  })

  TealAppDriver$new(
    data = data,
    modules = tm_g_scatterplotmatrix(
      label = "Scatterplot matrix",
      variables = list(
        data_extract_spec(
          dataname = "ADSL",
          select = select_spec(
            label = "Select variables:",
            choices = variable_choices(data[["ADSL"]]),
            selected = c("AGE", "RACE", "SEX"),
            multiple = TRUE,
            ordered = TRUE,
            fixed = FALSE
          )
        ),
        data_extract_spec(
          dataname = "ADRS",
          filter = filter_spec(
            label = "Select endpoints:",
            vars = c("PARAMCD", "AVISIT"),
            choices = value_choices(data[["ADRS"]], c("PARAMCD", "AVISIT"), c("PARAM", "AVISIT")),
            selected = "INVET - END OF INDUCTION",
            multiple = TRUE
          ),
          select = select_spec(
            label = "Select variables:",
            choices = variable_choices(data[["ADRS"]]),
            selected = c("AGE", "AVAL", "ADY"),
            multiple = TRUE,
            ordered = TRUE,
            fixed = FALSE
          )
        )
      ),
      plot_height = c(600, 200, 2000),
      plot_width = NULL,
      pre_output = NULL,
      post_output = NULL
    ),
    timeout = 3000
  )
}
