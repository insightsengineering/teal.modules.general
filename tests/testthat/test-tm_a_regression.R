describe("tests for module creation", {
  data <- teal_data()
  data <- within(data, {
    require(nestcolor)
    CO2 <- CO2
  })

  it("works with default arguments", {
    mod <- tm_a_regression(
      response = data_extract_spec(
         dataname = "CO2",
         select = select_spec(
           label = "Select variable:",
           choices = "uptake",
           selected = "uptake",
           multiple = FALSE,
           fixed = TRUE
         )
      ),
      regressor = data_extract_spec(
        dataname = "CO2",
        select = select_spec(
            label = "Select variables:",
            choices = variable_choices(data[["CO2"]], c("conc", "Treatment")),
            selected = "conc",
            multiple = TRUE,
            fixed = FALSE
        )
      )
    )
    testthat::expect_s3_class(mod, "teal_module")
  })
})
