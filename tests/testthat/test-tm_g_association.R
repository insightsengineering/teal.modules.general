test_that("tm_g_association fails initially", {

  data <- within(teal_data(), {
    require(nestcolor)
    CO2 <- CO2
    factors <- names(Filter(isTRUE, vapply(CO2, is.factor, logical(1L))))
    CO2[factors] <- lapply(CO2[factors], as.character)
  })

  module <- tm_g_association(
    ref = data_extract_spec(
      dataname = "CO2",
      select = select_spec(
        label = "Select variable:",
        choices = variable_choices("CO2", c("Plant", "Type", "Treatment")),
        selected = "Plant",
        fixed = FALSE
      )
    ),
    vars = data_extract_spec(
      dataname = "CO2",
      select = select_spec(
        label = "Select variables:",
        choices = variable_choices("CO2", c("Plant", "Type", "Treatment")),
        selected = "Treatment",
        multiple = TRUE,
        fixed = FALSE
      )
    )
  )

  args <- formals(tm_g_association)

  args$ref <- data_extract_spec(
    dataname = "CO2",
    select = select_spec(
      label = "Select variable:",
      choices = variable_choices("CO2", c("Plant", "Type", "Treatment")),
      selected = "Plant",
      fixed = FALSE
    )
  )
  args$vars <- data_extract_spec(
    dataname = "CO2",
    select = select_spec(
      label = "Select variables:",
      choices = variable_choices("CO2", c("Plant", "Type", "Treatment")),
      selected = "Treatment",
      multiple = TRUE,
      fixed = FALSE
    )
  )
  args$id <- "test"
  args$data <- reactive(data)
  args <- lapply(args, eval)
  testServer(module$server,
             args = args[names(formals(module$server))],
             expr = {
               expect_error(output_q())
             }
  )
})
