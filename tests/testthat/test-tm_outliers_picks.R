testthat::test_that("tm_outliers keeps compound keys for child datasets", {
  data <- teal.data::teal_data()
  data <- within(data, {
    ADSL <- teal.data::rADSL
    ADLB <- teal.data::rADLB
  })
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys[c("ADSL", "ADLB")]

  mod <- tm_outliers(
    outlier_var = teal.picks::picks(
      teal.picks::datasets("ADLB", "ADLB"),
      teal.picks::variables("AVAL", "AVAL", multiple = FALSE)
    ),
    categorical_var = teal.picks::picks(
      teal.picks::datasets("ADLB", "ADLB"),
      teal.picks::variables(c("PARAMCD", "AVISIT"), selected = NULL, multiple = FALSE)
    )
  )

  shiny::testServer(
    mod$server,
    args = c(list(id = "test", data = shiny::reactive(data)), mod$server_args),
    expr = {
      .change_selectors(selectors, outlier_var = "AVAL", categorical_var = NULL)
      session$setInputs(
        method = "IQR",
        iqr_slider = 1.5,
        split_outliers = FALSE,
        order_by_outlier = FALSE
      )

      result <- common_code_q()
      outliers <- result[["ANL_OUTLIER"]]
      extended <- result[["ANL_OUTLIER_EXTENDED"]]

      testthat::expect_true(all(c("STUDYID", "USUBJID", "PARAMCD", "AVISIT") %in% names(outliers)))
      testthat::expect_equal(nrow(extended), nrow(outliers))
    }
  )
})
