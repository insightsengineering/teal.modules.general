testthat::test_that("convert_metadata_to_dataframe returns a data frame with no rows if no metadata", {
  raw_metadata <- list(NULL, NULL)
  result <- convert_metadata_to_dataframe(raw_metadata, c("ADSL", "ADAE"))
  testthat::expect_equal(nrow(result), 0)
})

testthat::test_that("convert_metadata_to_dataframe returns a data frame with metadata if more than one dataset", {
  raw_metadata <- list(list(A = "a"), list(A = "a", B = "b"))
  result <- convert_metadata_to_dataframe(raw_metadata, c("ADSL", "ADAE"))
  testthat::expect_equal(
    result,
    data.frame(Dataset = c("ADSL", "ADAE", "ADAE"), Name = c("A", "A", "B"), Value = c("a", "a", "b"))
  )
})

testthat::test_that("convert_metadata_to_dataframe returns a data frame with metadata if one dataset", {
  raw_metadata <- list(list(A = "a", B = "b"))
  result <- convert_metadata_to_dataframe(raw_metadata, "ADSL")
  testthat::expect_equal(
    result,
    data.frame(Dataset = c("ADSL", "ADSL"), Name = c("A", "B"), Value = c("a", "b"))
  )
})

testthat::test_that("convert_metadata_to_dataframe converts metadata values to characters", {
  raw_metadata <- list(list(A = TRUE, B = 50, C = as.Date("2020-01-01"), D = 4L))
  result <- convert_metadata_to_dataframe(raw_metadata, "ADSL")
  testthat::expect_equal(
    result,
    data.frame(
      Dataset = rep("ADSL", 4),
      Name = c("A", "B", "C", "D"),
      Value = c("TRUE", "50", "2020-01-01", "4")
    )
  )
})
