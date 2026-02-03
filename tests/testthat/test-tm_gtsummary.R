testthat::describe("tm_gtsummary module creation", {

  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_gtsummary(
        by = mock_data_extract_spec(select_multiple = FALSE),
        include = mock_data_extract_spec(select_multiple = FALSE)
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with list of data extract specs", {
    testthat::expect_s3_class(
      tm_gtsummary(
        by = list(
          mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
          mock_data_extract_spec(dataname = "A", select_multiple = FALSE)
        ),
        include = list(
          mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
          mock_data_extract_spec(dataname = "A", select_multiple = FALSE)
        )
      ),
      "teal_module"
    )
  })

  it("creates a module with datanames taken from data extracts", {
    mod <- tm_gtsummary(
      by = list(
        mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
        mock_data_extract_spec(dataname = "A", select_multiple = FALSE)
      ),
      include = list(
        mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
        mock_data_extract_spec(dataname = "A", select_multiple = FALSE)
      )
    )

    testthat::expect_setequal(mod$datanames, "A")
  })

  it("creates a module that is bookmarkable", {
    mod <- tm_gtsummary(
      by = mock_data_extract_spec(select_multiple = FALSE),
      include = mock_data_extract_spec(select_multiple = FALSE)
    )

    testthat::expect_true(attr(mod, "teal_bookmarkable"))
  })

  it("accepts crane::tbl_roche_summary arguments", {
    testthat::expect_s3_class(
      tm_gtsummary(
        by = mock_data_extract_spec(select_multiple = FALSE),
        include = mock_data_extract_spec(select_multiple = FALSE),
        nonmissing = "always",
        percent = "cell"
      ),
      "teal_module")
  })
})

testthat::describe("tm_gtsummary input validation", {
  it("fails when 'label' is not a string", {
    testthat::expect_error(
      tm_gtsummary(
        label = 123,
        by = mock_data_extract_spec(select_multiple = FALSE),
        include = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "Assertion on 'label' failed"
    )
  })

  it("fails when 'by' is not a data_extract_spec or list", {
    testthat::expect_error(
      tm_gtsummary(
        by = "not a spec",
        include = mock_data_extract_spec(select_multiple = FALSE)
      ),
      "Assertion on 'by' failed"
    )
  })

  it("fails when 'by' allows multiple selection", {
    testthat::expect_error(
      tm_gtsummary(
        by = mock_data_extract_spec(select_multiple = TRUE),
        include = mock_data_extract_spec(select_multiple = TRUE)
      ),
      "'by' should not allow multiple selection"
    )
  })

  it("creates a teal_module object with list of data extract specs", {
    testthat::expect_error(
      tm_gtsummary(
        by = list(
          mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
          mock_data_extract_spec(dataname = "B", select_multiple = FALSE)
        ),
        include = list(
          mock_data_extract_spec(dataname = "A", select_multiple = FALSE),
          mock_data_extract_spec(dataname = "B", select_multiple = FALSE)
        )
      ),
      "Must have length 1, but has length 2"
    )
  })

  it("fails when 'include' is not a data_extract_spec or list", {
    testthat::expect_error(
      tm_gtsummary(
        by = mock_data_extract_spec(select_multiple = FALSE),
        include = "not a spec"
      ),
      "Assertion on 'include' failed"
    )
  })

  it("pass when 'include' allows multiple selection", {
    testthat::expect_no_error(
      tm_gtsummary(
        by = mock_data_extract_spec(select_multiple = FALSE),
        include = mock_data_extract_spec(select_multiple = TRUE)
      )
    )
  })

  it("fails when col_label is not character", {
    testthat::expect_error(
      tm_gtsummary(
        by = mock_data_extract_spec(select_multiple = FALSE),
        include = mock_data_extract_spec(select_multiple = TRUE),
        col_label = 1213
      ),
      "Assertion on 'col_label' failed"
    )
  })


  it("fails when decorators has invalid object types", {
    testthat::expect_error(
      tm_gtsummary(
        by = mock_data_extract_spec(select_multiple = FALSE),
        include = mock_data_extract_spec(select_multiple = TRUE),
        decorators = list(
          table = "not a teal_transform_module"
        )
      ),
      "Assertion on 'decorators' failed|Make sure that the named list contains"
    )
  })

  it("fails when decorators is to a different object", {
    testthat::expect_error(
      tm_gtsummary(
        by = mock_data_extract_spec(select_multiple = FALSE),
        include = mock_data_extract_spec(select_multiple = TRUE),
        decorators = list(
          plot = teal::teal_transform_module()
        )
      ),
      "must be a named list from these names: table"
    )
  })

  it("accepts valid decorators", {
    testthat::expect_s3_class(
      tm_gtsummary(
        by = mock_data_extract_spec(select_multiple = FALSE),
        include = mock_data_extract_spec(select_multiple = TRUE),
        decorators = list(
          table = teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
  })
})
