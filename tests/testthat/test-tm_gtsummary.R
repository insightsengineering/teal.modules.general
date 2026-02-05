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
      "teal_module"
    )
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

  # FIXME: depends on fix_check_decorators@main branch
  # it("fails when decorators is to a different object", {
  #   testthat::expect_error(
  #     tm_gtsummary(
  #       by = mock_data_extract_spec(select_multiple = FALSE),
  #       include = mock_data_extract_spec(select_multiple = TRUE),
  #       decorators = list(
  #         plot = teal::teal_transform_module()
  #       )
  #     ),
  #     "must be a named list from these names: table"
  #   )
  # })

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

# UI

testthat::describe("tm_gtsummary module ui behavior returns a htmltools tag or taglist", {
  it("with minimal arguments", {
    mod <- tm_gtsummary(
      by = mock_data_extract_spec(select_multiple = FALSE),
      include = mock_data_extract_spec(select_multiple = FALSE)
    )
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    checkmate::expect_multi_class(ui, c("shiny.tag", "shiny.tag.list"))
  })
})


# Server
create_gtsummary_module <- function(data, by_vars, include_vars, by_selected, include_selected, ...) {
  tm_gtsummary(
    by = list(
      teal.transform::data_extract_spec(
        dataname = "test_data",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices(
            data = isolate(data())[["test_data"]],
            by_vars
          ),
          selected = by_selected,
          multiple = FALSE
        )
      )
    ),
    include = list(
      teal.transform::data_extract_spec(
        dataname = "test_data",
        select = teal.transform::select_spec(
          choices = teal.transform::variable_choices(
            data = isolate(data())[["test_data"]],
            include_vars
          ),
          selected = include_selected,
          multiple = TRUE
        )
      )
    ),
    ...
  )
}


testthat::describe("tm_gtsummary module server behavior", {
  it("server function executes successfully through module interface", {
    data <- create_test_data(penguins)

    mod <- create_gtsummary_module(
      penguins,
      by_vars = c("species", "island", "sex", "year"),
      include_vars = c("island", "sex", "body_mass"),
      by_selected = c("species"),
      include_selected = c("island", "sex")
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test_data", data = data),
        mod$server_args
      ),
      {
        session$setInputs(
          "by-dataset_test_data_singleextract-select" = "species",
          "include-dataset_test_data_singleextract-select" = c("island", "sex")
        )

        testthat::expect_true(endsWith(get_code(print_output_decorated()), "table"))
        testthat::expect_true(inherits(table_r(), "gtsummary"))
      }
    )
  })
  it("server function generates table with 'include' being NULL", {
    data <- create_test_data(penguins)

    mod <- create_gtsummary_module(
      penguins,
      by_vars = c("species", "island", "sex", "year"),
      include_vars = c("island", "sex", "body_mass"),
      by_selected = c("species"),
      include_selected = c("island", "sex")
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test_data", data = data),
        mod$server_args
      ),
      {
        session$setInputs(
          "by-dataset_test_data_singleextract-select" = "species",
          "include-dataset_test_data_singleextract-select" = NULL
        )

        testthat::expect_true(endsWith(get_code(print_output_decorated()), "table"))
        testthat::expect_true(inherits(table_r(), "gtsummary"))
        testthat::expect_gt(length(unique(table_r()$table_body$variable)), 3L)
      }
    )
  })
  it("server function generates table with 'col_label'", {
    data <- create_test_data(penguins)

    col_label <- list(island = "Island", sex = "Sex")
    mod <- create_gtsummary_module(
      penguins,
      by_vars = c("species", "island", "sex", "year"),
      include_vars = c("island", "sex", "body_mass"),
      by_selected = c("species"),
      include_selected = c("island", "sex"),
      col_label = col_label
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test_data", data = data),
        mod$server_args
      ),
      {
        session$setInputs(
          "by-dataset_test_data_singleextract-select" = "species",
          "include-dataset_test_data_singleextract-select" = c("island", "sex"),
          col_label = col_label
        )
        testthat::expect_true(endsWith(get_code(print_output_decorated()), "table"))
        table <- table_r()
        testthat::expect_equal(table$inputs$label, col_label)
        testthat::expect_true(all(table$table_body$var_label %in% unlist(col_label)))
      }
    )
  })
})

# Decorators
