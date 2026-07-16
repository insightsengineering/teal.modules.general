testthat::describe("tm_tbl_summary module creation", {
  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_tbl_summary(
        by = mock_teal_picks(select_multiple = FALSE),
        include = mock_teal_picks(select_multiple = FALSE)
      ),
      "teal_module"
    )
  })

  it("creates a module with datanames taken from data extracts", {
    mod <- tm_tbl_summary(
      by = mock_teal_picks(dataname = "A", select_multiple = FALSE),
      include = mock_teal_picks(dataname = "A", select_multiple = FALSE)
    )

    testthat::expect_setequal(mod$datanames, "A")
  })

  it("creates a module that is bookmarkable", {
    mod <- tm_tbl_summary(
      by = mock_teal_picks(select_multiple = FALSE),
      include = mock_teal_picks(select_multiple = FALSE)
    )

    testthat::expect_true(attr(mod, "teal_bookmarkable"))
  })

  it("accepts crane::tbl_roche_summary arguments", {
    testthat::expect_s3_class(
      tm_tbl_summary(
        by = mock_teal_picks(select_multiple = FALSE),
        include = mock_teal_picks(select_multiple = FALSE),
        nonmissing = "always",
        percent = "cell"
      ),
      "teal_module"
    )
  })
})

testthat::describe("tm_tbl_summary input validation", {
  it("fails when 'label' is not a string", {
    testthat::expect_error(
      tm_tbl_summary(
        label = 123,
        by = mock_teal_picks(select_multiple = FALSE),
        include = mock_teal_picks(select_multiple = TRUE)
      ),
      "Assertion on 'label' failed"
    )
  })

  it("fails when 'by' allows multiple selection", {
    testthat::expect_error(
      tm_tbl_summary(
        by = mock_teal_picks(select_multiple = TRUE),
        include = mock_teal_picks(select_multiple = TRUE)
      ),
      "Must be a single selection"
    )
  })

  it("does not fail when 'include' is not a teal.picks", {
    testthat::expect_no_error(
      tm_tbl_summary(
        by = mock_teal_picks(select_multiple = FALSE),
        include = "COLUMN_NAME"
      )
    )
  })

  it("pass when 'include' allows multiple selection", {
    testthat::expect_no_error(
      tm_tbl_summary(
        by = mock_teal_picks(select_multiple = FALSE),
        include = mock_teal_picks(select_multiple = TRUE)
      )
    )
  })

  it("fails when col_label is not character", {
    testthat::expect_error(
      tm_tbl_summary(
        by = mock_teal_picks(select_multiple = FALSE),
        include = mock_teal_picks(select_multiple = TRUE),
        col_label = 1213
      ),
      "Assertion on 'col_label' failed"
    )
  })


  it("fails when decorators has invalid object types", {
    testthat::expect_error(
      tm_tbl_summary(
        by = mock_teal_picks(select_multiple = FALSE),
        include = mock_teal_picks(select_multiple = TRUE),
        decorators = list(
          table = "not a teal_transform_module"
        )
      ),
      "Assertion on 'decorators' failed|Make sure that the named list contains"
    )
  })

  it("fails when decorators is to a different object", {
    testthat::expect_error(
      tm_tbl_summary(
        by = mock_teal_picks(select_multiple = FALSE),
        include = mock_teal_picks(select_multiple = TRUE),
        decorators = list(
          plot = teal::teal_transform_module()
        )
      ),
      "The `decorators` must be a named list with:"
    )
  })

  it("accepts valid decorators", {
    testthat::expect_s3_class(
      tm_tbl_summary(
        by = mock_teal_picks(select_multiple = FALSE),
        include = mock_teal_picks(select_multiple = TRUE),
        decorators = list(
          table = teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
  })
})

# UI

testthat::describe("tm_tbl_summary module ui behavior returns a htmltools tag or taglist", {
  it("with minimal arguments", {
    mod <- tm_tbl_summary(
      by = mock_teal_picks(select_multiple = FALSE),
      include = mock_teal_picks(select_multiple = FALSE)
    )
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    checkmate::expect_multi_class(ui, c("shiny.tag", "shiny.tag.list"))
  })
})


# Server
create_gtsummary_module <- function(data, by_vars, include_vars, by_selected, include_selected, ...) {
  tm_tbl_summary(
    by = teal.picks::picks(
      teal.picks::datasets("test_data", "test_data"),
      variables(by_vars, by_selected, multiple = FALSE)
    ),
    include = teal.picks::picks(
      teal.picks::datasets("test_data", "test_data"),
      variables(include_vars, include_selected, multiple = TRUE)
    ),
    ...
  )
}

testthat::describe("tm_tbl_summary module server behavior", {
  it("server function executes successfully through module interface", {
    data <- create_test_data(mtcars)

    mod <- create_gtsummary_module(
      data,
      by_vars = c("am", "gear"),
      include_vars = c("carb", "cyl"),
      by_selected = c("am"),
      include_selected = c("carb", "cyl")
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test_data", data = data),
        mod$server_args
      ),
      {
        session$setInputs(
          "by-variables-selected" = "am",
          "include-variables-selected" = c("carb", "cyl")
        )
        session$flushReact()

        testthat::expect_true(endsWith(get_code(session$returned()), "table"))
        testthat::expect_true(inherits(table_r(), "gtsummary"))
      }
    )
  })
  it("server function generates table with 'include' being NULL", {
    data <- create_test_data(mtcars)

    mod <- create_gtsummary_module(
      data,
      by_vars = c("am", "gear"),
      include_vars = c("carb", "cyl"),
      by_selected = c("am"),
      include_selected = c("carb", "cyl")
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test_data", data = data),
        mod$server_args
      ),
      {
        session$setInputs(
          "by-variables-selected" = "am",
          "include-variables-selected" = NULL
        )
        session$flushReact()

        testthat::expect_true(endsWith(get_code(session$returned()), "table"))
        testthat::expect_true(inherits(table_r(), "gtsummary"))
        testthat::expect_gt(length(unique(table_r()$table_body$variable)), 3L)
      }
    )
  })
  it("server function generates table with 'col_label'", {
    data <- create_test_data(mtcars)

    col_label <- list(carb = "Carb", cyl = "Cyl")
    mod <- create_gtsummary_module(
      data,
      by_vars = c("am", "gear"),
      include_vars = c("carb", "cyl"),
      by_selected = c("am"),
      include_selected = c("carb", "cyl"),
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
          "by-dataset_test_data_singleextract-select" = "am",
          "include-dataset_test_data_singleextract-select" = c("carb", "cyl")
        )
        session$flushReact()

        testthat::expect_true(endsWith(get_code(session$returned()), "table"))
        table <- table_r()
        testthat::expect_equal(table$inputs$label, col_label)
        testthat::expect_true(all(table$table_body$var_label %in% unlist(col_label)))
      }
    )
  })
})

# Decorators
testthat::describe("tm_tbl_summary module server behavior with decorators", {
  it("one decorator executes successfully", {
    data <- create_test_data(mtcars)
    cap <- "Caption 1"
    mod <- create_gtsummary_module(
      data,
      by_vars = c("am", "gear"),
      include_vars = c("carb", "cyl"),
      by_selected = c("am"),
      include_selected = c("carb", "cyl"),
      decorators = list(table = teal::teal_transform_module())
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test_data", data = data),
        mod$server_args
      ),
      {
        session$setInputs(
          "by-dataset_test_data_singleextract-select" = "am",
          "include-dataset_test_data_singleextract-select" = c("carb", "cyl")
        )
        session$flushReact()
        testthat::expect_true(endsWith(get_code(session$returned()), "table"))
        testthat::expect_s3_class(table_r(), "gtsummary")
      }
    )
  })

  it("one decorator executes successfully", {
    data <- create_test_data(mtcars)
    mod <- create_gtsummary_module(
      data,
      by_vars = c("am", "gear"),
      include_vars = c("carb", "cyl"),
      by_selected = c("am"),
      include_selected = c("carb", "cyl"),
      decorators = list(table = teal::teal_transform_module(
        server = function(id, data) {
          reactive({
            within(data(), {
              table2 <- table
            })
          })
        }
      ))
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test_data", data = data),
        mod$server_args
      ),
      {
        session$setInputs(
          "by-dataset_test_data_singleextract-select" = "am",
          "include-dataset_test_data_singleextract-select" = c("carb", "cyl")
        )

        testthat::expect_true(endsWith(get_code(session$returned()), "table"))
        testthat::expect_true(grepl("table2 <-", get_code(session$returned()), fixed = TRUE))
        testthat::expect_s3_class(session$returned()$table2, "gtsummary")
      }
    )
  })
})
