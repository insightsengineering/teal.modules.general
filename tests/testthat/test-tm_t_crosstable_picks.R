testthat::describe("tm_t_crosstable module creation", {
  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_t_crosstable(
        label = "Cross Table",
        x = mock_teal_picks(select_multiple = TRUE),
        y = mock_teal_picks(select_multiple = FALSE)
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with default options", {
    testthat::expect_s3_class(
      tm_t_crosstable(
        x = mock_teal_picks(select_multiple = TRUE),
        y = mock_teal_picks(select_multiple = FALSE)
      ),
      "teal_module"
    )
  })

  it("creates a module with datanames taken from picks", {
    mod <- tm_t_crosstable(
      x = mock_teal_picks(dataname = "A", select_multiple = TRUE),
      y = mock_teal_picks(dataname = "B", select_multiple = FALSE)
    )

    testthat::expect_setequal(
      mod$datanames,
      c("A", "B")
    )
  })

  it("creates a module that is bookmarkable", {
    mod <- tm_t_crosstable(
      x = mock_teal_picks(select_multiple = TRUE),
      y = mock_teal_picks(select_multiple = FALSE)
    )

    testthat::expect_true(attr(mod, "teal_bookmarkable"))
  })
})

testthat::describe("tm_t_crosstable input validation", {
  it("fails when label is not a string", {
    testthat::expect_error(
      tm_t_crosstable(
        label = 123,
        x = mock_teal_picks(select_multiple = TRUE),
        y = mock_teal_picks(select_multiple = FALSE)
      ),
      "Assertion on 'label' failed"
    )
  })

  it("fails when x is not a picks or list", {
    testthat::expect_error(
      tm_t_crosstable(
        x = "not a spec",
        y = mock_teal_picks(select_multiple = FALSE)
      ),
      "Assertion on 'x' failed"
    )
  })

  it("fails when y is not a picks or list", {
    testthat::expect_error(
      tm_t_crosstable(
        x = mock_teal_picks(select_multiple = TRUE),
        y = "not a spec"
      ),
      "Assertion on 'y' failed"
    )
  })

  it("throws warning when y allows multiple selection", {
    testthat::expect_warning(
      tm_t_crosstable(
        x = mock_teal_picks(select_multiple = TRUE),
        y = mock_teal_picks(select_multiple = TRUE)
      ),
      "`y` accepts only a single variable selection",
      fixed = TRUE
    )
  })

  it("fails when show_percentage is not logical", {
    testthat::expect_error(
      tm_t_crosstable(
        x = mock_teal_picks(select_multiple = TRUE),
        y = mock_teal_picks(select_multiple = FALSE),
        show_percentage = "not logical"
      ),
      "Assertion on 'show_percentage' failed"
    )
  })

  it("fails when show_total is not logical", {
    testthat::expect_error(
      tm_t_crosstable(
        x = mock_teal_picks(select_multiple = TRUE),
        y = mock_teal_picks(select_multiple = FALSE),
        show_total = "not logical"
      ),
      "Assertion on 'show_total' failed"
    )
  })

  it("fails when remove_zero_columns is not logical", {
    testthat::expect_error(
      tm_t_crosstable(
        x = mock_teal_picks(select_multiple = TRUE),
        y = mock_teal_picks(select_multiple = FALSE),
        remove_zero_columns = "not logical"
      ),
      "Assertion on 'remove_zero_columns' failed"
    )
  })

  it("throws when decorators has invalid object types", {
    testthat::expect_error(
      tm_t_crosstable(
        x = mock_teal_picks(select_multiple = TRUE),
        y = mock_teal_picks(select_multiple = FALSE),
        decorators = list(
          table = "not a teal_transform_module"
        )
      ),
      "Assertion on 'decorators' failed|Make sure that the named list contains"
    )
  })

  it("accepts valid decorators", {
    testthat::expect_s3_class(
      tm_t_crosstable(
        x = mock_teal_picks(select_multiple = TRUE),
        y = mock_teal_picks(select_multiple = FALSE),
        decorators = list(
          table = teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
  })
})

create_crosstable_module <- function(data, x_vars, y_vars, x_selected, y_selected, ...) {
  tm_t_crosstable(
    x = teal.picks::picks(
      teal.picks::datasets("test_data", "test_data"),
      teal.picks::variables(x_vars, x_selected, multiple = TRUE)
    ),
    y = teal.picks::picks(
      teal.picks::datasets("test_data", "test_data"),
      teal.picks::variables(y_vars, y_selected, multiple = FALSE)
    ),
    ...
  )
}

testthat::describe("tm_t_crosstable module server behavior", {
  it("server function executes successfully through module interface", {
    test_data_df <- data.frame(
      x_var = factor(rep(c("A", "B", "C"), 10)),
      y_var = factor(rep(c("X", "Y"), 15))
    )
    data <- create_test_data(test_data_df)

    mod <- create_crosstable_module(
      data,
      x_vars = c("x_var", "y_var"),
      y_vars = c("x_var", "y_var"),
      x_selected = "x_var",
      y_selected = "y_var"
    )

    shiny::testServer(
      mod$server,
      args = c(list(id = "test", data = data), mod$server_args),
      expr = {
        .change_selectors(selectors, x = c("x_var"), y = c("y_var"))
        session$setInputs(
          "show_percentage" = TRUE,
          "show_total" = TRUE,
          "remove_zero_columns" = FALSE,
          join_fun = "join_fun"
        )
        testthat::expect_s4_class(validated_q(), "teal_data")
        testthat::expect_s4_class(output_q(), "teal_data")
        checkmate::expect_multi_class(table_r(), c("ElementaryTable", "VTableTree"))
      }
    )
  })

  it("server function generates table with show_percentage enabled", {
    test_data_df <- data.frame(
      x_var = factor(rep(c("A", "B", "C"), 10)),
      y_var = factor(rep(c("X", "Y"), 15))
    )
    data <- create_test_data(test_data_df)

    mod <- create_crosstable_module(
      data,
      x_vars = c("x_var", "y_var"),
      y_vars = c("x_var", "y_var"),
      x_selected = "x_var",
      y_selected = "y_var",
      show_percentage = TRUE
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        .change_selectors(selectors, x = c("x_var"), y = c("y_var"))
        session$setInputs(
          "show_percentage" = TRUE,
          "show_total" = TRUE,
          "remove_zero_columns" = FALSE
        )
        testthat::expect_s4_class(validated_q(), "teal_data")
        testthat::expect_s4_class(output_q(), "teal_data")
        checkmate::expect_multi_class(table_r(), c("ElementaryTable", "VTableTree"))
      }
    )
  })

  it("server function generates table with show_total enabled", {
    test_data_df <- data.frame(
      x_var = factor(rep(c("A", "B", "C"), 10)),
      y_var = factor(rep(c("X", "Y"), 15))
    )
    data <- create_test_data(test_data_df)

    mod <- create_crosstable_module(
      data,
      x_vars = c("x_var", "y_var"),
      y_vars = c("x_var", "y_var"),
      x_selected = "x_var",
      y_selected = "y_var",
      show_total = TRUE
    )

    shiny::testServer(
      mod$server,
      args = c(
        list(id = "test", data = data),
        mod$server_args
      ),
      expr = {
        .change_selectors(selectors, x = c("x_var"), y = c("y_var"))
        session$setInputs(
          "show_percentage" = TRUE,
          "show_total" = TRUE,
          "remove_zero_columns" = FALSE
        )
        testthat::expect_s4_class(validated_q(), "teal_data")
        testthat::expect_s4_class(output_q(), "teal_data")
        checkmate::expect_multi_class(table_r(), c("ElementaryTable", "VTableTree"))
      }
    )
  })
})
