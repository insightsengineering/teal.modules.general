testthat::describe("tm_front_page module creation", {
  it("creates a teal_module object with default options", {
    testthat::expect_s3_class(tm_front_page(), "teal_module")
  })

  it("creates a module that is bookmarkable", {
    expect_true(
      attr(tm_front_page(), "teal_bookmarkable", exact = TRUE)
    )
  })
})

testthat::describe("tm_front_page input validation", {
  it("fails when label is not a string", {
    testthat::expect_error(tm_front_page(label = 123), "label")
  })

  it("fails when header_text is not a character vector", {
    testthat::expect_error(tm_front_page(header_text = 123), "header_text")
    testthat::expect_error(tm_front_page(header_text = list("a", "b")), "header_text")
  })

  it("fails when tables is not a named list of data.frames", {
    testthat::expect_error(tm_front_page(tables = data.frame(a = 1)), "tables")
    testthat::expect_error(tm_front_page(tables = list(a = 1)), "tables")
    testthat::expect_error(tm_front_page(tables = list(data.frame(a = 1))), "tables")
  })

  it("fails when additional_tags is not a shiny.tag.list or html", {
    testthat::expect_error(tm_front_page(additional_tags = 123), "additional_tags")
    testthat::expect_error(tm_front_page(additional_tags = list()), "additional_tags")
  })

  it("fails when footnotes is not a character vector", {
    testthat::expect_error(tm_front_page(footnotes = 123), "footnotes")
    testthat::expect_error(tm_front_page(footnotes = list("a", "b")), "footnotes")
  })

  it("fails when datanames is not a character vector or NULL", {
    testthat::expect_error(tm_front_page(datanames = 123), "datanames")
    testthat::expect_error(tm_front_page(datanames = list("a", "b")), "datanames")
  })

  it("fails when transformators has invalid object types", {
    testthat::expect_error(
      tm_front_page(transformators = list("not a teal_transform_module")),
      "May only contain the following types: \\{teal_transform_module\\}"
    )
  })

  it("show_metadata is deprecated", {
    testthat::expect_error(tm_front_page(show_metadata = TRUE), "was deprecated")
  })

  it("accepts valid transformators", {
    testthat::expect_s3_class(
      tm_front_page(transformators = list(teal::teal_transform_module())),
      "teal_module"
    )
  })
})

testthat::describe("tm_front_page module ui behavior returns a htmltools tag or taglist", {
  it("with default arguments", {
    checkmate::expect_multi_class(tm_front_page()$ui(id = "test"), c("shiny.tag", "shiny.tag.list"))
  })
  it("with datanames", {
    mod <- tm_front_page(datanames = "iris")
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    testthat::expect_match(as.character(ui), "id=\"front_page_metabutton\"", all = TRUE, fixed = TRUE)
  })
  it("with footnotes", {
    mod <- tm_front_page(footnotes = c(foot = "A footnote"))
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    testthat::expect_match(as.character(ui), "A footnote", all = TRUE, fixed = TRUE)
    testthat::expect_match(as.character(ui), "<b>foot</b>", all = TRUE, fixed = TRUE)
  })
  it("with header", {
    mod <- tm_front_page(header_text = c(header = "A header"))
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    testthat::expect_match(as.character(ui), "A header", all = TRUE, fixed = TRUE)
    testthat::expect_match(as.character(ui), "<h3>header</h3>", all = TRUE, fixed = TRUE)
  })
  it("with tables", {
    mod <- tm_front_page(tables = list(t1 = data.frame(Info = c("A", "B"), Text = c("A", "B"))))
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    testthat::expect_match(as.character(ui), "id=\"front_page_tables\"", all = TRUE, fixed = TRUE)
  })
})

testthat::describe("tm_front_page module server behavior", {
  it("server function executes generates metadata table", {
    mod <- tm_front_page()
    data <- shiny::reactive(
      within(teal.data::teal_data(), {
        test_data <- data.frame(
          response = factor(rep(c("A", "B", "C"), 10)),
          x_var = factor(rep(c("X", "Y"), 15))
        )
        test_data2 <- iris
        attr(test_data, "metadata") <- list(meta = "data")
        attr(test_data2, "metadata") <- NULL
      })
    )
    shiny::testServer(
      mod$server,
      args = c(list(id = "test", data = data), mod$server_args),
      expr = {
        testthat::expect_equal(
          metadata_data_frame(),
          data.frame(Dataset = "test_data", Name = "meta", Value = "data")
        )
        checkmate::expect_character(output$metadata_table)
      }
    )
  })

  it("server function fails to validate metadata_table", {
    mod <- tm_front_page()
    data <- shiny::reactive(
      within(teal.data::teal_data(), {
        test_data <- data.frame(
          response = factor(rep(c("A", "B", "C"), 10)),
          x_var = factor(rep(c("X", "Y"), 15))
        )
      })
    )
    shiny::testServer(
      mod$server,
      args = c(list(id = "test", data = data), mod$server_args),
      expr = {
        testthat::expect_error(output$metadata_table, "The data has no associated metadata")
      }
    )
  })

  it("server function table from argument", {
    mod <- tm_front_page(tables = list(t1 = data.frame(Info = c("A", "B"), Text = c("A", "B"))))
    shiny::testServer(
      mod$server,
      args = c(list(id = "test", data = shiny::reactive(teal.data::teal_data())), mod$server_args),
      expr = checkmate::check_string(output$table_1)
    )
  })
})