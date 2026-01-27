testthat::describe("tm_data_tables module creation", {
  it("creates a teal_module object with default options", {
    testthat::expect_s3_class(tm_data_table(), "teal_module")
  })

  it("creates a module with valid transformators", {
    testthat::expect_s3_class(
      tm_data_table(transformators = list(teal::teal_transform_module())), "teal_module"
    )
  })

  it("creates a module that is not bookmarkable", {
    testthat::expect_true(attr(tm_data_table(), "teal_bookmarkable", exact = TRUE))
  })
})

testthat::describe("tm_data_tables input validation", {
  it("fails when label is not a string", {
    testthat::expect_error(tm_data_table(label = 123), "label")
    testthat::expect_error(tm_data_table(label = c("a", "b")), "label")
  })

  it("fails when variables_selected is not a named list of character vectors", {
    testthat::expect_error(tm_data_table(variables_selected = "not_a_list"), "variables_selected")
    testthat::expect_error(tm_data_table(variables_selected = list(123)), "variables_selected")
    testthat::expect_error(tm_data_table(variables_selected = list(c("A", "B"))), "variables_selected")
  })

  it("fails when variables_selected contains empty strings", {
    testthat::expect_error(
      tm_data_table(variables_selected = list(ADSL = c("STUDYID", ""))), "variables_selected"
    )
  })

  it("fails when datanames is not character", {
    testthat::expect_error(tm_data_table(datanames = 123), "datanames")
    testthat::expect_error(tm_data_table(datanames = list("ADSL")), "datanames")
  })

  it("fails when datanames contains empty strings", {
    testthat::expect_error(tm_data_table(datanames = c("ADSL", "")), "datanames")
  })

  it("fails when dt_args is not a list or contains invalid arguments", {
    testthat::expect_error(tm_data_table(dt_args = "not_a_list"), "dt_args")
    testthat::expect_error(tm_data_table(dt_args = list(invalid_arg = TRUE)), "dt_args")
  })

  it("succeeds with valid dt_args", {
    testthat::expect_s3_class(
      tm_data_table(dt_args = list(caption = "Test Table")), "teal_module"
    )
  })

  it("fails when dt_options is not a named list", {
    testthat::expect_error(tm_data_table(dt_options = "not_a_list"), "dt_options")
    testthat::expect_error(tm_data_table(dt_options = list("unnamed")), "dt_options")
  })

  it("succeeds with valid dt_options", {
    testthat::expect_s3_class(
      tm_data_table(dt_options = list(searching = TRUE, pageLength = 50)), "teal_module"
    )
  })

  it("fails when server_rendering is not a boolean", {
    testthat::expect_error(tm_data_table(server_rendering = "yes"), "server_rendering")
    testthat::expect_error(tm_data_table(server_rendering = 1), "server_rendering")
    testthat::expect_error(tm_data_table(server_rendering = NULL), "server_rendering")
  })

  it("succeeds when server_rendering is boolean", {
    testthat::expect_s3_class(tm_data_table(server_rendering = TRUE), "teal_module")
    testthat::expect_s3_class(tm_data_table(server_rendering = FALSE), "teal_module")
  })

  it("fails when pre_output is not a shiny.tag, shiny.tag.list, html or NULL", {
    testthat::expect_error(tm_data_table(pre_output = 123), "pre_output")
    testthat::expect_error(tm_data_table(pre_output = "text"), "pre_output")
  })

  it("succeeds with valid pre_output", {
    testthat::expect_s3_class(
      tm_data_table(pre_output = shiny::tags$div("Pre output")),
      "teal_module"
    )
    testthat::expect_s3_class(tm_data_table(pre_output = NULL), "teal_module")
  })

  it("fails when post_output is not a shiny.tag, shiny.tag.list, html or NULL", {
    testthat::expect_error(tm_data_table(post_output = 123), "post_output")
    testthat::expect_error(tm_data_table(post_output = "text"), "post_output")
  })

  it("succeeds with valid post_output", {
    testthat::expect_s3_class(
      tm_data_table(post_output = shiny::helpText("Help text")),
      "teal_module"
    )
    testthat::expect_s3_class(tm_data_table(post_output = NULL), "teal_module")
  })

  it("fails when transformators has invalid object types", {
    testthat::expect_error(
      tm_data_table(transformators = list("not a teal_transform_module")),
      "May only contain the following types: \\{teal_transform_module\\}"
    )
  })

  it("detects deprecated 'datasets_selected' argument", {
    lifecycle::expect_defunct(tm_data_table(datasets_selected = "ADSL"))
  })
})

testthat::describe("tm_data_table module ui behavior returns a htmltools tag or taglist", {
  it("with minimal arguments", {
    mod <- tm_data_table()
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    checkmate::expect_multi_class(ui, c("shiny.tag", "shiny.tag.list"))
  })
})


testthat::describe("tm_g_response module server behavior", {
  data <- within(teal.data::teal_data(), ADSL <- teal.data::rADSL)
  variables_selected <- list(ADSL = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"))
  set_shared_inputs <- function(session) {
    session$setInputs(
      "test-variables" = c("STUDYID", "USUBJID", "SUBJID", "SITEID", "AGE", "SEX"),
      if_filtered = TRUE,
      if_distinct = TRUE
    )
  }

  it("", {
    mod <- tm_data_table(variables_selected = variables_selected)
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(data))
      ),
      expr = {
        set_shared_inputs(session)
        session$flushReact()
        testthat::expect_s4_class(session$returned[["ADSL"]](), "teal_data")
      }
    )
  })
})
