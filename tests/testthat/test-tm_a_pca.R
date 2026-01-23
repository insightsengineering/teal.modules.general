testthat::describe("tm_a_pca module creation", {
  data <- within(teal.data::teal_data(), ADSL <- teal.data::rADSL)
  spec <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data = "ADSL", c("BMRKR1", "AGE", "EOSDY")),
      selected = c("BMRKR1", "AGE"),
      multiple = TRUE
    ),
    filter = NULL
  )
  it("creates a teal_module object with default options", {
    testthat::expect_s3_class(tm_a_pca(dat = spec), "teal_module")
  })

  it("creates a module with valid transformators", {
    testthat::expect_s3_class(
      tm_a_pca(
        dat = spec,
        transformators = list(teal::teal_transform_module())
      ),
      "teal_module"
    )
  })

  it("creates a module that is not bookmarkable", {
    testthat::expect_false(
      attr(tm_a_pca(dat = spec), "teal_bookmarkable", exact = TRUE)
    )
  })
})

testthat::describe("tm_a_pca input validation", {
  spec <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data = "ADSL", c("BMRKR1", "AGE", "EOSDY")),
      selected = c("BMRKR1", "AGE"),
      multiple = TRUE
    ),
    filter = NULL
  )

  it("fails when label is not a string", {
    testthat::expect_error(tm_a_pca(label = 123, dat = spec), "label")
  })

  it("fails when dat is not a list of data_extract_spec", {
    testthat::expect_error(tm_a_pca(dist_var = "not_a_spec"), "dist_var")
    testthat::expect_error(tm_a_pca(dist_var = list("not_a_spec")), "dist_var")
  })

  it("fails when dat has more than 1 dataset", {
    mod <- tm_a_pca(dat = list(spec, spec))

    testthat::expect_error(
      do.call(mod$ui, c(mod$ui_args, id = "test"), quote = TRUE),
      "more than one dataset in data_extract_spec"
    )
  })

  it("fails when `ggtheme` is not a valid choice", {
    testthat::expect_error(tm_a_pca(dat = spec, ggtheme = "not_a_theme"), "should be one of")
  })

  it("fails when `ggplot2_args` is not a list of ggplot2_args", {
    testthat::expect_error(
      tm_a_pca(dat = spec, ggplot2_args = list("not_ggplot2_args")),
      "ggplot2_args"
    )
    testthat::expect_error(
      tm_a_pca(dat = spec, ggplot2_args = list("Elbow plot" = "not_ggplot2_args")),
      "ggplot2_args"
    )
  })

  it("succeeds when `ggplot2_args` has supported parameters", {
    testthat::expect_s3_class(
      tm_a_pca(dat = spec, ggplot2_args = list(
        "default" = teal.widgets::ggplot2_args(),
        "Elbow plot" = teal.widgets::ggplot2_args(),
        "Circle plot" = teal.widgets::ggplot2_args(),
        "Biplot" = teal.widgets::ggplot2_args(),
        "Eigenvector plot" = teal.widgets::ggplot2_args()
      )),
      "teal_module"
    )
  })

  it("fails when `plot_height` is not numeric of length 3", {
    testthat::expect_error(tm_a_pca(dat = spec, plot_height = 100), "plot_height")
  })

  it("fails when plot_width is not numeric of length 3 or NULL", {
    testthat::expect_error(tm_a_pca(dat = spec, plot_width = 100), "plot_width")
  })

  it("fails when `rotate_xaxis_labels` is not boolean", {
    testthat::expect_error(tm_a_pca(dat = spec, rotate_xaxis_labels = 1), "rotate_xaxis_labels")
    testthat::expect_error(tm_a_pca(dat = spec, rotate_xaxis_labels = "text"), "rotate_xaxis_labels")
    testthat::expect_error(tm_a_pca(dat = spec, rotate_xaxis_labels = Sys.Date()), "rotate_xaxis_labels")
  })

  it("succeeds when `alpha` is between 0 and 1", {
    testthat::expect_s3_class(tm_a_pca(dat = spec, alpha = 1), "teal_module")
  })

  it("fails when `alpha` is not between 0 and 1", {
    testthat::expect_error(tm_a_pca(dat = spec, alpha = 2), "alpha")
    testthat::expect_error(tm_a_pca(dat = spec, alpha = -1), "alpha")
  })

  it("fails when `alpha` is not numeric of length 3 or NULL", {
    testthat::expect_error(tm_a_pca(dat = spec, alpha = c(5, 5), "alpha"))
    testthat::expect_error(tm_a_pca(dat = spec, alpha = c(5, 5, NULL), "alpha"))
    testthat::expect_error(tm_a_pca(dat = spec, alpha = c("a", "b", "c"), "alpha"))
  })

  it("succeeds when `size` is under 8", {
    testthat::expect_s3_class(tm_a_pca(dat = spec, size = 8), "teal_module")
  })

  it("fails when `size` is not below 8", {
    testthat::expect_error(tm_a_pca(dat = spec, size = 9), "size")
    testthat::expect_error(tm_a_pca(dat = spec, size = -1), "size")
  })

  it("fails when `size` is not numeric of length 3 or NULL", {
    testthat::expect_error(tm_a_pca(dat = spec, size = c(5, 5), "size"))
    testthat::expect_error(tm_a_pca(dat = spec, size = c(5, 5, NULL), "size"))
    testthat::expect_error(tm_a_pca(dat = spec, size = c("a", "b", "c"), "size"))
  })

  it("succeeds when `font_size` is 20", {
    testthat::expect_s3_class(tm_a_pca(dat = spec, font_size = 10), "teal_module")
  })

  it("fails when `font_size` is not below 20", {
    testthat::expect_error(tm_a_pca(dat = spec, font_size = 100), "font_size")
    testthat::expect_error(tm_a_pca(dat = spec, font_size = -1), "font_size")
  })

  it("fails when `font_size` is not numeric of length 3 or NULL", {
    testthat::expect_error(tm_a_pca(dat = spec, font_size = c(5, 5), "font_size"))
    testthat::expect_error(tm_a_pca(dat = spec, font_size = c(5, 5, NULL), "font_size"))
    testthat::expect_error(tm_a_pca(dat = spec, font_size = c("a", "b", "c"), "font_size"))
  })

  it("fails when pre_output is not a shiny.tag, shiny.tag.list, html or NULL", {
    testthat::expect_error(tm_a_pca(dat = spec, pre_output = 123), "pre_output")
  })

  it("fails when post_output is not a shiny.tag, shiny.tag.list, html or NULL", {
    testthat::expect_error(tm_a_pca(dat = spec, post_output = 123), "post_output")
  })

  it("fails when decorators is not a valid decorators list", {
    testthat::expect_error(
      tm_a_pca(dat = spec, decorators = list(foo = "bar")),
      "decorators"
    )
  })

  it("fails when transformators has invalid object types", {
    testthat::expect_error(
      tm_a_pca(dat = spec, transformators = list("not a teal_transform_module")),
      "May only contain the following types: \\{teal_transform_module\\}"
    )
  })
})

testthat::describe("tm_g_response module server behavior", {
  spec <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data = "ADSL", c("BMRKR1", "AGE", "EOSDY")),
      selected = c("BMRKR1", "AGE"),
      multiple = TRUE
    ),
    filter = NULL
  )
  data <- within(teal.data::teal_data(), ADSL <- teal.data::rADSL)
  teal.data::join_keys(data) <- teal.data::default_cdisc_join_keys["ADSL"]

  set_shared_inputs <- function(session, plot_type) {
    session$setInputs(
      "dat-dataset" = "ADSL",
      "dat-dataset_ADSL_singleextract-select" = c("AGE", "BMRKR1"),
      standardization = "center_scale",
      tables_display = c("importance", "eigenvector"),
      plot_type = plot_type,
      rotate_xaxis_labels = TRUE,
      na_action = "drop",
      alpha = 1,
      size = 10,
      font_size = 12,
      ggtheme = "gray"
    )
  }

  it("fails to execute with missing test", {
    mod <- tm_a_pca(dat = spec)
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(data))
      ),
      expr = {
        session$setInputs("dat-dataset" = "ADSL")
        session$flushReact()
        testthat::expect_error(session$returned())
      }
    )
  })

  it("returns teal_report object with valid inputs (elbow plot)", {
    mod <- tm_a_pca(dat = spec)
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(data))
      ),
      expr = {
        set_shared_inputs(session, "Plot that does not exist")
        session$flushReact()
        testthat::expect_error(session$returned(), "Unknown plot")
      }
    )
  })

  it("returns teal_report object with valid inputs (elbow plot)", {
    mod <- tm_a_pca(dat = spec)
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(data))
      ),
      expr = {
        set_shared_inputs(session, "Elbow plot")
        session$flushReact()
        testthat::expect_s4_class(session$returned(), "teal_report")
      }
    )
  })

  it("returns teal_report object with valid inputs (Circle plot)", {
    mod <- tm_a_pca(dat = spec)
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(data))
      ),
      expr = {
        set_shared_inputs(session, "Circle plot")
        session$setInputs(x_axis = "PC1", y_axis = "PC2", variables = c("AGE", "BMRKR1"))
        session$flushReact()
        testthat::expect_s4_class(session$returned(), "teal_report")
      }
    )
  })

  it("fails with only 1 variable (Circle plot)", {
    mod <- tm_a_pca(dat = spec)
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(data))
      ),
      expr = {
        set_shared_inputs(session, "Circle plot")
        session$setInputs("dat-dataset_ADSL_singleextract-select" = c("AGE"))
        session$flushReact()
        testthat::expect_error(session$returned(), "Please select more than 1 variable", fixed = TRUE)
      }
    )
  })

  it("fails with same x and y axis (Circle plot)", {
    mod <- tm_a_pca(dat = spec)
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(data))
      ),
      expr = {
        set_shared_inputs(session, "Circle plot")
        session$setInputs(x_axis = "PC1", y_axis = "PC1", variables = c("AGE", "BMRKR1"))
        session$flushReact()
        testthat::expect_error(session$returned(), "Please choose different X and Y axes.", fixed = TRUE)
      }
    )
  })

  it("returns teal_report object with valid inputs (Biplot)", {
    mod <- tm_a_pca(dat = spec)
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(data))
      ),
      expr = {
        set_shared_inputs(session, "Biplot")
        session$setInputs(x_axis = "PC1", y_axis = "PC2")
        session$flushReact()
        testthat::expect_s4_class(session$returned(), "teal_report")
      }
    )
  })

  it("returns teal_report object with no standardization (Biplot)", {
    mod <- tm_a_pca(dat = spec)
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(data))
      ),
      expr = {
        set_shared_inputs(session, "Biplot")
        session$setInputs(
          x_axis = "PC1",
          y_axis = "PC2",
          standardization = "none",
          variables = c("AGE", "BMRKR1"),
          "response-dataset_ADSL_singleextract-select" = "SEX"
        )
        session$flushReact()
        testthat::expect_s4_class(session$returned(), "teal_report")
      }
    )
  })

  it("returns teal_report object with valid inputs and response (Biplot)", {
    mod <- tm_a_pca(dat = spec)
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(data))
      ),
      expr = {
        set_shared_inputs(session, "Biplot")
        session$setInputs(
          x_axis = "PC1",
          y_axis = "PC2",
          variables = c("AGE", "BMRKR1"),
          "response-dataset_ADSL_singleextract-select" = "SEX"
        )
        session$flushReact()
        testthat::expect_s4_class(session$returned(), "teal_report")
      }
    )
  })

  it("returns teal_report object with valid inputs and non-factor response (Biplot)", {
    mod <- tm_a_pca(dat = spec)
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(data))
      ),
      expr = {
        set_shared_inputs(session, "Biplot")
        session$setInputs(
          x_axis = "PC1",
          y_axis = "PC2",
          variables = c("AGE", "BMRKR1"),
          "response-dataset_ADSL_singleextract-select" = "DTHADY"
        )
        session$flushReact()
        testthat::expect_s4_class(session$returned(), "teal_report")
      }
    )
  })

  it("returns teal_report object with valid inputs and Date response (Biplot)", {
    mod <- tm_a_pca(dat = spec)
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(within(data, ADSL$NEW_DATE <- as.Date(ADSL$DTHDT))))
      ),
      expr = {
        set_shared_inputs(session, "Biplot")
        session$setInputs(
          x_axis = "PC1",
          y_axis = "PC2",
          variables = c("AGE", "BMRKR1"),
          "response-dataset_ADSL_singleextract-select" = "LSTALVDT"
        )
        session$flushReact()
        testthat::expect_s4_class(session$returned(), "teal_report")
      }
    )
  })

  it("fails with response variable being used in PCA (Biplot)", {
    mod <- tm_a_pca(dat = spec)
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(within(data, ADSL$NEW_DATE <- as.Date(ADSL$DTHDT))))
      ),
      expr = {
        set_shared_inputs(session, "Biplot")
        session$setInputs(
          x_axis = "PC1",
          y_axis = "PC2",
          variables = c("AGE", "BMRKR1"),
          "response-dataset_ADSL_singleextract-select" = "AGE"
        )
        session$flushReact()
        testthat::expect_error(
          session$returned(),
          "Response must not have been used for PCA",
          fixed = TRUE
        )
      }
    )
  })

  it("returns teal_report object with valid inputs (Eigenvector plot)", {
    mod <- tm_a_pca(dat = spec)
    shiny::testServer(
      mod$server,
      args = c(
        mod$server_args,
        list(id = "test", data = reactive(data))
      ),
      expr = {
        set_shared_inputs(session, "Eigenvector plot")
        session$setInputs(pc = "PC1")
        session$flushReact()
        testthat::expect_s4_class(session$returned(), "teal_report")
      }
    )
  })
})

testthat::describe("tm_a_pca module ui behavior returns a htmltools tag or taglist", {
  spec <- data_extract_spec(
    dataname = "ADSL",
    select = select_spec(
      choices = variable_choices(data = "ADSL", c("BMRKR1", "AGE", "EOSDY")),
      selected = c("BMRKR1", "AGE"),
      multiple = TRUE
    ),
    filter = NULL
  )

  it("with minimal arguments", {
    mod <- tm_a_pca(dat = spec)
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    checkmate::expect_multi_class(ui, c("shiny.tag", "shiny.tag.list"))
  })
})
