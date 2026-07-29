describe("tests for module creation", {
  data <- teal_data()
  data <- within(data, {
    CO2 <- CO2 # nolint: object_name_linter.
  })

  response <- teal.picks::picks(
    teal.picks::datasets("CO2", "CO2"),
    teal.picks::variables("uptake", "uptake")
  )
  regressor <- teal.picks::picks(
    teal.picks::datasets("CO2", "CO2"),
    teal.picks::variables(c("conc", "Treatment"), "conc", multiple = TRUE)
  )

  it("works with default arguments", {
    mod <- tm_a_regression(
      response = response,
      regressor = regressor
    )
    testthat::expect_s3_class(mod, "teal_module")
  })

  it("works after setting plot arguments", {
    mod <- tm_a_regression(
      response = response,
      regressor = regressor,
      plot_height = c(500, 200, 1800),
      plot_width = c(500, 200, 1800),
      alpha = c(0.1, 0, 0.2),
      ggtheme = "bw"
    )
    testthat::expect_s3_class(mod, "teal_module")
  })

  it("works when setting ggplot2_args", {
    mod <- tm_a_regression(
      response = response,
      regressor = regressor,
      ggplot2_args = teal.widgets::ggplot2_args(
        labs = list(title = "User default title"),
        theme = list(legend.position = "right", legend.direction = "vertical")
      )
    )

    testthat::expect_s3_class(mod, "teal_module")
  })

  it("works with pre_output", {
    pre_output <- shiny::actionButton("pre_output", "My pre output")
    default_mod <- tm_a_regression(
      response = response,
      regressor = regressor
    )
    mod_pre_output <- tm_a_regression(
      response = response,
      regressor = regressor,
      pre_output = pre_output
    )

    testthat::expect_null(default_mod$ui_args$pre_output)
    testthat::expect_equal(mod_pre_output$ui_args$pre_output, pre_output)
  })

  it("works with post_output", {
    post_output <- shiny::actionButton("post_output", "My post output")
    default_mod <- tm_a_regression(
      response = response,
      regressor = regressor
    )
    mod_post_output <- tm_a_regression(
      response = response,
      regressor = regressor,
      post_output = post_output
    )

    testthat::expect_null(default_mod$ui_args$post_output)
    testthat::expect_equal(mod_post_output$ui_args$post_output, post_output)
  })

  it("accepts a transformator", {
    transformator_iris <- teal_transform_module(
      label = "Custom transformator for iris",
      ui = function(id) {
        ns <- NS(id)
        tags$div(
          numericInput(ns("n_rows"), "Number of rows to display", value = 6, min = 1, max = 150, step = 1)
        )
      },
      server = function(id, data) {
        moduleServer(id, function(input, output, session) {
          reactive({
            within(
              data(),
              iris <- head(iris, num_rows),
              num_rows = input$n_rows
            )
          })
        })
      }
    )

    mod <- tm_a_regression(
      response = response,
      regressor = regressor,
      transformators = list(t1 = transformator_iris)
    )

    testthat::expect_s3_class(mod, "teal_module")
  })

  it("accepts a decorator", {
    ggplot_caption_decorator <- function(default_caption = "I am a good decorator") {
      teal::teal_transform_module(
        label = "Caption",
        ui = function(id) {
          shiny::textInput(shiny::NS(id, "footnote"), "Footnote", value = default_caption)
        },
        server = function(id, data) {
          moduleServer(id, function(input, output, session) {
            reactive({
              within(data(),
                {
                  plot <- plot + ggplot2::labs(caption = footnote)
                },
                footnote = input$footnote
              )
            })
          })
        }
      )
    }

    mod <- tm_a_regression(
      response = response,
      regressor = regressor,
      decorators = list(all = ggplot_caption_decorator())
    )

    testthat::expect_s3_class(mod, "teal_module")
  })

  it("ui is of the expected type", {
    mod <- tm_a_regression(
      response = response,
      regressor = regressor
    )
    ui_regression <- do.call(mod$ui, c(list(id = "test"), mod$ui_args))

    testthat::expect_s3_class(ui_regression, "shiny.tag.list")
  })
})

describe("Test for invalidation of arguments", {
  data <- teal_data()
  data <- within(data, {
    CO2 <- CO2 # nolint: object_name_linter.
  })

  response <- teal.picks::picks(
    teal.picks::datasets("CO2", "CO2"),
    teal.picks::variables("uptake", "uptake")
  )
  regressor <- teal.picks::picks(
    teal.picks::datasets("CO2", "CO2"),
    teal.picks::variables(c("conc", "Treatment"), "conc", multiple = TRUE)
  )

  it("fails if label is not the expected type", {
    testthat::expect_error(
      tm_a_regression(response = response, regressor = regressor, label = 123),
      "Assertion on 'label' failed"
    )
  })

  it("fails if regressor is not the expected type", {
    testthat::expect_error(
      tm_a_regression(response = response, regressor = list(my_data_spec = character())),
      "Assertion on 'regressor' failed"
    )
  })

  it("fails if response is not the expected type", {
    testthat::expect_error(
      tm_a_regression(response = list(my_data_spec = character()), regressor = regressor),
      "Assertion on 'response' failed"
    )
  })

  it("fails if alpha is not the expected type", {
    testthat::expect_error(
      tm_a_regression(response = response, regressor = regressor, alpha = "wrong type"),
      "Assertion on 'alpha' failed"
    )
  })

  it("fails if size is not the expected type", {
    testthat::expect_error(
      tm_a_regression(response = response, regressor = regressor, size = "wrong type"),
      "Assertion on 'size' failed"
    )
  })

  it("fails if label_segment_threshold is not the expected type", {
    testthat::expect_error(
      tm_a_regression(
        response = response, regressor = regressor, label_segment_threshold = "wrong type"
      ),
      "Assertion on 'label_segment_threshold' failed"
    )
  })
})

describe("Test for server function", {
  data <- teal_data()
  data <- within(data, {
    CO2 <- CO2 # nolint: object_name_linter.
  })

  response <- teal.picks::picks(
    teal.picks::datasets("CO2", "CO2"),
    teal.picks::variables("uptake", "uptake")
  )
  regressor <- teal.picks::picks(
    teal.picks::datasets("CO2", "CO2"),
    teal.picks::variables(c("conc", "Treatment"), "conc", multiple = TRUE)
  )

  set_default_args <- function(session, plot_type, selectors, resp_val = "uptake", regr_val = "Treatment") {
    resp_sel <- selectors$response()
    regr_sel <- selectors$regressor()
    resp_sel$variables$selected <- resp_val
    regr_sel$variables$selected <- regr_val
    selectors$response(resp_sel)
    selectors$regressor(regr_sel)

    session$setInputs(
      "plot_type" = plot_type,
      "show_outlier" = FALSE,
      "ggtheme" = "gray",
      "alpha" = 0.1,
      "size" = 3
    )
  }

  it("server executes without error with default arguments", {
    mod <- tm_a_regression(response = response, regressor = regressor)
    shiny::testServer(
      mod$server,
      args = c(list(id = "test", data = shiny::reactive(data)), mod$server_args),
      expr = {
        set_default_args(session, "Response vs Regressor", selectors)
        session$flushReact()
        testthat::expect_s4_class(validated_q(), "teal_data")
        if (!isTRUE(as.logical(Sys.getenv("R_COVR", "FALSE")))) {
          output_result <- output_q()
          testthat::expect_s4_class(output_result, "teal_data")
          plot_result <- plot_r()
          testthat::expect_s3_class(plot_result, "ggplot")
        }
      }
    )
  })

  it("server executes without error with for Residuals vs Leverage", {
    mod <- tm_a_regression(response = response, regressor = regressor)
    shiny::testServer(
      mod$server,
      args = c(list(id = "test", data = shiny::reactive(data)), mod$server_args),
      expr = {
        set_default_args(session, "Residuals vs Leverage", selectors)
        session$flushReact()
        testthat::expect_s4_class(validated_q(), "teal_data")
        if (!isTRUE(as.logical(Sys.getenv("R_COVR", "FALSE")))) {
          output_result <- output_q()
          testthat::expect_s4_class(output_result, "teal_data")
          plot_result <- plot_r()
          testthat::expect_s3_class(plot_result, "ggplot")
        }
      }
    )
  })

  it("server executes without error with for Residuals vs Fitted", {
    mod <- tm_a_regression(response = response, regressor = regressor)
    shiny::testServer(
      mod$server,
      args = c(list(id = "test", data = shiny::reactive(data)), mod$server_args),
      expr = {
        set_default_args(session, "Residuals vs Fitted", selectors)
        session$flushReact()
        testthat::expect_s4_class(validated_q(), "teal_data")
        if (!isTRUE(as.logical(Sys.getenv("R_COVR", "FALSE")))) {
          output_result <- output_q()
          testthat::expect_s4_class(output_result, "teal_data")
          plot_result <- plot_r()
          testthat::expect_s3_class(plot_result, "ggplot")
        }
      }
    )
  })

  it("server executes without error with for Scale-Location", {
    mod <- tm_a_regression(response = response, regressor = regressor)
    shiny::testServer(
      mod$server,
      args = c(list(id = "test", data = shiny::reactive(data)), mod$server_args),
      expr = {
        set_default_args(session, "Scale-Location", selectors)
        session$flushReact()
        testthat::expect_s4_class(validated_q(), "teal_data")
        if (!isTRUE(as.logical(Sys.getenv("R_COVR", "FALSE")))) {
          output_result <- output_q()
          testthat::expect_s4_class(output_result, "teal_data")
          plot_result <- plot_r()
          testthat::expect_s3_class(plot_result, "ggplot")
        }
      }
    )
  })

  it("server executes without error with for Cook's distance", {
    mod <- tm_a_regression(response = response, regressor = regressor)
    shiny::testServer(
      mod$server,
      args = c(list(id = "test", data = shiny::reactive(data)), mod$server_args),
      expr = {
        set_default_args(session, "Cook's distance", selectors)
        session$flushReact()
        testthat::expect_s4_class(validated_q(), "teal_data")
        if (!isTRUE(as.logical(Sys.getenv("R_COVR", "FALSE")))) {
          output_result <- output_q()
          testthat::expect_s4_class(output_result, "teal_data")
          plot_result <- plot_r()
          testthat::expect_s3_class(plot_result, "ggplot")
        }
      }
    )
  })

  it("server executes without error with for Normal Q-Q", {
    mod <- tm_a_regression(response = response, regressor = regressor)
    shiny::testServer(
      mod$server,
      args = c(list(id = "test", data = shiny::reactive(data)), mod$server_args),
      expr = {
        set_default_args(session, "Normal Q-Q", selectors)
        session$flushReact()
        testthat::expect_s4_class(validated_q(), "teal_data")
        if (!isTRUE(as.logical(Sys.getenv("R_COVR", "FALSE")))) {
          output_result <- output_q()
          testthat::expect_s4_class(output_result, "teal_data")
          plot_result <- plot_r()
          testthat::expect_s3_class(plot_result, "ggplot")
        }
      }
    )
  })

  it("server executes without error with for Cook's dist vs Leverage", {
    mod <- tm_a_regression(response = response, regressor = regressor)
    shiny::testServer(
      mod$server,
      args = c(list(id = "test", data = shiny::reactive(data)), mod$server_args),
      expr = {
        set_default_args(session, "Cook's dist vs Leverage", selectors)
        session$flushReact()
        testthat::expect_s4_class(validated_q(), "teal_data")
        if (!isTRUE(as.logical(Sys.getenv("R_COVR", "FALSE")))) {
          output_result <- output_q()
          testthat::expect_s4_class(output_result, "teal_data")
          plot_result <- plot_r()
          testthat::expect_s3_class(plot_result, "ggplot")
        }
      }
    )
  })

  it("server has an error if data is not reactive", {
    mod <- tm_a_regression(response = response, regressor = regressor)
    non_reactive_data <- within(teal_data(), {
      CO2 <- CO2 # nolint: object_name_linter.
    })
    testthat::expect_error(
      shiny::testServer(
        mod$server,
        args = c(list(id = "test", data = non_reactive_data), mod$server_args),
        expr = {
          set_default_args(session, "Response vs Regressor", selectors, "uptake", "conc")
          session$flushReact()
        }
      ),
      "Assertion on 'data' failed"
    )
  })

  it("server throws error if response and regressor are the same variable", {
    mod <- tm_a_regression(response = response, regressor = regressor)
    testthat::expect_error(
      shiny::testServer(
        mod$server,
        args = c(list(id = "test", data = shiny::reactive(data)), mod$server_args),
        expr = {
          set_default_args(session, "Response vs Regressor", selectors, "uptake", "uptake")
          session$flushReact()
          validated_q()
        }
      ),
      "Response and Regressor must be different"
    )
  })

  it("server throws error if regressor has two choices for Response vs Regressor", {
    mod <- tm_a_regression(response = response, regressor = regressor)
    testthat::expect_error(
      shiny::testServer(
        mod$server,
        args = c(list(id = "test", data = shiny::reactive(data)), mod$server_args),
        expr = {
          set_default_args(session, "Response vs Regressor", selectors, "uptake", c("conc", "Treatment"))
          session$flushReact()
          output_q()
        }
      ),
      "This plot works only with single Regressor variable"
    )
  })
})
