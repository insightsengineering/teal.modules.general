create_dummy_module_data <- function() {
  teal_data()
  data <- within(data, {
    require(nestcolor)

    add_nas <- function(x) {
      x[sample(seq_along(x), floor(length(x) * runif(1, .05, .17)))] <- NA
      x
    }

  iris <- iris
  mtcars <- mtcars

  iris[] <- lapply(iris, add_nas)
  mtcars[] <- lapply(mtcars, add_nas)
  mtcars[["cyl"]] <- as.factor(mtcars[["cyl"]])
  mtcars[["gear"]] <- as.factor(mtcars[["gear"]])
  })
}

testthat::describe("tm_missing_data module creation", {
  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_missing_data(
        label = "Missing Data",
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with pre_output", {
    pre_output <- shiny::actionButton("pre_output", "My pre output")
    default_mod <- tm_missing_data(
        label = "Missing Data"
    )
    testthat::expect_null(default_mod$ui_args$pre_output)

    pre_output_mod <- tm_missing_data(
        label = "Missing Data",
        pre_output = pre_output
    )
    testthat::expect_equal(pre_output_mod$ui_args$pre_output, pre_output)
  })

  it("creates a teal_module object with post_output", {
    post_output <- shiny::actionButton("post_output", "My post output")
    default_mod <- tm_missing_data(
        label = "Missing Data"
    )
    testthat::expect_null(default_mod$ui_args$post_output)

    post_output_mod <- tm_missing_data(
        label = "Missing Data",
        post_output = post_output
    )
    testthat::expect_equal(post_output_mod$ui_args$post_output, post_output)
  })

  it("accepts valid ggtheme_theme options", {
    for (theme in c("gray", "bw", "linedraw", "light", "dark", "minimal", "classic", "void")) {
      testthat::expect_s3_class(
        tm_missing_data(
          label = "Missing Data",
          ggtheme = theme
        ),
        "teal_module"
      )
    }
  })

  it("accepts a decorator", {
    testthat::expect_s3_class(
      tm_missing_data(
        decorators = list(
          summary_plot = teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
  })

  it("accepts a transformator", {
    testthat::expect_s3_class(
      tm_missing_data(
        transformators = list(
          teal::teal_transform_module()
        )
      ),
      "teal_module"
    )
  })
})

testthat::describe("tm_missing_data input_validation", {
  it("fails when plot height is not valid", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        plot_height = c(100, 200, 300) # min > max
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot_height has wrong length", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        plot_height = c(600, 200)
      ),
      "Assertion on 'plot_height' failed"
    )
  })

  it("fails when plot width is not valid", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        plot_width = c(100, 200, 300) # min > max
      ),
      "Assertion on 'plot_width' failed"
    )
  })

  it("fails when datanames is wrong type", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        datanames = FALSE
      ),
      "Assertion on 'datanames' failed"
    )
  })

  it("fails when ggtheme is invalid name", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        ggtheme = "invalid"
      ),
      "should be one of"
    )
  })

  it("fails when ggplot_args", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        ggplot2_args = list("wrong ggplot2_args", "wrong_ggplot2_args"),
      ),
      "Assertion on 'ggplot2_args' failed"
    )
  })

  it("fails if decorator has not valid type", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        decorator = list(
          summary_plot = "wrong teal_transform_module"
        )
      ),
      "Assertion on 'decorators' failed"
    )
  })

  it("fails if transformator has not valid type", {
    testthat::expect_error(
      tm_missing_data(
        label = "Missing Data",
        transformator = list(
          "wrong teal_transform_module"
        )
      ),
      "Assertion on 'transformators' failed"
    )
  })
})

testthat::describe("tm_missing_data using single or all datasets ui functions", {
  it("uses diferent functions in single or all datasets", {
    mod_single <- tm_missing_data(datanames = "ADSL")
    mod_all <-  tm_missing_data()

    expect_false(as.character(mod_single$ui("test")) == as.character(mod_all$ui("test")))
  })
})


testthat::describe("tm_missing_data server with different datanames", {
  it("handles single dataset correctly", {
    data <- within(teal_data(), {
      iris <- iris
      iris$Sepal.Length[1:10] <- NA
    })
    datanames(data) <- "iris"
    
    shiny::testServer(
      app = srv_page_missing_data,
      args = list(
        data = reactive(data),
        datanames = "iris",
        parent_dataname = character(0),
        plot_height = c(600, 400, 5000),
        plot_width = NULL,
        ggplot2_args = list(),
        ggtheme = "gray",
        decorators = list()
      ),
      expr = {
        session$flushReact()
        testthat::expect_true(!is.null(output$dataset_tabs))
        testthat::expect_true(!is.null(output$dataset_encodings))
      }
    )
  })
  
  it("handles multiple datasets correctly", {
    data <- within(teal_data(), {
      iris <- iris
      mtcars <- mtcars
      iris$Sepal.Length[1:10] <- NA
      mtcars$mpg[1:5] <- NA
    })
    datanames(data) <- c("iris", "mtcars")
    
    shiny::testServer(
      app = srv_page_missing_data,
      args = list(
        data = reactive(data),
        datanames = c("iris", "mtcars"),
        parent_dataname = character(0),
        plot_height = c(600, 400, 5000),
        plot_width = NULL,
        ggplot2_args = list(),
        ggtheme = "gray",
        decorators = list()
      ),
      expr = {
        session$flushReact()
        testthat::expect_true(!is.null(output$dataset_tabs))
        testthat::expect_true(!is.null(output$dataset_encodings))
      }
    )
  })
  
  it("handles 'all' datanames correctly", {
    data <- within(teal_data(), {
      iris <- iris
      mtcars <- mtcars
      iris$Sepal.Length[1:10] <- NA
      mtcars$mpg[1:5] <- NA
    })
    datanames(data) <- c("iris", "mtcars")
    
    shiny::testServer(
      app = srv_page_missing_data,
      args = list(
        data = reactive(data),
        datanames = "all",
        parent_dataname = character(0),
        plot_height = c(600, 400, 5000),
        plot_width = NULL,
        ggplot2_args = list(),
        ggtheme = "gray",
        decorators = list()
      ),
      expr = {
        session$flushReact()
        testthat::expect_true(!is.null(output$dataset_tabs))
        testthat::expect_true(!is.null(output$dataset_encodings))
      }
    )
  })
  
  it("filters non-data.frame objects when datanames='all'", {
    data <- within(teal_data(), {
      iris <- iris
      mtcars <- mtcars
      my_vector <- 1:10  # Not a data.frame
      iris$Sepal.Length[1:10] <- NA
    })
    datanames(data) <- c("iris", "mtcars", "my_vector")
    
    shiny::testServer(
      app = srv_page_missing_data,
      args = list(
        data = reactive(data),
        datanames = "all",
        parent_dataname = character(0),
        plot_height = c(600, 400, 5000),
        plot_width = NULL,
        ggplot2_args = list(),
        ggtheme = "gray",
        decorators = list()
      ),
      expr = {
        session$flushReact()
        tabs_html <- as.character(output$dataset_tabs)
        
        testthat::expect_true(any(grepl("iris", tabs_html)))
        testthat::expect_true(any(grepl("mtcars", tabs_html)))
        
        testthat::expect_false(all(grepl("my_vector", tabs_html)))
      }
    )
  })
})
