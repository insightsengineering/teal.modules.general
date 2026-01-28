testthat::describe("tm_file_viewer module creation", {
  it("creates a teal_module object", {
    testthat::expect_s3_class(
      tm_file_viewer(
        label = "File Viewer",
        input_path = list("Current Working Directory" = ".")
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with default options", {
    testthat::expect_s3_class(
      tm_file_viewer(),
      "teal_module"
    )
  })

  it("creates a teal_module object with character vector input_path", {
    test_path <- system.file("sample_files", package = "teal.modules.general")
    testthat::expect_s3_class(
      tm_file_viewer(
        input_path = c(folder = test_path)
      ),
      "teal_module"
    )
  })

  it("creates a teal_module object with multiple paths", {
    test_path <- system.file("sample_files", package = "teal.modules.general")
    testthat::expect_s3_class(
      tm_file_viewer(
        input_path = list(
          folder = test_path,
          png = system.file("sample_files/sample_file.png", package = "teal.modules.general")
        )
      ),
      "teal_module"
    )
  })

  it("creates a module that is not bookmarkable", {
    mod <- tm_file_viewer()
    testthat::expect_false(attr(mod, "teal_bookmarkable"))
  })

  it("creates a module with NULL datanames", {
    mod <- tm_file_viewer()
    testthat::expect_null(mod$datanames)
  })
})

testthat::describe("tm_file_viewer input validation", {
  it("fails when label is not a string", {
    testthat::expect_error(
      tm_file_viewer(label = 123),
      "Assertion on 'label' failed"
    )
  })

  it("fails when input_path is not a list or character", {
    testthat::expect_error(
      tm_file_viewer(input_path = 123),
      "Assertion failed"
    )
  })

  it("handles empty label by converting to space", {
    mod <- tm_file_viewer(label = "")
    testthat::expect_s3_class(mod, "teal_module")
    testthat::expect_equal(mod$ui_args[["label"]], " ")
  })

  it("handles empty input_path by converting to empty list", {
    mod <- tm_file_viewer(input_path = "")
    testthat::expect_s3_class(mod, "teal_module")
    testthat::expect_equal(mod$ui_args[["input_path"]], list())
  })

  it("handles empty list input_path", {
    mod <- tm_file_viewer(input_path = list())
    testthat::expect_s3_class(mod, "teal_module")
    testthat::expect_equal(mod$ui_args[["input_path"]], list())
  })

  it("warns when input_path contains non-existent files", {
    testthat::expect_warning(
      tm_file_viewer(input_path = list(nonexistent = "/path/that/does/not/exist")),
      "Non-existent file or url path"
    )
  })

  it("warns when input_path is empty", {
    testthat::expect_warning(
      tm_file_viewer(input_path = list()),
      "No file or url paths were provided"
    )
  })
})

testthat::describe("tm_file_viewer module ui behavior", {
  it("returns a shiny.tag or shiny.tag.list", {
    mod <- tm_file_viewer()
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    testthat::expect_true(inherits(ui, c("shiny.tag", "shiny.tag.list")))
  })

  it("includes tree output in UI", {
    mod <- tm_file_viewer()
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    ui_str <- as.character(ui)
    testthat::expect_match(ui_str, "tree", ignore.case = TRUE)
  })

  it("includes output div in UI", {
    mod <- tm_file_viewer()
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    ui_str <- as.character(ui)
    testthat::expect_match(ui_str, "output", ignore.case = TRUE)
  })
})

testthat::describe("tm_file_viewer module server behavior", {
  it("server function requires file selection before displaying output", {
    test_path <- system.file("sample_files/sample_file.txt", package = "teal.modules.general")
    mod <- tm_file_viewer(input_path = list(txt = test_path))

    shiny::testServer(
      app = mod$server,
      args = c(list(id = "test"), mod$server_args),
      expr = {
        testthat::expect_error(
          output$output(),
          "Please select a file"
        )
      }
    )
  })

  it("server function initializes with empty input_path and renders empty tree", {
    mod <- suppressWarnings(tm_file_viewer(input_path = list()))

    shiny::testServer(
      app = mod$server,
      args = c(list(id = "test"), mod$server_args),
      expr = {
        tree_output <- output$tree()
        testthat::expect_type(tree_output, "list")
        testthat::expect_error(
          output$output(),
          "Please select a file"
        )
      }
    )
  })

  it("server function initializes with directory path and renders tree", {
    test_path <- system.file("sample_files", package = "teal.modules.general")
    mod <- tm_file_viewer(input_path = list(folder = test_path))

    shiny::testServer(
      app = mod$server,
      args = c(list(id = "test"), mod$server_args),
      expr = {
        tree_output <- output$tree()
        testthat::expect_type(tree_output, "list")
        testthat::expect_error(
          output$output(),
          "Please select a file"
        )
      }
    )
  })

  it("server function initializes with image file path and renders tree", {
    test_path <- system.file("sample_files/sample_file.png", package = "teal.modules.general")
    mod <- tm_file_viewer(input_path = list(png = test_path))

    shiny::testServer(
      app = mod$server,
      args = c(list(id = "test"), mod$server_args),
      expr = {
        tree_output <- output$tree()
        testthat::expect_type(tree_output, "list")
        testthat::expect_error(
          output$output(),
          "Please select a file"
        )
      }
    )
  })

  it("server function initializes with Rmd file and renders tree", {
    test_path <- system.file("sample_files/co2_example.Rmd", package = "teal.modules.general")
    if (file.exists(test_path)) {
      mod <- tm_file_viewer(input_path = list(rmd = test_path))

      shiny::testServer(
        app = mod$server,
        args = c(list(id = "test"), mod$server_args),
        expr = {
          tree_output <- output$tree()
          testthat::expect_type(tree_output, "list")
          testthat::expect_error(
            output$output(),
            "Please select a file"
          )
        }
      )
    }
  })
})

testthat::describe("tm_file_viewer input_path validation", {
  it("handles character vector with single path", {
    test_path <- system.file("sample_files", package = "teal.modules.general")
    mod <- tm_file_viewer(input_path = test_path)
    testthat::expect_s3_class(mod, "teal_module")
  })

  it("handles character vector with multiple paths", {
    test_path <- system.file("sample_files", package = "teal.modules.general")
    txt_path <- system.file("sample_files/sample_file.txt", package = "teal.modules.general")
    mod <- tm_file_viewer(input_path = c(test_path, txt_path))
    testthat::expect_s3_class(mod, "teal_module")
  })

  it("handles zero-length character input_path", {
    mod <- suppressWarnings(tm_file_viewer(input_path = character(0)))
    testthat::expect_s3_class(mod, "teal_module")
    testthat::expect_equal(mod$ui_args[["input_path"]], list())
  })

  it("filters out non-existent paths and keeps valid ones", {
    test_path <- system.file("sample_files/sample_file.txt", package = "teal.modules.general")
    mod <- suppressWarnings(
      tm_file_viewer(
        input_path = list(
          valid = test_path,
          invalid = "/nonexistent/path/file.txt"
        )
      )
    )
    testthat::expect_s3_class(mod, "teal_module")
    testthat::expect_length(mod$server_args$input_path, 1)
  })

  it("handles all non-existent paths", {
    mod <- suppressWarnings(
      tm_file_viewer(
        input_path = list(
          invalid1 = "/nonexistent/path1/file.txt",
          invalid2 = "/nonexistent/path2/file.txt"
        )
      )
    )
    testthat::expect_s3_class(mod, "teal_module")
    testthat::expect_length(mod$server_args$input_path, 0)
  })
})

testthat::describe("tm_file_viewer UI structure", {
  it("includes encoding section with tree", {
    mod <- tm_file_viewer()
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    ui_str <- as.character(ui)
    testthat::expect_match(ui_str, "Encodings", ignore.case = TRUE)
    testthat::expect_match(ui_str, "shinyTree", ignore.case = TRUE)
  })

  it("includes standard layout structure", {
    mod <- tm_file_viewer()
    ui <- do.call(what = mod$ui, args = c(mod$ui_args, list(id = "test")), quote = TRUE)
    ui_str <- as.character(ui)
    testthat::expect_match(ui_str, "output|encoding", ignore.case = TRUE)
  })
})

testthat::describe("tm_file_viewer URL handling", {
  it("handles valid URL in input_path", {
    test_url <- "https://www.r-project.org/"
    mod <- suppressWarnings(tm_file_viewer(input_path = list(r_project = test_url)))
    testthat::expect_s3_class(mod, "teal_module")
    if (length(mod$server_args$input_path) > 0) {
      testthat::expect_true(any(grepl("r-project", mod$server_args$input_path)))
    }
  })

  it("warns when URL is invalid", {
    testthat::expect_warning(
      tm_file_viewer(input_path = list(invalid_url = "https://nonexistent-domain-12345.com/file.txt")),
      "Non-existent file or url path"
    )
  })

  it("handles mix of valid files and URLs", {
    test_path <- system.file("sample_files/sample_file.txt", package = "teal.modules.general")
    test_url <- "https://www.r-project.org/"
    mod <- suppressWarnings(
      tm_file_viewer(
        input_path = list(
          file = test_path,
          url = test_url
        )
      )
    )
    testthat::expect_s3_class(mod, "teal_module")
    testthat::expect_true(any(grepl("sample_file.txt", mod$server_args$input_path)))
  })
})

testthat::describe("tm_file_viewer server file selection and display", {
  it("validates that file selection is required", {
    test_path <- system.file("sample_files/sample_file.txt", package = "teal.modules.general")
    mod <- tm_file_viewer(input_path = list(txt = test_path))

    shiny::testServer(
      app = mod$server,
      args = c(list(id = "test"), mod$server_args),
      expr = {
        testthat::expect_error(
          output$output(),
          "Please select a file"
        )
      }
    )
  })
})

testthat::describe("tm_file_viewer tree structure", {
  it("server renders tree structure for directory containing files", {
    test_path <- system.file("sample_files", package = "teal.modules.general")
    mod <- tm_file_viewer(input_path = list(folder = test_path))

    shiny::testServer(
      app = mod$server,
      args = c(list(id = "test"), mod$server_args),
      expr = {
        tree_output <- output$tree()
        testthat::expect_type(tree_output, "list")
        testthat::expect_error(
          output$output(),
          "Please select a file"
        )
      }
    )
  })

  it("server renders empty tree structure for empty input_path", {
    mod <- suppressWarnings(tm_file_viewer(input_path = list()))

    shiny::testServer(
      app = mod$server,
      args = c(list(id = "test"), mod$server_args),
      expr = {
        tree_output <- output$tree()
        testthat::expect_type(tree_output, "list")
        testthat::expect_error(
          output$output(),
          "Please select a file"
        )
      }
    )
  })

  it("server renders tree structure for single file", {
    test_path <- system.file("sample_files/sample_file.txt", package = "teal.modules.general")
    mod <- tm_file_viewer(input_path = list(txt = test_path))

    shiny::testServer(
      app = mod$server,
      args = c(list(id = "test"), mod$server_args),
      expr = {
        tree_output <- output$tree()
        testthat::expect_type(tree_output, "list")
        testthat::expect_error(
          output$output(),
          "Please select a file"
        )
      }
    )
  })

  it("server renders tree structure for multiple files", {
    test_path <- system.file("sample_files", package = "teal.modules.general")
    txt_path <- system.file("sample_files/sample_file.txt", package = "teal.modules.general")
    png_path <- system.file("sample_files/sample_file.png", package = "teal.modules.general")
    mod <- tm_file_viewer(input_path = list(txt = txt_path, png = png_path))

    shiny::testServer(
      app = mod$server,
      args = c(list(id = "test"), mod$server_args),
      expr = {
        tree_output <- output$tree()
        testthat::expect_type(tree_output, "list")
        testthat::expect_error(
          output$output(),
          "Please select a file"
        )
      }
    )
  })
})

testthat::describe("tm_file_viewer path resolution", {
  it("handles paths with special characters in names", {
    temp_dir <- tempfile()
    dir.create(temp_dir, recursive = TRUE)
    on.exit(unlink(temp_dir, recursive = TRUE))

    special_file <- file.path(temp_dir, "file with spaces.txt")
    writeLines("test content", special_file)

    mod <- tm_file_viewer(input_path = list(special = special_file))

    shiny::testServer(
      app = mod$server,
      args = c(list(id = "test"), mod$server_args),
      expr = {
        testthat::expect_error(
          output$output(),
          "Please select a file"
        )
      }
    )
  })

  it("handles relative paths correctly", {
    mod <- tm_file_viewer(input_path = list(current = "."))

    shiny::testServer(
      app = mod$server,
      args = c(list(id = "test"), mod$server_args),
      expr = {
        testthat::expect_error(
          output$output(),
          "Please select a file"
        )
      }
    )
  })

  it("normalizes paths correctly for file operations", {
    test_path <- system.file("sample_files/sample_file.txt", package = "teal.modules.general")
    mod <- tm_file_viewer(input_path = list(txt = test_path))

    shiny::testServer(
      app = mod$server,
      args = c(list(id = "test"), mod$server_args),
      expr = {
        testthat::expect_error(
          output$output(),
          "Please select a file"
        )
      }
    )
  })
})

testthat::describe("tm_file_viewer edge cases", {
  it("handles character(0) label correctly", {
    mod <- tm_file_viewer(label = character(0))
    testthat::expect_s3_class(mod, "teal_module")
    testthat::expect_equal(mod$label, " ")
  })

  it("handles input_path with unnamed elements", {
    test_path <- system.file("sample_files", package = "teal.modules.general")
    mod <- tm_file_viewer(input_path = list(test_path))
    testthat::expect_s3_class(mod, "teal_module")
  })

  it("handles input_path with mixed named and unnamed elements", {
    test_path <- system.file("sample_files", package = "teal.modules.general")
    txt_path <- system.file("sample_files/sample_file.txt", package = "teal.modules.general")
    mod <- tm_file_viewer(input_path = list(folder = test_path, txt_path))
    testthat::expect_s3_class(mod, "teal_module")
  })

  it("handles very long file paths", {
    temp_base <- tempfile()
    dir.create(temp_base, recursive = TRUE)
    on.exit(unlink(temp_base, recursive = TRUE))

    nested_path <- temp_base
    for (i in 1:5) {
      nested_path <- file.path(nested_path, paste0("level", i))
      dir.create(nested_path, recursive = TRUE)
    }

    test_file <- file.path(nested_path, "test.txt")
    writeLines("test", test_file)

    mod <- tm_file_viewer(input_path = list(nested = test_file))

    shiny::testServer(
      app = mod$server,
      args = c(list(id = "test"), mod$server_args),
      expr = {
        testthat::expect_error(
          output$output(),
          "Please select a file"
        )
      }
    )
  })

  it("handles nested directory structure in tree", {
    temp_base <- tempfile()
    dir.create(temp_base, recursive = TRUE)
    on.exit(unlink(temp_base, recursive = TRUE))

    level1 <- file.path(temp_base, "level1")
    dir.create(level1)
    level2 <- file.path(level1, "level2")
    dir.create(level2)
    test_file <- file.path(level2, "test.txt")
    writeLines("test", test_file)

    mod <- tm_file_viewer(input_path = list(nested = temp_base))

    shiny::testServer(
      app = mod$server,
      args = c(list(id = "test"), mod$server_args),
      expr = {
        testthat::expect_error(
          output$output(),
          "Please select a file"
        )
      }
    )
  })
})
