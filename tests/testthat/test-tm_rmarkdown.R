testthat::describe("srv_rmarkdown", {
  content <- c(
    "---",
    "title: \"R Markdown Report\"",
    "output: html_document",
    "---",
    "",
    "```{r}",
    "nrow(my_data)",
    "```"
  )

  data <- shiny::reactive(
    within(teal.data::teal_data(), {
      my_data <- CO2
    })
  )

  it("renders number of rows of `my_data` to markdown file", {
    shiny::testServer(
      srv_rmarkdown,
      args = list(
        id = "test", rmd_content = content, data = data,
        extra_transform = list(), allow_download = TRUE
      ),
      expr = {
        testthat::expect_match(readLines(rendered_path_r()), "    ## [1] 84", fixed = TRUE, all = FALSE)
      }
    )
  })

  it("returns a teal.reporter reactive", {
    shiny::testServer(
      srv_rmarkdown,
      args = list(
        id = "test", rmd_content = content, data = data,
        extra_transform = list(), allow_download = TRUE
      ),
      expr = {
        testthat::expect_s3_class(result, "reactive")
        testthat::expect_s4_class(result(), "teal_report")
      }
    )
  })

  it("renders expected html", {
    shiny::testServer(
      srv_rmarkdown,
      args = list(
        id = "test", rmd_content = content, data = data,
        extra_transform = list(), allow_download = TRUE
      ),
      expr = {
        .noWs <- c("before", "after", "outside", "after-begin", "before-end") # nolint: object_name_linter.
        testthat::expect_equal(
          gsub("\n", "", as.character(output$rmd_output$html)),
          as.character(
            shiny::tagList(
              shiny::tags$pre(
                .noWS = .noWs,
                shiny::tags$code(
                  .noWS = .noWs,
                  class = "language-r",
                  "nrow(my_data)"
                )
              ),
              shiny::tags$pre(
                .noWS = .noWs,
                shiny::tags$code(
                  .noWS = .noWs,
                  "## [1] 84"
                )
              )
            )
          )
        )
      }
    )
  })
})
