testthat::describe("set_chunk_attrs", {
  card <- teal.reporter::teal_card(
    "# Header",
    "Some text",
    structure(list(2), class = "chunk_output"),
    structure(list("1"), class = "chunk_output")
  )

  it("changes last chunk output with default parameters", {
    new_card <- set_chunk_attrs(card, list(dev.height = 200))
    testthat::expect_equal(attributes(new_card[[4]]), list(class = "chunk_output", dev.height = 200))
    testthat::expect_equal(attributes(new_card[[3]]), list(class = "chunk_output"))
  })

  it("changes last 2 chunks", {
    new_card <- set_chunk_attrs(card, list(dev.height = 200), n = 2)
    testthat::expect_equal(attributes(new_card[[4]]), list(class = "chunk_output", dev.height = 200))
    testthat::expect_equal(attributes(new_card[[3]]), list(class = "chunk_output", dev.height = 200))
  })

  it("only changes the numeric chunk_outputs", {
    new_card <- set_chunk_attrs(card, list(dev.height = 200), n = 2, inner_classes = "numeric")
    testthat::expect_equal(attributes(new_card[[3]]), list(class = "chunk_output", dev.height = 200))
    testthat::expect_equal(attributes(new_card[[4]]), list(class = "chunk_output"))
  })

  it("throws warning when last chunk is not chunk_output", {
    testthat::expect_warning(
      set_chunk_attrs(c(card, "yada"), list(new_attr = TRUE)),
      "The last element of the `teal_card` is not a `chunk_output` object. No attributes were modified."
    )
  })

  it("throws warning when second to last chunk is not chunk_output", {
    card_modified <- c(card[c(1, 2, 3)], "bla", card[[4]])
    testthat::expect_warning(
      set_chunk_attrs(card_modified, n = 3, list(new_attr = TRUE)),
      "The 2 to last element of the `teal_card` is not a `chunk_output` object. Skipping any further modifications."
    )
  })

  it("modifies all elements up until the first non-chunk output", {
    card_modified <- c(card[c(1, 2, 3)], "bla", card[[4]])
    new_card <- set_chunk_attrs(card_modified, n = 3, list(new_attr = TRUE), quiet = TRUE)
    testthat::expect_equal(attributes(new_card[[5]]), list(class = "chunk_output", new_attr = TRUE))
    testthat::expect_null(attributes(new_card[[4]]))
    testthat::expect_equal(attributes(new_card[[3]]), list(class = "chunk_output"))
  })
})

testthat::describe("set_chunk_dims", {
  it("skips when one of the dimensions is auto string", {
    pws <- list(dim = shiny::reactive(list("auto", 200)))
    q <- teal.reporter::teal_report()
    teal.reporter::teal_card(q) <- teal.reporter::teal_card("## Header", structure(list(2), class = "chunk_output"))
    q_r <- shiny::reactive(q)

    q_dims_r <- set_chunk_dims(pws, q_r)
    testthat::expect_equal(
      attributes(teal.reporter::teal_card(shiny::isolate(q_dims_r()))[[2]]),
      list(class = "chunk_output", dev.height = 200)
    )
  })
})