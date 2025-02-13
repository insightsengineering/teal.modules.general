# Create a mock data extact spec for tests
mock_data_extract_spec <- function(dataname = "MOCK_DATASET",
                                   select_choices = sample(LETTERS, sample(2:10, 1)),
                                   select_multiple = FALSE) {
  teal.transform::data_extract_spec(
    dataname = dataname,
    select = teal.transform::select_spec(
      choices = select_choices,
      multiple = select_multiple
    )
  )
}

normalize_math_italic_text <- function(text) {
  # Unicode range for mathematical italic (uppercase/lowercase)
  math_italic <- intToUtf8(seq(0x1D434, 0x1D467)) # A-z

  # Standard letters
  latin <- c(LETTERS, letters)

  # Replace math italic letters with standard ones
  stringr::str_replace_all(
    text,
    setNames(latin, unlist(stringr::str_split(math_italic, "")))
  )
}
