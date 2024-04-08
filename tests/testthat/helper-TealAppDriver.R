# Import non-exported TealAppDriver from `teal` package
TealAppDriver <- getFromNamespace("TealAppDriver", "teal")

# Helper function
simple_teal_data <- function() {
  data <- within(teal.data::teal_data(), {
    iris <- iris
    mtcars <- mtcars
  })
  teal.data::datanames(data) <- c("iris", "mtcars")
  data
}
