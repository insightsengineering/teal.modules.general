# BASIC SHINY APP

library(shiny)
ui <- fluidPage(
  selectInput("selected", "Variable", choices = names(iris)[-5], selected = names(iris)[1], multiple = TRUE),
  numericInput("centers", "bins", 10, min = 1),
  tableOutput("table")
)
server <- function(input, output, session) {
  data <- reactive(iris[, input$selected, drop = FALSE])

  clustering_data <-
    reactive({
      cbind(
        data(),
        clusters = kmeans(data(), centers = input$centers)$cluster
      )
    })

  output$table <- renderTable({
    clustering_data()
  })
}

shinyApp(ui, server)

# SHINY MODULE

kmeans_ui <- function(id, data = iris) {

  # pick only numeric
  data <- data[vapply(data, is.numeric, logical(1))]

  tagList(
    selectInput(NS(id, "selected"), "Variable", choices = names(data), selected = names(data)[1], multiple = TRUE),
    numericInput(NS(id, "centers"), "bins", 10, min = 1),
    tableOutput(NS(id, "table"))
  )
}

kmeans_server <- function(id, data= iris) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactive(data[, input$selected, drop = FALSE])

    clustering_data <-
      reactive({
        cbind(
          dataset(),
          clusters = kmeans(dataset(), centers = input$centers)$cluster
        )
      })

    output$table <- renderTable({
      clustering_data()
    })
  })
}

kmeans_app <- function(data = iris) {
  ui <- fluidPage(
    kmeans_ui("kmeans_mod", data = data)
  )
  server <- function(input, output, session) {
    kmeans_server("kmeans_mod", data = data)
  }
  shinyApp(ui, server)
}
library(shiny)
kmeans_app(data = mtcars)
kmeans_app(data = iris)
kmeans_app(data = swiss)



tm_kmeans <- function(label, data= NULL) {
  checkmate::assert_character(label)

  module(
    label = label,
    server = kmeans_server,
    ui = kmeans_ui,
    ui_args = list(data = data),
    server_args = list(data = data),
    filters = "all"
  )
}

library(teal)

app <- init(
  data = mtcars,
  modules = tm_kmeans(label = "K-means Clustering"),
  header = "Simple app with k-means clustering module"
)

shinyApp(app$ui, app$server)

# TEAL

ui_kmeans_example <- function(id, clustering_vars) {
  ns <- NS(id)
  teal.widgets::standard_layout(
    output = tableOutput(ns("table")),
    encoding = div(
      teal.transform::data_extract_ui(
        id = ns("clustering_vars"),
        label = "Variable",
        data_extract_spec = clustering_vars
      )
    )
  )
}

srv_kmeans_example <- function(id, data) {
  checkmate::assert_class(data, "tdata")
  moduleServer(id, function(input, output, session) {

    clustering_data <-
      reactive({
        cbind(
          input$data,
          clusters = kmeans(input$data[, input$selected], centers = input$centers)$cluster
        )
      })

    output$table <- renderTable({
      clustering_data()
    })

  })
}

# tm_kmeans <- function(label) {
#   checkmate::assert_character(label)
#
#   module(
#     label = label,
#     server = srv_kmeans_example,
#     ui = ui_kmeans_example
#   )
# }
#
# library(teal)
#
# app <- init(
#   data = teal_data(
#     dataset("IRIS", iris, code = "IRIS <- iris"),
#     check = TRUE
#   ),
#   modules = tm_kmeans(
#     label = "K-means Clustering"
#   ),
#   header = "Simple app with k-means clustering module"
# )
#
# if (interactive()) {
#   shinyApp(app$ui, app$server)
# }
