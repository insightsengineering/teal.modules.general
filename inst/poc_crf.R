library(teal)
library(DT)
library(labelled)
library(reactable)
pkgload::load_all("teal.modules.general")

# Note: Please add the `PATH_TO_DATA` and change the X, Y, and Z Administrations to the actual values in the data

data <- within(teal_data(), {
  library(dplyr)
  library(arrow)
  library(forcats)
  data_path <- "PATH_TO_DATA"

  swimlane_ds <- read_parquet(file.path(data_path, "swimlane_ds.parquet")) |>
    filter(!is.na(event_result), !is.na(event_study_day)) |>
    mutate(subject = forcats::fct_reorder(as.factor(subject), event_study_day, .fun = max))

  spiderplot_ds <- read_parquet(file.path(data_path, "spiderplot_ds.parquet")) |>
    mutate(subject = as.character(subject))
})

swim_plotly_specs <- list(
  list("plotly::add_markers", x = ~study_day, y = ~subject, color = ~catagory, symbol = ~catagory, data = quote(study_drug_administration)),
  list("plotly::add_markers", x = ~study_day, y = ~subject, color = ~catagory, symbol = ~catagory, data = quote(response_assessment)),
  list("plotly::add_markers", x = ~study_day, y = ~subject, color = ~catagory, symbol = ~catagory, data = quote(disposition)),
  list("plotly::add_segments", x = ~0, xend = ~study_day, y = ~subject, yend = ~subject, data = quote(max_subject_day), line = list(width = 1, color = "grey"), showlegend = FALSE),
  list("plotly::layout", xaxis = list(title = "Study Day"), yaxis = list(title = "Subject"))
)

swimlane_tm <- teal_transform_module(
  server = function(id, data) {
    reactive({
      data() |>
        within({
          disposition <- swimlane_ds |>
            filter(!is.na(event_study_day)) |>
            filter(event_type == "disposition") |>
            transmute(subject, event_type, catagory = event_result, study_day = event_study_day)

          response_assessment <- swimlane_ds |>
            filter(!is.na(event_study_day)) |>
            filter(event_type == "response_assessment") |>
            transmute(subject, event_type, catagory = event_result, study_day = event_study_day)

          study_drug_administration <- swimlane_ds |>
            filter(!is.na(event_study_day)) |>
            filter(event_type == "study_drug_administration") |>
            transmute(subject, event_type, catagory = event_result, study_day = event_study_day)

          max_subject_day <- swimlane_ds |>
            group_by(subject) |>
            summarise(study_day = max(event_study_day)) |>
            bind_rows(tibble(subject = unique(swimlane_ds$subject), study_day = 0))
        })
    })
  }
)

swimlane_ui_mod <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6, reactableOutput(ns("mm_response"))),
    column(6, reactableOutput(ns("tx_listing")))
  )
}

swimlane_srv_mod <- function(id,
                             data,
                             plotly_selected,
                             filter_panel_api) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    output$mm_response <- renderReactable({
      swimlane_ds <- data()[["swimlane_ds"]]
      col_defs <- list(
        subject = colDef(name = "Subject"),
        visit_name = colDef(name = "Visit Name"),
        visit_date = colDef(name = "Visit Date"),
        form_name = colDef(name = "Form Name"),
        source_system_url_link = colDef(
          name = "Source System URL Link",
          cell = function(value) {
            if (!is.na(value) && !is.null(value) && value != "") {
              htmltools::tags$a(href = value, target = "_blank", "Link")
            } else {
              "N/A"
            }
          }
        ),
        rspdn = colDef(name = "Assessment Performed"),
        rspd = colDef(name = "Response Date"),
        rspd_study_day = colDef(name = "Response Date Study Day"),
        orsp = colDef(name = "Response"),
        bma = colDef(name = "Best Marrow Aspirate"),
        bmb = colDef(name = "Best Marrow Biopsy"),
        comnts = colDef(name = "Comments")
      )
      mm_response <- swimlane_ds |>
        filter(event_study_day %in% plotly_selected()$x, subject %in% plotly_selected()$y) |>
        select(all_of(names(col_defs)))
      reactable(
        mm_response,
        columns = col_defs,
        defaultPageSize = 5,
        searchable = TRUE,
        sortable = TRUE
      )
    })

    output$tx_listing <- renderReactable({
      swimlane_ds <- data()[["swimlane_ds"]]

      col_defs <- list(
        site_name = colDef(name = "Site Name"),
        subject = colDef(name = "Subject"),
        visit_name = colDef(name = "Visit Name"),
        visit_date = colDef(name = "Visit Date"),
        form_name = colDef(name = "Form Name"),
        source_system_url_link = colDef(
          name = "Source System URL Link",
          cell = function(value) {
            if (!is.na(value) && !is.null(value) && value != "") {
              htmltools::tags$a(href = value, target = "_blank", "Link")
            } else {
              "N/A"
            }
          }
        ),
        txnam = colDef(name = "Study Drug Name"),
        txrec = colDef(name = "Study Drug Administered"),
        txrecrs = colDef(name = "Reason Study Drug Not Admin"),
        txd_study_day = colDef(name = "Date Administered Study Day"),
        date_administered = colDef(name = "Date Administered"),
        cydly = colDef(name = "Cycle Delay"),
        cydlyrs = colDef(name = "Cycle Delay Reason"),
        cydlyae = colDef(name = "Cycle Delay Adverse Event"),
        txdly = colDef(name = "Dose Delay"),
        txdlyrs = colDef(name = "Dose Delay Reason"),
        txdlyae = colDef(name = "AE related to Dose Delay"),
        txpdos = colDef(name = "Planned Dose per Admin"),
        txpdosu = colDef(name = "Planned Dose per Admin Unit"),
        frqdv = colDef(name = "Frequency"),
        txrte = colDef(name = "Route of Administration"),
        txform = colDef(name = "Dose Formulation"),
        txdmod = colDef(name = "Dose Modification"),
        txrmod = colDef(name = "Dose Modification Reason"),
        txdmae = colDef(name = "AE related to Dose Modification"),
        txad = colDef(name = "Total Dose Administered"),
        txadu = colDef(name = "Total Dose Administered Unit"),
        txd = colDef(name = "Date Administered"),
        txstm = colDef(name = "Start Time Administered"),
        txstmu = colDef(name = "Start Time Administered Unknown"),
        txed = colDef(name = "End Date Administered"),
        txetm = colDef(name = "End Time Administered"),
        txetmu = colDef(name = "End Time Administered Unknown"),
        txtm = colDef(name = "Time Administered"),
        txtmu = colDef(name = "Time Administered Unknown"),
        txed_study_day = colDef(name = "End Study Day"),
        infrt = colDef(name = "Infusion Rate"),
        infrtu = colDef(name = "Infusion Rate Unit"),
        tximod = colDef(name = "Infusion Modified?"),
        txirmod = colDef(name = "Reason for Infusion modification"),
        tximae = colDef(name = "AE related to Infusion Modification")
      )
      tx_listing <- swimlane_ds |>
        filter(event_study_day %in% plotly_selected()$x, subject %in% plotly_selected()$y) |>
        select(all_of(names(col_defs)))
      reactable(
        tx_listing,
        columns = col_defs,
        defaultPageSize = 5,
        searchable = TRUE,
        sortable = TRUE
      )
    })
  })
}

spider_plotly_specs <- list(
  list("plotly::add_markers", x = ~event_study_day, y = ~event_result, color = ~subject, data = quote(spiderplot_ds_filtered)),
  list("plotly::add_lines", x = ~event_study_day, y = ~event_result, data = quote(spiderplot_ds_filtered), color = ~subject, showlegend = FALSE)
)

spiderplot_tm <- teal_transform_module(
  ui = function(id) {
    selectInput(NS(id, "event_type"), "Select Event type", NULL)
  },
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      spiderplot_ds <- reactive(data()[["spiderplot_ds"]])
      observeEvent(spiderplot_ds(), {
        event_types <- unique(spiderplot_ds()$event_type)
        updateSelectInput(
          inputId = "event_type",
          choices = event_types[event_types != "response_assessment"]
        )
      })
      reactive({
        data() |>
          within(
            {
              spiderplot_ds_filtered <- spiderplot_ds |>
                filter(event_type == selected_event)
            },
            selected_event = input$event_type
          )
      })
    })
  }
)

spider_ui_mod <- function(id) {
  ns <- NS(id)
  fluidRow(
    column(6, reactableOutput(ns("recent_resp"))),
    column(6, reactableOutput(ns("all_resp")))
  )
}

spider_srv_mod <- function(id,
                           data,
                           plotly_selected,
                           filter_panel_api) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    all_resp_cols <- list(
      txarm = colDef(name = "Study Arm"),
      cohrt = colDef(name = "Study Cohort"),
      subject = colDef(name = "Subject"),
      event_result = colDef(name = "Response"),
      event_study_day = colDef(name = "Study Day"),
      visit_name = colDef(name = "Visit Name")
    )

    selected_recent_subject <- reactiveVal(NULL)

    all_resp <- reactive({
      if (!is.null(selected_recent_subject())) {
        data()[["spiderplot_ds"]] |>
          filter(event_type == "response_assessment") |>
          select(all_of(names(all_resp_cols))) |>
          filter(subject == selected_recent_subject())
      } else {
        selected_subjects <- data()[["spiderplot_ds"]] |>
          filter(event_study_day %in% plotly_selected()$x, event_result %in% plotly_selected()$y) |>
          pull(subject)
        data()[["spiderplot_ds"]] |>
          filter(event_type == "response_assessment") |>
          select(all_of(names(all_resp_cols))) |>
          filter(subject %in% selected_subjects)
      }
    })

    rank_response <- function(responses) {
      responses <- responses[!is.na(responses)]
      if (length(responses) == 0) {
        return(NA_character_)
      }
      response_hierarchy <- c(
        "SCR (Stringent Complete Response)",
        "CR (Complete Response)",
        "VGPR (Very Good Partial Response)",
        "PR (Partial Response)",
        "MR (Minimal/Minor Response)",
        "SD (Stable Disease)",
        "PD (Progressive Disease)"
      )
      responses[which.max(match(responses, response_hierarchy))]
    }

    recent_resp_cols <- list(
      txarm = colDef(name = "Study Arm"),
      cohrt = colDef(name = "Study Cohort"),
      subject = colDef(name = "Subject"),
      event_result = colDef(name = "Response"),
      event_study_day = colDef(name = "Study Day"),
      most_recent_response = colDef(name = "Most Recent Response"),
      best_response = colDef(name = "Best Response")
    )

    output$recent_resp <- renderReactable({
      best_resp <- all_resp() %>%
        group_by(subject) %>%
        filter(!is.na(subject)) %>%
        arrange(desc(event_study_day)) %>%
        slice(1) %>%
        mutate(
          most_recent_response = event_result,
          best_response = rank_response(all_resp()$event_result[all_resp()$subject == cur_group()])
        ) %>%
        ungroup()

      reactable(
        best_resp,
        columns = recent_resp_cols,
        selection = "single",
        onClick = "select"
      )
    })

    observeEvent(input$recent_resp_selected, {
      req(input$recent_resp_selected)
      selected_subjects <- reactableProxy("recent_resp") %>%
        getReactableState("selected")

      if (length(selected_subjects) > 0) {
        selected_subject <- output$recent_resp()$subject[selected_subjects]
        selected_recent_subject(selected_subject)
      }
    })

    output$all_resp <- renderReactable({
      reactable(
        all_resp(),
        columns = all_resp_cols
      )
    })
  })
}


app <- init(
  data = data,
  modules = modules(
    tm_p_swimlane2(
      label = "Spiderplot",
      plotly_specs = spider_plotly_specs,
      title = "Swimlane Efficacy Plot",
      transformators = list(spiderplot_tm),
      ui_mod = spider_ui_mod,
      srv_mod = spider_srv_mod,
      plot_height = 600
    ),
    tm_p_swimlane2(
      label = "Swimlane",
      plotly_specs = swim_plotly_specs,
      title = "Swim Lane - Duration of Tx",
      colors = c(
        "DEATH" = "black",
        "WITHDRAWAL BY SUBJECT" = "grey",
        "PD (Progressive Disease)" = "red",
        "SD (Stable Disease)" = "darkorchid4",
        "MR (Minimal/Minor Response)" = "sienna4",
        "PR (Partial Response)" = "maroon",
        "VGPR (Very Good Partial Response)" = "chartreuse4",
        "CR (Complete Response)" = "#3a41fc",
        "SCR (Stringent Complete Response)" = "midnightblue",
        "X Administration Injection" = "goldenrod",
        "Y Administration Infusion" = "deepskyblue3",
        "Z Administration Infusion" = "darkorchid"
      ),
      symbols = c(
        "DEATH" = "circle",
        "WITHDRAWAL BY SUBJECT" = "square",
        "PD (Progressive Disease)" = "circle",
        "SD (Stable Disease)" = "square-open",
        "MR (Minimal/Minor Response)" = "star-open",
        "PR (Partial Response)" = "star-open",
        "VGPR (Very Good Partial Response)" = "star-open",
        "CR (Complete Response)" = "star-open",
        "SCR (Stringent Complete Response)" = "star-open",
        "X Administration Injection" = "line-ns-open",
        "Y Administration Infusion" = "line-ns-open",
        "Z Administration Infusion" = "line-ns-open"
      ),
      transformators = list(swimlane_tm),
      ui_mod = swimlane_ui_mod,
      srv_mod = swimlane_srv_mod
    ),
    tm_data_table()
  ),
  filter = teal_slices(
    teal_slice(
      dataname = "swimlane_ds",
      varname = "subject"
    ),
    teal_slice(
      dataname = "swimlane_ds",
      varname = "cohrt"
    ),
    teal_slice(
      dataname = "swimlane_ds",
      varname = "txarm"
    ),
    teal_slice(
      dataname = "spiderplot_ds",
      varname = "subject"
    ),
    teal_slice(
      dataname = "spiderplot_ds",
      varname = "cohrt"
    ),
    teal_slice(
      dataname = "spiderplot_ds",
      varname = "txarm"
    ),
    count_type = "all"
  )
)

shinyApp(app$ui, app$server)
