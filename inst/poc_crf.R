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

  parent_ds <- bind_rows(
    swimlane_ds |> select(subject, cohrt, txarm),
    spiderplot_ds |> select(subject, cohrt, txarm)
  ) |> distinct()
})

join_keys(data) <- join_keys(
  join_key("parent_ds", "swimlane_ds", c("subject", "cohrt", "txarm")),
  join_key("parent_ds", "spiderplot_ds", c("subject", "cohrt", "txarm"))
)

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
          swimlane_ds <- swimlane_ds |>
            mutate(subject = forcats::fct_reorder(as.factor(subject), event_study_day, .fun = max))
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
  shinyjs::hidden(
    fluidRow(
      id = ns("reactive_tables"),
      column(
        6,
        class = "simple-card",
        tagList(
          h4("Multiple Myeloma Response"),
          reactableOutput(ns("mm_response"))
        )
      ),
      column(
        6,
        class = "simple-card",
        tagList(
          h4("Study Tx Listing"),
          reactableOutput(ns("tx_listing"))
        )
      )
    )
  )
}

swimlane_srv_mod <- function(id,
                             data,
                             plotly_selected,
                             filter_panel_api) {
  checkmate::assert_class(data, "reactive")
  checkmate::assert_class(isolate(data()), "teal_data")
  moduleServer(id, function(input, output, session) {
    observeEvent(plotly_selected(), once = TRUE, {
      shinyjs::show("reactive_tables")
    })

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
  list("plotly::add_lines", x = ~event_study_day, y = ~event_result, data = quote(spiderplot_ds_filtered), color = ~subject, showlegend = FALSE),
  list(
    "plotly::layout",
    xaxis = list(title = "Collection Date Study Day", zeroline = FALSE),
    yaxis = list(title = ~y_title),
    title = ~ paste0(y_title, " Over Time")
  )
)

spiderplot_tm <- teal_transform_module(
  ui = function(id) {
    selectInput(NS(id, "event_type"), "Select Y Axis", NULL)
  },
  server = function(id, data) {
    moduleServer(id, function(input, output, session) {
      spiderplot_ds <- reactive(data()[["spiderplot_ds"]])
      observeEvent(spiderplot_ds(), {
        event_types <- unique(spiderplot_ds()$event_type)
        updateSelectInput(
          inputId = "event_type",
          choices = event_types[!event_types %in% c("response_assessment", "latest_response_assessment")]
        )
      })
      reactive({
        data() |>
          within(
            {
              y_title <- selected_event
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
  shinyjs::hidden(
    fluidRow(
      id = ns("reactive_tables"),
      fluidRow(
        column(
          6,
          class = "simple-card",
          tagList(
            h4("Most Recent Resp and Best Resp"),
            reactableOutput(ns("recent_resp"))
          )
        ),
        column(
          6,
          class = "simple-card",
          tagList(
            h4("Multiple Myeloma Response"),
            reactableOutput(ns("all_resp"))
          )
        )
      ),
      fluidRow(
        column(
          6,
          class = "simple-card",
          tagList(
            h4("Disease Assessment - SPEP"),
            reactableOutput(ns("spep_listing"))
          )
        ),
        column(
          6,
          class = "simple-card",
          tagList(
            h4("Disease Assessment - SFLC"),
            reactableOutput(ns("sflc_listing"))
          )
        )
      )
    )
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

    observeEvent(plotly_selected(), once = TRUE, {
      shinyjs::show("reactive_tables")
    })

    all_resp <- reactive({
      selected_subjects <- data()[["spiderplot_ds"]] |>
        filter(event_study_day %in% plotly_selected()$x, event_result %in% plotly_selected()$y) |>
        pull(subject)
      data()[["spiderplot_ds"]] |>
        filter(event_type == "response_assessment") |>
        select(all_of(names(all_resp_cols))) |>
        filter(subject %in% selected_subjects)
    })

    output$all_resp <- renderReactable({
      reactable(
        all_resp(),
        columns = all_resp_cols
      )
    })

    recent_resp_cols <- list(
      subject = colDef(name = "Subject"),
      visit_name = colDef(name = "Visit Name"),
      rspdn = colDef(name = "Assessment Performed"),
      rspd = colDef(name = "Response Date"),
      rspd_study_day = colDef(name = "Response Date Study Day"),
      orsp = colDef(name = "Response"),
      bma = colDef(name = "Best Marrow Aspirate"),
      bmb = colDef(name = "Best Marrow Biopsy"),
      comnts = colDef(name = "Comments")
    )

    recent_resp <- reactive({
      data()[["spiderplot_ds"]] |>
        filter(event_type == "latest_response_assessment") |>
        filter(subject %in% all_resp()$subject) |>
        select(all_of(names(recent_resp_cols)))
    })

    output$recent_resp <- renderReactable({
      reactable(
        recent_resp(),
        columns = recent_resp_cols,
        selection = "single",
        onClick = "select"
      )
    })

    spep_cols <- list(
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
      comnts = colDef(name = "Comments"),
      asmntdn = colDef(name = "Assessment Not Done"),
      blq = colDef(name = "Serum M-protein too small to quantify"),
      coldr = colDef(name = "Collection Date"),
      cold_study_day = colDef(name = "Collection Study Day"),
      coltm = colDef(name = "Collection Time"),
      coltmu = colDef(name = "Collection Time Unknown"),
      lrspep1 = colDef(name = "Another Form added?"),
      mprte_raw = colDef(name = "Serum M-protein"),
      mprtec = colDef(name = "SPEP Serum M-protein detection")
    )

    spep <- reactive({
      data()[["spiderplot_ds"]] |>
        filter(event_type == "Serum M-protein") |>
        filter(subject %in% all_resp()$subject) |>
        select(all_of(names(spep_cols)))
    })

    output$spep_listing <- renderReactable({
      reactable(
        spep(),
        columns = spep_cols
      )
    })


    sflc_cols <- list(
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
      comnts = colDef(name = "Comments"),
      asmntdn = colDef(name = "Assessment Not Done"),
      blq = colDef(name = "Serum M-protein too small to quantify"),
      coldr = colDef(name = "Collection Date"),
      cold_study_day = colDef(name = "Collection Study Day"),
      coltm = colDef(name = "Collection Time"),
      coltmu = colDef(name = "Collection Time Unknown"),
      lchfrc = colDef(name = "Presence of Serum free light chains"),
      lchfr_raw = colDef(name = "Serum free light chain results"),
      klchf_raw = colDef(name = "Kappa free light chain results"),
      llchf_raw = colDef(name = "Lambda free light chain results"),
      klchp_raw = colDef(name = "Kappa-Lambda free light chain ratio"),
      mprte_raw = colDef(name = "Serum M-protein"),
      mprtec = colDef(name = "SPEP Serum M-protein detection")
    )

    sflc <- reactive({
      data()[["spiderplot_ds"]] |>
        filter(
          event_type %in% c(
            "Kappa free light chain quantity",
            "Lambda free light chain quantity",
            "Kappa-Lambda free light chain ratio"
          )
        ) |>
        filter(subject %in% all_resp()$subject) |>
        select(all_of(names(sflc_cols)))
    })

    output$sflc_listing <- renderReactable({
      reactable(
        sflc(),
        columns = sflc_cols
      )
    })

    observeEvent(input$recent_resp_selected, {
      print(input$recent_resp_selected)
      req(input$recent_resp_selected)
      selected_subjects <- reactableProxy("recent_resp") %>%
        getReactableState("selected")
      print(selected_subjects)
    })
  })
}


# Custom placement of the transformer
# custom_tm_p_swimlane2 <- function(plotly_specs, ui_mod, srv_mod, transformators = list()) {
#   mod <- tm_p_swimlane2(
#     label = "Spiderplot",
#     plotly_specs = plotly_specs,
#     title = "Swimlane Plot",
#     transformators = transformators,
#     ui_mod = ui_mod,
#     srv_mod = srv_mod,
#     plot_height = 600
#   )
#   mod$ui <- function(id, ui_mod, height) {
#     ns <- NS(id)
#     shiny::tagList(
#       sliderInput(ns("plot_height"), "Plot Height (px)", 400, 1200, height),
#       teal::ui_transform_teal_data(NS(gsub("-module$", "", id), "data_transform"), transformators),
#       plotly::plotlyOutput(ns("plot"), height = "100%"),
#       ui_mod(ns("brush_tables"))
#     )
#   }
#   mod
# }

app <- init(
  data = data,
  header = tags$head(tags$style(
    ".simple-card {
      padding: 20px;
      border-radius: 10px;
      border: 1px solid #ddd;
      box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      background-color: #fff;
    }
    .simple-card h4 {
      text-align: center;
    }"
  )),
  modules = modules(
    tm_p_swimlane2(
      label = "Spiderplot",
      plotly_specs = spider_plotly_specs,
      title = "Swimlane Plot",
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
      dataname = "parent_ds",
      varname = "subject"
    ),
    teal_slice(
      dataname = "parent_ds",
      varname = "cohrt"
    ),
    teal_slice(
      dataname = "parent_ds",
      varname = "txarm"
    ),
    count_type = "all"
  )
)

shinyApp(app$ui, app$server)
