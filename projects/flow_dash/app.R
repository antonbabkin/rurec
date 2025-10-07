
library(shiny)
library(dplyr)
library(arrow)
library(sf)
library(reactable)
library(leaflet)


# packaged data
appdata <- readRDS("app.rds")

#' Aggregate dataframe values at an ilevel
#' 
#' @param x dataframe to aggregate
#' @param id_cols additional columns to group by
#' @param detail_col column name with detail level code
#' @param value_cols columns to aggregate
#' @param ilevel level of aggregation
#' @param com_code only aggregate this code
agg_ilevel <- function(x,
                       id_cols = character(),
                       detail_col = "detail",
                       value_cols = c("value"),
                       ilevel = c("total", "sector", "summary", "u_summary", "detail"), 
                       com_code = NULL) {
  ilevel <- match.arg(ilevel)
  
  
  if (ilevel == "total") {
    y <- x %>%
      summarize(across(all_of(value_cols), sum),
                .by = (if (length(id_cols) > 0) id_cols else NULL))
  } else if (ilevel == "detail") {
    y <- x %>%
      select(all_of(id_cols), all_of(detail_col), all_of(value_cols)) %>%
      filter(.data[[detail_col]] == {{ com_code }})
  } else {
    if (is.null(com_code)) {
      concordance <- filter(appdata$com_codes, !is.na(detail))
    } else {
      concordance <- filter(appdata$com_codes, .data[[ilevel]] == {{ com_code }}, !is.na(detail))
    }
    y <- concordance %>%
      inner_join(x, join_by(detail == {{ detail_col }})) %>%
      summarize(across(all_of(value_cols), sum),
                .by = (if (length(id_cols) > 0) id_cols else NULL))
  }
  y
}

com_selector_choices <- function(ilevel, higher_code = NULL) {
  if (!is.null(higher_code) && higher_code == "TOTAL") return("TOTAL")
  
  x <- appdata$com_codes |>
    filter(ilevel == !!ilevel)
  x <- switch(
    ilevel,
    sector = x,
    summary = filter(x, sector == higher_code),
    u_summary = filter(x, summary == higher_code),
    detail = filter(x, u_summary == higher_code)
  )
  x |>
    mutate(label = paste0(code, ": ", title)) |> 
    pull(code, name = label) |> 
    as.list() %>%
    c(list("TOTAL" = "TOTAL"), .)
}

# ---- UI ----
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  fluidRow(
    # sector selector
    column(
      3,
      selectInput("com_sector", "Sector:", choices = com_selector_choices("sector"))
    ),
    # summary selector: only show if sector is selected
    column(
      3,
      conditionalPanel(
        "input.com_sector !== 'TOTAL'",
        selectInput("com_summary", "Summary:", choices = "TOTAL")
      )
    ),
    # u_summary selector: only show if summary is selected
    column(
      3,
      conditionalPanel(
        "input.com_summary !== 'TOTAL'",
        selectInput("com_u_summary", "Underlying Summary:", choices = "TOTAL")
      )
    ),
    # detail selector: only show if u_summary is selected
    column(
      3,
      conditionalPanel(
        "input.com_u_summary !== 'TOTAL'",
        selectInput("com_detail", "Detail:", choices = "TOTAL")
      )
    )
  ),
  fluidRow(tabsetPanel(
    tabPanel("commodities", reactableOutput("com_codes")),
    tabPanel(
      "out-sup-dem",
      tabsetPanel(
        tabPanel(
          "map",
          radioButtons(
            "map_osd_var",
            "Map variable",
            choices = c("output", "supply", "demand", "netsup", "exsup", "exdem"),
            inline = TRUE,
            selected = character(0)
          ),
          leafletOutput("map_osd")
        ),
        tabPanel("table", reactableOutput("tbl_outsupdem"))
      )
    ),
    tabPanel(
      "flows",
      radioButtons(
        "flow_dir",
        label = NULL,
        choices = c("Outflow (export)" = "out", "Inflow (import)" = "in"),
        inline = TRUE,
        selected = character(0)
      ),
      textOutput("flow_county"),
      tabsetPanel(
        tabPanel("map", leafletOutput("map_flows")),
        tabPanel("table", reactableOutput("tbl_flows"))
      )
    )
  ))
)


# ---- server ----
server <- function(input, output, session) {
  

  # ---- dynamic com selectors ----
  # when sector changes, update available summary choices
  observeEvent(input$com_sector, {
    freezeReactiveValue(input, "com_summary")
    freezeReactiveValue(input, "com_u_summary")
    updateSelectInput(inputId = "com_summary", choices = com_selector_choices("summary", input$com_sector))
  })
  # when summary changes, update available u_summary choices
  observeEvent(input$com_summary, {
    freezeReactiveValue(input, "com_u_summary")
    updateSelectInput(inputId = "com_u_summary", choices = com_selector_choices("u_summary", input$com_summary))
  })
  # when u_summary changes, update available detail choices
  observeEvent(input$com_u_summary, {
    updateSelectInput(inputId = "com_detail", choices = com_selector_choices("detail", input$com_u_summary))
  })
  
  # reduce com selectors to a list(ilevel, code)
  selected_com <- reactive({
    if (input$com_sector == "TOTAL") {
      # aggregate across all industries
      y <- list(ilevel = "total", code = NULL)
    } else if (input$com_summary == "TOTAL") {
      # aggregate across industries within selected sector
      y <- list(ilevel = "sector", code = input$com_sector)
    } else if (input$com_u_summary == "TOTAL") {
      # aggregate across industries within selected summary
      y <- list(ilevel = "summary", code = input$com_summary)
    } else if (input$com_detail == "TOTAL") {
      # aggregate across industries within selected u_summary
      y <- list(ilevel = "u_summary", code = input$com_u_summary)
    } else {
      # filter selected detail industry
      y <- list(ilevel = "detail", code = input$com_detail)
    }
    cat("com selection: ", y$ilevel, y$code, "\n")
    y
  })
  
  
  # ---- com codes table ----
  output$com_codes <- renderReactable({
    appdata$com_codes %>%
      {if (selected_com()$ilevel == "total") . else filter(., .data[[selected_com()$ilevel]] == selected_com()$code)} %>%
      select(!c(ilevel, code)) %>% 
      reactable(sortable = FALSE, defaultColDef = colDef(maxWidth = 100), columns = list(title = colDef(maxWidth = 1000)))
  })
  
  
  # ---- osd table ----
  
  # filter and aggregate output, supply and demand for selected industry
  tbl_outsupdem <- reactive({
    appdata$outsupdem %>%
      agg_ilevel(id_cols = "place", detail_col = "com_code", ilevel = selected_com()$ilevel, com_code = selected_com()$code,
                 value_cols = c("output", "supply", "demand", "netsup", "exsup", "exdem"))
  })
  
  output$tbl_outsupdem <- renderReactable(
    tbl_outsupdem() |>
      reactable(defaultColDef = colDef(format = colFormat(digits = 0, separators = TRUE)))
  )

  # ---- osd map ----
  # initial load of map when tab is first activated
  # init variable req'd in map update code
  map_osd_initialized <- reactiveVal(FALSE)
  output$map_osd <- renderLeaflet({
    cat("Initial map_osd render\n")
    m <- leaflet() |>
      setView(-96, 37.8, 4) |>
      addProviderTiles("CartoDB.Positron")
    map_osd_initialized(TRUE)
    m
  })
  
  # update OSD map polygons when map variable or commodity selection changes
  observe({
    # do not trigger until map has been initialized
    req(map_osd_initialized(), input$map_osd_var, tbl_outsupdem())
    cat("map_osd update\n")
    d <- appdata$geo |>
      left_join(tbl_outsupdem(), join_by(geoid == place))
    pal <- colorQuantile(palette = "RdYlBu", domain = NULL, n = 5)
    val <- d[[input$map_osd_var]]
    col <- pal(val)
    leafletProxy("map_osd", session) |>
      clearShapes() |>
      addPolygons(
        data = d,
        fillColor = col,
        fillOpacity = 0.6,
        weight = 0.2,
        color = "white"
      ) |>
      clearControls() |>
      addLegend(
        pal = pal, values = val, opacity = 0.7, title = NULL,
        position = "bottomright"
      )
  }) |>
    bindEvent(input$map_osd_var, tbl_outsupdem())
  
  
  # ---- flows ----
  
  # selected county from flows map
  flow_county <- reactiveVal()
  
  ## ---- dataframe ----
  # dataframe of flows for selected commodity, county and direction
  tbl_flows <- reactive({
    req(selected_com(), flow_county(), input$flow_dir)
    if (input$flow_dir == "in") {
      focus_col <- "to"
      id_col <- "from"
    } else {
      focus_col <- "from"
      id_col <- "to"
    }
    appdata$flows %>%
      filter(.data[[focus_col]] == flow_county()) %>%
      agg_ilevel(
        id_cols = id_col,
        detail_col = "com_code",
        value_cols = "flow",
        ilevel = selected_com()$ilevel,
        com_code = selected_com()$code
      )
  })
  
  
  ## ---- map ----
  output$flow_county <- renderText({
    if (is.null(flow_county()))
      "Click map to select county"
    else
      paste("Selected county:", flow_county())
  })

  # initial load of map when tab is first activated
  # init variable req'd in map update code
  map_flows_initialized <- reactiveVal(FALSE)
  output$map_flows <- renderLeaflet({
    cat("Initial map_flows render\n")
    m <- leaflet(appdata$geo) |>
      setView(-96, 37.8, 4) |>
      addProviderTiles("CartoDB.Positron") |>
      addPolygons(
        layerId = ~ geoid,
        weight = 1,
        opacity = 0.1,
        fillColor = "#808080",
        highlightOptions = highlightOptions(color = "black", weight = 2, opacity = 0.8, bringToFront = TRUE)
      )
    map_flows_initialized(TRUE)
    m
  })
  
  
  # Change focus county on shape click
  observe({
    event <- input$map_flows_shape_click
    if (!is.null(event)) {
      cat("map_flow click", event$id, "\n")
      flow_county(event$id)
    }
  })
  
  
  # update flow map polygons when commodity selection, direction or focus county change
  observe({
    # do not trigger until map has been initialized
    req(map_flows_initialized(), tbl_flows())
    cat("map_flows update\n")
    # all counties. zero flow will be NA
    id_col <- switch(input$flow_dir, `in` = "from", `out` = "to")
    d <- appdata$geo |>
      left_join(tbl_flows(), join_by(geoid == {{ id_col }}))
    focus_polygon <- filter(appdata$geo, geoid == flow_county())
    pal <- colorNumeric("Reds", domain = NULL, na.color = "#808080")
    leafletProxy("map_flows", session) |>
      clearShapes() |>
      addPolygons(
        data = d,
        layerId = ~ geoid,
        weight = 0.2,
        color = "white",
        label = ~ paste(geoid, flow),
        fillColor = ~ pal(log10(flow)),
        fillOpacity = 0.6,
        highlightOptions = highlightOptions(color = "black", weight = 2, opacity = 0.8, bringToFront = TRUE)
      ) |>
      addPolygons(data = focus_polygon, stroke = FALSE, fillColor = "black", fillOpacity = 1)
  }) |>
    bindEvent(input$flow_dir, selected_com(), flow_county())
  
  ## ----  table ----

  output$tbl_flows <- renderReactable(
    tbl_flows() |>
      reactable(defaultColDef = colDef(format = colFormat(digits = 0, separators = TRUE)))
  )
  
}

shinyApp(ui = ui, server = server)
