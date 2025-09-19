
library(shiny)
library(reactable)
library(tidyverse)

appdata <- readRDS("app.rds")

agg_outsupdem <- function(
    ilevel = c("total", "sector", "summary", "u_summary", "detail"), 
    code = NULL) {
  ilevel <- match.arg(ilevel)
  
  switch(
    ilevel,
    total = appdata$outsupdem,
    sector = appdata$outsupdem |>
      left_join(distinct(appdata$com_codes, detail, sector), join_by(com_code == detail)) |>
      filter(sector == code),
    summary = appdata$outsupdem |>
      left_join(distinct(appdata$com_codes, detail, summary), join_by(com_code == detail)) |>
      filter(summary == code),
    u_summary = appdata$outsupdem |>
      left_join(distinct(appdata$com_codes, detail, u_summary), join_by(com_code == detail)) |>
      filter(u_summary == code),
    detail = appdata$outsupdem |>
      filter(com_code == code)
  ) |>
    summarize(across(output:exdem, sum), .by = place)
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

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  fluidRow(
    # sector selector
    column(3, selectInput("com_sector", "Sector:", choices = com_selector_choices("sector"))),
    # summary selector: only show if sector is selected
    column(3, conditionalPanel(
      "input.com_sector !== 'TOTAL'",
      selectInput("com_summary", "Summary:", choices = "TOTAL")
    )),
    # u_summary selector: only show if summary is selected
    column(3, conditionalPanel(
      "input.com_summary !== 'TOTAL'",
      selectInput("com_u_summary", "Underlying Summary:", choices = "TOTAL")
    )),
    # detail selector: only show if u_summary is selected
    column(3, conditionalPanel(
      "input.com_u_summary !== 'TOTAL'",
      selectInput("com_detail", "Detail:", choices = "TOTAL")
    ))
  ),
  fluidRow(tabsetPanel(
    tabPanel("commodities", reactableOutput("com_codes")),
    tabPanel("outsupdem", reactableOutput("tbl_outsupdem"))
  ))
)


server <- function(input, output) {
  
  com_codes <- reactive({
    req(input$com_sector, input$com_summary, input$com_u_summary, input$com_detail)
    appdata$com_codes %>%
      {if (input$com_sector != "TOTAL") filter(., sector == input$com_sector) else .} %>%
      {if (input$com_summary != "TOTAL") filter(., summary == input$com_summary) else .} %>%
      {if (input$com_u_summary != "TOTAL") filter(., u_summary == input$com_u_summary) else .} %>%
      {if (input$com_detail != "TOTAL") filter(., detail == input$com_detail) else .}
  })
  
  # filter and aggregate output, supply and demand for selected industry
  tbl_outsupdem <- reactive({
    if (input$com_sector == "TOTAL") {
      # aggregate across all industries
      agg_outsupdem("total")
    } else if (input$com_summary == "TOTAL") {
      # aggregate across industries within selected sector
      agg_outsupdem("sector", input$com_sector)
    } else if (input$com_u_summary == "TOTAL") {
      # aggregate across industries within selected summary
      agg_outsupdem("summary", input$com_summary)
    } else if (input$com_detail == "TOTAL") {
      # aggregate across industries within selected u_summary
      agg_outsupdem("u_summary", input$com_u_summary)
    } else {
      # filter selected detail industry
      agg_outsupdem("detail", input$com_detail)
    }
  })
  
  
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
  
  output$com_codes <- renderReactable(
    com_codes() %>% 
      select(!c(ilevel, code)) %>% 
      reactable(sortable = FALSE, defaultColDef = colDef(maxWidth = 100), columns = list(title = colDef(maxWidth = 1000))))
  
  output$tbl_outsupdem <- renderReactable(
    tbl_outsupdem() |>
      reactable(defaultColDef = colDef(format = colFormat(digits = 0, separators = TRUE)))
  )
}

shinyApp(ui = ui, server = server)
