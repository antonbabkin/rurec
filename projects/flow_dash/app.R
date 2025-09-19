
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

ui <- fluidPage(
  fluidRow(
    # sector selector
    column(3, selectInput("com_sector", "Sector:", choices = appdata$com_codes %>% filter(ilevel == "sector") %>% pull(code) %>% c("TOTAL", .))),
    # summary selector: only show if sector is selected
    column(3, conditionalPanel(
      "input.com_sector !== 'TOTAL'",
      selectInput("com_summary", "Summary:", choices = "TOTAL")
    )),
    # u_summary selector: only show if summary is selected
    column(3, conditionalPanel(
      "input.com_summary !== 'TOTAL'",
      selectInput("com_u_summary", "Und. Summary:", choices = "TOTAL")
    )),
    # detail selector: only show if u_summary is selected
    column(3, conditionalPanel(
      "input.com_u_summary !== 'TOTAL'",
      selectInput("com_detail", "Detail:", choices = "TOTAL")
    ))
  ),
  fluidRow(tabsetPanel(
    tabPanel("codes", reactableOutput("com_codes")),
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
  
  
  observeEvent(input$com_sector, {
    freezeReactiveValue(input, "com_summary")
    freezeReactiveValue(input, "com_u_summary")
    updateSelectInput(
      inputId = "com_summary", 
      choices = if (input$com_sector == "TOTAL") "TOTAL" 
      else appdata$com_codes %>% filter(sector == input$com_sector, ilevel == "summary") %>% pull(code) %>% c("TOTAL", .))
  })
  observeEvent(input$com_summary, {
    freezeReactiveValue(input, "com_u_summary")
    updateSelectInput(
      inputId = "com_u_summary", 
      choices = if (input$com_summary == "TOTAL") "TOTAL"
      else appdata$com_codes %>% filter(summary == input$com_summary, ilevel == "u_summary") %>% pull(code) %>% c("TOTAL", .))
  })
  observeEvent(input$com_u_summary, {
    updateSelectInput(
      inputId = "com_detail", 
      choices = if (input$com_u_summary == "TOTAL") "TOTAL"
      else appdata$com_codes %>% filter(u_summary == input$com_u_summary, ilevel == "detail") %>% pull(code) %>% c("TOTAL", .))
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
