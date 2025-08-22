
library(shiny)
library(reactable)
library(tidyverse)

appdata <- readRDS("app.rds")

ui <- fluidPage(
  fluidRow(
    column(3, selectInput("com_sector", "Sector:", choices = appdata$com_codes %>% filter(ilevel == "sector") %>% pull(code) %>% c("TOTAL", .))),
    column(3, selectInput("com_summary", "Summary:", choices = "TOTAL")),
    column(3, selectInput("com_u_summary", "Und. Summary:", choices = "TOTAL")),
    column(3, selectInput("com_detail", "Detail:", choices = "TOTAL"))
  ),
  fluidRow(reactableOutput("com_codes"))
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
}

shinyApp(ui = ui, server = server)
