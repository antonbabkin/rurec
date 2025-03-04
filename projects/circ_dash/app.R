
library(shiny)
library(DT)
library(tidyverse)
library(leaflet)
library(sf)
library(glue)

# data = readRDS("projects/circ_dash/circ_shiny.rds")
data = readRDS("circ_shiny.rds")


ui <- fluidPage(fluidRow(
  column(7,
    selectInput(
      "circularity",
      "Circularity Indicator",
      choices = c(
        "Production capacity" = "production_capacity",
        "Trade capacity" = "trade_capacity",
        "Retention" = "retention",
        "Production Dependency" = "production_dependency",
        "Trade dependency" = "trade_dependency",
        "Autonomy" = "autonomy",
        "Trade openness" = "trade_openness",
        "Trade balance" = "trade_balance"
      ),
      selected = "retention"
    ),
    leafletOutput("map")
  ),
  column(5,
    # plotOutput("sector_dist"),
    selectInput(
      "outcome",
      label = "Economic Outcome",
      choices = c(
        "Number of establishments" = "est",
        "Employment" = "emp",
        "Real payroll" = "pay"
      ),
      selected = "emp"
    ),
    radioButtons("highlight", "Highlight", choices = c("metro", "non-metro", "both"), selected = "both", inline = TRUE),
    plotOutput("cir_gr")
  )), 
  DTOutput("table"))


server <- function(input, output) {
  
  selected_county = reactiveVal("55025")
  selected_state = reactiveVal("55")
  
  output$map = renderLeaflet({
    values = data$cir[[input$circularity]]
    if (input$circularity == "trade_balance") {
      pal = colorNumeric(palette = "RdYlBu", domain = c(-0.4, 0.4), reverse = TRUE)
    } else {
      pal = colorNumeric(palette = "magma", domain = values, reverse = TRUE)
    }
    
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      addPolygons(data = data$shp_st, weight = 4, fill = FALSE) %>%
      addPolygons(
        data = data$cir,
        layerId = ~place,
        color = pal(values),
        weight = 1,
        opacity = 0.9,
        label = ~place,
        popup = ~place,
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) %>%
      setView(lng = -89, lat = 43, zoom = 4) %>%
      addLegend(pal = pal, values = values, title = input$circularity)
  })
  
  # Change selected state and county on shape click
  observeEvent(input$map_shape_click, {
    shape_info <- input$map_shape_click
    if (!is.null(shape_info)) {
      selected_state(str_sub(shape_info$id, 1, 2))
      selected_county(shape_info$id)
    }
  })
  
  
  
  output$table = renderDT({
    f <- function(x) {
      x %>%
        select(!place) %>%
        mutate(
          across(c(retention, autonomy), \(x) prettyNum(round(x, 3), scientific = FALSE)),
          across(c(est, emp, pay), \(x) prettyNum(round(x), big.mark = ",", scientific = FALSE)),
          across(c(est_gr, emp_gr, pay_gr), \(x) paste0(formatC(x * 100, format = "f", digits = 1), "%"))
        ) %>%
        as.list()
    }
    
    d1 <- data$table %>%
      filter(place == "00000") %>%
      f()
    d2 <- data$table %>%
      filter(place == paste0(selected_state(), "000")) %>%
      f()
    d3 <- data$table %>%
      filter(place == selected_county()) %>%
      f()
    header <- htmltools::withTags(
      table(
        thead(
          tr(lapply(d1, \(x) th(style = "text-align: right;", x))),
          tr(lapply(d2, \(x) th(style = "text-align: right;", x))),
          tr(lapply(d3, \(x) th(style = "text-align: right;", x))),
          tr(
            th(rowspan = 2, "Name"),
            th(colspan = 2, style = "text-align: center;", "Circularity"),
            th(colspan = 2, style = "text-align: center;", "Number of establishments"),
            th(colspan = 2, style = "text-align: center;", "Employment"),
            th(colspan = 2, style = "text-align: center;", "Real payroll")
          ),
          tr(
            th("Retention"), th("Autonomy"),
            th("Level"), th("Growth rate"),
            th("Level"), th("Growth rate"),
            th("Level"), th("Growth rate"))
        )
      )
    )
    data$table %>%
      filter(str_sub(place, 1, 2) == selected_state(), str_sub(place, 3, 5) != "000") %>%
      select(!place) %>%
      datatable(
        rownames = FALSE, 
        container = header) %>%
      formatRound(c("retention", "autonomy"), digits = 3) %>%
      formatRound(c("est", "emp", "pay"), digits = 0) %>%
      formatPercentage(c("est_gr", "emp_gr", "pay_gr"), digits = 1)
  })
  
  
  output$cir_gr = renderPlot({
    gr_var = paste0(input$outcome, "_gr")
    pal <- switch(
      input$highlight,
      both = c("black", "black"),
      metro = c("gray", "black"),
      "non-metro" = c("black", "gray")
    )
    data$scat %>%
      filter(st == selected_state()) %>%
      ggplot() +
      geom_point(aes(.data[[input$circularity]], .data[[gr_var]], size = .data[[input$outcome]], color = metro)) +
      scale_color_manual(values = pal) +
      geom_point(aes(.data[[input$circularity]], .data[[gr_var]], size = .data[[input$outcome]]),
                 color = "red", data = filter(data$scat, place == selected_county())) +
      labs(title = paste0("Circularity and economic growth in ", data$st_names[selected_state()], ", 2010-2019")) +
      ylab(paste(input$outcome, "growth")) + 
      xlab(input$circularity) +
      scale_y_continuous(labels = scales::label_percent()) +
      theme(legend.position = "none", aspect.ratio = 3/4)
  }, res = 96)
  
}

shinyApp(ui = ui, server = server)
