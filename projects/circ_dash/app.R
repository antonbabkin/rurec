
library(shiny)
library(tidyverse)
library(leaflet)
library(sf)

# data = readRDS("projects/circ_dash/circ_shiny.rds")
data = readRDS("circ_shiny.rds")

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      plotOutput("sector_dist"),
      width = 6
    ),
    mainPanel(
      fluidPage(
        leafletOutput("map"),
        selectInput("circularity", label = "Circularity Indicator",
          choices = c("trade_balance", "production_capacity", "trade_capacity", "retention", 
                      "production_dependency", "trade_dependency", "autonomy", "trade_openness"))),
      width = 6
    )
  ),
  htmlOutput("info")
)


server <- function(input, output) {
  
  output$map = renderLeaflet({
    values = data$cir[[input$circularity]]
    if (input$circularity == "trade_balance") {
      pal = colorNumeric(palette = "RdYlBu", domain = c(-0.4, 0.4))
    } else {
      pal = colorNumeric(palette = "magma", domain = values)
    }
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = data$cir,
        layerId = ~place,
        color = pal(values),
        weight = 1,
        opacity = 0.9,
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) %>%
      setView(lng = -89, lat = 43, zoom = 4) %>%
      addLegend(pal = pal, values = values, title = input$circularity)
  })
  
  output$info = renderText({
    place = input$map_shape_click$id
    if (is.null(place)) place = "55025"
    x = data$cir %>%
      st_drop_geometry() %>%
      filter(place == !!place) %>%
      mutate(
        across(output:exdem, \(x) formatC(x, format = "d", big.mark = ",")),
        across(production_capacity:trade_openness, \(x) formatC(x, format = "f", digits = 3))
      ) %>%
      as.list()
    
    names(x) %>%
      map_chr(\(k) paste0(k, ": ", x[[k]])) %>%
      paste(collapse = "<br>")
    
  })
  
  output$sector_dist = renderPlot({
    place = input$map_shape_click$id
    if (is.null(place)) place = "55025"
    t = data$cir %>%
      filter(place == !!place) %>%
      pull(name)
    data$osd %>%
      filter(place == !!place, name != "output") %>%
      ggplot() +
      geom_col(aes(value, sec_code, fill = name), position = "dodge") +
      labs(title = paste(t, "County demand and supply by sector")) +
      ylab(NULL) + xlab(NULL) +
      theme(legend.position = "bottom", legend.title = element_blank())
  })
}

shinyApp(ui = ui, server = server)
