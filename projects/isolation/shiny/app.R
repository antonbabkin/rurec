
library(shiny)
library(tidyverse)
library(leaflet)
library(tmap)
library(sf)
library(glue)

# this does not work when published
# source("isolation.R")
data <- readRDS("app.rds")


tf_map <- function(dir = c("in", "out"), county, norm_flow) {
  dir <- match.arg(dir)
  pal <- switch(dir, `in` = "-hcl.reds3", `out` = "-hcl.blues3")
  
  d1 <- switch(
    dir,
    `in` = data$flow %>%
      filter(to == county) %>%
      add_row(from = county, flow = NA) %>%
      rename(place = from, output = to_output),
    `out` = data$flow %>%
      filter(from == county) %>%
      add_row(to = county, flow = NA) %>%
      rename(place = to, output = from_output)
  )
  
  if (norm_flow) {
    d1 <- d1 %>%
      mutate(flow = flow / output)
    lab_format <- scales::label_percent()
  } else {
    lab_format <- scales::label_currency(scale_cut = scales::cut_short_scale())
  }
  
  d2 <- data$geo %>%
    filter(contiguous) %>%
    inner_join(d1, "place")
  
  tm_shape(data$geo_st %>% filter(contiguous)) +
    tm_polygons(col = "black", col_alpha = 0.3, fill = NULL) +
    tm_shape(d2) +
    tm_polygons(
      col = NULL,
      fill = "flow",
      fill.scale = tm_scale_intervals(n = 5, style = "quantile", value.na = "green", label.na = "", label.format = lab_format, values = pal),
      fill.legend = tm_legend(title = paste0(dir, "flow"), frame = FALSE)
      # fill.legend = tm_legend(title = paste0(dir, "flow"), orientation = "landscape", frame = FALSE, width = 50)
    )
}




ui <- fluidPage(
  fluidRow(column(8, leafletOutput("map")), column(4, tableOutput("profile"))),
  fluidRow(checkboxInput("normalized_flow", "Normalize flows by county output?", value = FALSE)),
  fluidRow(column(6, tmapOutput("inflow")), column(6, tmapOutput("outflow")))
)


server <- function(input, output) {
  
  selected_county <- reactiveVal("55025")
  
  output$map <- renderLeaflet({
    pal <- colorQuantile(palette = "RdYlBu", domain = NULL, n = 5, reverse = TRUE)
    quantileFormat <- function(type, cuts, p) {
      n <- length(cuts)
      cuts <- format(round(cuts, 3), trim = TRUE, scientific = FALSE)
      paste(cuts[-n], "-", cuts[-1])
    }
    
    leaflet(data$prof) %>%
      addTiles() %>%
      addPolygons(
        color = ~pal(emp_gr), group = "emp_gr",
        layerId = ~paste0("emp", place),
        label = ~place,
        weight = 1, opacity = 0.9,
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~emp_gr, title = "emp_gr", labFormat = quantileFormat, group = "emp_gr", position = "bottomright") %>%
      addPolygons(
        color = ~pal(pop_gr), group = "pop_gr",
        layerId = ~paste0("pop", place),
        label = ~place,
        weight = 1, opacity = 0.9,
        highlightOptions = highlightOptions(color = "black", weight = 2, bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~pop_gr, title = "pop_gr", labFormat = quantileFormat, group = "pop_gr", position = "bottomright") %>%
      setView(lng = -95, lat = 38, zoom = 4) %>%
      addLayersControl(overlayGroups = c("emp_gr", "pop_gr"))
  })
  
  # Change selected state and county on shape click
  observeEvent(input$map_shape_click, {
    shape_info <- input$map_shape_click
    if (!is.null(shape_info)) {
      selected_county(str_sub(shape_info$id, 4))
    }
  })
  
  output$profile <- renderTable({
    data$prof_show %>%
      filter(place == selected_county()) %>%
      `rownames<-`("val") %>%
      t() %>%
      as_tibble(rownames = "var")
  })
  
  output$inflow <- renderTmap({tf_map("in", selected_county(), input$normalized_flow)})
  
  output$outflow <- renderTmap({tf_map("out", selected_county(), input$normalized_flow)})

}

shinyApp(ui = ui, server = server)
