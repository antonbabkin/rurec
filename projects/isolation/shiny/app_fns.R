library(tidyverse)
library(leaflet)
library(tmap)


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
      fill.legend = tm_legend(title = paste0(dir, "flow"), orientation = "landscape", frame = FALSE, width = 50)
    )
}

