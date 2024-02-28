
# This script generates visualizations

# R libraries ----
library(logger)
log_threshold(DEBUG)
library(arrow)
library(tidyverse)
library(glue)
library(REAT)

library(viridis)
library(ggiraph)
library(scales)
library(htmltools)
library(cowplot)
library(ggnewscale)
library(RColorBrewer)

# R scripts ----
source("R/basic_utilities.R", local = (util <- new.env()))
source("R/geography.R", local = (geog <- new.env()))
source("R/place_output.R", local = (place_output <- new.env()))
source("R/dataprep_bea_io.R", local = (bea_io <- new.env()))

source("R/connectedness.R", local = (connect <- new.env()))
source("R/dataprep.R", local = (dataprep <- new.env()))

options(scipen=999)

# Data objects ----
ipath <- list(
  # data dependencies
)

opath <- list(
  html_ = "data/visualization/html_{central_place}_{year}_{class_system}_{ilevel}_{bus_data}_{cbsa}_{paradigm}_{indicator}_{color}_{cluster_level}.rds"
  # data products
)


clear_outputs <- function() {
  util$clear_paths(opath)
}

# utility functions----

# get long format industry name
short2long <- function(short_name){
  switch(short_name, sec = {"sector"}, sum = {"summary"}, det = {"detail"})
}

# get title format from underscores name
underscores2title <- function(underscores_string){
  x <- underscores_string %>% 
    str_split(., "_")  %>% 
    unlist() %>% 
    toString() %>% 
    gsub(",", "", .) %>% 
    str_to_title()
  return(x)
}


# get rounded and formatted values
round_form <- function(x, 
                       decimal_place = 0){
  format(round(x, decimal_place), big.mark = ",", scientific = FALSE) 
}

#inverse hyperbolic sine function for scales package
ihs_trans <- function(){trans_new("ihs", function(x){asinh(x)}, function(x){sinh(x)} )}

# ggplot functions----

## hist ----

# return list of maximal and minimal elements from y-axes of two plot objects
comprable_yscale_range <- function(plot_1,
                                   plot_2, 
                                   ...){
  ymin <- min(layer_scales(plot_1)$y$range$range[1], 
              layer_scales(plot_2)$y$range$range[1])
  ymax <- max(layer_scales(plot_1)$y$range$range[2], 
              layer_scales(plot_2)$y$range$range[2])
  ggproto <- scale_y_continuous(limits = c(ymin, ymax), ...)
  return(ggproto)
} 

# list of common boilerplate plot components
boil_hist_theme <- function(){
  list(
    theme_minimal(),
    theme(axis.text.x = element_text(size = rel(.75), 
                                     angle = 300,
                                     hjust = 0),
          axis.text.y = element_text(size = rel(.75)),
          plot.subtitle = element_text(size = rel(.75), 
                                       vjust = -2), 
          plot.margin = margin(t = 0, 
                               r = 50, 
                               b = 0, 
                               l = 0, 
                               unit = "pt")), 
    labs(x = element_blank(),
         y = element_blank(),
         subtitle = element_blank(),
         title = element_blank())
  )
}

## maps ----

# a minimal map command
base_map <- function(spatial_dataframe, ...){
  sdf <- spatial_dataframe
  g <- ggplot(sdf, ...) + 
    geom_sf_interactive() +
    coord_sf(crs = "+init=EPSG:6579") 
  return(g)
}

# overwrite default aesthetics
# scale_fill_discrete <- function(...) scale_fill_brewer(... , palette = "Set3")
# scale_colour_discrete <- function(...) scale_colour_brewer(..., palette = "Set3")


# boilerplate ggplpot/geom_sf theme components for maps
boil_map_theme <- function(dataframe, 
                           fill_variable,
                           data_id = "place"){
  data <- dataframe
  fv <- fill_variable
  did <- data_id
  df <- list(
    geom_sf_interactive(
      aes(fill = data[[fv]],
          tooltip = if("STATE" %in% names(dataframe)){
            glue("Place: {NAME}, {STATE}\nFIPS: {place}\nValue: {if(is.numeric(data[[fv]])){round_form({data[[fv]]}, 2)}else{data[[fv]]}}")
          }else{
            glue("Place: {NAME}\nFIPS: {place}\nValue: {if(is.numeric(data[[fv]])){round_form({data[[fv]]}, 2)}else{data[[fv]]}}")
          },
          data_id = data[[did]]), 
      color = alpha("grey", 0.2)),
    labs(fill = underscores2title(fv)),
    theme_void(),
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.55)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.85, 0.2),
          plot.caption = element_text(hjust = 0.9, size = rel(0.5))) 
  )
  return(df)
}


# list of plots for inset mapping
usa_tile_list <- function(spatial_dataframe, ...){
  sdf <- spatial_dataframe
  lfe <- spatial_dataframe %>% {.[!grepl("^(02|15|60|66|69|72|78)", .[["place"]]), ]}
  ako <- spatial_dataframe %>% {.[grepl("^(02)", .[["place"]]), ]}
  hio <- spatial_dataframe %>% {.[grepl("^(15)", .[["place"]]), ]}
  pro <- spatial_dataframe %>% {.[grepl("^(72)", .[["place"]]), ]}
  g1 <- ggplot(lfe, ...) +
    geom_sf_interactive() 
    #coord_sf(crs = "+init=EPSG:4326")
    #coord_sf(crs = "+init=EPSG:6579")
  g2 <- ggplot(ako, ...) +
    geom_sf_interactive() + 
    #coord_sf(crs = "+init=EPSG:26934") + 
    theme(legend.position = "none")
  g3 <- ggplot(hio, ...) +
    geom_sf_interactive() + 
    #coord_sf(crs = "+init=EPSG:6629") + 
    theme(legend.position = "none")
  g4 <- ggplot(pro, ...) +
    geom_sf_interactive() + 
    #coord_sf(crs = "+init=EPSG:4437") + 
    theme(legend.position = "none")
  return(list(g1, g2, g3, g4))
}

# tiled layout for plot with AK, HI, and PR inset maps (optional)
usa_tile_map <- function(tile_plot_list, 
                         AK = TRUE,
                         HI = TRUE,
                         PR = TRUE,
                         interactive = TRUE){
  tpl <- tile_plot_list
  ggp <- ggdraw() +
    draw_plot(tpl[[1]])
  if (AK & nrow(tpl[[2]]$data)!=0){ggp <- ggp + draw_plot(tpl[[2]], x = 0.02, y = 0.02, width = 0.5, height = 0.35)}
  if (HI & nrow(tpl[[3]]$data)!=0){ggp <- ggp + draw_plot(tpl[[3]], x = 0.5, y = 0.07, width = 0.15, height = 0.15)}
  if (PR & nrow(tpl[[4]]$data)!=0){ggp <- ggp + draw_plot(tpl[[4]], x = 0.65, y = 0.02, width = 0.15, height = 0.15)}
  if (interactive){
    ggp <- girafe_plot(ggobj = ggp)
  }
  return(ggp)
}

# boilerplate tile map formatting 
boil_tile <- function(tile_list,
                      fill_variable,
                      caption,
                      ..., 
                      data_id = "place",
                      legend = TRUE){
  tl <- tile_list
  fv <- fill_variable
  cp <- caption
  arg <- list(...)
  df <- list()
  for (l in 1:length(tl)){
    ggp <- tl[[l]] +
      boil_map_theme(dataframe = tl[[l]]$data, fill_variable = fv, data_id = data_id) +
      arg +
      coord_sf(crs = switch(l, "+init=EPSG:4326", "+init=EPSG:26934", "+init=EPSG:6629", "+init=EPSG:4437")) + {
        if (legend){
          if (l != 1){theme(legend.position = "none")}
        } else {
          theme(legend.position = "none")
        }
      } + {
        if (l == 1){labs(caption = cp)}
      }
    df[[l]] <- ggp
  }
  return(df)
}



## interact ----
# return interactive ggplot girafe object
girafe_plot <- function(ggobj){
  girafe(ggobj = ggobj, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_selection(type = "multiple", 
                          only_shiny = FALSE,
                          css = "fill:black"),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                             background-color:gray;
                                             color:white;
                                             padding:10px;
                                             border-radius:5px;"),
           opts_toolbar = opts_toolbar(position = "top", saveaspng = TRUE) ))
}

# Temporal Absorption Density ----
## format data ----

substitute(connect$call_temporal_eca_table(set_of_years = 2000:2020, spatial = F, trim = NULL, ...) )

## viz ----

absorption_density_plot <- function(temporal_absorption_metric_table,
                                    fill_variable,
                                    id_variable,
                                    trans = NULL, #log10
                                    colorbreaks = c("#440154FF" = 2000:2006, 
                                                    "#1E9C89FF" = 2007:2016, 
                                                    "#D64B40FF" = 2017, 
                                                    "#FDE725FF" =  2018:2020),
                                    interactive = TRUE){
  df <- temporal_absorption_metric_table
  g <- ggplot(df) + 
    geom_density_interactive(aes(x = (.data[[fill_variable]]), 
                                 tooltip = .data[[id_variable]],
                                 data_id = .data[[id_variable]],
                                 `data-id` = .data[[id_variable]],
                                 color = .data[[id_variable ]]),
                             extra_interactive_params = "data-id") + 
    labs(color = "Year", x = underscores2title(fill_variable), y = "Density")
  if(!is.null(colorbreaks)){
    g <- g + 
      scale_color_manual_interactive(values = substr(names(colorbreaks), 1, 9),
                                            extra_interactive_params = "data-id",
                                            `data-id` = function(x) x, 
                                            data_id = function(x) x) 
    }
  if(!is.null(trans)){
    g <- g + scale_x_continuous(trans = trans)
    }  
  if (interactive) {
    g <- girafe_plot(g) 
    g[["x"]][["settings"]][["hover"]][["css"]] <- ".hover_data_SVGID_ { stroke:orange;r:12pt; }\ntext.hover_data_SVGID_ { stroke:none;fill:black;fill-opacity:1; }"
    }
  return(g)
}

  
# Industry Histogram ----
## format data ----

# tidy table of industry "output" for histograms
histogram_output_table <- function(intra_level_concordance,
                                   bea_description_concordance,
                                   io_factor_list,
                                   color = c("turbo", "viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "rainbow"),
                                   cluster_level = c("sec", "sum", "det"),
                                   cbsa = FALSE,
                                   central_place = NULL,
                                   trim = "^(60|66|69|72|78)|(999)$",
                                   cluster_subset = NULL) {
  color <- match.arg(color)
  cluster_level <- match.arg(cluster_level)
  ilc <- intra_level_concordance
  bdc <- bea_description_concordance
  iol <- io_factor_list
  if (!is.null(central_place)){
    if(cbsa){
      central_place <- geog$fips2cbsa(fips = central_place,
                                      year = year)
    }
    iol <- iol[iol$place == central_place, ]
  }
  if (!is.null(trim)){
    iol <- iol[!grepl(trim, iol$place), ]
  }
  df <- merge(ilc, bdc, by.x = names(ilc)[1], by.y = names(bdc)[1], all.x = TRUE) %>% 
    {left_join(iol, .)} 
  df <- df %>% 
    {data.frame("color" = do.call(color, as.list(length(unique(.$description)))),
                "description" = unique(.$description))} %>% 
    {inner_join(df, .)} %>% 
    subset(., select = -c(place)) %>% 
    {aggregate(.[sapply(.,is.numeric)], as.list(.[sapply(., \(x) !is.numeric(x))]), FUN=sum)} %>% 
    {arrange(., .[["indcode"]])}
  if (!is.null(cluster_subset)){
    df <- df[grepl(cluster_subset, df[[short2long(cluster_level)]]), ]
  }
  return(df)
}

# generate a table of industry/commodity output/supply/demand (for a place/places)
call_histogram_output_table <- function(year,
                                        ilevel = c("det", "sum", "sec"),
                                        class_system = c("industry", "commodity"),
                                        paradigm = c("factor", "domestic", "capital"),
                                        bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                        cbsa = FALSE,
                                        verbose = FALSE,
                                        central_place = NULL,
                                        trim = "^(60|66|69|72|78)|(999)$",
                                        cluster_subset = NULL,
                                        color = c("turbo", "viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "rainbow"),
                                        cluster_level = c("sec", "sum", "det")){
  ilevel <- match.arg(ilevel)
  cluster_level <- match.arg(cluster_level)
  bea_io$cluster_logic(ilevel, cluster_level)
  ilc <- bea_io$call_intra_level_concordance(year = year, 
                                             cluster_level = cluster_level)
  bdc <- bea_io$call_bea_description_concordance(year = year)
  iol <- place_output$call_factor_list(year = year,
                                       class_system = class_system,
                                       paradigm = paradigm,
                                       ilevel = ilevel,
                                       bus_data = bus_data,
                                       cbsa = cbsa,
                                       verbose = verbose)
  df <- histogram_output_table(intra_level_concordance = ilc,
                               bea_description_concordance = bdc,
                               io_factor_list = iol,
                               color = color,
                               cluster_level = cluster_level,
                               cbsa = cbsa,
                               central_place = central_place,
                               trim = trim,
                               cluster_subset = cluster_subset)

  return(df)
}

## viz ----

# histogram distribution of industry "output"
industry_histogram <- function(output_table,
                               indicator = c("gross_output", "intermediate_supply", "intermediate_demand", "net_supply", "net_demand"),
                               cluster_level = c("sec", "sum", "det"),
                               ilevel = c("det", "sum", "sec")){
  indicator <- match.arg(indicator)
  ilevel <- match.arg(ilevel)
  cluster_level <- match.arg(cluster_level)
  bea_io$cluster_logic(ilevel, cluster_level)
  df <- output_table
  subtitle <- indicator %>% underscores2title() %>% {paste0(., " ($1,000)")}
  hist_args <- function(df,
                        ilevel,
                        cluster_level,
                        subtitle){
    dl <- list(
      geom_col_interactive(aes(x = factor(.data[[short2long(cluster_level)]], levels = unique(.data[[short2long(cluster_level)]])),
                               y = .data[[indicator]],
                               fill = .data[["indcode"]],
                               tooltip = paste0(str_to_sentence(short2long(ilevel)), " Sector: ", .data[["indcode"]], "\n Value: ", round_form(.data[[indicator]]) ),
                               data_id = .data[["indcode"]]),
                           color = NA),
      scale_fill_manual(values = df[["color"]]),
      labs(subtitle = subtitle),
      scale_y_continuous(labels = comma),
      guides(fill = "none"),
      theme_minimal(),
      theme(axis.text.x = element_text(size = rel(.75), angle = 300, hjust = 0),
            axis.text.y = element_text(size = rel(.75)),
            plot.subtitle = element_text(size = rel(.75), vjust = -2), 
            plot.margin = margin(t = 0, r = 50, b = 0, l = 0, unit = "pt")), 
      labs(x = element_blank(),
           y = element_blank(),
           subtitle = element_blank(),
           title = element_blank())
    )
    return(dl)
  }
  g <- ggplot(df) + 
    hist_args(df = df,
               ilevel = ilevel,
               cluster_level = cluster_level,
               subtitle = subtitle)
  return(g)
}

# generate a histogram of industry/commodity output/supply/demand (for a place/places)
call_sector_histogram <- function(year,
                                  ilevel = c("det", "sum", "sec"),
                                  class_system = c("industry", "commodity"),
                                  paradigm = c("factor", "domestic", "capital"),
                                  bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                  cbsa = FALSE,
                                  verbose = FALSE,
                                  central_place = NULL,
                                  trim = "^(60|66|69|72|78)|(999)$",
                                  cluster_subset = NULL,
                                  indicator = c("gross_output", "intermediate_supply", "intermediate_demand", "net_supply", "net_demand"),
                                  color = c("turbo", "viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "rainbow"),
                                  cluster_level = c("sec", "sum", "det"),
                                  interactive = FALSE){
  ilevel <- match.arg(ilevel)
  cluster_level <- match.arg(cluster_level)
  bea_io$cluster_logic(ilevel, cluster_level)
  ilc <- bea_io$call_intra_level_concordance(year = year, 
                                             cluster_level = cluster_level)
  bdc <- bea_io$call_bea_description_concordance(year = year)
  iol <- place_output$call_factor_list(year = year,
                                       class_system = class_system,
                                       paradigm = paradigm,
                                       ilevel = ilevel,
                                       bus_data = bus_data,
                                       cbsa = cbsa,
                                       verbose = verbose)
  hot <- histogram_output_table(intra_level_concordance = ilc,
                                bea_description_concordance = bdc,
                                io_factor_list = iol,
                                color = color,
                                cluster_level = cluster_level,
                                cbsa = cbsa,
                                central_place = central_place,
                                trim = trim,
                                cluster_subset = cluster_subset)
  df <- industry_histogram(output_table = hot, 
                           indicator = indicator, 
                           cluster_level = cluster_level, 
                           ilevel = ilevel)
  if (interactive) {
    df <- girafe_plot(df)
    }
  return(df)
}

# generate an html histogram of industry/commodity output/supply/demand (for a place/places)
call_sector_histogram_html <- function(central_place,
                                       year,
                                       ilevel = c("det", "sum", "sec"),
                                       class_system = c("industry", "commodity"),
                                       paradigm = c("factor", "domestic", "capital"),
                                       bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                       cbsa = FALSE,
                                       verbose = FALSE,
                                       indicator = c("gross_output", "intermediate_supply", "intermediate_demand", "net_supply", "net_demand"),
                                       color = c("turbo", "viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "rainbow"),
                                       cluster_level = c("sec", "sum", "det")){
  ilevel <- match.arg(ilevel)
  class_system <- match.arg(class_system)
  paradigm <- match.arg(paradigm)
  bus_data <- match.arg(bus_data)
  indicator <- match.arg(indicator)
  color <- match.arg(color)
  cluster_level <- match.arg(cluster_level)
  cache_path <- glue(opath$html_)
  if (file.exists(cache_path)) {
    log_debug(paste("read from cache", cache_path))
    return(readRDS(cache_path))
  }
  sbc <- call_sector_histogram(year = year,
                               ilevel = ilevel,
                               class_system = class_system,
                               paradigm = paradigm,
                               bus_data = bus_data,
                               cbsa = cbsa,
                               verbose = verbose,
                               central_place = central_place,
                               trim = NULL,
                               cluster_subset = NULL,
                               indicator = indicator,
                               color = color,
                               cluster_level = cluster_level,
                               interactive = FALSE)
  df <- plotTag(sbc, alt = "") %>% 
    as.character()
  log_debug(paste("save to cache", cache_path))
  saveRDS(df, util$mkdir(cache_path))
  return(df)
}

# generate a paneled histogram plot of industry/commodity output/supply/demand (for a place/places)
call_sector_histogram_multi <- function(year,
                                          ilevel = c("det", "sum", "sec"),
                                          class_system = c("industry", "commodity"),
                                          paradigm = c("factor", "domestic", "capital"),
                                          bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                          cbsa = FALSE,
                                          verbose = FALSE,
                                          central_place = NULL,
                                          trim = "^(60|66|69|72|78)|(999)$",
                                          cluster_subset = NULL,
                                          color = c("turbo", "viridis", "magma", "inferno", "plasma", "cividis", "rocket", "mako", "rainbow"),
                                          cluster_level = c("sec", "sum", "det"),
                                          common_yscale = TRUE,
                                          interactive = FALSE){
  ps <- c("intermediate_supply", "intermediate_demand", "net_supply", "net_demand")
  df <- c()
  for (i in ps){
    df[[i]] <- call_sector_histogram(indicator = i,
                                     year = year,
                                     ilevel = ilevel,
                                     class_system = class_system,
                                     paradigm = paradigm,
                                     bus_data = bus_data,
                                     cbsa = cbsa,
                                     verbose = verbose,
                                     central_place = central_place,
                                     trim = trim,
                                     cluster_subset = cluster_subset,
                                     color = color,
                                     cluster_level = cluster_level,
                                     interactive = FALSE)
  }
  if (common_yscale){
    df[[1]] <- df[[1]] + comprable_yscale_range(df[[1]], df[[2]], labels = comma)
    df[[2]] <- df[[2]] + comprable_yscale_range(df[[1]], df[[2]], labels = comma)
    df[[3]] <- df[[3]] + comprable_yscale_range(df[[3]], df[[4]], labels = comma)
    df[[4]] <- df[[4]] + comprable_yscale_range(df[[3]], df[[4]], labels = comma)
  }
  df <- do.call(plot_grid, df)
  if (interactive) {
    df <- girafe_plot(df)
  }
  return(df)
}



# Categorical Choropleth ----

## format data ----

## viz ----

cat_choro_map <- function(
    spatial_dataframe,
    fill_variable,
    caption = NULL,
    legend = FALSE,
    interactive = TRUE){
  sdf <- spatial_dataframe
  fv <- fill_variable
  tl <- usa_tile_list(sdf)
  if (is.null(caption)){caption = "\n"} else {caption = caption}
  df <- boil_tile(tile_list = tl, 
                  fill_variable = fv, 
                  caption = caption,
                  data_id = fv,
                  legend = legend) %>%
    {usa_tile_map(tile_plot_list = .,
                  interactive = interactive)}
  return(df)
}
  

# Nominal (numerical) Choropleth ----

## format data ----

## viz ----

nominal_choro_map <- function(spatial_dataframe,
                              fill_variable,
                              caption = NULL,
                              interactive = TRUE){
  sdf <- spatial_dataframe
  fv <- fill_variable
  tl <- usa_tile_list(sdf)
  mn <-  min(sdf[[fv]], na.rm = T)
  mx <-  max(sdf[[fv]], na.rm = T)
  if (is.null(caption)){caption = "\n"} else {caption = caption}
  df <- boil_tile(tile_list = tl,
                  fill_variable = fv,
                  caption = caption,
                  scale_fill_viridis(direction = -1, limits=c(floor(mn), ceiling(mx) ), option = "D" ) ) %>%
    {usa_tile_map(tile_plot_list = .,
                  interactive = interactive)}
  return(df)
}



# Normalized Choropleth ----

## format data ----

substitute(connect$call_eca_table_spatial(year = 2012, impedance = T, functional_form = "distance", scalar_constant = 300, threshold = .25, trim = "^(60|66|69|78)|(999)$", ...))
substitute(geog$call_impedance_distribution_table(central_place = "01001", functional_form = "secant", scalar_constant = 200, ...))

## viz ----
normal_choro_map <- function(spatial_dataframe,
                             fill_variable,
                             caption = NULL,
                             interactive = TRUE){
  sdf <- spatial_dataframe
  fv <- fill_variable
  tl <- usa_tile_list(sdf)
  if (is.null(caption)){caption = "\n"} else {caption = caption}
  df <- boil_tile(tile_list = tl, 
                  fill_variable = fv, 
                  caption = caption, 
                  scale_fill_viridis(direction = -1, limits=c(floor(0), ceiling(1) ), option = "C" ) ) %>%
    {usa_tile_map(tile_plot_list = .,
                  interactive = interactive)}
  return(df)
}

# Divergent Choropleth ----

## format data ----

## viz ----

# divergent fill-scale color and distribution options (see trade flow potential map)
divergent_scales <- function(min_value, 
                             max_value,
                             scale_style = c("inverse_hyperbolic_sine", "full_spectrum_midpoint", "constant_spread_midpoint", "constant_spread_spectrum"),
                             colors = rev(brewer.pal(7, "RdBu")),  # see display.brewer.all()
                             label_min = "Net Negative",
                             label_max = "Net Positive"){
  scale_style <- match.arg(scale_style)
  mn <- min_value
  mx <- max_value
  #inverse hyperbolic sine scale transformation
  if (scale_style == "inverse_hyperbolic_sine"){
    return(
      scale_fill_gradientn(colors = colors, 
                           trans = "ihs", 
                           breaks = c(mn, 0, mx), 
                           labels = c(label_min, "0", label_max), 
                           values = rescale(c(asinh(mn), 0, asinh(mx))), 
                           limits = c(mn, mx) ) 
    )
  }
  #full spectrum from midpoint transformation
  if (scale_style == "full_spectrum_midpoint"){
    return(
      scale_fill_gradientn(colors = colors,  
                           breaks = c(mn+1, mx-1), 
                           labels = c(label_min, label_max),
                           limits = c(mn-1, mx+1),
                           values = rescale(c(mn, 0, mx)))
    )
  } 
  #constant spread from midpoint transformation with full spectrum shown
  if (scale_style == "constant_spread_midpoint"){
    return(
      scale_fill_gradientn(colors = colors, 
                           breaks = c(-max(abs(mn), mx)+1, max(abs(mn), mx)-1), 
                           labels = c(label_min, label_max), 
                           limits = c(-max(abs(mn), mx)-1, max(abs(mn), mx)+1) ) 
    )
  } 
  #constant spread from midpoint transformation with only used spectrum shown with value labels
  if (scale_style == "constant_spread_spectrum"){
    return(
      scale_fill_gradient2(low = "#2166AC", 
                           mid = "#F7F7F7", 
                           high = "#B2182B", 
                           midpoint = 0)
    )
  } 
}


# map of trade balance
diverge_choro_map <- function(spatial_dataframe, #table of trade flow potential with spatial components
                              fill_variable,
                              caption = NULL,
                              scale_style = c("inverse_hyperbolic_sine", "full_spectrum_midpoint", "constant_spread_midpoint", "constant_spread_spectrum"),
                              colors = rev(brewer.pal(7, "RdBu")), # see display.brewer.all()
                              label_min = "Net Negative",
                              label_max = "Net Positive",
                              interactive = TRUE){
  sdf <- spatial_dataframe
  fv <- fill_variable
  tl <- usa_tile_list(sdf)
  if (is.null(caption)){caption = "\n"} else {caption = caption}
  mn <-  min(sdf[[fv]], na.rm = T)
  mx <-  max(sdf[[fv]], na.rm = T)
  df <- boil_tile(tile_list = tl, 
                  fill_variable = fv, 
                  caption = caption, 
                  divergent_scales(min_value = mn,  max_value = mx, scale_style = scale_style, colors = colors, label_min = label_min, label_max = label_max) ) %>%
    {usa_tile_map(tile_plot_list = .,
                  interactive = interactive)}
  return(df)
}


# Trade Potential ----

## format data ----

substitute(place_output$call_extraction_table(2012, paradigm = "domestic", class_system = "commodity", cluster_subset = "^312120", ilevel = "det", cluster_level = "det", ...))

## viz ----


# map of trade balance
flow_potential_map <- function(spatial_dataframe, #table of trade flow potential with spatial components
                               fill_variable,
                               year = 2012,
                               cluster_subset,
                               cluster_level = c("sec", "sum", "det"),
                               scale_style = c("inverse_hyperbolic_sine", "full_spectrum_midpoint", "constant_spread_midpoint", "constant_spread_spectrum"),
                               label_min = "Net Supplier",
                               label_max = "Net Demander",
                               interactive = TRUE){
  cluster_level <- match.arg(cluster_level)
  caption <- bea_io$call_intra_level_concordance(year = year, cluster_level = cluster_level) %>%
    {.[grepl(cluster_subset, .[[1]]), short2long(cluster_level)]} %>%
    unique() %>%
    as.list() %>%
    {lapply(., function(x){paste0(x, ": ", bea_io$beacode2description(code = x, year = year)) }) } %>%
    paste(collapse = "\n")
  df <- diverge_choro_map(spatial_dataframe = spatial_dataframe,
                          fill_variable = fill_variable,
                          caption = caption,
                          scale_style = scale_style,
                          label_min = label_min, 
                          label_max = label_max,
                          interactive = interactive )
  return(df)
}


# generate a map of trade flow potential for intermediate supply and demand  
call_flow_potential_map <- function(year,
                                    ilevel = c("det", "sum", "sec"),
                                    class_system = c("industry", "commodity"),
                                    paradigm = c("factor", "domestic", "capital"),
                                    bus_data = c("cbp_imp", "cbp_raw", "infogroup"),
                                    verbose = FALSE,
                                    cluster_level = c("sec", "sum", "det"),
                                    cbsa = FALSE,
                                    cluster_subset = "^11",
                                    trim = "^(60|66|69|78)|(999)$",
                                    scale_style = c("inverse_hyperbolic_sine", "full_spectrum_midpoint", "constant_spread_midpoint", "constant_spread_spectrum"),
                                    fill_variable = "extract",
                                    interactive = TRUE){
  # TODO:make extraction_table input RAS or ECA friendly
  sdf <- call_extraction_table(year = year,
                               ilevel = ilevel,
                               class_system = class_system,
                               paradigm = paradigm,
                               bus_data = bus_data,
                               verbose = verbose,
                               cluster_level = cluster_level,
                               cbsa = cbsa,
                               cluster_subset = cluster_subset,
                               trim = trim)
  gp <- flow_potential_map(spatial_dataframe = sdf,
                           fill_variable = fill_variable,
                           cluster_subset = cluster_subset,
                           cluster_level = cluster_level,
                           year = year,
                           scale_style = scale_style,
                           interactive = interactive)
return(gp)
}


# Central Flow ----

## format data ----

substitute(connect$call_eca_table_spatial(2012, central_place = "01001", trim = "^(60|66|69|78)|(999)$", impedance = TRUE, functional_form = "secant", ...))

## viz ----

# map trade flow (max potential or RASed) for a place
place_trade_map <- function(place_trade_table,
                            fill_variable,
                            central_place, 
                            censor_scale_lowerbound = 0,
                            interactive = TRUE){
  ptt <- place_trade_table
  fv <- fill_variable
  mx <- max(ptt[[fv]])
  tl <- usa_tile_list(ptt)
  caption <- paste0("FIPS: ", central_place,"\n Place: ", geog$fips2name(central_place, long = T)) %>%
    paste(collapse = "\n")
  df <- boil_tile(tile_list = tl, 
                  fill_variable = fv, 
                  caption = caption, 
                  scale_fill_viridis(direction = -1, limits = c(censor_scale_lowerbound, mx)),
                  geom_sf_interactive(data = ptt[ptt$place == central_place, ], fill = "#8b0000", color = NA)) %>%
    {usa_tile_map(tile_plot_list = .,
                  interactive = interactive)}
  return(df)
}

# Clusters ----

## format data ----

substitute(connect$call_hierarchical_connectedness(year = 2012, paradigm = "domestic", class_system = "commodity", impedance = T, functional_form = "distance", scalar_constant = 300, threshold = .25, trim = "^(60|66|69|78)|(999)$", ...) %>% 
             {.[["Hierarchical_Discrete_table"]][["level_5"]]})

## viz ----

# map a place in a cluster with corresponding alpha/beta members (note: max alpha is not directly interpretable above level_0)
place_cluster_map <- function(eca_table,
                              fill_variable,
                              central_place, 
                              interactive = TRUE){
  eca <- eca_table
  fv <- fill_variable
  tl <- usa_tile_list(eca)
  caption <- paste0("FIPS: ", central_place,"\n Place: ", geog$fips2name(central_place, long = T)) %>%
    paste(collapse = "\n")
  df <- list()
  for (l in 1:length(tl)){
    df[[l]]  <- tl[[l]] +
      boil_map_theme(tl[[l]]$data, fv) +
      guides(fill = "none") +
      scale_fill_gradientn(colours = "grey80", guide = "none") +
      new_scale_fill() +
      geom_sf_interactive(data = subset(tl[[l]]$data, eca_membership == tl[[l]]$data$eca_membership[tl[[l]]$data$place == central_place]), aes(fill = eca_membership), color = alpha("grey", 0.2)) +
      scale_fill_manual(values = "#feb24c", guide = guide_legend(order = 2), labels = "Economic Catchment Alpha", name = NULL) +
      new_scale_fill() +
      geom_sf_interactive(data = subset(tl[[l]]$data, place == tl[[l]]$data$eca_membership[tl[[l]]$data$place == central_place]), aes(fill = eca_membership), color = alpha("grey", 0.2)) +
      scale_fill_manual(values = "#f03b20", guide = guide_legend(order = 3), labels = "Economic Catchment Beta", name = NULL) +
      new_scale_fill() +
      geom_sf_interactive(data = subset(tl[[l]]$data, place == central_place), aes(fill = eca_membership), color = alpha("grey", 0.2)) +
      scale_fill_manual(values = "#56B1F7", guide = guide_legend(order = 1), labels = "Place of Interest", name = NULL) +
      coord_sf(crs = switch(l, "+init=EPSG:4326", "+init=EPSG:26934", "+init=EPSG:6629", "+init=EPSG:4437")) +
      {if (l != 1){theme(legend.position = "none")}} +
      {if (l == 1){labs(caption = caption)}}
    labs(caption = caption)
  }
  df <- {usa_tile_map(tile_plot_list = df,
                  interactive = interactive)}
  return(df)
}


# map of eca memberships
cluster_map <- function(spatial_dataframe,
                        fill_variable,
                        caption = NULL,
                        interactive = TRUE){
  sdf <- spatial_dataframe
  fv <- fill_variable
  sdf$cluster_count <- ave(sdf[[fv]], sdf[[fv]], FUN = length)
  sdf$eca_name <- apply(sdf, 1, function(x){sdf[grepl(x[[fv]], sdf$place), ]$NAME} )
  if("STATE" %in% names(sdf)){sdf$eca_state <- apply(sdf, 1, function(x){sdf[grepl(x[[fv]], sdf$place), ]$STATE} )}
  tl <- usa_tile_list(sdf)
  if (is.null(caption)){
    caption = paste0("ECA clusters \n Count: ", length(unique(sdf[[fv]]))) %>%
      paste(collapse = "\n") 
  } else {caption = caption}
  df <- list()
  for (l in 1:length(tl)){
    df[[l]]  <- tl[[l]] +
      geom_sf_interactive(
        aes(fill = factor(.data[[fv]]),
            tooltip = if("STATE" %in% names(tl[[l]]$data)){
              glue("Place: {NAME}, {STATE}\nFIPS: {place}\nECA: {eca_membership}\nECA Name: {eca_name}, {eca_state}\nCount: {cluster_count}")
            }else{
              glue("Place: {NAME}\nFIPS: {place}\nECA: {eca_membership}\nECA Name: {eca_name}\nCount: {cluster_count}")
            },
            data_id = .data[["eca_membership"]]), 
        color = alpha("grey", 0.2)) +
      theme_void() +
      theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
            legend.key.size = unit(.2, "cm"),
            legend.title = element_text(size = rel(0.55)), 
            legend.text = element_text(size = rel(0.5)),
            legend.position = c(0.85, 0.2),
            plot.caption = element_text(hjust = 0.9, size = rel(0.5))) + 
      coord_sf(crs = switch(l, "+init=EPSG:4326", "+init=EPSG:26934", "+init=EPSG:6629", "+init=EPSG:4437")) +
      theme(legend.position = "none") +
      {if (l == 1){labs(caption = caption)}}
  }
  df <- {usa_tile_map(tile_plot_list = df,
                      interactive = interactive)}
  return(df)
}

# Central Delta ----

## format data ----

substitute(connect$call_temporal_eca_table(set_of_years = c(2012, 2017), central_place = "01001", spatial = T, trim = NULL, bind = F, ...) %>% 
             {connect$temporal_delta_central(.[[1]], .[[2]])}
           )

## viz ----

# map the change in trade potential for a place across time
place_trade_delta_map <- function(spatial_dataframe,
                                  fill_variable,
                                  set_of_years, 
                                  central_place,
                                  interactive = TRUE){
  sdf <- spatial_dataframe
  fv <- fill_variable
  tl <- usa_tile_list(sdf)
  caption = paste0("FIPS: ", central_place,"\n Place: ", geog$fips2name(central_place, long = T), "\n Years: ", set_of_years[1], " to ", set_of_years[2] ) %>%
    paste(collapse = "\n")
  mn <-  min(sdf[[fv]], na.rm = T)
  mx <-  max(sdf[[fv]], na.rm = T)
  df <- boil_tile(tile_list = tl, 
                  fill_variable = fv, 
                  caption = caption, 
                  scale_fill_gradientn(colors = rev(brewer.pal(7, "RdBu")), 
                                       trans = "ihs", 
                                       # breaks = c(asinh(mn), 0, asinh(mx)), 
                                       breaks = c(mn, 0, mx), 
                                       labels = c("Less Trade Potential", "", "More Trade Potential"), 
                                       values = rescale(c(asinh(mn), 0, asinh(mx))), 
                                       limits = c(mn, mx)) ) %>%
    {usa_tile_map(tile_plot_list = .,
                  interactive = interactive)}
  return(df)
}



# TODO: Address potential ambiguities in inputs of function flow e.g., flow_potential_map or place_trade_map or place_trade_delta_map

# Tests ----

test_viz <- function() {
  connect$call_temporal_eca_table(set_of_years = 2000:2020, spatial = F, trim = NULL) %>% 
    {absorption_density_plot(temporal_absorption_metric_table = ., fill_variable = "max_alpha", id_variable = "id_year")}
  call_sector_histogram(2012, paradigm = "domestic", class_system = "commodity", ilevel = "det", cluster_level = "sec", interactive = T)
  call_sector_histogram_multi(2012, paradigm = "domestic", class_system = "commodity", ilevel = "det", cluster_level = "sec", interactive = T)
  geog$call_impedance_distribution_table(central_place = "01001", functional_form = "secant", scalar_constant = 200) %>% 
    {normal_choro_map(spatial_dataframe = .[[1]], fill_variable = "impedance", caption = .[[2]])}
  connect$call_eca_table_spatial(year = 2012, impedance = T, functional_form = "distance", scalar_constant = 300, threshold = .25, trim = "^(60|66|69|78)|(999)$") %>% 
    {normal_choro_map(spatial_dataframe = ., fill_variable = "max_alpha")}
  
  geog$call_geog() %>% 
    {left_join(., geog$call_cbsa_concord(2013), by = "place")} %>% 
    mutate(CBSA_CODE = ifelse(is.na(CBSA_CODE), "rural", CBSA_CODE)) %>% 
  {cat_choro_map(., "CBSA_CODE")}
  
  dataprep$call_econ_dynam_ind(year = 2012) %>% 
    {left_join(geog$call_geog(year = 2012), ., by = "place")} %>%
    {nominal_choro_map(., fill_variable = "employment")}
  
 dataprep$call_econ_dynam_ind(year = 2012) %>% 
    {left_join(geog$call_geog(year = 2012), ., by = "place")} %>%
   {diverge_choro_map(., fill_variable = "population_grow_rate")}
 
  place_output$call_extraction_table(2012, paradigm = "domestic", class_system = "commodity", cluster_subset = "^312120", ilevel = "det", cluster_level = "det") %>% 
    {flow_potential_map(spatial_dataframe = ., fill_variable = "extract", cluster_subset = "^312120", cluster_level = "det")}
  # call_flow_potential_map(2012, paradigm = "domestic", class_system = "commodity", cluster_subset = "^312120", ilevel = "det", cluster_level = "det")
  connect$call_eca_table_spatial(2012, central_place = "01001", impedance = TRUE, functional_form = "secant") %>% 
    {place_trade_map(place_trade_table = ., central_place = "01001", fill_variable = "export_absorption", censor_scale_lowerbound = 0.01)}
  connect$call_hierarchical_connectedness(year = 2012, paradigm = "domestic", class_system = "commodity", impedance = T, functional_form = "distance", scalar_constant = 300, threshold = .25, trim = "^(60|66|69|78)|(999)$") %>% 
    {.[["Hierarchical_Discrete_table"]][["level_5"]]} %>% 
    {place_cluster_map(eca_table = ., fill_variable = "max_alpha", central_place = "01001")}
  connect$call_hierarchical_connectedness(year = 2012, paradigm = "domestic", class_system = "commodity", impedance = T, functional_form = "distance", scalar_constant = 300, threshold = .25, trim = "^(60|66|69|78)|(999)$") %>% 
    {.[["Hierarchical_Discrete_table"]][["level_5"]]} %>% 
    {cluster_map(spatial_dataframe = ., fill_variable = "eca_membership")}
  connect$call_temporal_eca_table(set_of_years = c(2012,2017), central_place = "01001", spatial = T, trim = NULL, bind = F) %>% 
    {connect$temporal_delta_central(.[[1]], .[[2]])} %>% 
    place_trade_delta_map(spatial_dataframe = ., fill_variable = "delta_export", set_of_years = c(2012,2017), central_place = "01001")
}


