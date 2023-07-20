#Maps and data vis

# Load and attach necessary packages
library(ggiraph)
library(glue)
library(ggnewscale)
library(cowplot)

# library(readxl)
# library(openxlsx)
# library(tidyr)
# library(tools)
# library(dplyr)
# 
# library(arrow)
# 
# library(ggplot2)
# library(RColorBrewer)
# library(scales)
# 
# library(magick)
# 
# library(tmaptools)
# 
# library(igraph)
# 
# library(ggridges)


source(file.path(rprojroot::find_rstudio_root_file(), "nbs", "r_backend_functions.R"))


# Display start time
log_info("Define viz functions start")


############ Bar charts of industry distributions by county
industry_distribution_barcharts <- function(data,
                                            interact = TRUE,
                                            short = TRUE,
                                            ...){
  ft <- c("gross_ouput", "factor_supply", "factor_demand", "net_demand", "net_supply")
  sn <- unique(distinct(data, DETAIL, SECTOR)[order(distinct(data, DETAIL, SECTOR)$DETAIL),]$SECTOR)
  pn <- unique(data$place)
  df <- c()
  for (j in ft){
    for (i in pn){
      class2title <- function(x){str_split(x, "_") %>% unlist() %>% toString() %>% gsub(",", "", .) %>% str_to_title()}
      d <- data[data$place == i & data[[j]] > 0, setdiff(names(data), setdiff(ft, j) )]
      df[[j]][[i]] <- ggplot(d, aes(
        if(isTRUE(short)){
          x = factor(SECTOR, levels = sn)
        }else{
          x = factor(description, levels = unique(description))
          },
        y = .data[[j]],
        fill = DETAIL))
      if(isTRUE(interact)){
        df[[j]][[i]] <- df[[j]][[i]] +
          geom_col_interactive(aes(tooltip = paste0("Detail Sector: ", .data[["DETAIL"]], "\n Value: ", format(round(.data[[j]], 0), big.mark = ",", scientific = FALSE) ),
                                   data_id = DETAIL), 
                               color = NA)
      } else {
        df[[j]][[i]] <- df[[j]][[i]] +
          geom_col(color = NA)
      }
      df[[j]][[i]] <- df[[j]][[i]] +
        scale_fill_manual(values = d$color) +
        scale_y_continuous(labels = scales::comma) +
        theme_minimal() +
        theme(axis.text.x = element_text(size = rel(.75), angle = 300, hjust = 0),
              axis.text.y = element_text(size = rel(.75)),
              plot.subtitle = element_text(size = rel(.75), vjust = -2), 
              plot.margin = margin(t = 0, r = 50, b = 0, l = 0, unit = "pt")) +
        labs(x = element_blank(),
             y = element_blank(),
             subtitle = paste0(class2title(j), " ($1,000)"),
             title = element_blank()) +
        guides(fill = "none")
    }
  }
  return(df)
}


############ html plots of bar charts of industry distributions by county [used in mapping interactive tooltips]
html_industry_dist_plots <- function(year,
                                     ilevel = c("det", "sum", "sec"),
                                     cbsa_clust = FALSE,
                                     flow_class = c("industry", "commodity"),
                                     net_class = c("factor", "gross"),
                                     data_dir = file.path("data", "robjs"),
                                     ...){
  ilevel <- match.arg(ilevel)
  flow_class <- match.arg(flow_class)
  net_class <- match.arg(net_class)
  b <- io_yeild_distribution(year, 
                             ilevel = ilevel, 
                             cbsa_clust = cbsa_clust, 
                             flow_class = flow_class, 
                             net_class = net_class, 
                             ...) %>% 
    industry_distribution_barcharts(., interact = FALSE)
  df <- vector("list", length = length(b))
  names(df) <- names(b)
  for (i in names(df)){
    print(i)
    print(Sys.time())
    df[[i]] <- vector("list", length = length(b[[i]]))
    for (p in 1:length(df[[i]])){
      df[[i]][p] <- htmltools::plotTag(b[[i]][p], alt = "") %>% as.character()
    }
    names(df[[i]]) <- names(b[[i]])
  }
  hp <- paste0("htmlplots", "_", ilevel,"level", "_", year, "year", "_", flow_class, "class", "_", "cbsa", if(isTRUE(cbsa_clust)){"clust"}else{"NA"}, "_", net_class, "class")
  saveRDS(h, file = file.path(find_rstudio_root_file(), data_dir, hp))
  return(df)
}

############ Multi plot of bar charts of industry distributions for a given county
industry_dist_plots <- function(central_place,
                                year,
                                cbsa_clust = FALSE,
                                ...){
  if(isTRUE(cbsa_clust)){central_place <- fips2cbsa(central_place)}
  idis <- io_yeild_distribution(year = year, 
                                cbsa_clust = cbsa_clust, 
                                ...)
  bd <- idis[idis$place == central_place, ]  %>% industry_distribution_barcharts(., ...)
  pobj <- plot_grid(bd$factor_supply[[central_place]],
                    bd$factor_demand[[central_place]],
                    bd$net_demand[[central_place]],
                    bd$net_supply[[central_place]])
  girafe(ggobj = pobj, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_tooltip(css = "font-family:sans-serif;
                                             background-color:gray;
                                             color:white;
                                             padding:10px;
                                             border-radius:5px;") ))
}



#status unknown
############ hierarchy GIF generator
national_hierarchy_gif <- function(df,
                                   folder,
                                   map_function, 
                                   caption = NULL,
                                   ...){
  r <- file.path(find_rstudio_root_file(), "data", folder)
  if(!dir.exists(r)){
    dir.create(r)
  }
  fn <- list()
  lev <- list(names(df))
  for(sn in names(df)){
    fp <- file.path(r, paste0(folder, "_", sn, ".png"))
    if(!file.exists(fp)){ 
      argList <- list()
      argList$df <- df[[sn]]
      argList$caption <- glue("Level {which(names(df) == sn)} Hierarchy\n 5% Isolation Threshold")
      do.call(map_function, argList)
      ggsave(fp)
    }
    fn[[which(names(df) == sn)]] <- fp
  }
  anim <- fn %>% unlist() %>% lapply(image_read) %>% image_join() %>% image_animate(fps = 1)
  image_write(image = anim,
              path = file.path(r, paste0(folder, ".gif")))
}

#status unknown
############ Distance impedance map GIF generator
national_progressiveimpedance_gif <- function(year,
                                              dist,
                                              impd,
                                              folder,
                                              map_function,
                                              ...){
  r <- file.path(find_rstudio_root_file(), "data", folder)
  if(!dir.exists(r)){
    dir.create(r)
  }
  fn <- list()
  for(d in dist){
    fp <- file.path(r, paste0(folder, "_", d, ".png"))
    if(!file.exists(fp)){
      if(impd == "B"){
        impedance = dist_matb(dist = miles2meters(d), ...)
      }
      if(impd == "C"){
        impedance = dprox_mat(boundary_limit = miles2meters(d), ...)
      }
      df <- spatial_connectedness(cbp_year = year,
                                  impedance = impedance,
                                  ...)
      argList <- list()
      argList$df <- df
      argList$caption <- glue("{d} Mile Impedance \n 5% Isolation Threshold")
      g <- do.call(map_function, argList)
      ggsave(fp)
    }
    fn[[which(dist == d)]] <- fp
  }
  anim <- fn %>% unlist() %>% lapply(image_read) %>% image_join() %>% image_animate(fps = 1)
  image_write(image = anim,
              path = file.path(r, paste0(folder, ".gif")))
}



############ Map arbitrary qualitative data (four color theorem) 
aqual_map <- function(df,
                      fcmt = FALSE,
                      tooltip = glue("Place: {NAME}\nECA: {eca_membership}"),
                      data_id = eca_membership,
                      ncols = 8,
                      minimize = FALSE,
                      palette = "Set2",
                      caption = NULL,
                      ...){
  if(fcmt == FALSE){
    g <- ggplot(df) + 
      geom_sf_interactive(aes(fill = factor(max_absorption_alpha), 
                              tooltip = {{tooltip}}, 
                              data_id = {{data_id}})) + 
      theme_void() +
      theme(legend.position = "none") + 
      labs(caption = caption)
  } else {
    stopifnot("dataframe needs sf geometry" = "sf" %in% class(df) == TRUE) 
    if(!"Ncol" %in% names(df)){
      df[["Ncol"]] <- map_coloring(df$geometry,
                                 ncols = ncols,
                                 minimize = minimize)
    }
      g <- ggplot(df) + 
        geom_sf_interactive(aes(fill = factor(Ncol), 
                                tooltip = {{tooltip}}, 
                                data_id = {{data_id}})) + 
        theme(legend.position = "none") + 
        scale_fill_brewer(palette = palette) + 
        labs(caption = caption)
  }
  girafe(ggobj = g,
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;fill:orange",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_selection(type = "multiple", 
                          only_shiny = FALSE,
                          css = "fill:black"),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(offx = 20, offy = 20,
                        css = "font-family:sans-serif;
                               background-color:gray;
                               color:white;
                               padding:10px;
                               border-radius:5px;",
                        use_cursor_pos = TRUE) ))
}


############ Map change in connectedness over time for a county 
absorption_delta_map <- function(central_place,
                         df, 
                         fill,
                         delta_min,
                         delta_max){
  g <- ggplot(df) +
    geom_sf_interactive(aes(fill = .data[[fill]], 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nAbsorption Change: {round(.data[[fill]], 5)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nAbsorption Change: {round(.data[[fill]], 5)}")
                            },
                            data_id = place
    ), 
    color = NA
    ) + 
    guides(alpha = "none") +
    coord_sf() +
    theme_void() +
    scale_fill_gradientn(colors = c("#d7191c", "#ffffbf", "#2b83ba"), values=rescale(c(delta_min, 0, delta_max)), limits=c(delta_min, delta_max)) +
    geom_sf_interactive(data = df[df$place==central_place,], fill = "#A020F0", color = NA) +
    labs(fill = if("STATE" %in% names(df)){
      glue("{df[df$place==central_place,]$NAME}, {df[df$place==central_place,]$STATE} \nAggregate: {round(sum(df[[fill]]), 3)} \nAbsorption Change")
    }else{
      glue("{df[df$place==central_place,]$NAME} \nAggregate: {round(sum(df[[fill]]), 3)} \nAbsorption Change")
    } )  +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5), color = "white"), 
          legend.text = element_text(size = rel(0.5), color = "white"),
          legend.position = c(0.9, 0.3),
          panel.background = element_rect(fill = "black")) 
  
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = TRUE) ))
}


############ Map absorption metrics
absorption_map <- function(df, 
                           add_html = FALSE,
                           fill = "max_absorption_alpha", 
                           fill_lab = "Max Absorption",
                           unit_scale = TRUE,
                           caption = paste0(5,"% Isolation Threshold")){
  if(!isTRUE(add_html)){
    g <- ggplot(df) +
      geom_sf_interactive(aes(fill = .data[[fill]], 
                              tooltip = if("STATE" %in% names(df)){
                                          glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[fill]], 5)}")
                                        }else{
                                          glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[fill]], 5)}")
                                          },
                              data_id = place), 
                          color = NA) + 
      guides(alpha = "none") +
      coord_sf() +
      theme_void() +
      theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
            legend.key.size = unit(.2, "cm"),
            legend.title = element_text(size = rel(0.5)), 
            legend.text = element_text(size = rel(0.5)),
            legend.position = c(0.9, 0.2)) + {
        if(isTRUE(unit_scale)){
          scale_fill_viridis(direction = -1, limits=c(floor(0), ceiling(1)))
        } else {
          scale_fill_viridis(direction = -1)
        } } + 
      labs(fill = fill_lab,
           caption = caption)
  } else {
      g <- ggplot(df) +
        geom_sf_interactive(aes(fill = .data[[fill]], 
                                tooltip = if("STATE" %in% names(df)){
                                  glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[fill]], 5)}\nIndustry Output\n{html_output}")
                                }else{
                                  glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[fill]], 5)}\nIndustry Output\n{html_output}")
                                },
                                data_id = place), 
                            color = NA) + 
        guides(alpha = "none") +
        coord_sf() +
        theme_void() +
        theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
              legend.key.size = unit(.2, "cm"),
              legend.title = element_text(size = rel(0.5)), 
              legend.text = element_text(size = rel(0.5)),
              legend.position = c(0.9, 0.2)) + {
                if(isTRUE(unit_scale)){
                  scale_fill_viridis(direction = -1, limits=c(floor(0), ceiling(1)))
                } else {
                  scale_fill_viridis(direction = -1)
                } } + 
        labs(fill = fill_lab,
             caption = caption)
  }
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330) ))
      
}

############ Hierarchical Map absorption metrics
hier_ab_map <- function(df, 
                        central_place,
                        threshold = .05){
    g <- ggplot() +
      geom_sf_interactive(aes(fill = eca_membership, 
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nCluster Count: {cluster_members_count}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nCluster Count: {cluster_members_count}")
                              },
                              data_id = place),
                          data = subset(df, eca_membership == df$eca_membership[df$place == central_place]),
                          color = NA) +
      scale_fill_manual(values = "#feb24c",
                        guide = guide_legend(order = 2),
                        labels = "Economic Catchment Area",
                        name = NULL) +
      #guides(fill=guide_legend(title=NULL)) +
      new_scale_fill() +
      geom_sf_interactive(aes(fill = eca_membership, 
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nCluster Count: {cluster_members_count}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nCluster Count: {cluster_members_count}")
                              },
                              data_id = place), 
                          data = subset(df, eca_membership != df$eca_membership[df$place == central_place]),
                          color = NA) +
      scale_fill_grey(start = 0.5,
                      end = 0.5,
                      guide = "none") +
      new_scale_fill() +
      geom_sf_interactive(data = df[df$place==central_place,], aes(fill = place), color = NA) +
      scale_fill_manual(values = "#56B1F7",
                        guide = guide_legend(order = 1),
                        labels = "Place of Interest",
                        name = NULL) +
      #guides(fill=guide_legend(title=NULL)) +
      new_scale_fill() +
      geom_sf_interactive(data = df[df$place == df$eca_membership[df$place == central_place],], aes(fill = place), color = NA) +
      scale_fill_manual(values = "#f03b20",
                        guide = guide_legend(order = 3),
                        labels = "Catchment Sink",
                        name = NULL) +
      #guides(fill=guide_legend(title=NULL)) +
      guides(alpha = "none") +
      coord_sf() +
      theme_void() +
      theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
            legend.key.size = unit(.2, "cm"),
            legend.title = element_text(size = rel(0.5)), 
            legend.text = element_text(size = rel(0.5)),
            legend.position = c(0.9, 0.2)) +
      {if(!is.null(threshold)){labs(caption = paste0(threshold*100,"% Isolation Threshold")) } }
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;") ))
}

############ Map cluster membership counts by absorption 
clustmember_map <- function(df, 
                            alpha = "max_absorption_alpha",
                            add_html = FALSE,
                            caption = paste0(5,"% Isolation Threshold")){
if(!isTRUE(add_html)){
  g <- ggplot() +
    geom_sf_interactive(aes(fill = cluster_members_count,
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                            },
                            data_id = place),
                        data = subset(df, cluster_category == "Cluster Sink" | cluster_category == "Isolated, Cluster Sink" ),
                        color = NA) + 
    labs(fill = "Cluster Sink") +
    scale_fill_gradient(low = "#feb24c", high = "#f03b20", guide = guide_colorbar(order = 2)) +
    new_scale_fill() +
    geom_sf_interactive(aes(fill = cluster_members_count,
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                            },
                            data_id = place),
                        data = subset(df, cluster_category == "Cluster Source"),
                        color = NA) + 
    labs(fill = "Cluster Source") +
    scale_fill_gradient(low = "#56B1F7", high = "#132B43", guide = guide_colorbar(order = 1)) +
    new_scale_fill() +
    geom_sf_interactive(aes(fill = cluster_category,
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}")
                            },
                            data_id = place),
                        data = subset(df, cluster_category == "Isolated"),
                        color = NA) + 
    guides(fill=guide_legend(title=NULL)) +
    scale_fill_manual(values = "#b2df8a",
                      guide = guide_legend(order = 3)) +
    coord_sf() +
    theme_void() +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.2)) +
    labs(caption = caption) 
    } else {
      g <- ggplot() +
        geom_sf_interactive(aes(fill = cluster_members_count,
                                tooltip = if("STATE" %in% names(df)){
                                  glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                                }else{
                                  glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                                },
                                data_id = place),
                            data = subset(df, cluster_category == "Cluster Sink" | cluster_category == "Isolated, Cluster Sink"),
                            color = NA) + 
        labs(fill = "Cluster Matches") +
        scale_fill_gradient(low = "#feb24c", high = "#f03b20", guide = guide_colorbar(order = 2)) +
        new_scale_fill() +
        geom_sf_interactive(aes(fill = cluster_members_count,
                                tooltip = if("STATE" %in% names(df)){
                                  glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                                }else{
                                  glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                                },
                                data_id = place),
                            data = subset(df, cluster_category == "Cluster Source"),
                            color = NA) + 
        labs(fill = "Cluster Source") +
        scale_fill_gradient(low = "#56B1F7", high = "#132B43", guide = guide_colorbar(order = 1)) +
        new_scale_fill() +
        geom_sf_interactive(aes(fill = cluster_category,
                                tooltip = if("STATE" %in% names(df)){
                                  glue("{NAME}, {STATE}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                                }else{
                                  glue("{NAME}\nFIPS: {place}\nMatch: {eca_membership}\nAlpha: {round(.data[[alpha]], 5)}\nIndustry Output\n{html_output}")
                                },
                                data_id = place),
                            data = subset(df, cluster_category == "Isolated"),
                            color = NA) + 
        guides(fill=guide_legend(title=NULL)) +
        scale_fill_manual(values = "#b2df8a",
                          guide = guide_legend(order = 3)) +
        coord_sf() +
        theme_void() +
        theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
              legend.key.size = unit(.2, "cm"),
              legend.title = element_text(size = rel(0.5)), 
              legend.text = element_text(size = rel(0.5)),
              legend.position = c(0.9, 0.2)) +
        labs(caption = caption) 
    }
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330),
           opts_toolbar = opts_toolbar(position = "top", saveaspng = FALSE) ))
}

############ Map place-centric connectedness for a county 
place_absorption_map <- function(central_place,
                                 df, 
                                 fill, 
                                 add_html = FALSE,
                                 unit_scale = TRUE){
  
  # if(fill == "export_absorption"){x <- "Export"}else{x <- "Import"}
  g <- ggplot(df) + {
    if(!isTRUE(add_html)){
    geom_sf_interactive(aes(fill = (round(.data[[fill]], 5)), 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nAbsorption: {round(.data[[fill]], 4)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nAbsorption: {round(.data[[fill]], 4)}")
                            },
                            data_id = place ), color = NA ) } else {
                              nis <- paste0("html_nis.", substr(fill, nchar(fill) - 3, nchar(fill)) )
                              nid <- paste0("html_nid.", substr(fill, nchar(fill) - 3, nchar(fill)) )
    geom_sf_interactive(aes(fill = (round(.data[[fill]], 5)), 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nAbsorption: {round(.data[[fill]], 4)}\nOutput Excess and Input Needs\n{.data[[nis]]}{.data[[nid]]}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nAbsorption: {round(.data[[fill]], 4)}\nOutput Excess and Input Needs\n{.data[[nis]]}{.data[[nid]]}")
                            },
                            data_id = place ), color = NA ) } 
    } + 
    guides(alpha = "none") +
    coord_sf() +
    theme_void() + {
      if(isTRUE(unit_scale)){
      scale_fill_viridis(direction = -1, limits=c(floor(0), ceiling(1)))
    } else {
      scale_fill_viridis(direction = -1)
      } } +
    geom_sf_interactive(data = df[df$place==central_place,], fill = "#8b0000", color = NA) +
    labs(fill = if("STATE" %in% names(df)){
      glue("{df[df$place==central_place,]$NAME}, {df[df$place==central_place,]$STATE} \nAbsorption")
    }else{
      glue("{df[df$place==central_place,]$NAME} \nAbsorption")
    } )  +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.3)) 
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330) ))
}


############ Map place-centric trade flows for a county (takes county-to-county matrix or sector list of county-to-county matrices as data inputs)
place_trade_map <- function(df,
                            central_place,
                            sector = NULL,
                            export_flows = TRUE, 
                            geog_year = "2013",
                            censor_scale_lowerbound = 0,
                            ...){
  df <- if(is.null(sector)){
    if(isTRUE(export_flows)){
      t(df[central_place, , drop = F] )
    } else {
        df[, central_place, drop = F]
    }
  } else {
      if(isTRUE(export_flows)){
        t(df[[sector]][central_place, , drop = F] )
      } else {
          df[[sector]][, central_place, drop = F]
      }
    } 
    df <- df %>% 
      as_tibble(rownames = "place") %>%
    rename("trade" = central_place)
  geog <- call_geog(geog_year)
  df <- join_space_with_connectedness(df, geog, ...)
  tfl <- if(isTRUE(export_flows)){
    "Outbound Trade"
  } else {
    "Inbound Trade"
    }
  g <- ggplot(df) + 
    geom_sf_interactive(aes(fill = (round(trade, 2)), 
                              tooltip = if("STATE" %in% names(df)){
                                glue("{NAME}, {STATE}\nFIPS: {place}\nQuantity: {round(trade, 2)}")
                              }else{
                                glue("{NAME}\nFIPS: {place}\nQuantity: {round(trade, 2)}")
                              }, 
                              data_id = place),
                          color = NA) + 
    guides(alpha = "none") +
    coord_sf() +
    theme_void() + 
      scale_fill_viridis(direction = -1, 
                         limits = c(floor(censor_scale_lowerbound), 
                                    ceiling(max(df$trade)))) +
    geom_sf_interactive(data = df[df$place==central_place,], fill = "#8b0000", color = NA) +
    labs(fill = if("STATE" %in% names(df)){
      glue("{df[df$place==central_place,]$NAME}, {df[df$place==central_place,]$STATE} \n{tfl}")
    }else{
      glue("{df[df$place==central_place,]$NAME} \n{tfl}")
    } )  +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.3),
          plot.caption = element_text(hjust = 0.9, size = rel(0.5)) ) +
    {
      if(is.null(sector)){
        labs(caption = "Aggregate Trade Flows") 
      } else { 
        labs(caption = glue(sector, ": ", beacode2description(code = sector), " trade flow")) 
      }
    }

  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330) ))
}



############ Map Outbound to Inbound trade flows (takes county-to-county matrix or sector list of county-to-county matrices as data inputs)
inbound2outbound_map <- function(df,
                                 fill = c("out_less_in", "out2in", "inbound", "outbound"), 
                                 sector = NULL,
                                 geog_year = "2013",
                                 ...){
  fill <- match.arg(fill)
  df <- if(is.null(sector)){
    inbound2outbound(df)
  } else { 
    inbound2outbound(df[[sector]])
  }
  geog <- call_geog(geog_year)
  df <- join_space_with_connectedness(df, geog, ...)
  
  g <- ggplot() + {
    if(fill == "out_less_in" | fill == "out2in"){
    geom_sf_interactive(aes(fill = round(if(fill == "out_less_in"){log(-.data[[fill]])}else{.data[[fill]]}, 2), 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                            },
                            data_id = place), 
                        color = NA,
                        data = subset(df, out_less_in < 0)) 
  } else {
    geom_sf_interactive(aes(fill = round(log(.data[[fill]]), 2), 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                            },
                            data_id = place), 
                        color = NA,
                        data = df) 
  }
    } + {
      if(fill == "out_less_in"){
        scale_fill_gradient(low = "#56B1F7", high = "#132B43") 
      } else if(fill == "out2in"){
        scale_fill_gradient(low = "#132B43", high = "#56B1F7") 
      } else {
        scale_fill_viridis(direction = -1)
      } 
                          } +
    labs(fill = if(fill == "out_less_in"){"Net Sink (log)"} else if(fill == "out2in"){"Sink Ratio"} else if(fill == "inbound"){"Inbound Volume (log)"} else if(fill == "outbound"){"Outbound Volume (log)"}) + 
    new_scale_fill() + {
      if(fill == "out_less_in" | fill == "out2in"){
          geom_sf_interactive(aes(fill = round(if(fill == "out_less_in"){log(.data[[fill]])}else{.data[[fill]]}, 2), 
                                  tooltip = if("STATE" %in% names(df)){
                                    glue("{NAME}, {STATE}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                                  }else{
                                    glue("{NAME}\nFIPS: {place}\nInbound: {round(inbound, 2)}\nOutbound: {round(outbound, 2)}\nRatio: {round(out2in, 2)}\nNet: {round(out_less_in, 2)}")
                                  },
                                  data_id = place), 
                              color = NA,
                              data = subset(df, out_less_in > 0))
      }
    } + {
      if(fill == "out_less_in" | fill == "out2in"){ scale_fill_gradient(low = "#feb24c", high = "#f03b20") }
    } + {
      if(fill == "out_less_in" | fill == "out2in"){ labs(fill = if(fill == "out_less_in"){"Net Source (log)"} else if(fill == "out2in"){"Source Ratio"}) }
    } +
  coord_sf() +
    theme_void() +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.2),
          plot.caption = element_text(hjust = 0.9, size = rel(0.5))) +
    {
      if(is.null(sector)){
        labs(caption = "Aggregate Sector Provenance") 
      } else { 
        labs(caption = glue(sector, ": ", beacode2description(code = sector), " provenance")) 
      }
    }
  
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330),
           opts_toolbar = opts_toolbar(position = "top", saveaspng = FALSE) ))
}


############ Map Net Supply and Demand of industry/commodity goods
net_sd_map <- function(df,
                       fill,
                       ...){

g <- ggplot() + 
  geom_sf_interactive(aes(fill = round((.data[[fill]]), 2), 
                          tooltip = if("STATE" %in% names(df)){
                            glue("{NAME}, {STATE}\nFIPS: {place}\nValue: {round({.data[[fill]]}, 2)}")
                          }else{
                            glue("{NAME}\nFIPS: {place}\nValue: {round({.data[[fill]]}, 2)}")
                          },
                          data_id = place), 
                      color = NA,
                      data = df[df[[fill]] > 0,]) +
  scale_fill_gradient(low = "#feb24c", high = "#f03b20") + 
  labs(fill = "Net Supply") +
  new_scale_fill() +
  geom_sf_interactive(aes(fill = round(abs(.data[[fill]]), 2), 
                          tooltip = if("STATE" %in% names(df)){
                            glue("{NAME}, {STATE}\nFIPS: {place}\nValue: {round(abs({.data[[fill]]}), 2)}")
                          }else{
                            glue("{NAME}\nFIPS: {place}\nValue: {round(abs({.data[[fill]]}), 2)}")
                          },
                          data_id = place), 
                      color = NA,
                      data = df[df[[fill]] < 0,]) +
  scale_fill_gradient(low = "#56B1F7", high = "#132B43") + 
  labs(fill = "Net Demand") +
  new_scale_fill() +
  geom_sf_interactive(aes(fill = round(abs(.data[[fill]]), 2),
                          tooltip = if("STATE" %in% names(df)){
                            glue("{NAME}, {STATE}\nFIPS: {place}\nValue: {round({.data[[fill]]}, 2)}")
                          }else{
                            glue("{NAME}\nFIPS: {place}\nValue: {round({.data[[fill]]}, 2)}")
                          },
                          data_id = place),
                      color = NA,
                      data = df[df[[fill]] == 0,]) +
  scale_fill_gradient(low = "grey80", high = "grey80", guide = "none") + 
  coord_sf() +
  theme_void() +
  theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
        legend.key.size = unit(.2, "cm"),
        legend.title = element_text(size = rel(0.5)), 
        legend.text = element_text(size = rel(0.5)),
        legend.position = c(0.9, 0.2),
        plot.caption = element_text(hjust = 0.9, size = rel(0.5))) +
  labs(caption = glue(fill, ": ", beacode2description(code = fill) ))

girafe(ggobj = g, 
       options = list(
         opts_hover(
           css = girafe_css(
             css = "stroke:gray;r:8pt;",
             text = "stroke:none;fill:black;fill-opacity:1;" ) ),
         opts_zoom(max = 5),
         opts_sizing = opts_sizing(rescale = TRUE),
         opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;",
                      use_cursor_pos = FALSE,
                      offx = 0, offy = 330),
         opts_toolbar = opts_toolbar(position = "top", saveaspng = FALSE) ))
}

############ Map ratio of NIS to NID
nis2nid_map <- function(df){
  g <- ggplot(df) +
    geom_sf_interactive(aes(fill = (round(nis2nid, 5)), 
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nTotal Output: {round(total_output, 0)}\nTotal Input: {round(total_input, 0)}\nNet Export Supply: {round(total_nis, 0)}\nNet Import Demand: {round(total_nid, 0)}\nRatio: {round(nis2nid, 2)} ")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nTotal Output: {round(total_output, 0)}\nTotal Input: {round(total_input, 0)}\nNet Export Supply: {round(total_nis, 0)}\nNet Import Demand: {round(total_nid, 0)}\nRatio: {round(nis2nid, 2)} ")
                            },
                            data_id = place
    ), 
    color = NA
    ) + 
    guides(alpha = "none") +
    coord_sf() +
    theme_void() +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
    labs(fill = glue("Export Supply to\n Import Demand Ratio"))  +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)), 
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.3)) 

  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                             background-color:gray;
                                             color:white;
                                             padding:10px;
                                             border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330) ))
  
}

absorption_density_plot <- function(df,
                                    fill,
                                    fill_lab,
                                    normalized = TRUE,
                                    trans = NULL,
                                    #colorbreaks = c("#440154FF" = 1:7 , "#1E9C89FF" = 8:17 , "#D64B40FF" = 18, "#FDE725FF" =  19:21)){
                                    colorbreaks = c("#440154FF" = 2000:2006, "#1E9C89FF" = 2007:2016 , "#D64B40FF" = 2017, "#FDE725FF" =  2018:2020)){

g <- ggplot(df) + 
     geom_density_interactive(aes(x = (.data[[fill]]), 
                                 tooltip = id,
                                 data_id = id,
                                 `data-id` = id,
                                 color = id),
                              extra_interactive_params = "data-id"
                              #position = position_fill(reverse = TRUE)
                              ) + { 
        if(!is.null(colorbreaks)){
          scale_color_manual_interactive(values = substr(names(colorbreaks), 1, 9),
                                         extra_interactive_params = "data-id",
                                         `data-id` = function(x) x, 
                                         data_id = function(x) x) 
        }     
      } + { 
        if(!is.null(trans)){
          scale_x_continuous(trans = trans)
        }     
      } + 
     labs(color = "CBP Year", x = paste(if(isTRUE(normalized)){"Normalized"}else{"Nominal"}, fill_lab), y = "Density")

girafe(ggobj = g, 
       options = list(
         opts_hover(
           css = girafe_css(
             css = "stroke:orange;r:12pt;",
             text = "stroke:none;fill:black;fill-opacity:1;" ) ),
         opts_zoom(max = 5),
         opts_sizing = opts_sizing(rescale = TRUE),
         opts_tooltip(css = "font-family:sans-serif;
                                           background-color:gray;
                                           color:white;
                                           padding:10px;
                                           border-radius:5px;") )
       )
}

############ Map various spatial impedance functions
impedance_distribution_map <- function(central_place = "20183", 
                                       decay_function = c("bisquare", "hyper", "gaus", "expo", "power", "dprox", "eprox", "bprox"),
                                       boundary_limit = 200,
                                       rms_width = 200,
                                       hyper_decay_constant = 200,
                                       expo_decay_constant = 200,
                                       decay_power = 1/8,
                                       queen = TRUE,
                                       ...){
  decay_function <- match.arg(decay_function)
  if(decay_function == "bisquare"){
    sf <- bisquare_impedance_mat(decay_zero = miles2meters(boundary_limit), ...)
    caption <- paste0("Bi-square Decay: ", boundary_limit," mile limit")}
  if(decay_function == "hyper"){
    sf <- hyper_impedance_mat(decay_constant = miles2meters(hyper_decay_constant), ...)
    caption <- paste0("Hyperbolic Secant Decay: ", hyper_decay_constant," mile scewness scalar")}
  if(decay_function == "gaus"){
    sf <- gaus_impedance_mat(rms_width = miles2meters(rms_width), ...)
    caption <- paste0("Gaussian Decay: ", rms_width," mile RMS scalar")}
  if(decay_function == "expo"){
    sf <- expo_impedance_mat(decay_constant = miles2meters(expo_decay_constant), ...)
    caption <- paste0("Exponential Decay: ", expo_decay_constant," mile disintegration scalar")}
  if(decay_function == "power"){
    sf <- power_impedance_mat(decay_power = decay_power, ...)
    caption <- paste0("Inverse Power Decay: decay power of ", decay_power)}
  if(decay_function == "dprox"){
    sf <- dprox_mat(boundary_limit = miles2meters(boundary_limit), ...)
    caption <- paste0("Uniform Center Proximity: ", boundary_limit," mile limit")}
  if(decay_function == "eprox"){
    sf <- dist_matb(boundary_limit = miles2meters(boundary_limit), ...)
    caption <- paste0("Uniform Edge Proximity: ", boundary_limit," mile limit")}
  if(decay_function == "bprox"){
    sf <- bprox_mat(queen = queen, ...)
    caption <- if(isTRUE(queen)){
      paste0("Queen Adjacent Borders")
    }else{
      paste0("Rook Adjacent Borders")}}
  sf <- data.frame(place = rownames(sf), 
                   imped = c(sf[, central_place, drop = FALSE]))
  df <- call_geog(...) %>% .[!grepl('^(02|15|72)', .$place), ] %>% left_join(., sf, by = "place")
  g <- ggplot() + 
    geom_sf_interactive(aes(fill = imped,
                            tooltip = paste0("Value: ", round(.data[["imped"]], 4), "\nCounty: ", .data[["NAME"]], "\nFIPS: ", .data[["place"]]), 
                            data_id = place), 
                        data = df, 
                        color = NA) + 
    guides(fill = guide_legend(title = "Impedance", 
                               reverse = TRUE)) + 
    theme_void() + 
    scale_fill_viridis(direction = -1, 
                       limits=c(floor(0), 
                                ceiling(1))) +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
           legend.key.size = unit(.2, "cm"),
           legend.title = element_text(size = rel(0.5)), 
           legend.text = element_text(size = rel(0.5)),
           legend.position = c(0.9, 0.3),
           plot.caption = element_text(hjust = 0.9, size = rel(0.5))) + 
    labs(caption = caption)
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:orange;r:12pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                             background-color:gray;
                                             color:white;
                                             padding:10px;
                                             border-radius:5px;") ))
}

############ Map hierarchy of neighbors from central place
hierarchy_of_neighbors_map <- function(central_place = "20183", 
                                       ...){
  df <- inner_join(call_geog(...), neighbor_of_neighbor(central_place, ...), by = "place")
  g <- ggplot() + 
    geom_sf_interactive(aes(fill = nn, 
                            tooltip = paste0("Interval level: ", .data[["nn"]], "\nCounty: ", .data[["NAME"]], "\nFIPS: ", .data[["place"]]), 
                            data_id = place), 
                        data = df, 
                        color = NA) + 
    guides(fill = guide_legend(title = "Neighbors", 
                               reverse = TRUE)) + 
    theme_void() +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.75)), 
          legend.text = element_text(size = rel(0.75)),
          legend.position = "none") + 
    labs(caption = paste0(df$COUNTY[df$nn=="n0"], ", ", df$STATE[df$nn=="n0"], " Hierarchy of Neighbors"))
  girafe(ggobj = g, 
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:orange;r:12pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                               background-color:gray;
                                               color:white;
                                               padding:10px;
                                               border-radius:5px;") ))
}

############ Map SNA metrics from county-by-county absorption matrix
sna_value_map <- function(df,
                          metric = c("eigen_centrality", 
                                     "alpha_centrality", 
                                     "closeness", 
                                     "harmonic_centrality", 
                                     "hub_score", 
                                     "authority_score", 
                                     "page_rank", 
                                     "strength"),
                          normalize = c("none", "row", "column"),
                          scale = FALSE,
                          mode = c("total", "in", "out", "all"),
                          geog_year = "2013",
                          ...){
  geog <- call_geog(geog_year)
  metric <- match.arg(metric)
  mode <- match.arg(mode)
  normalize <- match.arg(normalize)
  if(normalize == "row"){
    df <- df %>% apply(1, rescale) %>% t()
  }
  if(normalize == "column"){
    df <- df %>% apply(2, rescale)
  }
  df <- df %>% graph_from_adjacency_matrix(weighted = TRUE,
                                           mode = "directed")
  if(metric == "eigen_centrality"){
    df <- df %>% eigen_centrality(directed = TRUE, scale = scale) %>% .[[1]]
  }
  if(metric == "alpha_centrality"){
    df <- df %>% alpha_centrality(loops = TRUE)
  }
  if(metric == "closeness"){
    df <- df %>% closeness(mode = mode)
  }
  if(metric == "harmonic_centrality"){
    df <- df %>% harmonic_centrality()
  }
  if(metric == "hub_score"){
    df <- df %>% hub_score(scale = scale) %>% .[[1]]
  }
  if(metric == "authority_score"){
    df <- df %>% authority_score(scale = scale) %>% .[[1]]
  }
  if(metric == "page_rank"){
    df <- df %>% page_rank(directed = TRUE) %>% .[[1]]
  }
  if(metric == "strength"){
    df <- df %>% strength(mode = mode)
  }
  df <- as_tibble(df, rownames = "place") %>% 
    join_space_with_connectedness(., geog, ...)
  g <- ggplot(df) +
    geom_sf_interactive(aes(fill = (round(value, 5)),
                            tooltip = if("STATE" %in% names(df)){
                              glue("{NAME}, {STATE}\nFIPS: {place}\nValue: {round(value, 5)}")
                            }else{
                              glue("{NAME}\nFIPS: {place}\nValue: {round(value, 5)}")
                            },
                            data_id = place),
                        color = NA) +
    guides(alpha = "none") +
    coord_sf() +
    theme_void() +
    scale_fill_viridis(direction = -1) +
    labs(fill = 
         if(metric == "eigen_centrality"){"Eigen Centrality"} else 
           if(metric == "alpha_centrality"){"Alpha Centrality"} else 
             if(metric == "closeness"){"Closeness"} else 
               if(metric == "harmonic_centrality"){"Harmonic Centrality"} else 
                 if(metric == "hub_score"){"Hub Score"} else 
                   if(metric == "authority_score"){"Authority Score"} else 
                     if(metric == "page_rank"){"Page Rank"} else 
                       if(metric == "strength"){"Strength"}
         ) +
    theme(plot.margin = margin(t = 1, r = 1, b = 10, l = 1, unit = "pt"),
          legend.key.size = unit(.2, "cm"),
          legend.title = element_text(size = rel(0.5)),
          legend.text = element_text(size = rel(0.5)),
          legend.position = c(0.9, 0.3),
          plot.caption = element_text(hjust = 0.9, size = rel(0.5)) )

  girafe(ggobj = g,
         options = list(
           opts_hover(
             css = girafe_css(
               css = "stroke:gray;r:8pt;",
               text = "stroke:none;fill:black;fill-opacity:1;" ) ),
           opts_zoom(max = 5),
           opts_sizing = opts_sizing(rescale = TRUE),
           opts_tooltip(css = "font-family:sans-serif;
                                             background-color:gray;
                                             color:white;
                                             padding:10px;
                                             border-radius:5px;",
                        use_cursor_pos = FALSE,
                        offx = 0, offy = 330),
           opts_toolbar = opts_toolbar(position = "top", saveaspng = FALSE) ))
}


# Display end time
log_info("Define viz functions end")





