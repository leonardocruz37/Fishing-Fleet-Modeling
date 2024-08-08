library(ggplot2)
library(ggpubr)
library(raster)
library(rgdal)
library(terra)
library(tidyterra)
library(maptools)
library(sf)

path <- "~/Project Fishing Fleet Modeling"

data('wrld_simpl')

eez <- spTransform(readOGR(paste0(path,'/data/eez/eez_boundaries_v12.shp')),CRS('+proj=robin'))
wrld_simpl <- spTransform(wrld_simpl,CRS('+proj=robin'))

map_border <- st_graticule() |>
  st_bbox() |>
  st_as_sfc() |>
  st_transform(3857) |>
  st_segmentize(500000) |>
  st_transform(st_crs(wrld_simpl)) |>
  st_cast("POLYGON")

base_raster <- raster(paste0(path,'/environmental variables/present/thetao_baseline_2000_2019_depthsurf_74ff_39fa_9adc_U1714141988978.nc'))
base_raster <- raster::aggregate(base_raster, 1/res(base_raster)) * 0

create_plot <- function(df, title, limits){
  
  df_raster <- project(rast(rasterize(df[1:2], base_raster, df$layer, fun = sum, na.rm = FALSE)), '+proj=robin')
  
  g <- ggplot() +
    geom_spatraster(data = df_raster) +
    geom_path(data = eez, aes(x = long, y = lat, group = group), color = 'grey50', linetype = 5, linewidth = 0.05, inherit.aes = FALSE) +
    geom_polygon(data = wrld_simpl, aes(x = long, y = lat, group = group), fill = 'lightgrey', color = 'grey', linewidth = 0.1) +
    geom_sf(data = map_border, fill = NA, color ='black', linewidth = 0.1) +
    labs(title = title, fill = expression(paste(Delta, '2100-2020'))) +
    scale_fill_gradient2(high = c('#cdd2e4', '#a6a2cc', '#7a74ad', '#513294', '#241349'),
                         low = c('#783b16', '#ae5d25', '#e0842a', '#fbb268', '#fedbae'),
                         mid = '#FFFFF0',
                         na.value = 'white', limits = limits) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),
          axis.line = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          plot.title = element_text(size = 7, vjust = 0.5, hjust = 0.5, family = "Helvetica"),
          axis.text = element_text(size = 5, family = "Helvetica"),
          axis.title = element_text(size = 5, family = "Helvetica"),
          legend.text = element_text(size = 4, family = "Helvetica", hjust = 1),
          legend.title = element_text(size = 5, family = "Helvetica"))
  
  return(g)
  
}

create_plot_beta <- function(df, title, limits){
  
  df_raster <- project(rast(rasterize(df[1:2], base_raster, df$beta.sor, fun = sum, na.rm = FALSE)), '+proj=robin')
  
  g <- ggplot() +
    geom_spatraster(data = df_raster) +
    geom_path(data = eez, aes(x = long, y = lat, group = group), color = 'grey50', linetype = 5, linewidth = 0.05, inherit.aes = FALSE) +
    geom_polygon(data = wrld_simpl, aes(x = long, y = lat, group = group), fill = 'lightgrey', color = 'grey', linewidth = 0.1) +
    geom_sf(data = map_border, fill = NA, color ='black', linewidth = 0.1) +
    labs(title = title, fill = 'Dissimilarity') +
    scale_fill_whitebox_c(
      palette = "viridi", direction = 1,
      n.breaks = 5,
      guide = guide_legend(reverse = TRUE)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      plot.title = element_text(size = 7, vjust = 0.5, hjust = 0.5, family = "Helvetica"),
      axis.text = element_text(size = 5, family = "Helvetica"),
      axis.title = element_text(size = 5, family = "Helvetica"),
      legend.text = element_text(size = 4, family = "Helvetica", hjust = 1),
      legend.title = element_text(size = 5, family = "Helvetica")
    )
  
  return(g)
  
}
