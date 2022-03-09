##################################################################
#' Claudinei Oliveira dos Santos
#' Biologo | Msc. Ecologia | Dr. Ciências Ambientais
#' claudineisan@pastoepixel.com
##################################################################

#' usando ggplot2 para construir mapas;
#' dados de usa da terra obtidos de Somões et al 2017
#'DOI: https://www.nature.com/articles/s41597-020-0371-4

###
#' Packages, functions and configurations
options(scipen = 9999)
library(raster)
library(tidyverse)


###
#' input data
#setwd('defina o work directory')

#'shapefile com limites dos estados
uf_lim <- shapefile('data/pa_br_estados_250_2013_ibge/pa_br_estados_250_2013_ibge.shp')
mt_lim <- uf_lim[uf_lim$NM_UF == 'MT', ]
mt_lim_fty <- fortify(mt_lim)

#' dados de uso da terra MT 2017
lu_mt <- raster("data/mt_2017_v3_1.tif")

###
#' pre-process
df_lu_mt <- data.frame(rasterToPoints(lu_mt))
names(df_lu_mt) <- c('lon', 'lat', 'landuse')

#' Selecionar classes de interesse
sub_df_lu_mt <-  df_lu_mt %>% 
  filter(landuse %in% c(1:13))
sub_df_lu_mt$landuse <- as.character(sub_df_lu_mt$landuse)

data =  sub_df_lu_mt
ggplot_map <- ggplot(data = data, aes(x= lon, y = lat)) +
  geom_polygon(data = mt_lim_fty,  aes(group = group,  x = long,  y = lat), 
               fill = '#EAEAEA', colour = '#000000', lwd = 0.8) +
  geom_tile(aes(fill = landuse)) +
  scale_fill_manual(values = c("1" = "#b3cc33", #legenda inserida manualmente
                               # "2" = "#be94e8", 
                               # "3" = "#10773e", 
                               # "4" = "#eeefce",
                               # "5" = "#e4a540",
                               # "6" = "#a4507d", 
                               # "7" = "#c948a2", 
                               # "8" = "#be5b1d",
                               # "9" = "#f09cde",
                               # "10" = "#877712",
                               # "11" = "#614040", 
                               # "12" = "#1b5ee4", 
                               "13" = "#0cf8c1"),
                    labels = c('Cerrado', #legenda inserida manualmente
                               # 'Fallow_Cotton', 
                               # 'Forest', 
                               # 'Pasture', 
                               # 'Soy_Corn',
                               # 'Soy_Cotton', 
                               # 'Soy_Fallow', 
                               # 'Soy_Millet', 
                               # 'Soy_Sunflower', 
                               # 'Sugarcane', 
                               # 'Urban_Area', 
                               # 'Water',
                               'Secondary_Vegetation')) +
  coord_equal() +
  xlab('Longitude') +
  ylab('Latitude') +
  ggtitle('Land use - Mato Grosso - 2017') +
  theme(axis.text = element_text(colour = '#666666', size=10),
        axis.title=element_text(colour = '#000000', size=12),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(colour = '#666666', size = 8, face = 'plain'),
        legend.position = 'top',
        legend.direction = 'horizontal',
        plot.title = element_text(colour = '#000000', size=14, face = 'bold', angle = 0, hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(colour = '#666666', size=12, face = 'plain'),
  )
#ggplot_map

###
#' save graph
ggsave(filename = 'landuse_mt_2017.png',
       plot = ggplot_map,
       dpi = 300, 
       width = 25,
       height = 20, 
       units = "cm",  
       bg = "transparent")


##################################################################
