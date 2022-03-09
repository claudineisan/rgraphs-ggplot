##################################################################
#' Claudinei Oliveira dos Santos
#' Biologo | Msc. Ecologia | Dr. Ciências Ambientais
#' claudineisan@pastoepixel.com
##################################################################

#' usando ggplot2 para construir mapas;
#' dados de qualidade das pastagens obtidos de Santos et al 2022
#'DOI: https://www.mdpi.com/2072-4292/14/4/1024

###
#' Packages, functions and configurations
options(scipen = 9999)
library(raster)
library(sf)
library(tidyverse)

###
#' input data
lim_mu <- shapefile("data/mu_novacrixas_250_2013_ibge.shp")
lim_mu_fty <- fortify(lim_mu)

img_mu_10 <- raster("data/mu_novacrixas_pasture_quality_2010_lapig.tif")
img_mu_18 <- raster("data/mu_novacrixas_pasture_quality_2018_lapig.tif")
img_mu_10_18 <- stack(img_mu_10, img_mu_18)

###
#' pre-process
df_img_mu <- as_tibble(data.frame(rasterToPoints(img_mu_10_18)))
names(df_img_mu) <- c('lon', 'lat', 'Year 2010', 'Year 2018')

gath_df_img_mu <- df_img_mu %>% 
  gather(key = 'variables', 
         value = 'values',
         -c(lon, lat))

gath_df_img_mu$values <- as.character(gath_df_img_mu$values)
gath_df_img_mu$values <- str_replace(gath_df_img_mu$values, '3', '2')
gath_df_img_mu$values <- str_replace(gath_df_img_mu$values, '4', '3')

data =  na.omit(gath_df_img_mu)
ggplot_map <- ggplot(data = data, aes(x= lon, y = lat)) +
  geom_polygon(data = lim_mu_fty,  aes(group = group,  x = long,  y = lat), 
               fill = '#EAEAEA', colour = '#000000', lwd = 0.8) +
  geom_tile(aes(fill = values)) +
  facet_wrap(variables ~ .) +
  scale_fill_manual(values = c("1" = "#D36C63",
                               "2" = "#D7DF23",
                               "3" = "#05733F"),
                    labels = c('Severe',
                               'Intermediate',
                               'Absent')) +
  coord_equal() +
  xlab('Longitude') +
  ylab('Latitude') +
  ggtitle('Pasture quality - 2010 e 2018') +
  theme(axis.text = element_text(colour = '#666666', size=14),
        axis.title=element_text(colour = '#000000', size=12, face = 'bold'),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(colour = '#666666', size = 14, face = 'plain'),
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        plot.title = element_text(colour = '#000000', size=14, face = 'bold', hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(colour = '#666666', size=14, face = 'bold.italic'),
  )


tbl_df_img_mu <- data.frame(t(table(gath_df_img_mu$variables, gath_df_img_mu$values)))
tbl_df_img_mu$classes <- rep(c('Severe', 'Intermediate', 'Absent'),2)

data <- tbl_df_img_mu
ggplot_donut <- ggplot(data, aes(x = 2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) + 
  # scale_fill_brewer(palette = "Dark2") +
  scale_fill_manual(values = c("1" = "#D36C63",
                               "2" = "#D7DF23",
                               "3" = "#05733F"),
                    labels = c('Severe',
                               'Intermediate',
                               'Absent')) +
  # geom_text(aes(y = Freq, label = classes), col = "white", size = 5) +
  theme_void() +
  xlim(.2,2.5) +
  facet_wrap(Var2 ~ .)+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.line.y = element_blank(),
        axis.line.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(colour = '#666666', size = 14, face = 'plain'),
        legend.position = 'none',
        legend.direction = 'horizontal',
        plot.title = element_text(colour = '#000000', size=14, face = 'bold', hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(colour = '#666666', size=14, face = 'bold.italic'),
  )


###
#' save graph
ggsave(filename = 'pasture_quality_2010_2018.png',
       plot = ggplot_map,
       dpi = 300, 
       width = 25,
       height = 20, 
       units = "cm",  
       bg = "white")

ggsave(filename = 'pasture_quality_pie_2010_2018.png',
       plot = ggplot_donut,
       dpi = 300, 
       width = 25,
       height = 15, 
       units = "cm",  
       bg = "white")

##################################################################
