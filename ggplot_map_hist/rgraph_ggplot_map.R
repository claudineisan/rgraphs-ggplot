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
lim_mu <- shapefile("data/shape/mu_novacrixas_250_2013_ibge/mu_novacrixas_250_2013_ibge.shp")
lim_mu_fty <- fortify(lim_mu)

img_mu_10 <- raster("data/raster/mu_novacrixas_pasture_quality_2010_lapig.tif")
img_mu_18 <- raster("data/raster/mu_novacrixas_pasture_quality_2018_lapig.tif")
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


tbl_df_img_mu <- gath_df_img_mu %>%
  select(variables, values) %>% 
  group_by(variables) %>% 
  summarise(leng = table(values)) %>% 
  mutate(perc = NA,
         classes = c('Severe', 'Intermediate', 'Absent')
         )

tbl_df_img_mu[tbl_df_img_mu$variables %in% 'Year 2010', ]$perc <- 
  round((tbl_df_img_mu[tbl_df_img_mu$variables %in% 'Year 2010', ]$leng /  
  sum(tbl_df_img_mu[tbl_df_img_mu$variables %in% 'Year 2010', ]$leng))*100,0)

tbl_df_img_mu[tbl_df_img_mu$variables %in% 'Year 2018', ]$perc <- 
  round((tbl_df_img_mu[tbl_df_img_mu$variables %in% 'Year 2018', ]$leng /  
           sum(tbl_df_img_mu[tbl_df_img_mu$variables %in% 'Year 2018', ]$leng))*100,0)



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

data <- tbl_df_img_mu
ggplot_donut <- ggplot(data, aes(x = 2, y = perc, fill = classes)) +
  geom_bar(stat = "identity") +
  coord_polar("y", start = 0) + 
  scale_fill_manual(values = c("Severe" = "#D36C63", "Intermediate" = "#D7DF23", "Absent" = "#05733F"),
                    labels = c('Sev', 'Int', 'Abs')) +
  theme_void() +
  xlim(.2,2.5) +
  facet_wrap(variables ~ .)+
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
        legend.position = 'bottom',
        legend.direction = 'horizontal',
        plot.title = element_text(colour = '#000000', size=14, face = 'bold', hjust = 0.5),
        strip.background = element_blank(),
        strip.text = element_text(colour = '#666666', size=14, face = 'bold.italic'),
  )
ggplot_donut

###
#' save graph
ggsave(filename = 'pasture_quality_2010_2018.png',
       plot = ggplot_map,
       dpi = 300, 
       width = 25,
       height = 20, 
       units = "cm",  
       bg = "white")

ggsave(filename = 'graph/pasture_quality_pie_2010_2018.png',
       plot = ggplot_donut,
       dpi = 300, 
       width = 25,
       height = 20, 
       units = "cm",  
       bg = "white")

##################################################################
