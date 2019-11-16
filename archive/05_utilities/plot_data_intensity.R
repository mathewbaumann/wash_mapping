setwd('/homes/adesh/results/')
.libPaths('/share/code/geospatial/adesh/r_packages')

library(rgeos); library(rgdal); library(maptools)
library(ggplot2)
library(ggpubr)
library(raster)
library(dplyr)

rm(list = ls())

### Read in lakes, mask, and ad0 shapefile ###
shp <- shapefile('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/africa_ad0.shp')
iso_gaul <- read.csv('/home/j/WORK/11_geospatial/pandemic_indicator/data/raw/geographies/id_convert_ammended.csv')

water <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_piped.csv')
water_nids <- water %>% group_by(country) %>% summarize(water_sources = length(unique(nid)))
water_nids <- rename(water_nids, ISO3 = country)
water_nids <- left_join(water_nids, iso_gaul, by = 'ISO3')
water_nids <- water_nids %>% select(ISO3, GAUL, water_sources) %>% rename(ADM0_CODE = GAUL)

sani <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_imp.csv')
sani_nids <- sani %>% group_by(country) %>% summarize(sani_sources = length(unique(nid)))
sani_nids <- rename(sani_nids, ISO3 = country)
sani_nids <- left_join(sani_nids, iso_gaul, by = 'ISO3')
sani_nids <- sani_nids %>% select(ISO3, GAUL, sani_sources) %>% rename(ADM0_CODE = GAUL)


shp@data <- left_join(shp@data, water_nids, by = 'ADM0_CODE')
shp@data <- left_join(shp@data, sani_nids, by = 'ADM0_CODE')
shp@data$water_sources <- ifelse(is.na(shp@data$water_sources), 0, shp@data$water_sources)
shp@data$sani_sources <- ifelse(is.na(shp@data$sani_sources), 0, shp@data$sani_sources)

 shp@data$id = rownames(shp@data)
 shp.points = fortify(shp, region="id")
 shp.df = left_join(shp.points, shp@data, by="id")

png('/homes/adesh/results/water/water_data_intensity.png',width = 1200, height = 1200)
print(
 ggplot(shp.df) +
   aes(long,lat,group=group,fill=water_sources) + 
      geom_polygon() +
      geom_path(color="black") +
      coord_equal() +
      theme_classic() +
        theme(axis.line = element_blank(), axis.text = element_blank(),
                      axis.ticks = element_blank()) +
        xlab('') + ylab('') + 
      scale_fill_gradientn(colours = rev(c('#f7fcfd','#e5f5f9','#ccece6',
        '#99d8c9','#66c2a4','#41ae76','#238b45','#006d2c','#00441b')),
                             na.value = "#C0C0C0", name = 'Number of Sources')
      )
dev.off()

png('/homes/adesh/results/sani/sani_data_intensity.png',width = 1200, height = 1200)
print(
 ggplot(shp.df) +
   aes(long,lat,group=group,fill=sani_sources) + 
      geom_polygon() +
      geom_path(color="black") +
      coord_equal() +
      theme_classic() +
        theme(axis.line = element_blank(), axis.text = element_blank(),
                      axis.ticks = element_blank()) +
        xlab('') + ylab('') + 
      scale_fill_gradientn(colours = rev(c('#f7fcfd','#e5f5f9','#ccece6',
        '#99d8c9','#66c2a4','#41ae76','#238b45','#006d2c','#00441b')),
                             na.value = "#C0C0C0", name = 'Number of Sources')
      )
dev.off()