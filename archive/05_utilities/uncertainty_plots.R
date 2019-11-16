setwd('/homes/adesh/results/')
.libPaths('/share/code/geospatial/adesh/r_packages')

library(rgeos); library(rgdal); library(maptools)
library(ggplot2)
library(ggpubr)
library(raster)
library(dplyr)

rm(list = ls())

### Read in lakes, mask, and ad0 shapefile ###
mask <- raster('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/mask_master.tif')
lakes <- raster('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/lakes_all_2.tif')
shp <- shapefile('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/africa_ad0.shp')

### Process shapefile for ggplot2 ###
shp@data$id = rownames(shp@data)
shp.points = fortify(shp, region="id")
shp.df = left_join(shp.points, shp@data, by="id")

### Process mask for ggplot2 ###
mask_df <- rasterToPoints(mask)
mask_df <- data.frame(mask_df)
colnames(mask_df) <- c("long", 'lat', 'mask')

### Process lakes for ggplot2 ###
lakes_df <- rasterToPoints(lakes)
lakes_df <- data.frame(lakes_df)
colnames(lakes_df) <- c("long", 'lat', 'lakes')

# read in raster brick of hybrid
lri <- brick('water/w_imp_greater_median_ci_hybrid.tif')

indicator <- 'w_imp_greater'
indi_fam <- 'water'
file_type <- '_hybrid_uncertainty'
### Process indicator raster to data frame for ggplot2 ###
lri_list <- list()
for (i in 1:nlayers(lri)) {
    
    lri_df <- rasterToPoints(lri[[i]])
    lri_df <- data.frame(lri_df)
    colnames(lri_df) <- c("long", 'lat', paste0(indicator))
    lri_df$year <- 2015
    
    lri_list[[i]] <- lri_df
}
lri_df <- do.call(rbind, lri_list)
lri_df[,indicator] <- as.character(lri_df[,indicator])

color_vec <-  c(rgb(245,240,244,maxColorValue = 255), rgb(239,211,218,maxColorValue = 255), rgb(234,169,178,maxColorValue = 255), rgb(232,130,142,maxColorValue = 255),
                rgb(204,215,234,maxColorValue = 255), rgb(202,190,209,maxColorValue = 255), rgb(196,163,186,maxColorValue = 255), rgb(192,130,155,maxColorValue = 255),
                rgb(177,202,230,maxColorValue = 255), rgb(172,175,205,maxColorValue = 255), rgb(158,146,182,maxColorValue = 255), rgb(159,129,165,maxColorValue = 255),
                rgb(137,181,222,maxColorValue = 255), rgb(130,159,203,maxColorValue = 255), rgb(130,142,188,maxColorValue = 255), rgb(121,120,168,maxColorValue = 255))
names(color_vec) <- c('11','12','13','14','21','22','23','24',
                      '31','32','33','34','41','42','43','44')
# Plot using ggplot2
plot_list <- list()
for (i in c(2015)) {
    message(i)
    plotdat <- filter(lri_df, year == i)
    
    gg <- ggplot() + 

        # Plot mean raster
        geom_raster(data = plotdat, aes_(fill = as.name(indicator), 
                                         y = quote(lat), x = quote(long))) +
        coord_equal(ratio = 1) +
        
        # Plot mask
        annotate(geom = 'raster', x = mask_df$long, y = mask_df$lat,
                    fill = 'grey') +
        
        # Plot lakes
        annotate(geom = 'raster', x = lakes_df$long, y = lakes_df$lat,
                    fill = 'blue') +

        # Plot admin borders
        geom_path(data = shp.df, aes(x = long, y = lat, group = group), color = 'black') +

        # aesthetics
        theme_classic() +
        theme(axis.line = element_blank(), axis.text = element_blank(),
                      axis.ticks = element_blank()) +
        xlab('') + ylab('') + ggtitle(i) + theme(legend.position = 'none') +
        scale_fill_manual(values =color_vec)

    plot_list[[length(plot_list) + 1]] <- gg
}

png(paste0('/homes/adesh/results/',indi_fam,'/',indicator,file_type,'.png'),
    width = 1200, height = 1200)
    plot_list[[1]]
dev.off()