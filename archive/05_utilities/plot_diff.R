.libPaths('/share/code/geospatial/adesh/r_packages')
rm(list = ls())
library(rgeos); library(rgdal); library(maptools)
library(ggplot2)
library(ggpubr)
library(raster)
library(dplyr)

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

### Read in indicator rasters from mbg output ###
indi_fam <- 'water'
indicator <- 'ws_diff'
run_date <- '2017_11_01_16_42_03'
var_name <- 'Median Difference'
#file_type <- '_iqr_traditional'

file_types <- c('_median')

for (file_type in file_types) {
    message(file_type)
    # improved
    setwd('/homes/adesh/results/')
    lri <- brick('water/ws_diff_median.tif')

    ### Process indicator raster to data frame for ggplot2 ###
    lri_list <- list()
    for (i in 1:nlayers(lri)) {
        
        lri_df <- rasterToPoints(lri[[i]])
        lri_df <- data.frame(lri_df)

        #### WRITE LEGEND NAME #####
        colnames(lri_df) <- c("long", 'lat', var_name)
        lri_df$year <- 1999+i
        
        lri_list[[i]] <- lri_df
    }
    lri_df <- do.call(rbind, lri_list)

    # Plot using ggplot2
    plot_list <- list()
    for (i in c(2015)) {
        message(i)
        plotdat <- filter(lri_df, year == i)
        
        gg <- ggplot() + 

            # Plot mean raster #### WRITE LEGEND NAME #####
            geom_raster(data = plotdat, aes_(fill = as.name(var_name), 
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
            xlab('') + ylab('') + ggtitle(i) +
            scale_fill_gradientn(colours = c('#67001f','#b2182b','#d6604d',
                        '#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061'),
                                 na.value = "#C0C0C0")
        plot_list[[length(plot_list) + 1]] <- gg
    }

    png(paste0('/homes/adesh/results/',indi_fam,'/',indicator,file_type,'.png'),
        width = 1200, height = 1200)
        print(plot_list[[1]])
    dev.off()

    tiff(paste0('/homes/adesh/results/',indi_fam,'/',indicator,file_type,'.tiff'),
        width = 1200, height = 1200)
        print(plot_list[[1]])
    dev.off()
}