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
indi_fam <- 'sani'
indicator <- 's_imp'
run_date <- '2017_11_04_03_36_57'
#file_type <- '_iqr_traditional'

file_types <- c('target_probs')

for (file_type in file_types) {
    message(file_type)
    # improved
    setwd(paste0('/share/geospatial/mbg/wash/', indicator, '/output/',run_date,
          '/pred_derivatives/',file_type))
    lri <- lapply(list.files()[grep(paste0('.tif'), 
            list.files())], raster)
    lri <- do.call(raster::merge, lri)

    ### Process indicator raster to data frame for ggplot2 ###
    lri_list <- list()
    for (i in 1:nlayers(lri)) {
        
        lri_df <- rasterToPoints(lri[[i]])
        lri_df <- data.frame(lri_df)

        #### WRITE LEGEND NAME #####
        colnames(lri_df) <- c("long", 'lat', 'At Least Basic \nSanitation')
        lri_df$year <- 2015
        
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
            geom_raster(data = plotdat, aes_(fill = as.name('At Least Basic \nSanitation'), 
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
            xlab('') + ylab('') 

            if (file_type == 'target_probs') { 
                gg <- gg + scale_fill_gradientn(colours = c('#fff7ec','#fee8c8','#fdd49e',
                        '#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000'),
                                 na.value = "#C0C0C0", limits = c(0,1))
            } else {
                gg <- gg + scale_fill_gradientn(colours = rev(c('#67001f','#b2182b','#d6604d',
                        '#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061')),
                                 na.value = "#C0C0C0")
            }

        plot_list[[length(plot_list) + 1]] <- gg
    }

    # Save plots as pdf
    pdf(paste0('/homes/adesh/results/',indi_fam,'/',indicator,'_',file_type,'.pdf'))
    print(ggarrange(plotlist = plot_list, common.legend = T, legend = 'right'))
    dev.off()

    #writeRaster(lri, file = paste0('/homes/adesh/results/',indi_fam,'/',indicator,'_',file_type,'.tif'),
    #    format = 'GTiff', overwrite = T)

    png(paste0('/homes/adesh/results/',indi_fam,'/',indicator,'_',file_type,'.png'),
        width = 1200, height = 1200)
        print(plot_list[[1]])
    dev.off()

    tiff(paste0('/homes/adesh/results/',indi_fam,'/',indicator,'_',file_type,'.tiff'),
        width = 1200, height = 1200)
        print(plot_list[[1]])
    dev.off()
}