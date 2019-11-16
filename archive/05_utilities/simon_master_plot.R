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

for (h in c('water','sani')) {
    for (j in c('TRUE','FALSE')) {
        message(paste0(h,j))
        indi_fam <- h
        cumulative <- j
        
        setwd(paste0('/homes/adesh/results/',indi_fam,'/'))
        if (indi_fam == 'water') {
            files <- c('w_imp_greater_median.tif','w_unimp_median.tif','w_surface_median.tif')
        } else {
            files <- c('s_imp_median.tif','s_unimp_median.tif','s_od_median.tif')  
        }
        master <- lapply(files, brick)
        if (cumulative) {
            master[[2]] <- master[[1]] + master[[2]]
            master <- master[1:2]
        }
        plot_list <- list()
        for (j in 1:length(master)) {
            message(paste0('file: ',j))
            lri <- master[[j]]

        ### Process indicator raster to data frame for ggplot2 ###
            lri_list <- list()
            for (k in 1:nlayers(lri)) {
                message(paste0('layer: ',k))
                lri_df <- rasterToPoints(lri[[k]])
                lri_df <- data.frame(lri_df)

                #### WRITE LEGEND NAME #####
                colnames(lri_df) <- c("long", 'lat', 'Prevalence')
                lri_df$year <- 1999+k
                
                lri_list[[k]] <- lri_df
            }
            lri_df <- do.call(rbind, lri_list)

            # Plot using ggplot2
            for (i in c(2000,2005,2010,2015,999)) {
                message(i)
                if (i == 999) {
                plotdat <- filter(lri_df, year == 2015)
                plotdat$Prevalence <- 1
                i <- 'Sum'
                } else {
                 plotdat <- filter(lri_df, year == i)   
                }
                
                gg <- ggplot() + 

                    # Plot mean raster #### WRITE LEGEND NAME #####
                    geom_raster(data = plotdat, aes_(fill = as.name('Prevalence'), 
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
                    xlab('') + ylab('') +
                    scale_fill_gradientn(colours = (c('#fff7ec','#fee8c8','#fdd49e',
                        '#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000')),
                                         na.value = "#C0C0C0", limits = c(0,1))

                plot_list[[length(plot_list) + 1]] <- gg
            }
        }

            #pdf(paste0('/homes/adesh/results/',indi_fam,'/',indicator,file_type,'.pdf'), width = 8.27, height = 11.69)
        if (cumulative) {
            png(paste0('/homes/adesh/results/',indi_fam,'/simon_plot_sum_cumulative.png'),
                width = 1200, height = 1200)
            print(ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],
                            plot_list[[6]],plot_list[[7]],plot_list[[8]],plot_list[[9]],
                            plot_list[[5]],plot_list[[5]],plot_list[[5]],plot_list[[5]],

                            common.legend = T, legend = 'right',
                            nrow = 3, ncol = 4))
            dev.off()
        } else {
            png(paste0('/homes/adesh/results/',indi_fam,'/simon_plot_sum.png'),
                width = 1200, height = 1200)
            print(ggarrange(plot_list[[1]],plot_list[[2]],plot_list[[3]],plot_list[[4]],
                            plot_list[[6]],plot_list[[7]],plot_list[[8]],plot_list[[9]],
                            plot_list[[11]],plot_list[[12]],plot_list[[13]],plot_list[[14]],
                            plot_list[[5]],plot_list[[5]],plot_list[[5]],plot_list[[5]],

                            common.legend = T, legend = 'right',
                            nrow = 4, ncol = 4))
            dev.off()
        }
    }
}        

