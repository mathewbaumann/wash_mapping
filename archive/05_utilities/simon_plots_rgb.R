#.libPaths('/share/code/geospatial/adesh/r_packages')
rm(list = ls())
library(rgeos); library(rgdal); library(maptools)
library(ggplot2)
library(ggpubr)
library(raster)
library(dplyr)
library(gridExtra)
library(gridGraphics)
library(grid)
### Read in lakes, mask, and ad0 shapefile ###
mask <- raster('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/mask_master.tif')
lakes <- raster('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/lakes_all_2.tif')
shp <- shapefile('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/africa_ad0.shp')

indi_fam <- 'water'
setwd(paste0('/homes/adesh/results/',indi_fam,'/'))
if (indi_fam == 'water') {
    files <- c('w_imp_greater_median.tif','w_unimp_median.tif','w_surface_median.tif')
} else {
    files <- c('s_imp_median.tif','s_unimp_median.tif','s_od_median.tif')  
}

master <- lapply(files, brick)

plot_list <- list()
for (i in c(1,6,11,16)) {
    message(i)
    brick_00 <- brick(master[[1]][[i]], master[[2]][[i]], master[[3]][[i]])
    #brick_00_1_dist <- ecdf(brick_00[[1]]@data@values)
    #brick_00_2_dist <- ecdf(brick_00[[2]]@data@values)
    #brick_00_3_dist <- ecdf(brick_00[[3]]@data@values)

    #brick_00[[1]]@data@values <- brick_00_1_dist(brick_00[[1]]@data@values)
    #brick_00[[2]]@data@values <- brick_00_1_dist(brick_00[[2]]@data@values)
    #brick_00[[3]]@data@values <- brick_00_1_dist(brick_00[[3]]@data@values)

    brick_00[[1]]@data@values <- round(quantile(0:255, brick_00[[1]]@data@values), digits = 0)
    brick_00[[2]]@data@values <- round(quantile(0:255, brick_00[[2]]@data@values), digits = 0)
    brick_00[[3]]@data@values <- round(quantile(0:255, brick_00[[3]]@data@values), digits = 0)

    #greens
    greens <- c(rgb(red = c(0,rep(0,9)), green = c(0,(quantile(0:255,seq(0.1,1,by=0.1)))), blue = c(0,rep(0,9)),
                  maxColorValue = 255))[1:10]

    par(mar = c(0,0,0,0))
    plot(shp, border = 'grey')
    plot(brick_00[[1]], col = greens, legend = F, axes = F, box = F, add = T)
    plot(mask, col = 'black', legend = F, axes = F, box = F, add = T)
    plot(lakes, col = 'black', legend = F, axes = F, box = F, add = T)
    plot(shp, border = 'grey', add = T)
    grid.echo()
    plot_list[[length(plot_list)+1]] <- grid.grab()
    invisible(dev.off())

    # blue
    blues <- c(rgb(red = c(0,rep(0,9)), green = c(0,rep(0,9)), blue = c(0,(quantile(0:255,seq(0.1,1,by=0.1)))),
                  maxColorValue = 255))[1:10]
    
    par(mar = c(0,0,0,0))
    plot(shp, border = 'grey')
    plot(brick_00[[2]], col = blues, legend = F, axes = F, box = F, add = T)
    plot(mask, col = 'black', legend = F, axes = F, box = F, add = T)
    plot(lakes, col = 'black', legend = F, axes = F, box = F, add = T)
    plot(shp, border = 'grey', add = T)
    grid.echo()
    plot_list[[length(plot_list)+1]] <- grid.grab()
    invisible(dev.off())

    # red
    reds <- c(rgb(red = c(0,(quantile(0:255,seq(0.1,1,by=0.1)))), green = c(0,rep(0,9)), blue = c(0,rep(0,9)),
                  maxColorValue = 255))[1:10]

    par(mar = c(0,0,0,0))
    plot(shp, border = 'grey')
    plot(brick_00[[3]], col = reds, legend = F, axes = F, box = F, add = T)
    plot(mask, col = 'black', legend = F, axes = F, box = F, add = T)
    plot(lakes, col = 'black', legend = F, axes = F, box = F, add = T)
    plot(shp, border = 'grey', add = T)
    grid.echo()
    plot_list[[length(plot_list)+1]] <- grid.grab()
    invisible(dev.off())
    
    par(mar = c(0,0,0,0))
    plot(shp, border = 'grey')
    plotRGB(brick_00, r = 3, b = 2, g = 1, add = T)
    plot(mask, col = 'black', legend = F, axes = F, box = F, add = T)
    plot(lakes, col = 'black', legend = F, axes = F, box = F, add = T)
    plot(shp, border = 'grey', add = T)
    grid.echo()
    plot_list[[length(plot_list)+1]] <- grid.grab()
    invisible(dev.off())

}

png(paste0('/homes/adesh/results/',indi_fam,'/',indi_fam,'_simon_plots_rgb_v2.png'),
     width = 1200, height = 1200)
grid.arrange(arrangeGrob(plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], 
                ncol = 1),
             arrangeGrob(plot_list[[5]], plot_list[[6]], plot_list[[7]], plot_list[[8]], 
                ncol = 1),
             arrangeGrob(plot_list[[9]], plot_list[[10]], plot_list[[11]], plot_list[[12]], 
                ncol = 1),
             arrangeGrob(plot_list[[13]], plot_list[[14]], plot_list[[15]], plot_list[[16]], 
                ncol = 1),
             ncol = 4)
dev.off()