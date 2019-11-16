rm(list = ls())
# Set library and load packages
root <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
## drive locations
commondir      <- sprintf('/share/geospatial/mbg/common_inputs')
package_list <- c(t(read.csv(sprintf('%s/package_list.csv',commondir),header=FALSE)))
package_list <- package_list[!package_list %in% c('INLA','seegMBG')]

# TBD: Remve all 'setwd()'
core_repo <- repo <-  '/share/code/geospatial/adesh/mbg/'
setwd(repo)

library('INLA')
library('rgdal')
library('raster')
library('dplyr')
library('seegMBG')
library('rgeos')
library('feather')
library('pacman')

package_lib <- '/home/j/temp/geospatial/singularity_packages/3.5.0'
.libPaths(package_lib)

for(package in package_list) {
  library(package, lib.loc = package_lib, character.only=TRUE)
}


shp <- commandArgs()[6]
indic <- commandArgs()[7]
run_date <- commandArgs()[8]
warning(paste(shp,indic,run_date))

if (indic == 'water') {
  levels <- c('network','piped','imp','unimp','surface')
  polydat <- as.data.table(read_feather('/home/j/WORK/11_geospatial/wash/data/cwed/water_2019_10_24.feather'))
} else {
  levels <- c('network','piped','imp','unimp','od')
  polydat <- as.data.table(read_feather('/home/j/WORK/11_geospatial/wash/data/cwed/sani_2019_10_24.feather'))
}

polydat <- subset(polydat, is.na(lat) & !is.na(shapefile) & !is.na(location_code))
setnames(polydat,"year_start", "surv_year")
setnames(polydat,"int_year", "year_start")
subset <- polydat[which(polydat$shapefile == shp),]

shape_master <- readRDS(paste0('/share/geospatial/rds_shapefiles/',shp,'.rds'))
subset <- subset[which(location_code %in% shape_master$GAUL_CODE),]

for (pid in levels) {
  setwd('/home/j/WORK/11_geospatial/wash/data/resamp')
  generated_pts <- list()
  
  subset_loc <- subset[,setdiff(names(subset),setdiff(levels,pid)), with = F] 
  
  for (loc in unique(subset$location_code)) {
    shape <- shape_master[shape_master$GAUL_CODE == loc,]
    subset_loc2 <- subset_loc[which(location_code == loc),]
    
    for (q in 1:nrow(subset_loc2)) {
      
      subset_loc3 <- subset_loc2[q,]
      
      year <- subset_loc3$year_start
      if (year <= 2000) {
        pop_raster <- raster('/home/j/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 1)
      } else {
        if (year > 2000 & year <= 2005) {
          pop_raster <- raster('/home/j/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 2)
        } else {
          if (year > 2005 & year <= 2010) {
            pop_raster <- raster('/home/j/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 3)
          } else {
            pop_raster <- raster('/home/j/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 4)
          }
        } 
      }
      
      raster_crop <- mask(crop(x = pop_raster, y = shape), shape)
      if (length(unique(raster_crop)) < 1) {
        samp_pts <- gCentroid(shape)@coords
        samp_pts <- as.data.frame(samp_pts)
        samp_pts$weight <- 1
        
      } else {
        samp_pts <- getPoints(shape = shape, raster = raster_crop, n = 0.01, perpixel = T, prob = T)  
        samp_pts <- as.data.frame(samp_pts)
      }
      
      names(samp_pts) <- c("long", "lat","weight")
      samp_pts$shapefile <- shp
      
      subset_loc3 <- left_join(samp_pts, subset_loc3, by = 'shapefile')
      subset_loc3$point <- 0
      
      generated_pts[[length(generated_pts) + 1]] <- subset_loc3
    }
    
  }
  generated_pts2 <- do.call(rbind, generated_pts)
  if(nrow(generated_pts2) > 0){
    if (!(indic %in% list.files())) {dir.create(paste0(indic))}
    setwd(indic)
    if (!(pid %in% list.files())) {dir.create(paste0(pid))}
    setwd(pid)
    if (!(run_date %in% list.files())) {dir.create(paste0(run_date))}
    setwd(as.character(run_date))
    write.csv(generated_pts2, file = paste0(shp,'.csv'))
  }
}

