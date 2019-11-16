rm(list = ls())
# Set library and load packages
root <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
package_list <- c('dplyr','raster')
if(Sys.info()[1]=="Windows") {
  for(package in package_list) {
    library(package, character.only = T)
  }
} else {
  package_lib <- ifelse(grepl("geos", Sys.info()[4]),
                        paste0(root,'temp/geospatial/geos_packages'),                      		
                        paste0(root,'temp/geospatial/packages'))
  .libPaths(package_lib)     
  for(package in package_list) {
    library(package, lib.loc = package_lib, character.only=TRUE)
  }
}

shp <- commandArgs()[3]
run_date <- commandArgs()[4]

load('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_2017_09_13.RData')
subset <- polydat[which(polydat$shapefile == shp),]
shape_master <- shapefile(paste0('/home/j//WORK/11_geospatial/05_survey shapefile library/Shapefile directory/',shp,'.shp'))

dir.create(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/error_log/', run_date))
setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/error_log/', run_date))

loc_errors <- c()
for (loc in unique(subset$location_code)) {
  val <- loc %in% shape_master$GAUL_CODE
  if (!val) {
    loc_errors[length(loc_errors)+1] <- loc
  } else {loc_errors[length(loc_errors)+1] <- 'present'}
}
write.csv(data.frame(loc = loc_errors, shp = shp), file = paste0('loc_code_error','_',shp,'.csv'))