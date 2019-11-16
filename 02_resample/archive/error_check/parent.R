# Shapefile and Location Code Checker: Parent
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

# Input filepath for collapsed polydata
load('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_2017_09_13.RData')

# Input survey shapefile library
shp_dir <- '/home/j/WORK/11_geospatial/05_survey shapefile library/Shapefile directory/'

# 1. Check if all shapefile names are valid
setwd(shp_dir)
all_shp <- gsub('.shp','',list.files())
wrong_shp <- setdiff(polydat$shapefile, all_shp)

mydat <- filter(polydat, !(shapefile %in% wrong_shp))

nodes <- 'geos'
proj <- ifelse(nodes == 'geos',
               '-P proj_geo_nodes -l gn=TRUE',                      		
               '-P proj_geospatial')
user <- "adesh"
setwd('/share/code/geospatial/adesh/wash_mapping/02_resample/error_check')
run_date <- Sys.Date()

dir.create(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/error_log/'))
dir.create(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/error_log/', run_date))
write.csv(data.frame(shp = wrong_shp), file = paste0('/home/j/WORK/11_geospatial/wash/data/resamp/error_log/', run_date,'/',
                                                     'shapefile_error.csv'))


for (shp in unique(mydat$shapefile)) { 
  jname <- paste(shp, sep = "_")
  mycores <- 4
  sys.sub <- paste0("qsub ",proj,paste0(" -e /share/temp/sgeoutput/",user,"/errors -o /share/temp/sgeoutput/",user,"/output "),
                    "-cwd -N ", jname, " ", "-pe multi_slot ", mycores, " ")
  script <- "child.R"
  r_shell <- ifelse(nodes == 'geos',
                    'r_shell_geos.sh',                      		
                    'r_shell.sh')
  
  args <- paste(shp, run_date)
  system(paste(sys.sub, r_shell, script, args)) 
}
