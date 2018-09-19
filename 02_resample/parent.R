.libPaths('/share/code/geospatial/adesh/r_packages')
library(feather)
library(dplyr)

nodes <- 'geos'
proj <- ifelse(nodes == 'geos',
        '-P proj_geo_nodes -l gn=TRUE',                      		
        '-P proj_geospatial')
user <- "adesh"

setwd('/share/code/geospatial/adesh/wash_mapping/02_resample/')
indicators <- c("water", 'sani')
run_date <- Sys.Date()

for (indic in indicators) {
  if (indic == 'water') {
     polydat <- read_feather('/home/j/WORK/11_geospatial/wash/data/cwed/water_2018_09_07.feather')
  } else {
    polydat <- read_feather('/home/j/WORK/11_geospatial/wash/data/cwed/sani_2018_09_07.feather')
  }

  polydat <- filter(polydat, is.na(lat) & !is.na(shapefile) & !is.na(location_code))
  
  for (shp in unique(polydat$shapefile)) { 
    jname <- paste(indic, shp, sep = "_")
    mycores <- 4
    sys.sub <- paste0("qsub ",proj,paste0(" -e /share/temp/sgeoutput/",user,"/errors -o /share/temp/sgeoutput/",user,"/output "),
                      "-cwd -N ", jname, " ", "-pe multi_slot ", mycores, " ")
    script <- "child.R"
    r_shell <- ifelse(nodes == 'geos',
                      'r_shell_geos.sh',                      		
                      'r_shell.sh')
    
    args <- paste(shp, indic, run_date)
    system(paste(sys.sub, r_shell, script, args)) 
  }
}