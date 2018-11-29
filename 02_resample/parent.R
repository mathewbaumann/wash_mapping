rm(list = ls())
# Set library and load packages
root <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
## drive locations
commondir      <- sprintf('/share/geospatial/mbg/common_inputs')
package_list <- c(t(read.csv(sprintf('%s/package_list.csv',commondir),header=FALSE)))


# TBD: Remve all 'setwd()'
core_repo <- repo <-  '/share/code/geospatial/adesh/mbg/'
setwd(repo)

# Load MBG packages and functions
message('Loading in required R packages and MBG functions')
source(paste0(repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = repo)
library(feather)

nodes <- ''
proj <- ifelse(nodes == 'geos',
               '-P proj_geo_nodes -l gn=TRUE',                      		
               '-P proj_geospatial')
user <- "baumannm"

setwd('/share/code/geospatial/baumannm/wash_mapping_current/02_resample/')
indicators <- c('sani','water')
run_date <- Sys.Date()

for (indic in indicators) {
  if (indic == 'water') {
    polydat <- read_feather('/home/j/WORK/11_geospatial/wash/data/cwed/water_2018_11_15.feather')
  } else {
    polydat <- read_feather('/home/j/WORK/11_geospatial/wash/data/cwed/sani_2018_11_15.feather')
  }
  
  polydat <- subset(polydat, is.na(lat) & !is.na(shapefile) & !is.na(location_code))
  
  for (shp in unique(polydat$shapefile)) { 
    jname <- paste(indic, shp, sep = "_")
    mycores <- 4
    sys.sub <- paste0("qsub ",proj,paste0(" -e /share/temp/sgeoutput/",user,"/errors -o /share/temp/sgeoutput/",user,"/output "),
                      "-cwd -N ", jname, " ", "-pe multi_slot ", mycores, " ")
    script <- "child.R"
    r_shell <- ifelse(nodes == 'geos',
                      'r_shell_geos.sh',                      		
                      '/share/singularity-images/rstudio/shells/r_shell_singularity_3501.sh')
    
    args <- paste(shp, indic, run_date)
    system(paste(sys.sub, r_shell, script, args)) 
  }
}
