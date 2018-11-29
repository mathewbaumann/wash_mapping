rm(list = ls())

# Define if you are running code loally
local <- F

# Set repo & library path 
if(Sys.info()[1]!="Windows") {
  if(!local) {
    root <- "/home/j/"
    package_lib <- ifelse(grepl("geos", Sys.info()[4]),
                          paste0(root,'temp/geospatial/geos_packages'),
                          paste0(root,'temp/geospatial/packages'))
    .libPaths(package_lib)
  } else {
    package_lib <- .libPaths()
    root <- '/home/j/'
  }
} else {
  package_lib <- .libPaths()
  root <- 'J:/'
}

repo <- '/share/code/geospatial/baumannm/wash_mapping_current/01_collapse/'

files <- file.info(list.files(paste0('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash'), pattern = '*.feather', full.names=TRUE))
files <- files[with(files, order(as.POSIXct(ctime), decreasing = TRUE)), ]
latest_postextraction <- unlist(strsplit(rownames(files)[1], "/"))
latest_postextraction <- latest_postextraction[length(latest_postextraction)]
input_version <- gsub('.feather', '', latest_postextraction)
input_version <- gsub('ptdat_sani_unconditional__', '', input_version)
input_version <- gsub('polydat_sani_unconditional__', '', input_version)
input_version <- gsub('ptdat_water_unconditional__', '', input_version)
input_version <- gsub('polydat_water_unconditional__', '', input_version)

setwd(repo)
source('functions/cw_indi.R')

library(dplyr)
library(feather)

setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')
points <- read_feather(paste0('ptdat_sani_unconditional__', input_version,'.feather'))
poly <- read_feather(paste0('polydat_sani_unconditional__', input_version,'.feather'))

setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/IPUMS/feather')
ipums <- list.files(pattern = 'sani_')
ipums <- lapply(ipums, read_feather)
ipums <- do.call(rbind, ipums)

ipums$location_code <- as.character(ipums$location_code)
alldat <- as.data.frame(bind_rows(points, poly, ipums))
alldat$iso3 <- substr(alldat$iso3, 1, 3)
cw_dat <- cw_sani(alldat)
today <- gsub("-", "_", Sys.Date())

write_feather(cw_dat, 
			  paste0('/home/j/WORK/11_geospatial/wash/data/cwed/sani_',
			  	     today, '.feather'))

###
setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')
points <- read_feather(paste0('ptdat_water_unconditional__', input_version,'.feather'))
poly <- read_feather(paste0('polydat_water_unconditional__', input_version,'.feather'))

alldat <- as.data.frame(bind_rows(points, poly))
alldat$iso3 <- substr(alldat$iso3, 1, 3)
cw_dat <- cw_water(alldat)

write_feather(cw_dat, 
			  paste0('/home/j/WORK/11_geospatial/wash/data/water_',
			  	     today, '.feather'))

