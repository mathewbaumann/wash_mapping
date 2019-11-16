rm(list = ls())

# Define if you are running code loally
local <- F

# Set repo & library path 
if(Sys.info()[1]!="Windows") {
  if(!local) {
    root <- "/home/j/"
    l <- '/hime/limited_use/'
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

south_asia <- c('BGD','BTN','IND','LKA','NPL','PAK')
repo <- '/share/code/geospatial/baumannm/wash_mapping_current/01_collapse/'

files <- file.info(list.files(paste0('/ihme/limited_use/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash'), pattern = '*.feather', full.names=TRUE))
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
library(data.table)

setwd('/ihme/limited_use/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')
points <- read_feather(paste0('ptdat_sani_unconditional__', input_version,'.feather'))
poly <- read_feather(paste0('polydat_sani_unconditional__', input_version,'.feather'))
tabs <- fread('tabulated_data/sani.csv')

setwd('/ihme/limited_use/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/IPUMS/feather')
ipums <- list.files(pattern = 'sani_')
ipums <- lapply(ipums, read_feather)
ipums <- do.call(rbind, ipums)

ipums$location_code <- as.character(ipums$location_code)
alldat <- as.data.frame(bind_rows(points, poly, ipums))
tabs <- tabs[,c(1:16)]
tabs$admin_name <- NULL
tabs$filepath <- NULL
tabs$year_start <- tabs$start_year
tabs$start_year <- NULL
tabs$total_hh <- tabs$N
tabs$sum_of_sample_weights <- tabs$N
tabs$sum_old_N <- tabs$N
tabs$N <- NULL
tabs$piped_cw <- 0
tabs$well_cw <- 0
tabs$well_imp <- 0
tabs$well_unimp <- 0
tabs$spring_cw <- 0
tabs$spring_imp <- 0
tabs$spring_unimp <- 0
tabs$row_id <- c((nrow(alldat) + 1) : (nrow(alldat) + nrow(tabs)))

tabs$location_code <- as.character((tabs$location_code))
tabs$total_hh <- as.numeric((tabs$total_hh))
alldat <- as.data.frame(bind_rows(alldat, tabs))
alldat$iso3 <- substr(alldat$iso3, 1, 3)
alldat <- subset(alldat, iso3 %in% south_asia)
cw_dat <- cw_sani(alldat)
today <- gsub("-", "_", Sys.Date())

cw_dat <- subset(cw_dat, year_start > 1999)

write_feather(cw_dat, 
			  paste0('/home/j/WORK/11_geospatial/wash/data/cwed/sani_',
			  	     today, '.feather'))

###
setwd('/ihme/limited_use/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')
points <- read_feather(paste0('ptdat_water_unconditional__', input_version,'.feather'))
poly <- read_feather(paste0('polydat_water_unconditional__', input_version,'.feather'))
alldat <- as.data.frame(bind_rows(points, poly))
tabs <- fread('tabulated_data/water_old.csv')
tabs <- tabs[,c(1:16)]
tabs$admin_name <- NULL
tabs$filepath <- NULL
tabs$year_start <- tabs$start_year
tabs$start_year <- NULL
tabs$total_hh <- tabs$N
tabs$sum_of_sample_weights <- tabs$N
tabs$sum_old_N <- tabs$N
tabs$N <- NULL
tabs$piped_imp <- 0
tabs$piped_cw <- 0
tabs$well_cw <- 0
tabs$well_imp <- 0
tabs$well_unimp <- 0
tabs$spring_cw <- 0
tabs$spring_imp <- 0
tabs$spring_unimp <- 0
tabs$row_id <- c((nrow(alldat) + 1) : (nrow(alldat) + nrow(tabs)))


tabs$location_code <- as.character((tabs$location_code))
tabs$surface <- as.numeric((tabs$surface))
tabs <- subset(tabs, !is.na(piped) & !is.na(imp) & !is.na(unimp) & !is.na(surface))
alldat <- as.data.frame(bind_rows(alldat, tabs))
alldat$iso3 <- substr(alldat$iso3, 1, 3)
alldat <- subset(alldat, iso3 %in% south_asia)

cw_dat <- cw_water(alldat)

cw_dat <- subset(cw_dat, year_start > 1999)

write_feather(cw_dat, 
			  paste0('/home/j/WORK/11_geospatial/wash/data/cwed/water_',
			  	     today, '.feather'))

