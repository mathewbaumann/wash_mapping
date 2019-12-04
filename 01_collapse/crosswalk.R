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

repo <- '/share/code/geospatial/baumannm/wash_mapping_current/01_collapse/'

files <- file.info(list.files(paste0('/ihme/limited_use/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash'), pattern = '*.feather', full.names=TRUE))
files <- files[with(files, order(as.POSIXct(ctime), decreasing = TRUE)), ]
latest_postextraction <- unlist(strsplit(rownames(files)[1], "/"))
latest_postextraction <- latest_postextraction[length(latest_postextraction)]
input_version <- gsub('.feather', '', latest_postextraction)
input_version <- substr(input_version,(nchar(input_version) - 9),nchar(input_version))


setwd(repo)
source('functions/cw_indi.R') #cw functions
source('functions/tabulated_data_fix.R') #handle India tabulated data

library(dplyr)
library(feather)
library(data.table)

setwd('/ihme/limited_use/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')
points <- read_feather(paste0('ptdat_sani_unconditional__', input_version,'.feather'))
poly <- read_feather(paste0('polydat_sani_unconditional__', input_version,'.feather'))
tabs <- fread('tabulated_data/sani.csv')
tabs <- tabs[,-c('notes','avg_hh_size','_num_hhs'), with = F]

setwd(paste0('/ihme/limited_use/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/IPUMS/feather/', input_version))
ipums <- list.files(pattern = 'sani_')
ipums <- lapply(ipums, read_feather)
ipums <- do.call(rbind, ipums)

ipums$location_code <- as.character(ipums$location_code)
alldat <- as.data.frame(bind_rows(points, poly, ipums))
tabs$admin_name <- NULL
tabs$filepath <- NULL
tabs$year_start <- tabs$start_year
tabs$start_year <- NULL
tabs$total_hh <- tabs$N
tabs$sum_of_sample_weights <- tabs$N
tabs$sum_old_N <- tabs$N
tabs$N <- NULL
tabs$row_id <- c((nrow(alldat) + 1) : (nrow(alldat) + nrow(tabs)))

tabs$location_code <- as.character((tabs$location_code))
tabs$total_hh <- as.numeric((tabs$total_hh))
tabs <- subset(tabs, !survey_series %in% c('SWACHHTA_REPORT', 'NARSS', 'NATIONAL HEALTH PROFILE'))
alldat <- as.data.frame(bind_rows(alldat, tabs))
alldat$iso3 <- substr(alldat$iso3, 1, 3)
cw_dat <- cw_sani(alldat)
today <- gsub("-", "_", Sys.Date())

cw_dat <- subset(cw_dat, year_start > 1999)
sb2 <- rename(sb2, imp = s_imp_other, piped = s_piped, network = s_network, 
              unimp = s_unimp, od = s_od,
              year_start = year, sum_old_N = total_hh) %>%
  dplyr::select(-imp_cw)
sb2$int_year <- sb2$year_start
sb2$year_median <- sb2$year_start
sb2$N <- sb2$sum_old_N
sb2$row_id <- c(max(cw_dat$row_id):(max(cw_dat$row_id) + nrow(sb2) - 1))
sb2$sum_of_sample_weights <- NA
cw_dat <- rbind(cw_dat, sb2)

write_feather(cw_dat, 
			  paste0('/home/j/WORK/11_geospatial/wash/data/cwed/sani_',
			  	     today, '.feather'))

###
setwd('/ihme/limited_use/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')
points <- read_feather(paste0('ptdat_water_unconditional__', input_version,'.feather'))
poly <- read_feather(paste0('polydat_water_unconditional__', input_version,'.feather'))
alldat <- as.data.frame(bind_rows(points, poly))
tabs <- fread('tabulated_data/water.csv')
tabs <- tabs[,-c('notes','avg_hh_size','_num_hhs'), with = F]
tabs$admin_name <- NULL
tabs$filepath <- NULL
tabs$year_start <- tabs$start_year
tabs$start_year <- NULL
tabs$total_hh <- tabs$N
tabs$sum_of_sample_weights <- tabs$N
tabs$sum_old_N <- tabs$N
tabs$N <- NULL
tabs$row_id <- c((nrow(alldat) + 1) : (nrow(alldat) + nrow(tabs)))


tabs$location_code <- as.character((tabs$location_code))
tabs$surface <- as.numeric((tabs$surface))
tabs <- subset(tabs, !is.na(piped) & !is.na(imp) & !is.na(unimp) & !is.na(surface))
alldat <- as.data.frame(bind_rows(alldat, tabs))
alldat$iso3 <- substr(alldat$iso3, 1, 3)
cw_dat <- cw_water(alldat)

cw_dat <- subset(cw_dat, year_start > 1999)

write_feather(cw_dat, 
			  paste0('/home/j/WORK/11_geospatial/wash/data/cwed/water_',
			  	     today, '.feather'))

