#source("/snfs2/HOME/gmanny/backups/Documents/Repos/wash_mapping/01_collapse/plot.R")

cores <- 5
package_lib <- '/snfs1/temp/geospatial/geos_packages'
.libPaths(package_lib)
library(data.table)
library(feather)
library(magrittr)

indicator <- 'sani' #water or sani
var <- 'imp' #imp, unimp, surface, od, piped

title <- paste(var, indicator, sep=" ")

message('loading collapsed points')
#ptdat
#load("/snfs1/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/ptdat_7_20_2017.RData")
pts <- list.files("/snfs1/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash", pattern="ptdat", full.names = T)
pts <- grep(pts, pattern=".feather$", value=T)
pts <- grep(pts, pattern=indicator, value=T)
pts <- grep(pts, pattern="2018", value=T)
pts <- grep(pts, pattern="country", invert=T, value=T) %>% sort
pts <- pts[length(pts)]
ptdat <- read_feather(pts)
#load("/snfs1/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/ptdat_water_unconditional__2017_10_27.Rdata")
#save(ptdat, file="/snfs1/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/ptdat_water_unconditional__2017_10_25.Rdata")

message('loading collapsed polygons')
#polydat
#load("/snfs1/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_7_20_2017.RData")
polys <- list.files("/snfs1/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash", pattern="polydat", full.names = T)
polys <- grep(polys, pattern=".feather$", value=T)
polys <- grep(polys, pattern=indicator, value=T)
polys <- grep(polys, pattern="2018", value=T)
polys <- grep(polys, pattern="country", invert=T, value=T) %>% sort
polys <- polys[length(polys)]
polydat <- read_feather(polys)
#polydat <- read_feather("/snfs1/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_water_unconditional__2017_12_18.feather")

#save(polydat, file="/snfs1/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_water_unconditional__2017_10_25.Rdata")
#load("/snfs1/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_water_unconditional__2017_10_27.Rdata")

message("rbinding points and polys together")
ptdat <- rbind(polydat, ptdat, fill=T) %>% data.table
#fix for broken UGA shp
ptdat[shapefile == 'UGA_regions_2014_custom', shapefile := 'UGA_regions_custom']
ptdat <- ptdat[iso3 != "TRUE"]
ptdat <- ptdat[shapefile == "TRUE", shapefile := NA]
ptdat[shapefile == "", shapefile := NA]
ptdat[, location_code := as.numeric(location_code)]
setnames(ptdat, "total_hh", "N")
#returns ptdat data frame with data from non-IPUMS or AHS extractions
# names(ptdat)
# "id_short"      "nid"           "iso3"          "lat"
# "long"          "shapefile"     "location_code" "survey_series"
# "urban"         "year_start"    "total_hh"      "piped"
# "surface"       "imp"           "unimp"         "sdg_imp"


# message("cleaning data for plot code")
# ptdat[, water := imp]
# #ptdat_drop <- c("id_short", "piped", "surface", "imp", "unimp", "sdg_imp")
# #ptdat[, (ptdat_drop) := NULL]
# # names(ptdat)
# # "nid"           "iso3"          "lat"
# # "long"          "shapefile"     "location_code" "survey_series"
# # "urban"         "year_start"    "total_hh"      "water"
# ptdat_poly <- ptdat[is.na(lat) & is.na(long) & !is.na(shapefile) & !is.na(location_code), ]
# ptdat_poly <- ptdat_poly[, water := mean(water, na.rm=T), by=list(shapefile, location_code)]
# ptdat_poly <- ptdat_poly[, N := sum(total_hh, na.rm=T), by=list(shapefile, location_code)]
# ptdat_poly <- ptdat_poly[, total_hh := NULL]
# ptdat_poly <- ptdat_poly[, point := 0]
# 
# ptdat_points <- ptdat[!is.na(lat) & !is.na(long), ]
# ptdat_points <- ptdat_points[, water := mean(water, na.rm=T), by=list(lat, long)]
# ptdat_points <- ptdat_points[, N := sum(total_hh, na.rm=T), by=list(lat, long)]
# ptdat_points <- ptdat_points[, total_hh := NULL]
# ptdat_points <- ptdat_points[, point := 1]
# 
# ptdat <- rbind(ptdat_poly, ptdat_points, fill=T)
# names(ptdat)
# "nid"           "iso3"          "lat"           "point"
# "long"          "shapefile"     "location_code" "survey_series"
# "urban"         "year_start"    "N"             "water"

#add IPUMS
#message("loading ipums")
#path <- "/snfs1/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/IPUMS"
#f <- list.files(path, full.names=T)
#l <- lapply(f, fread)
#ipums <- rbindlist(l, fill=T, use.names=T)
# names(ipums)
# "nid"           "survey_series" "iso3"          "year_start"
# "N"             "point"         "shapefile"     "location_code"
# "sani"          "lat"           "long"
#ipums[, water := sani]
#ipums[, sani := NULL]


#add IND_AHS
#message("loading IND_AHS")
#load("/snfs1/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/IND_AHS_2010_2013_234353_polys_collapsed.Rdata")
#IND_AHS_collapse <- as.data.table(IND_AHS_collapse)
# names(IND_AHS_collapse)
# [1] "nid"                 "iso3"                "geospatial_id"
# [4] "point"               "shapefile"           "location_code"
# [7] "survey_series"       "survey_name"         "year_start"
# [10] "year_end"            "survey_module"       "strata"
# [13] "psu"                 "hh_id"               "urban"
# [16] "w_source_drink"      "t_type"              "shared_san"
# [19] "hhweight"            "hh_size"             "clusters_in_polygon"
#IND_AHS_collapse <- IND_AHS_collapse[, N := sum(hh_size, na.rm=T), by=list(shapefile, location_code)]
#drop <- c("geospatial_id", "survey_name", "year_end", "survey_module", "strata", "psu", "hh_id", "w_source_drink", "t_type", "shared_san", "clusters_in_polygon", "hh_size")
#IND_AHS_collapse <- IND_AHS_collapse[, (drop) := NULL]

# temporarily generating random numbers as water score. This is a temporary fix. 
#IND_AHS_collapse <- IND_AHS_collapse[, water := runif(nrow(IND_AHS_collapse))*3]
# names(IND_AHS_collapse)
# [1] "nid"           "iso3"          "point"         "shapefile"
# [5] "location_code" "survey_series" "year_start"    "urban"
# [9] "hhweight"      "N"             "water"
#IND_AHS_collapse <- IND_AHS_collapse[, keep_poly, with=F]

#w_collapsed <- rbind(ptdat, ipums, IND_AHS_collapse, fill=T) %>% data.table()
w_collapsed <- ptdat %>% as.data.table()

setnames(w_collapsed, "nid", "svy_id")
setnames(w_collapsed, "year_start", "start_year")
setnames(w_collapsed, "survey_series", "source")
setnames(w_collapsed, "iso3", "country")
setnames(w_collapsed, "lat", "latitude")
setnames(w_collapsed, "long", "longitude")

srvys <- c("MACRO_DHS", "MACRO_AIS", "MACRO_MIS", "UNICEF_MICS", "IPUMS_CENSUS", "WB_LSMS", "WB_CWIQ", "CDC_RHS", "PMA2020")
w_collapsed[!grepl(paste0(srvys, collapse="|"), source), source:="COUNTRY_SPECIFIC"]
for (survey in srvys){
  w_collapsed[grepl(survey, source), source:=survey]
}

#message("Saving collapsed and rbound datatable")
#save(w_collapsed, file="/snfs1/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/7_19.RData")
#load("/snfs1/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/7_19.RData")

# prep for data coverage plotting
repo <- '/share/code/geospatial/ngraetz/mbg/'
setwd(repo)
root <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
j <- root
j_root <- j

#    dependent on the machine where the user runs the code.
                                  # Ensures packages look for dependencies here when called with library().
#    Necessary for seeg libraries.
source('mbg_central/mbg_functions.R')                   # Functions to run MBG model.
source('mbg_central/prep_functions.R')                  # Functions to setup MBG run
source('mbg_central/covariate_functions.R')             # Functions to prep and transform 5*5 covariates
source('mbg_central/misc_functions.R')                  # Other computational MBG-related functions.
source('mbg_central/post_estimation_functions.R')
source('mbg_central/gbd_functions.R')
source('mbg_central/shiny_functions.R')
source('mbg_central/holdout_functions.R')
source('mbg_central/polygon_functions.R')
source('mbg_central/shapefile_functions.R')
source('mbg_central/collapse_functions.R')
source('mbg_central/seegMBG_transform_functions.R')     # Using Roy's edit for now that can take temporally varying covariates,
#   TODO: will need to send pull request to seegMBG of github
package_list <- c('survey', 'foreign', 'rgeos', 'data.table','raster','rgdal','INLA','seegSDM','seegMBG','plyr','dplyr', 'foreach', 'doParallel')
for(package in package_list) {
  library(package, lib.loc = package_lib, character.only=TRUE)
}

w_collapsed[, country := substr(country, 1, 3)]
w_collapsed[country == "KOS", country := "SRB"]

# start data coverage plotting
source("/snfs2/HOME/gmanny/backups/Documents/Repos/lbd_core/mbg_central/graph_data_coverage.R")
source("/snfs2/HOME/gmanny/backups/Documents/Repos/lbd_core/mbg_central/shapefile_functions.R")
source("/snfs2/HOME/gmanny/backups/Documents/Repos/lbd_core/mbg_central/polygon_functions.R")
message("start coverage function")
regions <- c("africa", "south_asia", "se_asia", "latin_america", "middle_east")
#regions <- rev(regions)
regions <- "africa" #remove this whan Ani updates the collapse code to include more countries
for (reg in regions){
  message(reg)
  coverage_maps <- try(graph_data_coverage_values(df = w_collapsed,
                                                  var = var,
                                                  title = title,
                                                  year_min = '1997',
                                                  year_max = '2018',
                                                  year_var = 'start_year',
                                                  region = reg,
                                                  sum_by = 'n',
                                                  since_date = "2018-1-01",
                                                  cores = cores,
                                                  indicator = indicator,
                                                  high_is_bad = FALSE,
                                                  return_maps = TRUE,
                                                  legend_title = "Prevalence",
                                                  color_scheme = "classic",
                                                  extra_file_tag = var,
                                                  save_on_share = FALSE,
                                                  fast_shapefiles=T))
}