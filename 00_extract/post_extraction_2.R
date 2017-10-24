###################################
# POST UBCOV EXTRACTION DATA CLEANING FOR GEOSPATIAL DATA EXTRACTIONS & GEOGRAPHY MATCHING
# PIONEERED BY ANNIE BROWNE
# UPDATED & OVERHAULED BY MANNY GARCIA
# EMAIL ABROWNE@WELL.OX.AC.UK
# EMAIL GMANNY@UW.EDU

# INSTRUCTIONS:
# UBCOV OUTPUTS MUST BE SAVED IN LIMITED USE DIRECTORY
###################################
rm(list=ls())

j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")

topic <- "wash"
folder_in <- paste0(j, "LIMITED_USE/LU_GEOSPATIAL/ubCov_extractions/", topic, "_2") #where your extractions are stored
folder_out <- paste0(j, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/", topic) #where you want to save the big csv of all your extractions together

cores <- 30 #qlogin -now n -pe multi_slot 30 -P proj_geospatial -l geos_node=TRUE
#source('/snfs2/HOME/gmanny/backups/Documents/Repos/wash_mapping/00_extract/post_extraction_2.R')

package_lib <- paste0(j,'/temp/geospatial/geos_packages')
.libPaths(package_lib)
if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}
p_load(haven, plyr, data.table, magrittr, parallel, doParallel, dplyr, feather)

options(warn=-1)
module_date <- Sys.Date()
module_date <- gsub("-", "_", module_date)

iso <- read.csv(paste0(j, "temp/gmanny/geospatial_stages_priority.csv"), stringsAsFactors=F)
#stage_2a <- iso[(iso$Stage == "1" | iso$Stage == "2a" | iso$Stage == "2b"), "alpha.3"] %>% as.character %>% sort()
stage_2a <- iso[(iso$Stage == "1" | iso$Stage == "2a" | iso$Stage == "2b" | iso$Stage == "3"), "alpha.3"] %>% as.character %>% sort()
af <- stage_2a

read_add_name_col <- function(file){
  #FOR GEOGRAPHY CODEBOOKS. READS THEM IN AND ADDS A COLUMN WITH THEIR CORRESPONDING SURVEY_SERIES
  message(file)
  rn <- gsub(".csv", "", file, ignore.case=T)
  spl <- strsplit(rn, "/") %>% unlist()
  svy <- spl[length(spl)]
  #df <- fread(file, data.table=T)
  #get errors in character encoding from geog codebook and fix
  #previously: df <- read.csv(file, encoding="windows-1252")
  df <- read.csv(file, encoding="windows-1252", stringsAsFactors = F)
  df <- as.data.table(df)
  df[, survey_series := svy]
  #df <- lapply(df, as.character)
  return(df)
}

all_to_char_df <- function(df){
  df <- data.frame(lapply(df, as.character), stringsAsFactors = FALSE)
  return(df)
}

say_file_and_read_dta <- function(file, keep){
  #FOR UBCOV EXTRACTIONS. READ THEM IN AND DROP COLUMNS THAT DON'T COME FROM YOUR EXTRACTIONS
  message(file)
  dta <- try(read_dta(file))
  #drop all columns that don't contain data intended to extract from ubcov
  stay <- grep(paste0(keep, collapse="|"), names(dta), value=T)
  #bye <- grep("map", names(dta), value=T)
  #bye <- c(bye, "nid_n", "year_n", "end_year_n")
  #dta <- dta[, !(names(dta) %in% bye)]
  #dta <- dta[, (names(dta) %in% stay)]
  if ("data.frame" %in% class(dta)){
    return(dta)
  }
}

force_points_to_nearest_land <- function(geo){
  stages <- read.csv(paste0(j, "temp/gmanny/geospatial_stages_priority.csv"), stringsAsFactors=F)
  s1 <- subset(stages, Stage == 1)

  #remove this line when expanding the function to work outside of Africa
  #geo <- geo[iso3 %in% s1$alpha.3,]
  #update this shapefile to global when expanding function for outside Africa
  load(paste0(j, "temp/gmanny/Geography Codebook/global_land_polygon.Rds"))
  shp <- polygon
  #shp <- readOGR(paste0(j, "WORK/11_geospatial/05_survey shapefile library/Shapefile directory/Africa_clean.shp"))
  shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

  geo_points <- geo[!is.na(lat) & !is.na(long), ]
  geo_points <- geo_points[order(iso3)]

  #geopositioning points
  coordinates(geo_points) <- ~long+lat
  proj4string(geo_points) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  geo_points <- as(geo_points, "SpatialPoints")
  shp <- as(shp, "SpatialPolygons")
  o <- over(geo_points, shp)
  in_h2o <- o[is.na(o)] %>% names
  geo_points <- data.frame(geo_points)
  fix <- geo_points[in_h2o,]

  fix_m <- fix[,c("long", "lat")] %>% data.matrix

  #acceptable_points is a custom shapefile of points between Africa's coastline and 12km inland
  acceptable_points <- readOGR(paste0(j, "WORK/11_geospatial/05_survey shapefile library/Shapefile directory/af_pts_win_12km_coast.shp"))
  acceptable_points <- acceptable_points %>% data.frame %>% data.matrix
  acceptable_points <- acceptable_points[, c("coords.x1", "coords.x2")]

  fixed <- data.frame()
  for (i in 1:nrow(fix_m)){
    distances <- spDistsN1(acceptable_points, fix_m[i,])
    m_dist <- acceptable_points[distances == min(distances)]
    if (length(m_dist) > 2){
      if (length(m_dist)%%2 == 0){
        first <- length(m_dist)/2
        last <- first+1
        f <- data.frame(long=m_dist[first], lat=m_dist[last])
      } else{
        message("Contact Manny about an issue with the geography codebooks. Not all points will be properly shifted from sea to land.")
        f <- data.frame(long=m_dist[1], lat=m_dist[2])
      }
    } else{
      f <- data.frame(long=m_dist[1], lat=m_dist[2])
    }
    fixed <- rbind.fill(fixed, f)
  }

  #verify that points were correctly moved
  fixed_v <- fixed
  coordinates(fixed_v) <- ~long+lat
  proj4string(fixed_v) <- CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  o <- over(fixed_v, shp)
  if (length(o[is.na(o)]) > 0){
    #if this is > 0 contact Manny to adjust code.
    message(paste(length(o[is.na(o)]), "points aren't being properly moved from sea to land."))
    #possibilities for this error include:
    #shapefile incorrectly geopositiong land points
    #wrong data types being fed to the over function
    #an update to your version of R or one of the geospatial R packages
    #a mystery. contact manny or another geospatial data analyst to troubleshoot
  }

  for (i in 1:nrow(fix)){
    geo[lat == fix[i, "lat"] & long == fix[i, "long"], c("lat", "long") := list(fixed[i, "lat"], fixed[i, "long"])]
  }

  return(geo)
}

adjust_year_start_to_int_year <- function(df){
  nids_missing_int_year <- df[!is.na(int_year), nid]
  nids_with_big_differences <- df[int_year > year_start + 5 | int_year < start_year,  list(nid, ihme_loc_id, year_start, year_end, survey_name, change := year_start - int_year)]
  write.csv(nids_with_big_differences, paste0(folder_out, "/changed_years.csv"), row.names=F)
  updated <- df[!is.na(int_year) & int_year <= year_start+5 & int_year >= year_start, year_start := int_year]
  return(updated)
}

extractions <- list.files(folder_in, pattern=".DTA$", ignore.case=T, full.names=T)
extractions <- grep("IPUMS_CENSUS", extractions, invert=T, value = T) #IPUMS is handled separately
extractions <- grep("234353", extractions, invert=T, value=T) #234353 is a massive India dataset that slows everything down and gets us killed on the cluster. It is handled separately.
extractions <- extractions[!grepl("/a", extractions)] #omit chinese surveys that aren't actually in the folder but that the cluster keeps finding


#GET ALL UBCOV EXTRACTIONS AND RBINDLIST THEM TOGETHER, done in parallel for speed/impatience
message("foreach topics")
#top <- mclapply(extractions, say_file_and_read_dta, keep=keep, mc.cores=cores, mc.preschedule=F) #preschedule set to F, prevents the "long vectors not supported yet" error
message("make cluster")
cl <- makeCluster(cores)
message("register cluster")
registerDoParallel(cl)
clusterCall(cl, function(x) .libPaths(x), .libPaths())
message("start foreach")
excluded_surveys <- c(8556, #dropping MEX/NATIONAL_HEALTH_SURVEY_ENSA due to bad weighting
                      261889, 261887) #MAL_ED due to non-representative sample from hospital visits

top <- foreach(i=1:length(extractions), .packages="haven") %dopar% {
  #library(haven, lib.loc = "/snfs1/temp/geospatial/packages")
  dta <- read_dta(extractions[i])
  if (unique(dta$nid) %in% excluded_surveys){
    return(NULL)
  } else{
    return(dta)
  }
}
message("foreach finished")
message("closing cluster")
stopCluster(cl)

message("rbindlist them all together")
topics <- rbindlist(top, fill=T, use.names=T)
#topics[!is.na(int_year) & int_year <= year_start+5 & int_year >= year_start, year_start := int_year]
if (topic == "wash"){
  message("dropping duplicate women in single household (so household sizes aren't duplicated)")
  topics[is.na(hhweight) & !is.na(pweight), hhweight := pweight]
  #"hw_soap1", "hw_soap2", "hw_soap3"
  #custom fix for mics handwash soap
  topics[hw_soap1 == 0, hw_soap := 0]
  topics[hw_soap2 == 0, hw_soap := 0]
  topics[hw_soap3 == 0, hw_soap := 0]
  topics[hw_soap1 == 1, hw_soap := 1]
  topics[hw_soap2 == 1, hw_soap := 1]
  topics[hw_soap3 == 1, hw_soap := 1]
  
  #fix nid 11788
  topics[nid == 11788 & t_type == "42", t_type := "Pit latrine with ventilation pipe on site"]
  topics[nid == 11788 & t_type == "43", t_type := "Pit latrine with ventilation pipe off site"]
  topics[nid == 11788 & t_type == "52", t_type := "Pit latrine without ventilation pipe on site"]
  topics[nid == 11788 & t_type == "53", t_type := "Pit latrine without ventilation pipe off site"]
  topics[nid == 11788 & t_type == "62", t_type := "Bucket toilet on site"]
  topics[nid == 11788 & t_type == "63", t_type := "Bucket toilet off site"]
  
  drop <- c("cooking_fuel", "line_id", "sex_id", "age_year", "age_month", "age_day", "pweight", "cooking_fuel_mapped", "w_treat", "w_filter", "w_boil", "w_bleach", "nid_n", "year", "t_type_multi", "w_solar", "w_cloth", "w_settle", "hw_soap1", "hw_soap2", "hw_soap3")
  topics <- topics[, (drop):= NULL]
  wn <- topics[survey_module == "WN", ]
  wn_key <- c("psu", "hh_id")
  wn <- unique(wn, by=wn_key)
  topics <- topics[survey_module != "WN", ]
  topics <- rbind.fill(topics, wn)
}
#message("save a copy in case the cluster kills you later")
#save(topics, file=paste0(folder_out, "/topics_no_geogs_", module_date, ".Rdata"))


##Get all geog codebooks and package them together
message("get all geography codebooks")
files <- list.files(paste0(j, "WORK/11_geospatial/05_survey shapefile library/codebooks"), pattern=".csv$", ignore.case = T, full.names = T)
files <- grep("Copy|together|linkage|IPUMS|special", files, value = T, invert = T) #IPUMS is handled separately
message("lapply geographies into list")
geogs <- lapply(files, read_add_name_col)
message("rbind geography codebooks together")
geo <- rbindlist(geogs, fill=T, use.names=T)

#geo[grepl(pattern = "^-[[:digit:]]|^[[:digit:]]", lat), lat:=NA]
#geo[grepl(pattern = "^-[[:digit:]]|^[[:digit:]]", long), long:=NA]
#coerce lat/longs to numeric, convert iso3s to standard format
geo[is.na(admin_level), admin_level := "NA"] #set NA values for admin_level to "NA" as a string to keep the following line from dropping them because of bad R logic
geo <- geo[admin_level != "0", ] #drop anything matched to admin0
#Encoding(geo$lat) <- 'windows-1252'
#Encoding(geo$long) <- 'windows-1252'
#Encoding(geo$geospatial_id) <- 'windows-1252'
geo[, unique_id := paste(nid, iso3, geospatial_id, sep="_")]
#dedupe the geography codebook by geospatial_id and nid
geo <- distinct(geo, unique_id, .keep_all=T)
geo[, lat := as.numeric(lat)]
geo[, long := as.numeric(long)]
#geo <- geo[, iso3 := substr(iso3, 1, 3)]
geo[iso3 == "KOSOVO", iso3 := "SRB"]


#message("Moving points jittered to the ocean to nearest land")
#message("Note: this functionality currently only works for Africa. If necessary for other regions, please update the shapefiles this function uses or contact Manny Garcia.")
#geo <- force_points_to_nearest_land(geo)

#make everything the same data type to merge
message("make types between merging datasets match")
if (class(topics$nid) == "numeric"){
  geo[, nid := as.numeric(nid)]
} else if (class(topics$nid) == "character"){
  geo[, nid := as.character(nid)]
} else{
  message("update code to accomodate topics nid as")
  message(class(topics$nid))
}

if (class(topics$geospatial_id) == "numeric"){
  geo[, geospatial_id := as.numeric(geospatial_id)]
} else if (class(topics$geospatial_id) == "character"){
  geo[, geospatial_id := as.character(geospatial_id)]
} else{
  message("update code to accomodate topics nid as")
  message(class(topics$geospatial_id))
}

geo_keep <- c("nid", "iso3", "geospatial_id", "point", "lat", "long", "shapefile", "location_code", "survey_series", "admin_level")
geo_k <- geo[, geo_keep, with=F]

message("merge both datasets together")
all <- merge(geo_k, topics, by.x=c("nid", "iso3", "geospatial_id"), by.y=c("nid", "ihme_loc_id", "geospatial_id"), all.x=F, all.y=T)
#rm(topics)
#rm(geo)
all[!is.na(latitude) & is.na(lat), lat := latitude]
all[!is.na(longitude) & is.na(long), long := longitude]

#set start_year to weighted mean of int_year for clusters with int_years that are reasonable
all[(!is.na(int_year) & int_year <= year_start+5 & int_year >= year_start) & (!is.na(lat) & !is.na(long)), start_year := weighted.mean(int_year, weight=hhweight), by=c("nid", "psu", "lat", "long")]

#set start_year to a weighted mean of the int_years if the int_years are reasonable and not for point data
all[(!is.na(int_year) & int_year <= year_start+5 & int_year >= year_start) & (is.na(lat) | is.na(long)), start_year := weighted.mean(int_year, weight=hhweight), by="nid"]


if (topic == "wash"){
  message("custom wash fixes")
  drop <- c("latitude", "longitude")
  all <- all[, (drop):=NULL]
  
  #fix pma water and soap obs custom var
  all <- all[is.na(hw_water), hw_water := hw_water_pma]
  all <- all[is.na(hw_soap), hw_soap := hw_soap_pma]
  
  #reduce number of time and distance to water variables
  #set correct data types
  all <- all[, mins_ws := as.numeric(mins_ws)]
  all <- all[, mins_queue_plus_trip := as.numeric(mins_queue_plus_trip)]
  all <- all[, mins_queue_ws := as.numeric(mins_queue_ws)]
  
  #set NA queue times to 0
  all <- all[!is.na(mins_ws) & is.na(mins_queue_ws), mins_queue_ws := 0]
  #set NA time to water units to correct unit
  all <- all[is.na(mins_ws_unit), mins_ws_unit := mins_ws_manual_unit]
  #double one-way trip times
  all <- all[!is.na(mins_ws) & mins_ways == 1, mins_ws := 2*mins_ws]
  all <- all[!is.na(mins_queue_plus_trip) & mins_ways == 1, mins_queue_plus_trip := 2*mins_queue_plus_trip]
  all <- all[!is.na(mins_ws) & mins_ways == 1, mins_ways := 2]
  all <- all[!is.na(mins_queue_plus_trip) & mins_ways == 1, mins_ways := 2]
  #fix known unit differences
  all <- all[!is.na(mins_queue_unit) & !is.na(mins_ws_unit) & mins_ws_unit != mins_queue_unit & mins_ws_unit == "Hour", mins_ws := 60*mins_ws]
  all <- all[!is.na(mins_queue_unit) & !is.na(mins_ws_unit) & mins_ws_unit != mins_queue_unit & mins_ws_unit == "Hour", mins_ws_unit := "Minute"]
  all <- all[!is.na(mins_queue_unit) & !is.na(mins_ws_unit) & mins_ws_unit != mins_queue_unit & mins_queue_unit == "Hour", mins_queue_ws := 60*mins_queue_ws]
  all <- all[!is.na(mins_queue_unit) & !is.na(mins_ws_unit) & mins_ws_unit != mins_queue_unit & mins_queue_unit == "Hour", mins_queue_unit := "Minute"]
  
  #document known issues with trip and queue time
  unit_problems <- all[!is.na(mins_queue_unit) & !is.na(mins_ws_unit) & mins_ws_unit != mins_queue_unit, list(nid, mins_queue_unit, mins_ws_unit)]
  if (nrow(unit_problems) > 0){
    #alert if temporal units are different
    message(paste("there are issues with nids", unique(unit_problems$nid), collapse=" "))
    write.csv(unit_problems, "/snfs1/temp/gmanny/wash_water_trip_unit_issues.csv", row.names=F)
  }
  
  make_negtive <- function(number){
    if (number < 0){
      return(number)
    } else{
      return(number*-1)
    }
  }
  
  #when units are equal, sum queue and trip time together
  all <- all[, time_to_source := mins_queue_plus_trip]
  #sum positive numbers together
  all <- all[!is.na(mins_ws) & !is.na(mins_ws_unit) & (mins_ws_unit == mins_queue_unit | is.na(mins_ws_unit) | is.na(mins_queue_unit)) & mins_ws > 0 & mins_queue_ws > 0, time_to_source := mins_ws + mins_queue_ws]
  #sub negative numbers together
  all <- all[!is.na(mins_ws) & !is.na(mins_ws_unit) & mins_ws_unit == mins_queue_unit & mins_ws < 0 | mins_queue_ws < 0, time_to_source := make_negative(mins_ws) + make_negative(mins_queue_ws)]
  
  all <- all[!is.na(mins_ws) & !is.na(mins_queue_ws) & is.na(mins_ws_unit), time_to_source := mins_ws + mins_queue_ws]
  
  #fix missing PMA minutes to water source values
  all[grepl("PMA2020", survey_name) & (mins_ws == -88 | mins_ws == -99), mins_ws := NA]
  all[grepl("PMA2020", survey_name) & (mins_queue_plus_trip == -88 | mins_queue_plus_trip == -99), mins_queue_plus_trip := NA]
  
  #recalc distances to water
  
  
  
  message("drop duplicate HH entries and cleanup hh_sizes")
  ####
  # 0. Set hh_size values to NA for nid 157397 7438
  nids_without_unique_hh_ids <- c(157397, 7438, 24915)
  all[nid %in% nids_without_unique_hh_ids, hh_size := NA]
  # 1. separate NA hh_size values from dataset
  
  #drop data that doesn't need a hh_size crosswalk and that has NA hh_sizes
  #all <- all[!is.na(hh_size) & !is.na(t_type) & !is.na(w_source_drink) & !(nid %in% nids_that_need_hh_size_crosswalk), ]
  
  #create indicator for hh_size missingness
  all[, missingHHsize := sum(is.na(hh_size)), by=nid]
  all[, obs := .N, by=nid]
  all[, pct_miss_hh_size := 100 * missingHHsize / obs]
  all[, is_hh := pct_miss_hh_size > 0]
  
  #subset cases where all hh_sizes are present. in these, each row is a HH
  has_hh_size <- all[pct_miss_hh_size > 0, ]
  has_hh_size[, uq_id := paste(nid, psu, hh_id, year_start, lat, long, shapefile, location_code, sep="_")] #includes space-time
  has_hh_size[, prev_uq_id := paste(nid, psu, hh_id, sep="_")] 
  diff <- length(unique(has_hh_size$uq_id)) - length(unique(has_hh_size$prev_uq_id))
  message(paste("There are", diff, "more unique households from including spacetime than excluding."))
  hhhs <- distinct(has_hh_size, uq_id, .keep_all=T)
  
  #subset cases where all hh_sizes are missing. these are HHM
  missing_hh_size <- all[pct_miss_hh_size <= 0, ]
  missing_hh_size[, hh_size := 1]
  
  packaged <- rbind(hhhs, missing_hh_size, fill=T)
  
  nids_that_need_hh_size_crosswalk <- c(20998, #MACRO_DHS_IN UGA 1995 WN
                                        32144, 32138, 1301, 1308, 1322) #BOL/INTEGRATED_HH_SURVEY_EIH
  packaged[nid %in% nids_that_need_hh_size_crosswalk, hh_size := NA]
  
  # pt_collapse <- packaged[!is.na(lat) & !is.na(long),]
  # poly_collapse <- packaged[(is.na(lat) & is.na(long)) & (!is.na(shapefile) & !is.na(location_code))]
  # ## ADD URBAN ^^^^
  # # 3. 
  # #
  # #
  # #
  # #
  # #
  # ####
  # #save(packaged, file="/snfs1/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/peru_08_04_2017.RData")
  # #stop("You just saved a Peru-Specific dataset. Should that still be in the code?")
  # #nids <- unique(packaged$nid)
  # #for (n in nids){
  # #  message(paste("nid", n))
  # #  message(sum(packaged[nid == n, ]$hh_size, na.rm=T))
  # #  message()
  # #}
  # pt_collapse <- all[!is.na(lat) & !is.na(long), ]
  # #if hh_size is 99 make it NA
  # pt_collapse[is.na(hh_size), hhs := 1]
  # pt_collapse[is.na(hh_size), hh_size := sum(hhs, na.rm=T), by=list(nid, hh_id, geospatial_id, hhweight, 
  #                                                                   year_start, iso3, lat, long, w_source_drink, 
  #                                                                   w_source_other, mins_ws, dist_ws, dist_ws_unit, 
  #                                                                   t_type, shared_san, shared_san_num, hw_station, 
  #                                                                   hw_water, hw_soap)]
  # pt_collapse[, hhs := NULL]
  # key <- c("nid", "hh_id", "geospatial_id", "hhweight", "year_start", "iso3", "lat", "long", 
  #          "w_source_drink", "w_source_other", "mins_ws", "dist_ws", 
  #          "dist_ws_unit", "t_type", "shared_san", "shared_san_num", 
  #          "hw_station", "hw_water", "hw_soap")
  # pt_collapse <- unique(pt_collapse, by=key)
  # pt_collapse <- pt_collapse[nid %in% nids_that_need_hh_size_crosswalk, hh_size := NA]
  # #if (any(pt_collapse$hh_size > 1000)) { stop("household sizes are crazy big in the points")}
  # 
  # #l <- nrow(pt_collapse)*0.2
  # #pt_sample <- pt_collapse[sample(nrow(pt_collapse), l), ]
  # #save(pt_sample, file=paste0(folder_out, "/sample_points_collapsed_", module_date, ".Rdata"))
  # message("saving point data")
  # save(pt_collapse, file=paste0(folder_out, "/points_collapsed_", module_date, ".Rdata"))
  # 
  # #collapse polys by shp/location_id
  # all[shapefile=="", shapefile:=NA]
  # all[location_code=="", location_code:=NA]
  # poly_collapse <- all[!is.na(shapefile) & !is.na(location_code) & (is.na(lat) | is.na(long)),]
  # poly_collapse <- poly_collapse[is.na(hh_size), hhs:=1]
  # poly_collapse <- poly_collapse[is.na(hh_size), hh_size := sum(hhs), by=list(nid, hh_id, geospatial_id, hhweight, year_start, iso3, 
  #                                                                             shapefile, location_code, w_source_drink, w_source_other, 
  #                                                                             mins_ws, dist_ws, dist_ws_unit, t_type, shared_san, 
  #                                                                             shared_san_num, hw_station, hw_water, hw_soap)]
  # poly_collapse[, hhs := NULL]
  # #poly_collapse <- poly_collapse[, clusters_in_collapse := uniqueN(psu), by=list(nid, hh_id, geospatial_id, hhweight, year_start, iso3, shapefile, location_code, w_source_drink, w_source_other, mins_ws, dist_ws, dist_ws_unit, t_type, shared_san, shared_san_num, hw_station, hw_water, hw_soap)]
  # poly_collapse <- poly_collapse[nid %in% nids_that_need_hh_size_crosswalk, hh_size := NA]
  # #setkey(poly_collapse, shapefile, location_code, w_source_drink, w_source_other, mins_ws, dist_ws, dist_ws_unit, t_type, shared_san, shared_san_num, hw_station, hw_water, hw_soap)
  # key <- c("nid", "hh_id", "geospatial_id", "hhweight", "year_start", "iso3", "shapefile", "location_code", 
  #          "w_source_drink", "w_source_other", "mins_ws", "dist_ws", 
  #          "dist_ws_unit", "t_type", "shared_san", "shared_san_num", 
  #          "hw_station", "hw_water", "hw_soap")
  # poly_collapse <- unique(poly_collapse, by=key)
  #if (any(poly_collapse$hh_size > 1000)) { stop("household sizes are crazy big in the polygons")}
  #l <- nrow(poly_collapse)*0.2
  #poly_sample <- poly_collapse[sample(nrow(poly_collapse), l), ]
  #save(poly_sample, file=paste0(folder_out, "/sample_polys_collapsed_", module_date, ".Rdata"))
  
  message("saving points")
  pt_collapse <- packaged[!is.na(lat) & !is.na(long), ]
  save(pt_collapse, file=paste0(folder_out, "/points_", module_date, ".Rdata"))
  write_feather(pt_collapse, path=paste0(folder_out, "/points_", module_date, ".feather"))
  
  message("saving polygons")
  poly_collapse <- packaged[(is.na(lat) | is.na(long)) & !is.na(shapefile) & !is.na(location_code), ]
  save(poly_collapse, file=paste0(folder_out, "/poly_", module_date, ".Rdata"))
  write_feather(poly_collapse, path=paste0(folder_out, "/poly_", module_date, ".feather"))
  
  #message("saving data")
  #save(packaged, file=paste0(folder_out, "/packaged_dataset_", module_date, ".Rdata"))
} else if (topic == "diarrhea"){
  message("saving to J")
  all <- all[!is.na(had_diarrhea), ]
  #all <- all[age_year < 5,]
  save(all, file=paste0(folder_out, "/", module_date, ".Rdata"))
}
# to_collapse <- subset(all, !is.na(hh_id) & (is.na(hh_size) | survey_module != "HH"))
# no_collapse <- subset(all, is.na(hh_id) | (!is.na(hh_size) | survey_module == "HH"))
# #to_collapse <- subset(all, !is.na(hh_id) & is.na(hh_size) & !is.na(psu))
# #no_collapse <- subset(all, is.na(hh_id) | !is.na(hh_size) | is.na(psu))
# cols <- names(to_collapse)
# to_collapse <- to_collapse[is.na(hh_size), hhs := 1]
# has_hhid <- to_collapse[is.na(hh_size), hh_size := sum(hhs), by=list(nid, psu, hh_id)]
# setkeyv(has_hhid, cols)
# h <- unique(has_hhid)
# all <- rbind.fill(h, no_collapse)

#keep <- grep(paste0(drop_x, collapse="|"), names(all), value=T, invert=T)

#all <- subset(all, select=keep)

#message("saving .Rdata file of matched extractions")
#save(all, file=paste0(folder_out, "/", module_date, ".Rdata"))

message("subsetting out unmatched geographies")
gnid <- unique(geo$nid)
#fix <- subset(all, is.na(shapefile) & (is.na(lat) | is.na(long)) )
fix <- subset(all, !(all$nid %in% gnid))
fix <- data.frame(fix)
fix_collapse <- unique(fix[c("nid", "iso3", "year_start", "survey_name")])
fix_collapse <- merge(fix_collapse, iso, by.x="iso3", by.y="alpha.3", all.x=T)

message("writing csv of unmatched extractions")
fix_outpath <- paste0("/snfs1/LIMITED_USE/LU_GEOSPATIAL/geo_matched/", topic, "/new_geographies_to_match.csv")
write.csv(fix_collapse, fix_outpath, row.names=F)

#SAVE .Rdata OF EXTRACTIONS MATCHED TO GEOGRAPHIES