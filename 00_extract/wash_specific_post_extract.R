#source("/snfs2/HOME/gmanny/backups/Documents/Repos/geospatial-data-prep/common/wash_specific_post_extract.R")
message("dropping duplicate women in single household (so household sizes aren't duplicated)")
all[is.na(hhweight) & !is.na(pweight), hhweight := pweight]
#"hw_soap1", "hw_soap2", "hw_soap3"
#custom fix for mics handwash soap
all[hw_soap1 == 0, hw_soap := 0]
all[hw_soap2 == 0, hw_soap := 0]
all[hw_soap3 == 0, hw_soap := 0]
all[hw_soap1 == 1, hw_soap := 1]
all[hw_soap2 == 1, hw_soap := 1]
all[hw_soap3 == 1, hw_soap := 1]

drop <- c("line_id", "sex_id", "age_year", "age_month", "age_day", "pweight", "cooking_fuel_mapped", "w_treat", "w_filter", "w_boil", "w_bleach", "nid_n", "year", "t_type_multi", "w_solar", "w_cloth", "w_settle", "hw_soap1", "hw_soap2", "hw_soap3")
all <- all[, (drop):= NULL]
wn <- all[survey_module == "WN", ]
wn_key <- c("psu", "hh_id")
wn <- distinct(wn, psu, hh_id, .keep_all=T)
all <- all[survey_module != "WN", ]
all <- rbind(all, wn, fill=T, use.names=T)

message("custom wash fixes")
drop <- c("latitude", "longitude")
all <- all[, (drop):=NULL]

make_negtive <- function(number){
  if (number < 0){
    return(number)
  } else{
    return(number*-1)
  }
}


message("drop duplicate HH entries and cleanup hh_sizes")
####
# 0. Set hh_size values to NA for nid 157397 7438
nids_without_unique_hh_ids <- c(157397, 7438, 24915)
all[nid %in% nids_without_unique_hh_ids, hh_size := NA]


#subset cases where all hh_sizes are present. Make sure each Row is a HH
has_hh_size_no_id <- all[!is.na(hh_size) & is.na(hh_id), ]
has_hh_size_id <- all[!is.na(hh_size) & !is.na(hh_id), ]
has_hh_size_id[, uq_id := paste(nid, psu, geospatial_id, hh_id, year_start, lat, long, shapefile, location_code, sep="_")] #includes space-time
has_hh_size_id[, prev_uq_id := paste(nid, psu, hh_id, sep="_")]
diff <- length(unique(has_hh_size_id$uq_id)) - length(unique(has_hh_size_id$prev_uq_id))
message(paste("There are", diff, "more unique households from including spacetime than excluding."))
hhhs <- distinct(has_hh_size_id, uq_id, .keep_all=T)

#subset cases where all hh_sizes are missing and each row is not a HH. Set hh_size to 1
missing_hh_size <- all[is.na(hh_size) & survey_module != 'HH', ]
missing_hh_size[, hh_size := 1]

missing_hh_size_hh <- all[is.na(hh_size) & survey_module == 'HH', ]

packaged <- rbind(hhhs, has_hh_size_no_id, fill=T)
rm(hhhs)
rm(has_hh_size_no_id)
packaged <- rbind(packaged, missing_hh_size, fill=T)
rm(missing_hh_size)
packaged <- rbind(packaged, missing_hh_size_hh, fill=T)
rm(missing_hh_size_hh)


excluded_surveys <- c(8556, #dropping MEX/NATIONAL_HEALTH_SURVEY_ENSA due to bad weighting
                      261889, 261887) #MAL_ED due to non-representative sample from hospital visits
packaged <- packaged[!(nid %in% excluded_surveys),]

message('hard coding survey 235348 to polys')
packaged <- packaged[nid == 235348, lat := NA]
packaged <- packaged[nid == 235348, long := NA]

#message("saving points")
pt_collapse <- packaged[!is.na(lat) & !is.na(long), ]
#set start_year to int_year for point data
pt_collapse[, year_experiment := start_year]
pt_collapse[!is.na(int_year), year_experiment := int_year]
#save(pt_collapse, file=paste0(folder_out, "/points_", today, ".Rdata"))


#message("saving polygons")
poly_collapse <- packaged[(is.na(lat) | is.na(long)) & !is.na(shapefile) & !is.na(location_code), ]
#set polygon years to a weighted mean
poly_collapse[, year_experiment := start_year]
poly_collapse[, year_experiment := weighted.mean(int_year, weight=hhweight, na.rm=T), by=c("nid")]
#save(poly_collapse, file=paste0(folder_out, "/poly_", today, ".Rdata"))



library(feather)
message("Point Feather")
write_feather(pt_collapse, path=paste0(folder_out, "/points_", today, ".feather"))

#break up files because featherss dont like the size of our data
message('Poly Feathers')
n <- nrow(poly_collapse)
poly1 <- poly_collapse[1:ceiling(n/8),]
poly2 <- poly_collapse[(ceiling(n/8) + 1):(ceiling(n/8) * 2),]
poly3 <- poly_collapse[((ceiling(n/8) * 2) + 1):(ceiling(n/8) * 3),]
poly4 <- poly_collapse[((ceiling(n/8) * 3) + 1):(ceiling(n/8) * 4),]
poly5 <- poly_collapse[((ceiling(n/8) * 4) + 1):(ceiling(n/8) * 5),]
poly6 <- poly_collapse[((ceiling(n/8) * 5) + 1):(ceiling(n/8) * 6),]
poly7 <- poly_collapse[((ceiling(n/8) * 6) + 1):(ceiling(n/8) * 7),]
poly8 <- poly_collapse[((ceiling(n/8) * 7) + 1):n,]
write_feather(poly1, path=paste0(folder_out, "/poly1_", today, ".feather"))
write_feather(poly2, path=paste0(folder_out, "/poly2_", today, ".feather"))
write_feather(poly3, path=paste0(folder_out, "/poly3_", today, ".feather"))
write_feather(poly4, path=paste0(folder_out, "/poly4_", today, ".feather"))
write_feather(poly5, path=paste0(folder_out, "/poly5_", today, ".feather"))
write_feather(poly6, path=paste0(folder_out, "/poly6_", today, ".feather"))
write_feather(poly7, path=paste0(folder_out, "/poly7_", today, ".feather"))
write_feather(poly8, path=paste0(folder_out, "/poly8_", today, ".feather"))
