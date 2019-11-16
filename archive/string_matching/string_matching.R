#source("/snfs2/HOME/gmanny/backups/Documents/Repos/wash_mapping/string_matching/string_matching.R")

if(!require(pacman)) {
  install.packages("pacman"); require(pacman)}

p_load(data.table, magrittr, plyr)

j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")

today <- gsub("-", "_", Sys.Date())

message("loading polygons")
if (!("poly_collapse" %in% ls())) load(paste0(j, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/polys_collapsed_2017_07_20.Rdata"))

message("loading points")
if (!("pt_collapse" %in% ls())) load(paste0(j, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/points_collapsed_2017_07_20.Rdata"))

message("combining")
w_collapse <- rbind(poly_collapse, pt_collapse, fill=T)

message("cleanup unnecessary columns")
keep <- c("nid", "iso3", "point", "lat", "long", "shapefile", "location_code", 
          "survey_series", "year_start", "strata", "psu", "hhweight", "int_year",
          "int_month", "t_type", "mins_ws", "hh_size", "w_source_drink", "urban",
          "w_source_other", "shared_san", "sewage", "dist_ws", "shared_san_num",
          "hw_station", "dist_ws_unit", "hw_water", "hw_soap", "clusters_in_polygon")

w_collapse <- w_collapse[, keep, with=F]

message("subset to africa")
iso <- read.csv(paste0(j, "temp/gmanny/geospatial_stages_priority.csv"), stringsAsFactors=F)
af <- iso[(iso$Stage == "1"), "alpha.3"] %>% as.character %>% sort()
af_wash <- w_collapse[iso3 %in% af, ]

w_collapse <- af_wash

message("collect strings")
w_str <- w_collapse$w_source_drink %>% unique
w_o_str <- w_collapse$w_source_other %>% unique
t_str <- w_collapse$t_type %>% unique
#ts_str <- paste(w_collapse$t_type, w_collapse$sewage, sep=" ") %>% unique

message("read in current string matches")
water_drink <- read.csv(paste0(j, "WORK/11_geospatial/wash/definitions/w_source_defined_updated_2017_05_24.csv"), stringsAsFactors = F)
water_other <- read.csv(paste0(j, "WORK/11_geospatial/wash/definitions/2nd_w_other_defined_updated_2017_05_18.csv"), stringsAsFactors = F)
toilet <- read.csv(paste0(j, "WORK/11_geospatial/wash/definitions/t_type_defined_updated_2017_05_25.csv"), stringsAsFactors = F)

w_def <- water_drink$string
w_o_def <- water_other$string
t_def <- toilet$string

message("find unmatched strings")
w_fix <- w_str[!(w_str %in% w_def)]
w_o_fix <- w_o_str[!(w_o_str %in% w_o_def)]
t_fix <- t_str[!(t_str %in% t_def)]

message("rbind them to original dataframe")
w_new <- data.frame(string = w_fix, stringsAsFactors = F)
w_o_new <- data.frame(string = w_o_fix, stringsAsFactors = F)
t_new <- data.frame(string = t_fix, stringsAsFactors = F)

water_drink <- rbind.fill(water_drink, w_new)
water_other <- rbind.fill(water_other, w_o_new)
toilet <- rbind.fill(toilet, t_new)

message("writing csvs to J:/WORK/11_geospatial/wash/definitions")
write.csv(water_drink, paste0(j, "WORK/11_geospatial/wash/definitions/w_source_defined_updated_", today, ".csv"), row.names=F)
write.csv(water_other, paste0(j, "WORK/11_geospatial/wash/definitions/w_other_defined_updated_", today, ".csv"), row.names=F)
write.csv(toilet, paste0(j, "WORK/11_geospatial/wash/definitions/t_type_defined_updated_", today, ".csv"), row.names=F)