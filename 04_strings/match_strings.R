# Clear environment
rm(list = ls())

#source("/snfs2/HOME/gmanny/backups/Documents/Repos/wash_mapping/04_strings/match_strings.R")
package_lib <- '/snfs1/temp/geospatial/geos_packages'
.libPaths(package_lib)
library(magrittr)
library(data.table)
library(feather)
j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
l <- ifelse(Sys.info()[1]=="Windows", "L:/", "/ihme/limited_use/")

stg_mast <- fread('/home/j/WORK/11_geospatial/10_mbg/stage_master_list.csv')

most_recent_extracts <- list.files(paste0(l, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/"), full.names = T, pattern=".feather$") %>% grep(value=T, pattern="poly", invert=T)
extract_info <- file.info(most_recent_extracts)
extract_info$path <- rownames(extract_info)
extract_info <- data.table(extract_info)
most_recent_point <- extract_info[order(mtime, decreasing = T), path][1]

most_recent_extracts <- list.files(paste0(l, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/"), full.names = T, pattern=".feather$") %>% grep(value=T, pattern="point", invert=T)
extract_info <- file.info(most_recent_extracts)
extract_info$path <- rownames(extract_info)
extract_info <- data.table(extract_info)
most_recent_poly <- extract_info[order(mtime, decreasing = T), path][1]

message("Loading big extraction .Rdata")
# load(most_recent_extract)
# #called all. should rename to packaged. subset only to surveys that have data at or beyond 1997
# packaged <- all[all$year_end >= 1997, ]
# #rm(all)
point <- read_feather(most_recent_point)
pt1_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly1_', '2019_09_26', '.feather')))
pt2_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly2_', '2019_09_26', '.feather')))
pt3_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly3_', '2019_09_26', '.feather')))
pt4_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly4_', '2019_09_26', '.feather')))
pt5_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly5_', '2019_09_26', '.feather')))
pt6_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly6_', '2019_09_26', '.feather')))
pt7_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly7_', '2019_09_26', '.feather')))
pt8_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly8_', '2019_09_26', '.feather')))

pt_collapse <- rbind(pt1_collapse, pt2_collapse, fill = TRUE)
rm(pt1_collapse, pt2_collapse)
pt_collapse <- rbind(pt_collapse, pt3_collapse, fill = TRUE)
rm(pt3_collapse)
pt_collapse <- rbind(pt_collapse, pt4_collapse, fill = TRUE)
rm(pt4_collapse)
pt_collapse <- rbind(pt_collapse, pt5_collapse, fill = TRUE)
rm(pt5_collapse)
pt_collapse <- rbind(pt_collapse, pt6_collapse, fill = TRUE)
rm(pt6_collapse)
pt_collapse <- rbind(pt_collapse, pt7_collapse, fill = TRUE)
rm(pt7_collapse)
pt_collapse <- rbind(pt_collapse, pt8_collapse, fill = TRUE)
rm(pt8_collapse)
packaged <- as.data.table(rbind(point, pt_collapse))

packaged$w_source_drink <- tolower(packaged$w_source_drink)
packaged$w_source_other <- tolower(packaged$w_source_other)

packaged$t_type <- tolower(packaged$t_type)
packaged$sewage <- tolower(packaged$sewage)

packaged[!is.na(sewage) & !is.na(t_type), t_type := paste0(t_type, ' ', sewage)]
packaged[is.na(t_type) & !is.na(sewage), t_type := sewage]

packaged <- unique(packaged[,c('nid','iso3','year_start','w_source_drink','t_type')])
packaged <- subset(packaged, !iso3 %in% subset(stg_mast, Stage == 3)$iso3) %>% subset(year_start > 1999)


w <- fread(paste0(j, "WORK/11_geospatial/wash/definitions/", "w_source_defined_by_nid_2019_09_20.csv"))
t <- fread(paste0(j, "WORK/11_geospatial/wash/definitions/", "t_type_defined_by_nid_2019_09_23.csv"))


w_new <- merge(unique(packaged[,c('nid','iso3','year_start','w_source_drink')]), w, by = c('nid','iso3','year_start','w_source_drink'), all.x = T)
t_new <- merge(unique(packaged[,c('nid','iso3','year_start','t_type')]), t, by = c('nid','iso3','year_start','t_type'), all.x = T)

w_new <- subset(w_new, !is.na(sdg))
t_new <- subset(t_new, !is.na(sdg))


write.csv(w_new, paste0(j, "WORK/11_geospatial/wash/definitions/", "w_source_defined_by_nid_2019_09_27.csv"))
write.csv(t_new, paste0(j, "WORK/11_geospatial/wash/definitions/", "t_type_defined_by_nid_2019_09_27.csv"))
