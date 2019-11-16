rm(list=ls())

#source("/snfs2/HOME/gmanny/backups/Documents/Repos/wash_mapping/00_extract/id_incomplete_defintion_nids.R")

indicator <- "t_type" #w_source or t_type

j <- ifelse(Sys.info()[1]=="Windows", "J:/", "/snfs1/")
.libPaths(paste0(j, "temp/geospatial/geos_packages/"))

library(data.table)
library(feather)
library(magrittr)
library(dplyr)

stages <- read.csv(paste0(j, "temp/gmanny/geospatial_stages_priority.csv"), stringsAsFactors=F) %>% data.table
africa <- stages[Stage == 1, alpha.3]

defs <- list.files(paste0(j, "WORK/11_geospatial/wash/definitions"), pattern=indicator, full.names = T) %>% grep(value=T, pattern="other", invert=T) #%>% grep(pattern="overwrite", value=T) #temp fix to J permissions issue
def <- defs[length(defs)] #gets latest defintions file

w_defs <- read.csv(def, encoding="windows-1252", stringsAsFactors = F)
#turning this into a data table was causing it to drop strings. please don't do that.
w_defs <- distinct(w_defs, string, sdg, .keep_all=T)

feathers <- list.files(paste0(j, "LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash"), pattern=".feather$", ignore.case = T, full.names=T)

#get latests point and polygon feathers
pts <- grep(pattern="points", x=feathers, value = T)
pts <- pts[length(pts)]
polys <- grep(pattern="poly", x=feathers, value=T)
polys <- polys[length(polys)]

message("Reading points")
pt <- read_feather(pts)

message("Reading polys")
poly <- read_feather(polys)

all <- rbindlist(list(pt, poly), fill=T, use.names=T)
#subset to relevant stage
relevant <- all[substr(iso3, 1, 3) %in% africa & year_start > 1997]

Encoding(relevant$w_source_drink) <- 'windows-1252'

message("Merging")

merge_key <- ifelse(indicator == "w_source", "w_source_drink", indicator)
relevant <- data.frame(relevant)
#merging large data.tables causes specific strings to (consistently and reproduicibly) drop. this is concerning. Make sure everything is a data frame before merging.
try(setnames(w_defs, "string", merge_key))
start_time <- Sys.time()
m <- left_join(relevant, w_defs, by=merge_key)
end_time <- Sys.time()
message(paste("Merge took", as.character(end_time - start_time), "seconds."))
#m <- merge(relevant, w_defs, all.x = T, by.x=merge_key, by.y="string")

indic_ct <- data.table(m)
indic_ct[grepl("_imp", sdg), sdg := "imp"]
indic_ct[grepl("_unimp", sdg), sdg := "unimp"]
indic_ct[grepl("piped", sdg), sdg := "piped"]
indic_ct[grepl("_cw", sdg), sdg := "imp, unimp"]
indic_ct[sdg == "bottled", sdg := NA]
indic_ct[sdg == "", sdg := NA]

#add column indicating whether nid has:
#piped, imp (including spring_imp & well_imp), unimp (including spring_unimp & well_unimp), surface
indic_ct <- distinct(indic_ct, nid, iso3, year_start, year_end, survey_name, sdg)

nids <- unique(indic_ct$nid)
#iterate through that vector
for (id in nids){
  #get unique & sorted vector of strings it contains
  strs <- sort(indic_ct[nid==id, sdg])
  str <- paste(strs, collapse=", ")
  strs <- strsplit(str, ", ") %>% unlist %>% unique %>% sort
  str <- paste(strs, collapse=", ")
  #message(strs)
  indic_ct[nid == id, contents := paste(strs, collapse=", ")]
  indic_ct[contents == "", contents := NA]
}

af_only_contents <- indic_ct[, N := uniqueN(nid), by=contents] %>% distinct(contents, N)

indic_ct <- distinct(indic_ct, nid, iso3, year_start, year_end, survey_name, contents)


write.csv(indic_ct, paste0(j, "temp/gmanny/", indicator, "_africa_surveys_in_need_of_definition_help.csv"), row.names=F)
write.csv(af_only_contents, paste0(j, "temp/gmanny/africa", indicator, "_question_option_types_aggregated.csv"), row.names=F)
# Make aggregation of missingness by type of missingness instaed of by NID
#get unique vector of nids


#collapse & comma-separate that vector
#make it a contents column
#distinct by contents column, aggregate by number of surveys
