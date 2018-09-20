#Rewrite progress for wash collapse
#Mathew Baumann


rm(list = ls())


cores <- 40

files <- file.info(list.files(paste0('/home/j/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash'), pattern = '*.feather', full.names=TRUE))
files <- files[with(files, order(as.POSIXct(ctime), decreasing = TRUE)), ]
latest_postextraction <- unlist(strsplit(rownames(files)[1], "/"))
latest_postextraction <- latest_postextraction[length(latest_postextraction)]
input_version <- gsub('.feather', '', latest_postextraction)
input_version <- gsub('poly_', '', input_version)
input_version <- gsub('points_', '', input_version)

repo <- '/share/code/geospatial/baumannm/wash_mapping/01_collapse/'
root <- "/home/j/"

# Detach all but base packages
detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices",
                      "package:utils","package:datasets","package:methods",
                      "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list)
    detach(package, character.only=TRUE)
}
detachAllPackages()

# Load and install, if necessary, needed packages
packages <- c('dplyr', 'feather', 'parallel', 'doParallel')
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
source("/share/code/geospatial/lbd_core/mbg_central/setup.R")
mbg_setup(package_list = packages, repos="/share/code/geospatial/lbd_core/mbg_central")




message(paste("Loading point data data"))
points <- read_feather(paste0(root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/points_', input_version, '.feather'))

message(paste("Loading poly data data"))
polys <- read_feather(paste0(root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly_', input_version, '.feather'))

pt_collapse <- rbind(points, polys)
Encoding(pt_collapse$w_source_drink) <- "UTF-8"
Encoding(pt_collapse$w_source_other) <- "UTF-8"
Encoding(pt_collapse$t_type) <- "UTF-8"
pt_collapse$w_source_drink <- tolower(pt_collapse$w_source_drink)
pt_collapse$w_source_other <- tolower(pt_collapse$w_source_other)
pt_collapse$t_type <- tolower(pt_collapse$t_type)
pt_collapse$iso3 <- substr(pt_collapse$iso3, 1, 3)

source('/home/j/temp/baumannm/scripts/pe_and_collapse/collapse_nids_sep_test.R')
unique_nids <- unique(pt_collapse$nid)


message("Make cluster")
cl <- makeCluster(cores)
clusterEvalQ(cl, .libPaths('/home/j/temp/geospatial/singularity_packages/3.5.0'))
message("Register cluster")
registerDoParallel(cl)
message("Start foreach")
#Read in each .dta file in parallel - returns a list of data frames
collapsed_nids <- foreach(i=1:length(unique_nids), .packages = c('dplyr', 'feather')) %dopar% {
  nids <- collapse_non_ipums(indi_fam = 'water', survey = unique_nids[i])
  return(nids)
}
message("Foreach finished")
message("Closing cluster")
stopCluster(cl)



