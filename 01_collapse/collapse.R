#### Set Up Environment ####
# Clear environment
rm(list = ls())

# Define if you are running code loally
local <- F

files <- file.info(grep('IPUMS', list.files(paste0('/home/j/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash'), pattern = '.feather', full.names=TRUE), invert = T, value = T ))
files <- files[with(files, order(as.POSIXct(ctime), decreasing = TRUE)), ]
latest_postextraction <- unlist(strsplit(rownames(files)[1], "/"))
latest_postextraction <- latest_postextraction[length(latest_postextraction)]
input_version <- gsub('.feather', '', latest_postextraction)
input_version <- gsub('poly_', '', input_version)
input_version <- gsub('points_', '', input_version)
input_version <- gsub('poly1_', '', input_version)
input_version <- gsub('poly2_', '', input_version)

# Set repo & library path 
# if(Sys.info()[1]!="Windows") {
#   if(!local) {
#     root <- "/home/j/"
#     package_lib <- ifelse(grepl("geos", Sys.info()[4]),
#                           paste0(root,'temp/geospatial/geos_packages'),
#                           paste0(root,'temp/geospatial/packages'))
#     .libPaths(package_lib)
#   } else {
#     package_lib <- .libPaths()
#     root <- '/home/j/'
#   }
# } else {
#   package_lib <- .libPaths()
#   root <- 'J:/'
# }

root <- "/home/j/"

repo <- '/share/code/geospatial/baumannm/wash_mapping_current/01_collapse/'

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
packages <- c('dplyr', 'feather', 'data.table')
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, library, character.only = T)


increment <- 1

file_type <- 'poly'
indi_fam <- 'water'
index <- 1
agg_level <- 'country'

#### Load functions ####
for (file_type in c('pt','poly')){
  message(paste("Loading",file_type, "data"))
  rm(pt_collapse)
  message('Loading Data...')
  # Load data
  if (!("pt_collapse" %in% ls()) & file_type == 'pt') {
    pt_collapse <- as.data.table(read_feather(paste0(root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/points_', input_version, '.feather')))
    #pt_collapse <- subset(pt_collapse, nid == 365281)
    # Encoding(pt_collapse$w_source_drink) <- "UTF-8"
    # Encoding(pt_collapse$w_source_other) <- "UTF-8"
    # Encoding(pt_collapse$t_type) <- "UTF-8"
    # Encoding(pt_collapse$sewage) <- "UTF-8"
    pt_collapse$w_source_drink <- tolower(pt_collapse$w_source_drink)
    pt_collapse$w_source_other <- tolower(pt_collapse$w_source_other)
    pt_collapse$t_type <- tolower(pt_collapse$t_type)
    pt_collapse$sewage <- tolower(pt_collapse$sewage)
    data_type <- 'pt'
  } 
  
  if (!("pt_collapse" %in% ls()) & file_type == 'poly') {
    pt1_collapse <- as.data.table(read_feather(paste0(root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly1_', input_version, '.feather')))
    pt2_collapse <- as.data.table(read_feather(paste0(root,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly2_', input_version, '.feather')))
    pt_collapse <- rbind(pt1_collapse, pt2_collapse, fill = TRUE)
    pt_collapse <- subset(pt_collapse, nid == 148344)
    # Encoding(pt_collapse$w_source_drink) <- "UTF-8"
    # Encoding(pt_collapse$w_source_other) <- "UTF-8"
    # Encoding(pt_collapse$t_type) <- "UTF-8"
    # Encoding(pt_collapse$sewage) <- "UTF-8"
    pt_collapse$w_source_drink <- tolower(pt_collapse$w_source_drink)
    pt_collapse$w_source_other <- tolower(pt_collapse$w_source_other)
    pt_collapse$t_type <- tolower(pt_collapse$t_type)
    pt_collapse$sewage <- tolower(pt_collapse$sewage)
    data_type <- 'poly'
  }
  
  if(file_type != 'ipums'){
    #concatenate t_type and sewage
    pt_collapse[!is.na(sewage) & !is.na(t_type), t_type := paste0(t_type, ' ', sewage)]
    pt_collapse[is.na(t_type) & !is.na(sewage), t_type := sewage]
  }
  
  if (file_type == 'ipums') {
    ipums_dir <- '/home/j/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/IPUMS_feathers'
    files <- list.files(ipums_dir, '.feather')
    files_length <- length(files)
  } else {
    files <- list(pt_collapse)
    files_length <- length(files)
  }
  
  for (index in 1:files_length) {
    if (file_type == 'ipums') {
      ipums <- T
      setwd(ipums_dir)
      pt_collapse <- read_feather(files[index])
      pt_collapse$t_type <- as.character(pt_collapse$t_type)
      pt_collapse$sewage <- as.character(pt_collapse$sewage)
      # Encoding(pt_collapse$t_type) <- "UTF-8"
      # Encoding(pt_collapse$sewage) <- "UTF-8"
      pt_collapse$t_type <- tolower(pt_collapse$t_type)
      pt_collapse$sewage <- tolower(pt_collapse$sewage)
      
      if (all(is.na(unique(pt_collapse$lat)))) {
        data_type <- 'poly'
      } else {
        data_type <- 'pt'
      }
      
      message('Skipping water for IPUMS due to non-standard data')
      indicators <- 'sani'
      
    } else {
      pt_collapse <- files[[1]]
      ipums <- F
      indicators <- c('sani', 'water')
      rm(files)
    }
    
    ### Standardize iso3s
    pt_collapse$iso3 <- substr(pt_collapse$iso3, 1, 3)
    
    for (indi_fam in indicators) {
      rm(definitions)
      message(paste('Processing:', indi_fam))
      
      for (agg_level in c('')) {
        message(paste("Collapsing",indi_fam, "with", agg_level, "agg_level"))
        message('Loading Definitions...')
        
        if (ipums) {
          if (indi_fam == 'sani') {
            definitions <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/IPUMS_sani_defs.csv'),
                                    encoding="windows-1252", stringsAsFactors = F)
            definitions <- select(definitions, nid, toilet, sewage, sani)
            
            definitions$toilet <- tolower(definitions$toilet)
            definitions$sewage <- tolower(definitions$sewage)
            definitions$sani <- tolower(definitions$sani)
            
            definitions$toilet <- ifelse(definitions$toilet == "" | is.na(definitions$toilet),
                                         NA, definitions$toilet)
            definitions$sewage <- ifelse(definitions$sewage == "" | is.na(definitions$sewage),
                                         NA, definitions$sewage)
            definitions$sani <- ifelse(definitions$sani == "" | is.na(definitions$sani),
                                       NA, definitions$sani)
          }
        } else {
          if (!("definitions" %in% ls())) {
            if (indi_fam == "sani") {
              definitions <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/t_type_defined_2018_11_08.csv'),
                                      encoding="windows-1252", stringsAsFactors = F)
              definitions <- select(definitions, string, sdg)
            } else {
              definitions <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/w_source_defined_2018_11_08.csv'),
                                      encoding="windows-1252", stringsAsFactors = F) 
              definitions <- select(definitions, string, sdg, jmp)
            }
          }
          
          # Prep definitions file for further processing
          #definitions$string <- iconv(definitions$string, 'windows-1252', 'UTF-8')
          definitions$string <- tolower(definitions$string)
          definitions$sdg <- ifelse(definitions$sdg == "" | is.na(definitions$sdg),
                                    NA, definitions$sdg)
          definitions$string <- ifelse(definitions$string == "" | is.na(definitions$string),
                                       NA, definitions$string)
          definitions <- distinct(definitions)
        }
        
        rm(list = setdiff(ls(),c('definitions','pt_collapse','definitions2','indi_fam',
                                 'repo','data_type','root','agg_level', 'sdg', 'ipums', 'files', 'index', 'files_length',
                                 'file_type', 'ipums_dir', 'input_version', 'increment')))
        
        message("Importing functions...")
        setwd(repo)
        source('functions/initial_cleaning.R')
        source('functions/hh_cw.R')
        source('functions/address_missing.R')
        source('functions/cw_indi.R')
        source('functions/agg_wash.R')
        source('functions/define_wash.R')
        source('functions/write_cw.R')
        
        #### Subset & Shape Data ####
        message("Initial Cleaning...")
        temp_list <- initial_cleaning(census = T)
        ptdat <- temp_list[[1]]; ptdat_0 <- temp_list[[2]]
        rm(temp_list)
        
        #### Define Indicator ####
        message("Defining Indicator...")
        ptdat <- define_indi(sdg_indi = T, census = ipums)
        
        #### Address Missingness ####
        message("Addressing Missingness...")
        
        # Remove clusters with more than 20% weighted missingness
        ptdat <- rm_miss()
        if (nrow(ptdat) == 0) {
          next
        }
        
        # Remove cluster_ids with missing hhweight or invalid hhs
        miss_wts <- unique(ptdat$id_short[which(is.na(ptdat$hhweight))])
        ptdat <- filter(ptdat, !(id_short %in% miss_wts))
        ptdat <- filter(ptdat, hhweight != 0)
        
        #invalid_hhs <- unique(ptdat$id_short[which(ptdat$hh_size <= 0)])
        # ptdat <- filter(ptdat, !(id_short %in% invalid_hhs))
        ptdat <- as.data.table(ptdat)
        ptdat[hh_size == 0, hh_size := NA]
        
        if (nrow(ptdat) == 0) {
          next
        }
        
        # Crosswalk missing household size data
        message("Crosswalking HH Sizes...")
        if (!ipums) {
          ptdat <- hh_cw_reg(data = ptdat)
        } else {
          ptdat <- assign_ipums_hh()
        }
        
        # Remove missing observations
        ptdat <- filter(ptdat, !is.na(imp))
        
        if (nrow(ptdat) == 0) {
          next
        }
        
        #### Aggregate Data ####
        # Bookmarking dataset so it can be looped over for conditional switch
        ptdat_preagg <- ptdat
        
        # Conditional switch is to switch collapsing for conditional vs unconditional indicators
        conditional <- 'unconditional'
        
        # Reseting the dataset to preagregate
        ptdat <- ptdat_preagg
        message(paste("Conditional variables status:",conditional))
        
        # Aggregate indicator to cluster level
        message("Aggregating Data...")
        ptdat <- agg_indi()
        
        # Skip the rest of the process if no rows of data are left
        if (nrow(ptdat) == 0) {
          next
        }
        
        # Delete cw dictionary if ipums already added to it previously
        if (index == 1 & ipums) {
          message('Checking if IPUMS already added to CW data previously')
          original <- try(read.csv('/home/j/WORK/11_geospatial/wash/definitions/cw_sani_2.csv', stringsAsFactors = F),
                          silent = T)
          
          if (class(original) == 'try-error') {
            rm(original)
          }  else {
            if ('ipums' %in% original$data_type) {
              system('rm /home/j/WORK/11_geospatial/wash/definitions/cw_sani_2.csv')
            } 
            rm(original)
          }  
        }
        
        # Write crosswalking dictionary
        message('Output CW files')    
        write_cw_ratio(census = ipums)
        
        #save poly and point collapses
        message("Saving Collapsed Data...")
        today <- gsub("-", "_", Sys.Date())
        
        ptdat$row_id <- c(increment:(nrow(ptdat) - 1 + increment))
        increment <- nrow(ptdat) + increment
        
        if (!ipums) {
          if (data_type == "poly") {
            polydat <- ptdat
            rm(ptdat)
            write_feather(polydat, paste0(root,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_",
                                          indi_fam, '_', conditional, '_', agg_level, '_', today, ".feather"))
          } else{
            write_feather(ptdat, paste0(root,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/ptdat_",
                                        indi_fam, '_', conditional, '_', agg_level, '_', today, ".feather"))
          }
        }
        
        if (ipums) {
          write_feather(ptdat, paste0(root,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/IPUMS/",
                                      indi_fam, '_', conditional, '_', agg_level, '_', today, '_', files[index]))
        }
        
      }
    }
  }
}
