#### Set Up Environment ####
# Clear environment
rm(list = ls())

# Define if you are running code loally
local <- F

files <- file.info(grep('IPUMS', list.files(paste0('/ihme/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash'), pattern = '.feather', full.names=TRUE), invert = T, value = T ))
files <- files[with(files, order(as.POSIXct(ctime), decreasing = TRUE)), ]
latest_postextraction <- unlist(strsplit(rownames(files)[1], "/"))
latest_postextraction <- latest_postextraction[length(latest_postextraction)]
input_version <- gsub('poly8_', '', latest_postextraction)

root <- "/home/j/"
l <- '/ihme/limited_use/'

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

#create row_ids
increment <- 1

#### Load functions ####
for (file_type in c('pt','poly','ipums')){
  message(paste("Loading",file_type, "data"))
  rm(pt_collapse)
  message('Loading Data...')
  # Load data
  if (!("pt_collapse" %in% ls()) & file_type == 'pt') {
    pt_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/points_', input_version)))
    pt_collapse$w_source_drink <- tolower(pt_collapse$w_source_drink)
    pt_collapse$w_source_other <- tolower(pt_collapse$w_source_other)
    pt_collapse$t_type <- tolower(pt_collapse$t_type)
    pt_collapse$sewage <- tolower(pt_collapse$sewage)
    data_type <- 'pt'
  } 
  
  if (!("pt_collapse" %in% ls()) & file_type == 'poly') {
    #dataset too large for one feather
    pt1_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly1_', input_version)))
    pt2_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly2_', input_version, '.feather')))
    pt3_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly3_', input_version, '.feather')))
    pt4_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly4_', input_version, '.feather')))
    pt5_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly5_', input_version, '.feather')))
    pt6_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly6_', input_version, '.feather')))
    pt7_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly7_', input_version, '.feather')))
    pt8_collapse <- as.data.table(read_feather(paste0(l,'LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly8_', input_version, '.feather')))
    pt_collapse <- rbindlist(list(pt1_collapse, pt2_collapse, pt3_collapse,
                                  pt4_collapse, pt5_collapse, pt6_collapse,
                                  pt7_collapse, pt8_collapse), fill = TRUE)
    rm(pt1_collapse, pt2_collapse, pt3_collapse,
            pt4_collapse, pt5_collapse, pt6_collapse,
            pt7_collapse, pt8_collapse)

    pt_collapse$w_source_drink <- tolower(pt_collapse$w_source_drink)
    pt_collapse$w_source_other <- tolower(pt_collapse$w_source_other)
    
    #hot fix for one string
    pt_collapse[nid %in% c(14063, 14027) & t_type %like% '/ANOTHER HOUSEHOLD', t_type := "neighbour's/another household's pit latrine"]
    pt_collapse$t_type <- tolower(pt_collapse$t_type)
    pt_collapse$sewage <- tolower(pt_collapse$sewage)
    data_type <- 'poly'
  }
  
  if(file_type != 'ipums'){
    #concatenate t_type and sewage, get rid of extra whitespace
    #ipums is seperated by t_type and sewage, we should swap to 
    #this method but string matching is already done for this way
    pt_collapse[!is.na(sewage) & !is.na(t_type), t_type := paste0(t_type, ' ', sewage)]
    pt_collapse[is.na(t_type) & !is.na(sewage), t_type := sewage]
    pt_collapse[,t_type := trimws(t_t_type)]
    pt_collapse[,w_source_drink := trimws(w_source_drink)]
  }
  
  if (file_type == 'ipums') {
    ipums_dir <- '/ihme/limited_use/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/IPUMS_feathers'
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
      indicators <- c('water','sani')
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
            definitions <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/IPUMS_sani_defs_2019_09_17.csv'),
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
              definitions <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/t_type_defined_by_nid_2019_12_05.csv'),
                                      encoding="windows-1252", stringsAsFactors = F)
              definitions$iso3 <- substr(definitions$iso3, 1, 3)
              definitions <- unique(definitions)
              definitions$t_type <- tolower(definitions$t_type)
              definitions$sdg <- ifelse(definitions$sdg == "" | is.na(definitions$sdg),
                                        NA, definitions$sdg)
              definitions$t_type <- ifelse(definitions$t_type == "" | is.na(definitions$t_type),
                                           NA, definitions$t_type)
            } else {
              definitions <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/w_source_defined_by_nid_2019_12_05.csv'),
                                      encoding="windows-1252", stringsAsFactors = F) 
              definitions$iso3 <- substr(definitions$iso3, 1, 3)
              definitions <- unique(definitions)
              definitions$w_source_drink <- tolower(definitions$w_source_drink)
              definitions$sdg <- ifelse(definitions$sdg == "" | is.na(definitions$sdg),
                                        NA, definitions$sdg)
              definitions$w_source_drink <- ifelse(definitions$w_source_drink == "" | is.na(definitions$w_source_drink),
                                           NA, definitions$w_source_drink)
            }
          }
          definitions <- distinct(definitions)
        }
        
        rm(list = setdiff(ls(),c('definitions','pt_collapse','definitions2','indi_fam',
                                 'repo','data_type','root', 'l','agg_level', 'sdg', 'ipums', 'files', 'index', 'files_length',
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
        ptdat <- temp_list[[1]]
        ptdat_0 <- temp_list[[2]]
        rm(temp_list)
        
        #### Define Indicator ####
        message("Defining Indicator...")
        ptdat <- define_indi(sdg_indi = T, census = ipums)
        
        #### Address Missingness ####
        message("Addressing Missingness...")
        
        #Remove clusters with more than 20% indicator missingness
        ptdat <- data.table(rm_miss())
        if (nrow(ptdat) == 0) {
          next
        }
        
        # Remove cluster_ids with missing hhweight
        #ID cluster_ids with missing hhweight
        #decided to use 10% unweighted criteria instead of 0 tolerance
        missing.wts <- idMissing(ptdat, this.var="hhweight", criteria=.1, wt.var=NA)
        #ID points with hhweight<0 (invalid)
        #drop clusters with more than 10% invalid, then drop invalid rows 
        invalid.wts <- idMissing(ptdat, this.var="hhweight", criteria=.1, wt.var=NA, check.threshold = T, threshold=0)
        remove.clusters <- c(missing.wts,
                             invalid.wts) %>% unique
        #remove these clusters and proceed
        message('dropping ', length(remove.clusters), 
                ' clusters based on variable missingness/invalidity above cluster-level criteria thresholds')
        ptdat <- ptdat[!(id_short %in% remove.clusters)]
        
        
        ptdat <- as.data.table(ptdat)
        #crosswalk household sizes of 0
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
            write_feather(polydat, paste0(l,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_",
                                          indi_fam, '_', conditional, '_', agg_level, '_', today, ".feather"))
          } else{
            write_feather(ptdat, paste0(l,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/ptdat_",
                                        indi_fam, '_', conditional, '_', agg_level, '_', today, ".feather"))
          }
        }

        if (ipums) {
          dir.create(paste0(l,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/IPUMS/feather/", today), showWarnings = F)
          write_feather(ptdat, paste0(l,"LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/IPUMS/feather/", today, '/',
                                      indi_fam, '_', conditional, '_', agg_level, '_', today, '_', files[index]))
        }
        
      }
    }
  }
}
