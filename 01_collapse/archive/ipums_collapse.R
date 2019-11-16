#### Set Up Environment ####
# Clear environment
rm(list = ls())

# Define if you are running code loally
local <- T

# Set repo & library path 
if(Sys.info()[1]!="Windows") {
  if(!local) {
    root <- "/home/j/"
    package_lib <- ifelse(grepl("geos", Sys.info()[4]),
                          paste0('/share/code/geospatial/adesh/r_packages'),
                          paste0('/share/code/geospatial/adesh/r_packages_prod'))
    .libPaths(package_lib)
  } else {
    package_lib <- .libPaths()
    root <- '/home/j/'
  }
} else {
  package_lib <- .libPaths()
  root <- 'J:/'
}

repo <- ifelse(Sys.info()[1]=="Windows", 'C:/Users/adesh/Documents/WASH/wash_code/01_collapse/',
               ifelse(local, '/home/adesh/Documents/wash_mapping/01_collapse',
                '/share/code/geospatial/adesh/wash_mapping/01_collapse/'))

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
packages <- c('dplyr', 'feather')
new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, library, character.only = T)

# Set wd to ipums directory and generate vector of ipums file names
setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/IPUMS_feathers')
file_names <- list.files(pattern = '.feather')

# Initialize resutls lists for ipums collapses by pt/poly
poly_list <- list()
pt_list <- list()

# Loop over ipums files and collapse them
for (file in file_names){
  
  # Load Data
  rm(pt_collapse)
  message('Loading Data...')
  setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/IPUMS_feathers')
  pt_collapse <- read_feather(file)

  # Determine data type and appropriately adjust encoding
  if (all(is.na(unique(pt_collapse$lat)))) {data_type <- 'poly'}
  
  if (!("pt_collapse" %in% ls()) & data_type == 'pt') {
    Encoding(pt_collapse$w_source_drink) <- "UTF-8"
    Encoding(pt_collapse$w_source_other) <- "UTF-8"
    Encoding(pt_collapse$t_type) <- "UTF-8"
    pt_collapse$w_source_drink <- tolower(pt_collapse$w_source_drink)
    pt_collapse$w_source_other <- tolower(pt_collapse$w_source_other)
    pt_collapse$t_type <- tolower(pt_collapse$t_type)
  } 
    
  if (!("pt_collapse" %in% ls()) & data_type == 'poly') {
    Encoding(pt_collapse$w_source_drink) <- "UTF-8"
    Encoding(pt_collapse$w_source_other) <- "UTF-8"
    Encoding(pt_collapse$t_type) <- "UTF-8"
    pt_collapse$w_source_drink <- tolower(pt_collapse$w_source_drink)
    pt_collapse$w_source_other <- tolower(pt_collapse$w_source_other)
    pt_collapse$t_type <- tolower(pt_collapse$t_type)
  }

  for (indi_fam in c('sani')) {
    rm(definitions)

    # Load indicator definitions and appropriately format strings
    for (agg_level in c('')) {
      message(paste("Collapsing",indi_fam, "with", agg_level, "agg_level"))
      message('Loading Definitions...')
      if (!("definitions" %in% ls())) {
        if (indi_fam == "sani") {
          definitions <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/t_type_defined_2018_01_24.csv'),
                                  encoding="windows-1252", stringsAsFactors = F)
          definitions <- select(definitions, string, sdg)
        } else {
          definitions <- read.csv(paste0(root,'WORK/11_geospatial/wash/definitions/w_source_defined_2018_01_24.csv'),
                                  encoding="windows-1252", stringsAsFactors = F) 
        }
      }

      definitions <- select(definitions, string, sdg, jmp)

      definitions$string <- iconv(definitions$string, 'windows-1252', 'UTF-8')
      definitions$string <- tolower(definitions$string)
      definitions$sdg <- ifelse(definitions$sdg == "" | is.na(definitions$string),
                                NA, definitions$sdg)
      definitions$string <- ifelse(definitions$sdg == "", NA, definitions$string)
      definitions$sdg <- ifelse(, NA, definitions$sdg)
      if (indi_fam == "water") {
        definitions$jmp <- ifelse(is.na(definitions$string), NA, definitions$jmp)
      }
      definitions <- distinct(definitions)
      
      # Clean up workspace
      rm(list = setdiff(ls(),c('definitions','pt_collapse','definitions2','indi_fam',
        'repo','data_type','root','agg_level', 'sdg', 'pt_list','poly_list')))

      # Import collapse functions
      message("Importing functions...")
      setwd(repo)
      source('functions/initial_cleaning.R')
      source('functions/hh_cw.R')
      source('functions/address_missing.R')
      source('functions/cw_indi.R')
      source('functions/agg_wash.R')
      source('functions/define_wash.R')

      # Remove clusters with more than 20% weighted missingness
      if (indi_fam == 'sani') {
        pt_collapse$t_type <- 'composting toilet'
      } else {
        pt_collapse$w_source_drink <- 'protected source'
        pt_collapse$w_source_other <- 'protected source'
      }

      #### Subset & Shape Data ####
      message("Initial Cleaning...")
      temp_list <- initial_cleaning()
      ptdat <- temp_list[[1]]; ptdat_0 <- temp_list[[2]]; rm(temp_list)


      #### Define Indicator #### # update definitions to use ipums definitions
      message("Defining Indicator...")
      ptdat <- define_indi(sdg_indi = T, ipums = T)

      # Remove cluster_ids with missing hhweight or invalid hhs
      miss_wts <- unique(ptdat$id_short[which(is.na(ptdat$hhweight))])
      ptdat <- filter(ptdat, !(id_short %in% miss_wts))

      invalid_hhs <- unique(ptdat$id_short[which(ptdat$hh_size <= 0)])
      ptdat <- filter(ptdat, !(id_short %in% invalid_hhs))


      #### Aggregate Data ####
      # Bookmarking dataset so it can be looped over for conditional switch
      ptdat_preagg <- ptdat
      
      # Conditional switch is to switch collapsing for conditional vs unconditional indicators
      for (conditional in c('unconditional')) {
        # Reseting the dataset to preagregate
        ptdat <- ptdat_preagg
        message(paste("Conditional variables status:",conditional))
        # Aggregate indicator to cluster level
        message("Aggregating Data...")
        ptdat <- agg_indi()

        # create sdg improved for sdg era
        if(indi_fam == 'water') {ptdat$sdg_imp <- ptdat$piped + ptdat$imp}
        
        #save poly and point collapses
        message("Saving Collapsed Data...")
        today <- gsub("-", "_", Sys.Date())
          
        if (data_type == "poly") {
          poly_list[[length(poly_list)+1]] <- ptdat
        } else{
          pt_list[[length(pt_list)+1]] <- ptdat
        }
      }
    }
  }
  rm(ptdat, ptdat_preagg, ptdat_0, pt_collapse); gc()
}

polydat <- do.call(rbind, poly_list)

polydat$imp <- 1
polydat$unimp <- 0
polydat$od <- 0

write_feather(polydat,
                '/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/IPUMS/feather/sani_polydat_dummy.feather')