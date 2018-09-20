collapse_non_ipums <- function(data = pt_collapse, indi_fam = NULL, survey = NULL, agg_level = '') {
  #Testing with base values
  pt_copy <- filter(pt_collapse, nid == survey)
  data_type <- ifelse(unique(pt_copy$point) == 0, 'poly', 'pt')
  message(paste('Processing:', indi_fam))


  message("Importing functions...")
  setwd('/share/code/geospatial/baumannm/wash_mapping/01_collapse/')
  source('functions/initial_cleaning.R')
  source('functions/hh_cw.R')
  source('functions/address_missing.R')
  source('functions/cw_indi.R')
  source('functions/agg_wash.R')
  source('functions/define_wash.R')
  source('functions/write_cw.R')

  # #### Subset & Shape Data ####
  # message("Initial Cleaning...")
  # temp_list <- initial_cleaning(mydat = pt_copy, census = F, var_fam = indi_fam, dat_type = data_type)
  # ptdat <- temp_list[[1]]; ptdat_0 <- temp_list[[2]]
  # rm(temp_list)
  #
  # #### Define Indicator ####
  # message("Defining Indicator...")
  # ptdat <- define_indi(sdg_indi = T, census = F, mydat = ptdat)
  #
  # #### Address Missingness ####
  # message("Addressing Missingness...")
  #
  # # Remove clusters with more than 20% weighted missingness
  # ptdat <- rm_miss(mydat = ptdat, var_family = indi_fam, agg = agg_level, dt_type = data_type)
  # if (nrow(ptdat) == 0) {
  #   next
  # }

  # Remove cluster_ids with missing hhweight or invalid hhs
  miss_wts <- unique(ptdat$id_short[which(is.na(ptdat$hhweight))])
  ptdat <- filter(ptdat, !(id_short %in% miss_wts))
  ptdat <- filter(ptdat, hhweight != 0)

  invalid_hhs <- unique(ptdat$id_short[which(ptdat$hh_size <= 0)])
  ptdat <- filter(ptdat, !(id_short %in% invalid_hhs))

  if (nrow(ptdat) == 0) {
    next
  }

  # Crosswalk missing household size data
  message("Crosswalking HH Sizes...")
  ptdat <- hh_cw_reg(data = ptdat, dt = data_type)


  # Remove missing observations
  ptdat <- filter(ptdat, !is.na(imp))

  if (nrow(ptdat) == 0) {
    next
  }

  # Conditional switch is to switch collapsing for conditional vs unconditional indicators
  conditional <- 'unconditional'

  # Reseting the dataset to preagregate
  message(paste("Conditional variables status:",conditional))

  # Aggregate indicator to cluster level
  message("Aggregating Data...")
  ptdat <- agg_indi()

  # Skip the rest of the process if no rows of data are left
  if (nrow(ptdat) == 0) {
    next
  }
  # Write crosswalking dictionary
  #message('Output CW files')
  #write_cw_ratio(census = F)

  return(ptdat)
}
