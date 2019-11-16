detachAllPackages <- function() {
  basic.packages <- c("package:stats","package:graphics","package:grDevices",
                      "package:utils","package:datasets","package:methods",
                      "package:base", "package:rio")
  package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
  package.list <- setdiff(package.list,basic.packages)
  if (length(package.list)>0)  for (package in package.list) 
    detach(package, character.only=TRUE)
}
detachAllPackages()

library(dplyr)
library(data.table)

rm(list = ls())
indi_fam <- 'water'

#for some reason this column is a factor?
read_convert_weights <- function(x){
  temp <- fread(x, stringsAsFactors = F)
  temp$sum_of_sample_weights <- as.numeric(temp$sum_of_sample_weights)
  return(temp)
}

exc <- fread('/snfs1/WORK/11_geospatial/wash/stg2_pub/data/2019-10-28/data_tracking/nids_included.csv')

format_id <- function(df, indicator) {
  df <- as.data.frame(df)
  remove_col <- grep('V', names(df))
  df <- df[,setdiff(names(df), names(df)[remove_col])]
  df <- df %>%
    group_by(nid) %>% 
    mutate(sum_of_sample_weights = ifelse(is.na(sum_of_sample_weights), weight * N, sum_of_sample_weights)) %>%
    mutate(year = weighted.mean(x = year, w = sum_of_sample_weights)) %>%
    ungroup() 
  df$N <- round(df$N, digits = 7)
  df[,indicator] <- round(df[,indicator], digits = 7)
  df <- filter(df, N > 0)
  df <- filter(df, year > 1999)
  df <- filter(df, !is.na(latitude), !is.na(longitude))
  df$year <- round(df$year)
  
  # recalculate prop
  df$prop <- as.numeric(df[[indicator]])/as.numeric(df$N)
  
  df <- subset(df, nid %in% exc_nids)
  df <- as.data.table(df)
  return(df)
}

if (indi_fam == 'water' ) {
  exc_nids <- exc$w_include
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/network/2019-10-24/'))
  network <- lapply(list.files(), read_convert_weights)
  network <- do.call(rbind, network)
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/imp/2019-10-24/'))
  imp <- lapply(list.files(), read_convert_weights)
  imp <- do.call(rbind, imp)
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/piped/2019-10-24/'))
  piped <- lapply(list.files(), read_convert_weights)
  piped <- do.call(rbind, piped)
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/unimp/2019-10-24/'))
  unimp <- lapply(list.files(), read_convert_weights)
  unimp <- do.call(rbind, unimp)
  
  for (i in c('network','piped','imp', 'unimp')) {  
    mydat <- get(i)
    
    names(mydat)[which(names(mydat) == i)] <- 'indi'
    
    mydat <- mydat %>% dplyr::select(-lat.y, -long.y) %>%
      rename(latitude = lat.x, longitude = long.x,
             year = year_start,
             country = iso3) %>% 
      mutate(indi_bin = indi*N) %>%
      rename(indi_prop = indi)
    
    names(mydat)[which(names(mydat) == 'indi_bin')] <- paste0('w_',i)
    names(mydat)[which(names(mydat) == 'indi_prop')] <- paste0(i, '_prop')
    assign(i,mydat)
  }
  
  imp_denom <- dplyr::select(imp, w_imp, shapefile, location_code, nid, year)
  imp_denom <- distinct(imp_denom)
  piped_denom <- dplyr::select(piped, w_piped, shapefile, location_code, nid, year)
  piped_denom <- distinct(piped_denom)
  
  unimp <- left_join(unimp, imp_denom, by = c('shapefile','location_code','nid','year'))
  unimp <- left_join(unimp, piped_denom, by = c('shapefile','location_code','nid','year'))
  
  network <- left_join(network, piped_denom, by = c('shapefile','location_code','nid','year'))
  network <- mutate(network, N = w_piped) %>%
    rename(w_network_cr = network_prop, prop = network_prop) %>%
    mutate(w_network_cr = prop * N) %>%
    select(-w_piped, -w_network) %>%
    filter(N > 0)
  
  unimp <- mutate(unimp, N = ((N)) - (w_imp) - w_piped) %>% mutate(unimp_prop = w_unimp/N) %>%
    rename(prop = unimp_prop, w_unimp_cr = w_unimp) %>%
    dplyr::select(-w_imp, -w_piped) %>%
    filter(N > 0)
  
  piped <- left_join(piped, imp_denom, by = c('shapefile','location_code','nid','year'))
  piped <- mutate(piped, piped_prop = w_piped/N) %>%
    rename(prop = piped_prop) %>%
    dplyr::select(-w_imp) %>%
    filter(N > 0)
  
  imp <- left_join(imp, piped_denom, by = c('shapefile','location_code','nid','year'))
  
  imp <- mutate(imp, N = ((N)) - w_piped) %>% mutate(imp_prop = w_imp/N) %>%
    rename(prop = imp_prop, w_imp_cr = w_imp) %>%
    dplyr::select(-w_piped) %>%
    filter(N > 0)
  rm(mydat, imp_denom, piped_denom)
  
  piped_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_piped.csv',
                       stringsAsFactors = F)
  setnames(piped_pt,"year", "surv_year")
  setnames(piped_pt,"int_year", "year")
  piped_pt <- dplyr::select(piped_pt, -imp)
  piped <- dplyr::select(piped, -V1)
  piped <- rbind(piped, piped_pt)
  piped <- as.data.table(piped)
  piped[is.na(year_median), year_median := surv_year]
  piped <- format_id(piped, 'w_piped')
  write.csv(piped, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_piped.csv', row.names = FALSE)
  system("rm /share/geospatial/mbg/input_data/w_piped.csv")
  write.csv(piped, '/share/geospatial/mbg/input_data/w_piped.csv', row.names = FALSE)
  rm(piped_pt)
  
  network_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_network_cr.csv',
                         stringsAsFactors = F)
  setnames(network_pt,"year", "surv_year")
  setnames(network_pt,"int_year", "year")

  network <- network %>% select(-V1)
  network <- rbind(network, network_pt)
  network <- as.data.table(network)
  network[is.na(year_median), year_median := surv_year]
  network <- format_id(network, 'w_network_cr')
  write.csv(network, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_network_cr.csv', row.names = FALSE)
  system("rm /share/geospatial/mbg/input_data/w_network_cr.csv")
  write.csv(network, '/share/geospatial/mbg/input_data/w_network_cr.csv', row.names = FALSE)
  rm(network_pt)
  
  unimp_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_unimp_cr.csv',
                       stringsAsFactors = F)
  setnames(unimp_pt,"year", "surv_year")
  setnames(unimp_pt,"int_year", "year")
  unimp_pt <- select(unimp_pt, -piped,-w_piped)
  unimp <- select(unimp, -V1)
  unimp <- rbind(unimp, unimp_pt)
  unimp <- as.data.table(unimp)
  unimp[is.na(year_median), year_median := surv_year]
  unimp <- format_id(unimp, 'w_unimp_cr')
  write.csv(unimp, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_unimp_cr.csv', row.names = FALSE)
  system("rm /share/geospatial/mbg/input_data/w_unimp_cr.csv")
  write.csv(unimp, '/share/geospatial/mbg/input_data/w_unimp_cr.csv', row.names = FALSE)
  rm(unimp_pt)
  
  imp_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_imp_cr.csv',
                     stringsAsFactors = F)
  setnames(imp_pt,"year", "surv_year")
  setnames(imp_pt,"int_year", "year")
  imp <- select(imp, -V1)
  imp <- rbind(imp, imp_pt)
  imp <- as.data.table(imp)
  imp[is.na(year_median), year_median := surv_year]
  imp <- format_id(imp, 'w_imp_cr')
  write.csv(imp, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_imp_cr.csv', row.names = FALSE)
  system("rm /share/geospatial/mbg/input_data/w_imp_cr.csv")
  write.csv(imp, '/share/geospatial/mbg/input_data/w_imp_cr.csv', row.names = FALSE)
  rm(imp_pt)
}

indi_fam <- 'sani'
if (indi_fam == 'sani' ) {
  exc_nids <- exc$s_include
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/network/2019-10-24/'))
  network <- lapply(list.files(), read_convert_weights)
  network <- do.call(rbind, network)
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/piped/2019-10-24/'))
  piped <- lapply(list.files(), read_convert_weights)
  piped <- do.call(rbind, piped)
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/imp/2019-10-24/'))
  imp <- lapply(list.files(), read_convert_weights)
  imp <- do.call(rbind, imp)
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/unimp/2019-10-24/'))
  unimp <- lapply(list.files(),read_convert_weights)
  unimp <- do.call(rbind, unimp)
  
  for (i in c('network','piped', 'imp', 'unimp')) {  
    mydat <- get(i)
    
    names(mydat)[which(names(mydat) == i)] <- 'indi'
    
    mydat <- mydat %>% select(-lat.y, -long.y) %>%
      rename(latitude = lat.x, longitude = long.x,
             year = year_start,
             country = iso3) %>% 
      mutate(indi_bin = indi*N) %>%
      rename(indi_prop = indi)
    
    names(mydat)[which(names(mydat) == 'indi_bin')] <- paste0('s_',i)
    names(mydat)[which(names(mydat) == 'indi_prop')] <- paste0(i, '_prop')
    
    assign(i,mydat)
  }
  
  imp_denom <- select(imp, s_imp, shapefile, location_code, nid, year)
  imp_denom <- distinct(imp_denom)
  piped_denom <- select(piped, s_piped, shapefile, location_code, nid, year)
  piped_denom <- distinct(piped_denom)
  
  
  imp <- left_join(imp, piped_denom, by = c('shapefile','location_code','nid','year'))
  imp <- mutate(imp, N = N - s_piped) %>% mutate(imp_prop = s_imp/N) %>%
    rename(s_imp_cr = s_imp, prop = imp_prop) %>%
    select(-s_piped) %>%
    filter(N > 0)
  
  unimp <- left_join(unimp, imp_denom, by = c('shapefile','location_code','nid','year'))
  unimp <- left_join(unimp, piped_denom, by = c('shapefile','location_code','nid','year'))
  unimp <- mutate(unimp, N = N - s_imp - s_piped) %>% mutate(unimp_prop = s_unimp/N) %>%
    rename(s_unimp_cr = s_unimp, prop = unimp_prop) %>%
    select(-s_imp, -s_piped) %>%
    filter(N > 0)
  
  network <- left_join(network, piped_denom, by = c('shapefile','location_code','nid','year'))
  network <- mutate(network, N = s_piped) %>%
    rename(s_network_cr = network_prop, prop = network_prop) %>%
    mutate(s_network_cr = prop * N) %>%
    select(-s_piped, -s_network) %>%
    filter(N > 0)
  
  piped <- rename(piped, prop = piped_prop)
  rm(mydat, imp_denom, piped_denom)
  
  unimp_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp_cr.csv',
                       stringsAsFactors = F)
  setnames(unimp_pt,"year", "surv_year")
  setnames(unimp_pt,"int_year", "year")
  unimp_pt <- select(unimp_pt, -piped, -network)
  unimp <- unimp %>% select(-V1)
  unimp <- rbind(unimp, unimp_pt)
  unimp <- as.data.table(unimp)
  unimp[is.na(year_median), year_median := surv_year]
  unimp <- format_id(unimp, 's_unimp_cr')
  write.csv(unimp, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp_cr.csv', row.names = FALSE)
  system("rm /share/geospatial/mbg/input_data/s_unimp_cr.csv")
  write.csv(unimp, '/share/geospatial/mbg/input_data/s_unimp_cr.csv', row.names = FALSE)
  rm(unimp_pt)
  
  network_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_network_cr.csv',
                         stringsAsFactors = F)
  setnames(network_pt,"year", "surv_year")
  setnames(network_pt,"int_year", "year")
  network <- network %>% select(-V1)
  network <- rbind(network, network_pt)
  network <- as.data.table(network)
  network[is.na(year_median), year_median := surv_year]
  network <- format_id(network, 's_network_cr')
  write.csv(network, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_network_cr.csv', row.names = FALSE)
  system("rm /share/geospatial/mbg/input_data/s_network_cr.csv")
  write.csv(network, '/share/geospatial/mbg/input_data/s_network_cr.csv', row.names = FALSE)
  rm(network_pt)
  
  
  imp_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_imp_cr.csv',
                     stringsAsFactors = F)
  setnames(imp_pt,"year", "surv_year")
  setnames(imp_pt,"int_year", "year")
  imp <- imp %>% select(-V1)
  imp <- rbind(imp, imp_pt)
  imp <- as.data.table(imp)
  imp[is.na(year_median), year_median := surv_year]
  imp <- format_id(imp, 's_imp_cr')
  write.csv(imp, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_imp_cr.csv', row.names = FALSE)
  system("rm /share/geospatial/mbg/input_data/s_imp_cr.csv")
  write.csv(imp, '/share/geospatial/mbg/input_data/s_imp_cr.csv', row.names = FALSE)
  rm(imp_pt)
  
  piped_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_piped.csv',
                       stringsAsFactors = F)
  setnames(piped_pt,"year", "surv_year")
  setnames(piped_pt,"int_year", "year")
  piped_pt <- select(piped_pt, -network)
  piped <- select(piped, -V1)
  piped <- rbind(piped, piped_pt)
  piped <- as.data.table(piped)
  piped[is.na(year_median), year_median := surv_year]
  piped <- format_id(piped, 's_piped')
  write.csv(piped, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_piped.csv', row.names = FALSE)
  system("rm /share/geospatial/mbg/input_data/s_piped.csv")
  write.csv(piped, '/share/geospatial/mbg/input_data/s_piped.csv', row.names = FALSE)
  rm(piped_pt)
}
