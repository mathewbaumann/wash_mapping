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
# water_outliers <- c(30394, 151568, 22114, 32189, 19557,
#                     235215, 11774, 142934, 20722, 58185,
#                     31831, 280228, 30777, 9439, 56148, 21173, 286788,
#                     19088, 1927, 257045, 206075, 24890, 9522,
#                     24915, 256267, 31797)

#stuff for appending datasets when cant run full dataset
# w_piped1 <- fread('/share/geospatial/mbg/input_data/w_piped.csv')
# w_network_cr1 <- fread('/share/geospatial/mbg/input_data/w_network_cr.csv')
# w_imp_cr1 <- fread('/share/geospatial/mbg/input_data/w_imp_cr.csv')
# w_unimp_cr1 <- fread('/share/geospatial/mbg/input_data/w_unimp_cr.csv')
# 
# write.csv(w_piped1, '/home/j/temp/baumannm/w_piped1.csv')
# write.csv(w_network_cr1, '/home/j/temp/baumannm/w_network_cr1.csv')
# write.csv(w_imp_cr1, '/home/j/temp/baumannm/w_imp_cr1.csv')
# write.csv(w_unimp_cr1, '/home/j/temp/baumannm/w_unimp_cr1.csv')

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
  # remove <- c()
  # for(i in 1:length(imp)){
  #   if(length(colnames(imp[[i]])) != 20){
  #     remove <- c(remove, i)
  #   }
  # }
  #imp <- imp[-remove]
  # imp[[6]]$sum_of_sample_weights <- as.numeric(imp[[6]]$sum_of_sample_weights)
  # imp[[46]]$sum_of_sample_weights <- as.numeric(imp[[46]]$sum_of_sample_weights)
  imp <- do.call(rbind, imp)
  #imp <- filter(imp, !(nid %in% water_outliers))
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/piped/2019-10-24/'))
  piped <- lapply(list.files(), read_convert_weights)
  # remove <- c()
  # for(i in 1:length(piped)){
  #   if(length(colnames(piped[[i]])) != 20){
  #     remove <- c(remove, i)
  #   }
  # }
  #piped <- piped[-remove]
  # piped[[6]]$sum_of_sample_weights <- as.numeric(piped[[6]]$sum_of_sample_weights)
  # piped[[46]]$sum_of_sample_weights <- as.numeric(piped[[46]]$sum_of_sample_weights)
  piped <- do.call(rbind, piped)
  #piped <- filter(piped, !(nid %in% water_outliers))
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/unimp/2019-10-24/'))
  unimp <- lapply(list.files(), read_convert_weights)
  # remove <- c()
  # for(i in 1:length(unimp)){
  #   if(length(colnames(unimp[[i]])) != 20){
  #     remove <- c(remove, i)
  #   }
  # }
  #unimp <- unimp[-remove]
  # unimp[[6]]$sum_of_sample_weights <- as.numeric(unimp[[6]]$sum_of_sample_weights)
  # unimp[[46]]$sum_of_sample_weights <- as.numeric(unimp[[46]]$sum_of_sample_weights)
  unimp <- do.call(rbind, unimp)
  #unimp <- filter(unimp, !(nid %in% water_outliers))
  
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
  
  # w_unimp_cr <- ptdat
  # w_unimp_cr <- select(w_unimp_cr, -surface)
  # w_unimp_cr <- mutate(w_unimp_cr, point = 1, weight = 1, w_unimp_cr = (unimp*N), w_imp = (imp*N), w_piped = (piped*N))
  # w_unimp_cr <- rename(w_unimp_cr, country = iso3, year = year_start, N = N, latitude = lat,
  #                      longitude = long)
  # w_unimp_cr <- mutate(w_unimp_cr, N = ((N)) - (w_imp) - w_piped, prop = w_unimp_cr/N) %>%
  #   select(-imp, -w_imp, -unimp, w_piped, piped) %>%
  #   filter(N > 0)
  
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
  
  #imp <- rename(imp, prop = imp_prop)
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
  # piped <- subset(piped, !nid %in% w_piped1$nid)
  # piped <- rbind(piped, w_piped1, fill = T)
  piped <- format_id(piped, 'w_piped')
  write.csv(piped, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_piped.csv', row.names = FALSE)
  system("rm /share/geospatial/mbg/input_data/w_piped.csv")
  write.csv(piped, '/share/geospatial/mbg/input_data/w_piped.csv', row.names = FALSE)
  rm(piped_pt)
  
  network_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_network_cr.csv',
                         stringsAsFactors = F)
  setnames(network_pt,"year", "surv_year")
  setnames(network_pt,"int_year", "year")
  #network_pt <- select(network_pt, -X)
  #unimp <- select(unimp, -surv_year)
  #network_pt <- select(network_pt, -piped, -network)
  network <- network %>% select(-V1)
  network <- rbind(network, network_pt)
  network <- as.data.table(network)
  network[is.na(year_median), year_median := surv_year]
  # network <- subset(network, !nid %in% w_network_cr1$nid)
  # network <- rbind(network, w_network_cr1, fill = T)
  network <- format_id(network, 'w_network_cr')
  write.csv(network, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_network_cr.csv', row.names = FALSE)
  system("rm /share/geospatial/mbg/input_data/w_network_cr.csv")
  write.csv(network, '/share/geospatial/mbg/input_data/w_network_cr.csv', row.names = FALSE)
  rm(network_pt)
  
  unimp_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_unimp_cr.csv',
                       stringsAsFactors = F)
  setnames(unimp_pt,"year", "surv_year")
  setnames(unimp_pt,"int_year", "year")
  #X <- c('piped','w_piped')
  unimp_pt <- select(unimp_pt, -piped,-w_piped)
  unimp <- select(unimp, -V1)
  unimp <- rbind(unimp, unimp_pt)
  unimp <- as.data.table(unimp)
  unimp[is.na(year_median), year_median := surv_year]
  unimp <- format_id(unimp, 'w_unimp_cr')
  # unimp <- subset(unimp, !nid %in% w_unimp_cr1$nid)
  # unimp <- rbind(unimp, w_unimp_cr1, fill = T)
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
  # imp <- subset(imp, !nid %in% w_imp_cr1$nid)
  # imp <- rbind(imp, w_imp_cr1, fill = T)
  write.csv(imp, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_imp_cr.csv', row.names = FALSE)
  system("rm /share/geospatial/mbg/input_data/w_imp_cr.csv")
  write.csv(imp, '/share/geospatial/mbg/input_data/w_imp_cr.csv', row.names = FALSE)
  rm(imp_pt)
}

indi_fam <- 'sani'
# sani_outliers <- c(214640, 30394, 22114, 21970, 235215,
#                     286657, 11774, 106512, 81004, 142934, 55973,
#                     157065, 285893, 26930, 58185, 31831, 21331,
#                     77395, 280228, 7919, 21393, 77387, 157059,
#                     161662, 218581, 157058, 30777, 283013, 3935, 
#                     34279, 26433, 56241, 1927, 12896, 206075,
#                     2039, 2063, 11516, 11540, 4818)
# s_piped1 <- fread('/share/geospatial/mbg/input_data/s_piped.csv')
# s_network_cr1 <- fread('/share/geospatial/mbg/input_data/s_network_cr.csv')
# s_imp_cr1 <- fread('/share/geospatial/mbg/input_data/s_imp_cr.csv')
# s_unimp_cr1 <- fread('/share/geospatial/mbg/input_data/s_unimp_cr.csv')
# 
# write.csv(s_piped1, '/home/j/temp/baumannm/s_piped1.csv')
# write.csv(s_network_cr1, '/home/j/temp/baumannm/s_network_cr1.csv')
# write.csv(s_imp_cr1, '/home/j/temp/baumannm/s_imp_cr1.csv')
# write.csv(s_unimp_cr1, '/home/j/temp/baumannm/s_unimp_cr1.csv')

if (indi_fam == 'sani' ) {
  exc_nids <- exc$s_include
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/network/2019-10-24/'))
  network <- lapply(list.files(), read_convert_weights)
  # remove <- c()
  # for(i in 1:length(network)){
  #   if(length(colnames(network[[i]])) != 20){
  #     remove <- c(remove, i)
  #   }
  # }
  #network <- network[-remove]
  # network[[47]]$sum_of_sample_weights <- as.numeric(network[[47]]$sum_of_sample_weights)
  network <- do.call(rbind, network)
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/piped/2019-10-24/'))
  piped <- lapply(list.files(), read_convert_weights)
  # remove <- c()
  # for(i in 1:length(piped)){
  #   if(length(colnames(piped[[i]])) != 20){
  #     remove <- c(remove, i)
  #   }
  # }
  #piped <- piped[-remove]
  # piped[[47]]$sum_of_sample_weights <- as.numeric(piped[[47]]$sum_of_sample_weights)
  piped <- do.call(rbind, piped)
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/imp/2019-10-24/'))
  imp <- lapply(list.files(), read_convert_weights)
  # remove <- c()
  # for(i in 1:length(imp)){
  #   if(length(colnames(imp[[i]])) != 20){
  #     remove <- c(remove, i)
  #   }
  # }
  #imp <- imp[-remove]
  # imp[[47]]$sum_of_sample_weights <- as.numeric(imp[[47]]$sum_of_sample_weights)
  imp <- do.call(rbind, imp)
  #imp <- filter(imp, !(nid %in% sani_outliers))
  
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/unimp/2019-10-24/'))
  unimp <- lapply(list.files(),read_convert_weights)
  # remove <- c()
  # for(i in 1:length(unimp)){
  #   if(length(colnames(unimp[[i]])) != 20){
  #     remove <- c(remove, i)
  #   }
  # }
  #unimp <- unimp[-remove]
  # unimp[[47]]$sum_of_sample_weights <- as.numeric(unimp[[47]]$sum_of_sample_weights)
  unimp <- do.call(rbind, unimp)
  #unimp <- filter(unimp, !(nid %in% sani_outliers))
  
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
  # network <- mutate(network, N = s_piped) %>% mutate(network_prop = s_network/N) %>%
  #   rename(s_network_cr = s_network, prop = network_prop) %>%
  #   select(-s_piped) %>%
  #   filter(N > 0)
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
  #unimp <- select(unimp, -surv_year)
  unimp <- unimp %>% select(-V1)
  unimp <- rbind(unimp, unimp_pt)
  unimp <- as.data.table(unimp)
  unimp[is.na(year_median), year_median := surv_year]
  # unimp <- subset(unimp, !nid %in% s_unimp_cr1$nid)
  # unimp <- rbind(unimp, s_unimp_cr1, fill = T)
  unimp <- format_id(unimp, 's_unimp_cr')
  write.csv(unimp, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp_cr.csv', row.names = FALSE)
  system("rm /share/geospatial/mbg/input_data/s_unimp_cr.csv")
  write.csv(unimp, '/share/geospatial/mbg/input_data/s_unimp_cr.csv', row.names = FALSE)
  rm(unimp_pt)
  
  network_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_network_cr.csv',
                         stringsAsFactors = F)
  setnames(network_pt,"year", "surv_year")
  setnames(network_pt,"int_year", "year")
  #unimp <- select(unimp, -surv_year)
  #network_pt <- select(network_pt, -piped, -network)
  network <- network %>% select(-V1)
  network <- rbind(network, network_pt)
  network <- as.data.table(network)
  network[is.na(year_median), year_median := surv_year]
  # network <- subset(network, !nid %in% s_network_cr1$nid)
  # network <- rbind(network, s_network_cr1, fill = T)
  network <- format_id(network, 's_network_cr')
  write.csv(network, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_network_cr.csv', row.names = FALSE)
  system("rm /share/geospatial/mbg/input_data/s_network_cr.csv")
  write.csv(network, '/share/geospatial/mbg/input_data/s_network_cr.csv', row.names = FALSE)
  rm(network_pt)
  
  
  imp_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_imp_cr.csv',
                     stringsAsFactors = F)
  setnames(imp_pt,"year", "surv_year")
  setnames(imp_pt,"int_year", "year")
  #setnames(imp_pt,"s_imp", "s_imp_cr")
  #imp_pt <- select(imp_pt, -piped, -network)
  imp <- imp %>% select(-V1)
  imp <- rbind(imp, imp_pt)
  imp <- as.data.table(imp)
  imp[is.na(year_median), year_median := surv_year]
  # imp <- subset(imp, !nid %in% s_imp_cr1$nid)
  # imp <- rbind(imp, s_imp_cr1, fill = T)
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
  # piped <- subset(piped, !nid %in% s_piped1$nid)
  # piped <- rbind(piped, s_piped1, fill = T)
  piped <- format_id(piped, 's_piped')
  write.csv(piped, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_piped.csv', row.names = FALSE)
  system("rm /share/geospatial/mbg/input_data/s_piped.csv")
  write.csv(piped, '/share/geospatial/mbg/input_data/s_piped.csv', row.names = FALSE)
  rm(piped_pt)
}
