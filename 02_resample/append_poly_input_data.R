library(dplyr)

rm(list = ls())
indi_fam <- 'water'
water_outliers <- c(30394, 151568, 22114, 32189, 19557,
                    235215, 11774, 142934, 20722, 58185,
                    31831, 280228, 30777, 9439, 56148, 21173, 286788,
                    19088, 1927, 257045, 206075, 24890, 9522,
                    24915, 256267, 31797)


if (indi_fam == 'water' ) {
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/imp/2018-09-07/'))
  imp <- lapply(list.files(), read.csv, stringsAsFactors = F)
  imp <- do.call(rbind, imp)
  imp <- filter(imp, !(nid %in% water_outliers))

  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/piped/2018-09-07/'))
  piped <- lapply(list.files(), read.csv, stringsAsFactors = F)
  piped <- do.call(rbind, piped)
  piped <- filter(piped, !(nid %in% water_outliers))

  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/water/unimp/2018-09-07/'))
  unimp <- lapply(list.files(), read.csv, stringsAsFactors = F)
  unimp <- do.call(rbind, unimp)
  unimp <- filter(unimp, !(nid %in% water_outliers))

  for (i in c('piped','imp', 'unimp')) {  
    mydat <- get(i)
    
    names(mydat)[which(names(mydat) == i)] <- 'indi'
    
    mydat <- mydat %>% select(-X, -lat.y, -long.y) %>%
      rename(latitude = lat.x, longitude = long.x,
             year = year_start,
             country = iso3) %>% 
      mutate(indi_bin = round(indi*N)) %>%
      mutate(N = round(N)) %>%
      rename(indi_prop = indi)
    
    names(mydat)[which(names(mydat) == 'indi_bin')] <- paste0('w_',i)
    names(mydat)[which(names(mydat) == 'indi_prop')] <- paste0(i, '_prop')
    
    assign(i,mydat)
  }
  
  imp_denom <- select(imp, w_imp, shapefile, location_code, nid, year)
  imp_denom <- distinct(imp_denom)
  
  unimp <- left_join(unimp, imp_denom, by = c('shapefile','location_code','nid','year'))
  unimp <- mutate(unimp, N = N - w_imp) %>% mutate(unimp_prop = w_unimp/N) %>%
    rename(w_unimp_cr = w_unimp, prop = unimp_prop) %>%
    select(-w_imp) %>%
    filter(N > 0)
  
  piped <- left_join(piped, imp_denom, by = c('shapefile','location_code','nid','year'))
  piped <- mutate(piped, N = w_imp) %>% mutate(piped_prop = w_piped/N) %>%
             rename(w_piped_cr = w_piped, prop = piped_prop) %>%
             select(-w_imp) %>%
             filter(N > 0)
  
  imp <- rename(imp, prop = imp_prop)
  rm(mydat, imp_denom)
  
  piped_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_piped_cr.csv',
                       stringsAsFactors = F)
  piped_pt <- select(piped_pt, -X)
  piped <- rbind(piped, piped_pt)
  write.csv(piped, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_piped_cr.csv')
  rm(piped_pt)
  
  unimp_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_unimp_cr.csv',
                       stringsAsFactors = F)
  unimp_pt <- select(unimp_pt, -X)
  unimp <- rbind(unimp, unimp_pt)
  write.csv(unimp, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_unimp_cr.csv')
  rm(unimp_pt)
  
  imp_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_imp.csv',
                     stringsAsFactors = F)
  imp_pt <- select(imp_pt, -X)
  imp <- rbind(imp, imp_pt)
  write.csv(imp, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_imp.csv')
  rm(imp_pt)
}

rm(list = ls())
indi_fam <- 'sani'
sani_outliers <- c(214640, 30394, 22114, 21970, 235215,
                    286657, 11774, 106512, 81004, 142934, 55973,
                    157065, 285893, 26930, 58185, 31831, 21331,
                    77395, 280228, 7919, 21393, 77387, 157059,
                    161662, 218581, 157058, 30777, 283013, 3935, 
                    34279, 26433, 56241, 1927, 12896, 206075,
                    2039, 2063, 11516, 11540, 4818)

if (indi_fam == 'sani' ) {
  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/imp/2018-09-07/'))
  imp <- lapply(list.files(), read.csv, stringsAsFactors = F)
  imp <- do.call(rbind, imp)
  imp <- filter(imp, !(nid %in% sani_outliers))

  setwd(paste0('/home/j/WORK/11_geospatial/wash/data/resamp/sani/unimp/2018-09-07/'))
  unimp <- lapply(list.files(), read.csv, stringsAsFactors = F)
  unimp <- do.call(rbind, unimp)
  unimp <- filter(unimp, !(nid %in% sani_outliers))

  for (i in c('imp', 'unimp')) {  
    mydat <- get(i)
    
    names(mydat)[which(names(mydat) == i)] <- 'indi'
    
    mydat <- mydat %>% select(-X, -lat.y, -long.y) %>%
      rename(latitude = lat.x, longitude = long.x,
             year = year_start,
             country = iso3) %>% 
      mutate(indi_bin = round(indi*N)) %>%
      mutate(N = round(N)) %>%
      rename(indi_prop = indi)
    
    names(mydat)[which(names(mydat) == 'indi_bin')] <- paste0('s_',i)
    names(mydat)[which(names(mydat) == 'indi_prop')] <- paste0(i, '_prop')
    
    assign(i,mydat)
  }
  
  imp_denom <- select(imp, s_imp, shapefile, location_code, nid, year)
  imp_denom <- distinct(imp_denom)
  
  unimp <- left_join(unimp, imp_denom, by = c('shapefile','location_code','nid','year'))
  unimp <- mutate(unimp, N = N - s_imp) %>% mutate(unimp_prop = s_unimp/N) %>%
    rename(s_unimp_cr = s_unimp, prop = unimp_prop) %>%
    select(-s_imp) %>%
    filter(N > 0)
  
  imp <- rename(imp, prop = imp_prop)
  rm(mydat, imp_denom)
  
  unimp_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp_cr.csv',
                       stringsAsFactors = F)
  unimp_pt <- select(unimp_pt, -X)
  unimp <- rbind(unimp, unimp_pt)
  write.csv(unimp, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp_cr.csv')
  rm(unimp_pt)
  
  imp_pt <- read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_imp.csv',
                     stringsAsFactors = F)
  imp_pt <- select(imp_pt, -X)
  imp <- rbind(imp, imp_pt)
  write.csv(imp, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_imp.csv')
  rm(imp_pt)
}
