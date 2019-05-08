######################################################
##### HOUSEHOLD SIZE CROSSWALKING CODE ###############

cw <- function(data, debug = F, var_family = indi_fam) {
  
  if (debug) {browser()}
  library(dplyr)
  
  # Remove all missing hh_size obs
  data <- filter(data, !is.na(hh_size))
  
  # Duplicate data into reference and comparison sets with dummy encoding
  data_1 <- data %>% mutate(cw = 1)
  
  data_2 <- data %>% mutate(cw = 0)
  data_2$hh_size <- 1
  data_2$id_short <- data_2$id_short + max(data_1$id_short)
  
  data <- rbind(data_1, data_2)
  
  if (var_family == 'water') {
    data <- rename(data, indi = piped) }
  
  if (var_family == 'sani') {
    data <- rename(data, indi = od) }
  
  if (var_family == 'hw') {
    data <- rename(data, indi = hw_station) }
  
  # Aggregate data into clusters
  data <- data %>% mutate(wt_indi = hhweight*indi*hh_size, wt_denom = hhweight*hh_size) %>% 
    group_by(id_short, cw) %>% 
    summarize(wtavg_indi = sum(wt_indi, na.rm = T)/sum(wt_denom, na.rm = T),
              total_hh = sum(hh_size)) 
  
  # Fit a binomial model and get a ratio estimate for crosswalking missing household sizes
  model <- glm(data = data, formula = total_hh ~ cw, family = poisson,
               weights = data$total_hh)
  ratio <- model$coefficients['cw']
  ratio <- exp(ratio)
  return(ratio)
}

hh_cw <- function(data, debug = F, var_family = indi_fam, reg, dtype) {
  
  if (debug) {browser()}
  
  if (length(data[which(is.na(data$hh_size)),1]) < 1) {
    test <- data.frame()
    print("No missing hh_sizes!")
    return(list(data,test))
  } else {
    
    library(dplyr)
    
    # Split data into urban and rural
    urban <- filter(data, urban == 1)
    rural <- filter(data, urban == 0)
    overall <- data
    
    # Obtain urban-rural specific ratios and overal ratios in case U/R is
    # missing
    u_ratio <- cw(urban)
    r_ratio <- cw(rural)
    o_ratio <- cw(overall)
    
    # Plug in ratios into hh_sizes based on urban-rural specificity
    results <- data.frame(urban = c(1,0,2), ratio = c(u_ratio,r_ratio,o_ratio),
                          region  = reg, data_type = dtype, indi_fam = var_family)
    data$hh_size[which(is.na(data$hh_size) &
                         data$urban == 1)] <- u_ratio
    data$hh_size[which(is.na(data$hh_size) &
                         data$urban == 0)] <- r_ratio
    data$hh_size[which(is.na(data$hh_size) &
                         is.na(data$urban))] <- o_ratio
    
    # Print ratios
    print(results)
    return(list(data, results))
    
  }
}

hh_cw_reg <- function(data, var_family = indi_fam, dt = data_type) {
  
  library(dplyr)
  
  #message('Only African Data is currently CWed by reg')
  message('The regs are sssa_hi, cssa, wsssa, name_hi, and essa_hilo')
  # sssa_hi <- c('NAM','BWA','ZAF')
  # cssa <- c('CAF','GAB','GNQ','COD','COG','AGO','STP')
  # name_hi <- c('MAR','DZA','TUN','LBY','EGY')
  # essa_hilo <- c('SDN','ERI','DJI','SOM','ETH','SSD',
  #                'SSD','UGA','KEN','RWA','BDI','TZA',
  #                'MWI','MOZ','ZMB','MDG','ZWE','SWZ','LSO',
  #                'COM')
  # wssa <- c('CPV','SEN','GMB','GIN','GNB','SLE','MLI','LBR',
  #           'CIV','GHA','TGO','BEN','NGA','NER','TCD','CMR',
  #           'BFA','MRT')
  # africa <- c(sssa_hi, cssa, name_hi, essa_hilo, wssa)
  regions <- read.csv('/home/j/WORK/11_geospatial/diarrhea/01_data/00_identifiers/geoid_stg2.csv')
  regions <- regions[,c('iso3', 'region')]
  regions$iso3 <- as.character(regions$iso3)
  regions$region <- as.character(regions$region)
  regions <- regions[complete.cases(regions),]
  
  region_list <- unique(regions$region)
  results <- list()
  ratios <- list()
  
  region_list <- region_list[region_list != 'excluded']
  i <- 1
  
  for(reg in region_list){
    sub_reg <- subset(regions, region == reg)
    list <- as.vector(sub_reg$iso3)
    assign(reg, list)
    
    
    message(reg)
    mydat <- filter(data, iso3 %in% get(reg))
    if (nrow(mydat)>0) {
      output <- hh_cw(data = mydat, var_family = var_family,
                      reg = reg, dtype = dt)
      results[[i]] <- output[[1]]
      ratios[[i]] <- output[[2]]
      i <- i + 1
    }else{
      message('NOOOOOOOOOO')
    }
  }
  
  
  # message('essa')
  # mydat <- filter(data, iso3 %in% sssa_hi)
  # if (nrow(mydat)>0) {
  #   output <- hh_cw(data = mydat, var_family = var_family,
  #                         reg = 'essa', dtype = dt)
  #   results[[1]] <- output[[1]]
  #   ratios[[1]] <- output[[2]]
  # }
  # 
  # message('afr_horn')
  # mydat <- filter(data, iso3 %in% afr_horn)
  # if (nrow(mydat)>0) {
  #   output <- hh_cw(data = mydat, var_family = var_family,
  #                         reg = 'afr_horn', dtype = dt)
  #   results[[2]] <- output[[1]]
  #   ratios[[2]] <- output[[2]]
  # }
  # 
  # message('cssa')
  # mydat <- filter(data, iso3 %in% cssa)
  # if (nrow(mydat)>0) {
  #   output <- hh_cw(data = mydat, var_family = var_family,
  #                         reg = 'cssa', dtype = dt)
  #   results[[3]] <- output[[1]]
  #   ratios[[3]] <- output[[2]]
  # }
  # 
  # message('essa_hilo')
  # mydat <- filter(data, iso3 %in% essa_hilo)
  # if (nrow(mydat)>0) {
  #   output <- hh_cw(data = mydat, var_family = var_family,
  #                         reg = 'essa_hilo', dtype = dt)
  #   results[[4]] <- output[[1]]
  #   ratios[[4]] <- output[[2]]
  # }
  # 
  # message('name_hi')
  # mydat <- filter(data, iso3 %in% name_hi)
  # if (nrow(mydat)>0) {
  #   output <- hh_cw(data = mydat, var_family = var_family,
  #                         reg = 'name_hi', dtype = dt)
  #   results[[5]] <- output[[1]]
  #   ratios[[5]] <- output[[2]]
  # }
  # 
  # message('non africa')
  # mydat <- filter(data, !(iso3 %in% africa))
  # if (nrow(mydat)>0) {
  #   output <- hh_cw(data = mydat, var_family = var_family,
  #                         reg = 'non africa', dtype = dt)
  #   results[[6]] <- output[[1]]
  #   ratios[[6]] <- output[[2]]
  # }
  message('rbind results')
  results <- do.call(rbind, results)
  message('rbind ratios')
  ratios <- do.call(rbind, ratios)
  ratios$urban <- as.numeric(ratios$urban)
  ratios$ratio <- as.numeric(ratios$ratio)
  
  original <- try(read.csv('/home/j/WORK/11_geospatial/wash/definitions/hh_size_ratios_2.csv'),
                  silent = T)
  
  if (class(original) == 'try-error') {
    rm(original)
  }
  
  if (exists('original')) {
    data_present <- unlist(strsplit(unique(as.character(original$data_type)), ','))
    data_present <- gsub(' ', '', data_present)
    
    indi_present <- unlist(strsplit(unique(as.character(original$indi_fam)), ','))
    indi_present <- gsub(' ', '', indi_present)
    
    original <- select(original, -X)
  } else {
    data_present <- ''
    indi_present <- ''
  }
  
  if ((dt %in% data_present) & (var_family %in% indi_present)) {
    write.csv(ratios, '/home/j/WORK/11_geospatial/wash/definitions/hh_size_ratios_2.csv')
  } else {
    if (data_present != '') {
      ratios <- bind_rows(ratios, original)
      ratios <- filter(ratios, indi_fam == 'sani')
    } else {
      ratios$data_type <- dt
      ratios$indi_fam <- var_family
    }
  }
  message('writing hh ratios')
  write.csv(ratios, '/home/j/WORK/11_geospatial/wash/definitions/hh_size_ratios_2.csv')
  
  return(results)
}

assign_ipums_hh <- function(mydat = ptdat, dt = data_type) {
  current_iso3 <- unique(mydat$iso3)
  
  # sssa_hi <- c('NAM','BWA','ZAF')
  # cssa <- c('CAF','GAB','GNQ','COD','COG','AGO','STP')
  # name_hi <- c('MAR','DZA','TUN','LBY','EGY')
  # essa_hilo <- c('SDN','ERI','DJI','SOM','ETH','SSD',
  #                'SSD','UGA','KEN','RWA','BDI','TZA',
  #                'MWI','MOZ','ZMB','MDG','ZWE','SWZ','LSO',
  #                'COM')
  # wssa <- c('CPV','SEN','GMB','GIN','GNB','SLE','MLI','LBR',
  #           'CIV','GHA','TGO','BEN','NGA','NER','TCD','CMR',
  #           'BFA','MRT')
  # africa <- c(sssa_hi, cssa, name_hi, essa_hilo, wssa)
  # 
  # current_reg <- ifelse(current_iso3 %in% sssa_hi, 'sssa_hi',
  #                       ifelse(current_iso3 %in% cssa, 'cssa',
  #                         ifelse(current_iso3 %in% name_hi, 'name_hi',
  #                           ifelse(current_iso3 %in% essa_hilo, 'essa_hilo',
  #                             ifelse(current_iso3 %in% wssa, 'wssa', 'non africa'
  #                         )))))
  
  regions <- read.csv('/home/j/WORK/11_geospatial/diarrhea/01_data/00_identifiers/geoid_stg2.csv')
  regions <- regions[,c('iso3', 'region')]
  regions$iso3 <- as.character(regions$iso3)
  regions$region <- as.character(regions$region)
  regions <- regions[complete.cases(regions),]
  
  region_list <- unique(regions$region)
  results <- list()
  ratios <- list()
  
  region_list <- region_list[region_list != 'excluded']
  
  for(reg in region_list){
    sub_reg <- subset(regions, region == reg)
    list <- as.vector(sub_reg$iso3)
    assign(reg, list)
    
    if (current_iso3 %in% get(reg)){
      current_reg <- reg
    }
  }
  
  if(!exists('current_reg')){
    message(paste0('DROPPING SURVEY FROM ', current_iso3))
    return(mydat[FALSE,])
  }
  
  ratios <- read.csv('/home/j/WORK/11_geospatial/wash/definitions/hh_size_ratios_2.csv',
                     stringsAsFactors = F)
  ratios <- filter(ratios, region == current_reg & data_type == dt)
  
  mydat$hh_size[which(is.na(mydat$hh_size) &
                        mydat$urban == 1)] <- ratios$ratio[which(ratios$urban == 1)]
  mydat$hh_size[which(is.na(mydat$hh_size) &
                        mydat$urban == 0)] <- ratios$ratio[which(ratios$urban == 2)]
  mydat$hh_size[which(is.na(mydat$hh_size) &
                        is.na(mydat$urban))] <- ratios$ratio[which(ratios$urban == 0)]
  
  return(mydat) 
}