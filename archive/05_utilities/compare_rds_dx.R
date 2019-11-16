rm(list = ls())
indi <- 's_imp'

library(dplyr)
library(ggplot2)
library(tidyr)

run_dates <- c('2018_03_22_20_12_48', '2018_03_22_20_13_18', '2018_03_22_20_13_25')

for (rd in run_dates) {
  setwd(paste0('/share/geospatial/mbg/wash/',indi,'/output/', rd))
  
  draws <- list.files(pattern = 'admin_draws')
  
  for (i in 1:length(draws)) {
    load(draws[i])
    assign(
      gsub('_0.RData', '',
           gsub(paste0(indi, "_unraked_admin_draws_eb_bin0_"), '', draws[i])), 
      admin_0)
    
    assign(
      paste0(gsub('_0.RData', '',
                  gsub(paste0(indi,"_unraked_admin_draws_eb_bin0_"), '',
                       draws[i])), '_key'), 
      sp_hierarchy_list)
    
    rm(sp_hierarchy_list)
  }
  
  regions <- c('cssa','wssa','essa_hilo','sssa_hi', 'name_hi')
  
  # if (indi != 'w_imp') {
  #   regions <- c(regions, 'name_hi')
  # }
  
  gaul_iso3 <- read.csv("/home/j/WORK/11_geospatial/10_mbg/gaul_to_loc_id.csv")
  gaul_iso3 <- dplyr::select(gaul_iso3, GAUL_CODE, ihme_lc_id) %>%
    rename(ADM0_CODE = GAUL_CODE,
           country = ihme_lc_id)
  
  for (reg in regions) {
    ad0 <- get(reg)
    admin0 <- dplyr::select(ad0, year, ADM0_CODE)
    admin0$median <- apply(ad0[,3:252], 1, median)
    admin0$uci <- apply(ad0[,3:252], 1, quantile, 0.975)
    admin0$lci <- apply(ad0[,3:252], 1, quantile, 0.025)
    
    ad0_key <- distinct(dplyr::select(get(paste0(reg, '_key')),
                                      ADM0_CODE, ADM0_NAME))
    ad0_key$ADM0_NAME <- as.character(ad0_key$ADM0_NAME)
    admin0 <- left_join(admin0, ad0_key)
    assign(reg, admin0)
    
    if (rd == run_dates[1]) {
      if (indi %in% c('w_imp','s_imp')) {
        input <- read.csv(list.files(pattern = paste0('input_data_bin0_', reg)),
                          stringsAsFactors = F)
      } 
      
      input <- left_join(input, gaul_iso3)
      input <- left_join(input, ad0_key)
      input <- input %>%
        group_by(nid, year, point, ADM0_NAME, country, ADM0_CODE) %>%
        summarize(prev = weighted.mean(w = weighted_n, x = prop),
                  ss = sum(weighted_n))
      assign(paste0(reg, '_input'), input)  
    }
    
  }
  assign(rd, bind_rows(cssa,wssa,essa_hilo,name_hi,sssa_hi))
  if (rd == run_dates[1]) {
    assign('input', bind_rows(cssa_input,wssa_input,essa_hilo_input,name_hi_input,sssa_hi_input))
  }
}
  
rm(ad0, admin0, admin_0, admin_1, admin_2, ad0_key,
     cssa_input,wssa_input,essa_hilo_input,name_hi_input,sssa_hi_input,
     cssa,wssa,essa_hilo,name_hi,sssa_hi)
  
data_list <- list()
for (i in 1:length(run_dates)) {
  data_list[[i]] <- get(run_dates[i])
}

lines <- list()
ribbons <- list()
for (i in unique(data_list[[1]]$ADM0_NAME)) {
    print(i)
    input2  <- filter(input, ADM0_NAME == i)
    
    gg1 <- ggplot() +
           geom_ribbon(data = filter(data_list[[1]], ADM0_NAME == i), aes(x = year, ymin = lci, ymax = uci, alpha = 0.5, fill = paste0(run_dates[1]))) +
           geom_ribbon(data = filter(data_list[[2]], ADM0_NAME == i), aes(x = year, ymin = lci, ymax = uci, alpha = 0.5, fill = paste0(run_dates[2]))) +
           geom_ribbon(data = filter(data_list[[3]], ADM0_NAME == i), aes(x = year, ymin = lci, ymax = uci, alpha = 0.5, fill = paste0(run_dates[3]))) +
           #geom_ribbon(data = filter(data_list[[4]], ADM0_NAME == i), aes(x = year, ymin = lci, ymax = uci, alpha = 0.5, fill = paste0(run_dates[4]))) +
       
           theme_bw() +
           ggtitle(paste(i, reg, indi)) +
           ylim(0,1)
    
    
    if (nrow(input2) > 0) {
      gg1 <- gg1 + geom_point(data = input2, aes(x = year, y = prev, size = ss,
                                                 col = 'data', shape = as.factor(point)), col = 'black') 
    }
    
    gg2 <- ggplot() +
      geom_line(data = filter(data_list[[1]], ADM0_NAME == i), aes(x = year, y = median, col = paste0(run_dates[1]))) +
      geom_line(data = filter(data_list[[2]], ADM0_NAME == i), aes(x = year, y = median, col = paste0(run_dates[2]))) +
      geom_line(data = filter(data_list[[3]], ADM0_NAME == i), aes(x = year, y = median, col = paste0(run_dates[3]))) +
      #geom_line(data = filter(data_list[[4]], ADM0_NAME == i), aes(x = year, y = median, col = paste0(run_dates[4]))) +
    
      theme_bw() +
      ggtitle(paste(i, reg, indi)) +
      ylim(0,1)
    
    
    if (nrow(input2) > 0) {
      gg2 <- gg2 + geom_point(data = input2, aes(x = year, y = prev, size = ss,
                                                 col = 'data', shape = as.factor(point)), col = 'black') 
    }
  lines[[length(lines)+1]] <- gg2
  ribbons[[length(ribbons)+1]] <- gg1
  }     

pdf(paste0('/homes/adesh/',indi,'_iso3_diag_03.23_v1.pdf'))
for (i in 1:length(lines)) {
  print(lines[[i]])
  print(ribbons[[i]])  
}
dev.off()

  