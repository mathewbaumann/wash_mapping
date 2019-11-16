library(tidyverse)
library(raster)
library(tmap)
# Define gaul list
gaul_list <- c(195)

# Read in iso to gaul converters
iso_gaul <- read_csv('J:/WORK/11_geospatial/pandemic_indicator/data/raw/geographies/id_convert_ammended.csv')
iso_list <- iso_gaul$ISO3[which(iso_gaul$GAUL %in% gaul_list)]

# Read in country level estimates
poly_cntry <- read_csv('J:/WORK/11_geospatial/wash/data/agg/water_poly_agg_cntry_2017-07-12.csv')
pt_cntry <- read_csv('J:/WORK/11_geospatial/wash/data/agg/water_pt_agg_cntry_2017-07-12.csv')
cntry_dat <- rbind(poly_cntry, pt_cntry)
cntry_dat[which(cntry_dat$iso3 %in% c('KEN_44798', 'KEN_35619',
                                      'KEN_35659', 'KEN_35672')),] <- 'KEN'

cntry_dat[which(cntry_dat$iso3 %in% c('IDN_4741', 'IDN_4742')),] <- 'IDN'
cntry_dat_reg <- filter(cntry_dat, iso3 %in% iso_list)
cntry_dat_reg <- rename(cntry_dat_reg, cntry_est = piped)
cntry_dat_reg$cntry_est <- as.numeric(cntry_dat_reg$cntry_est)
cntry_dat_reg$year_start <- as.numeric(cntry_dat_reg$year_start)



## Read in shapefiles
# Africa country shapefile
ad0 <- shapefile('J:/WORK/11_geospatial/09_MBG_maps/misc_files/africa_ad0.shp')
ad0_reg <- ad0[which(ad0$ADM0_CODE %in% gaul_list),]

# Population raster
pop_00 <- raster('J:/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 1)
pop_05 <- raster('J:/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 2)
pop_10 <- raster('J:/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 3)
pop_15 <- raster('J:/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 4)
pop_list <- list(pop_00, 
                 pop_05, pop_05, pop_05, pop_05, pop_05,
                 pop_10, pop_10, pop_10, pop_10, pop_10, 
                 pop_15, pop_15, pop_15, pop_15, pop_15)

# Read in mbg input data
input_data <- read_csv('J:/WORK/11_geospatial/10_mbg/input_data/w_piped.csv', col_types = 'dddcddcdccdddddd')
input_data <- rename(input_data, MBG_raw = prop)
id_reg <- filter(input_data, country %in% iso_list)
id_reg$year <- as.numeric(id_reg$year)
id_reg <- rename(id_reg, MBG_raw_dat = N)

# Read in mbg results
mbg_results <- brick('J:/WORK/11_geospatial/wash/results/water/2017_08_07_12_34_56/w_piped_prediction_eb_bin0_195_0.grd')

# Read in mbg covariates
load('J:/WORK/11_geospatial/wash/results/water/2017_08_07_12_34_56/2017_08_07_12_34_56_bin0_195_0.RData')

## Process mbg results & covariates
# Aggregate population
zonal_pop <- list()
for (i in 1:4) {
  zonal_pop[[i]] <- extract(pop_list[[i]], ad0_reg, fun = sum, df = T, na.rm = T)[,2]
}

# Aggregate mbg results
mbg_agg <- list()
for (i in 1:16) {
  message(i)
  
  num1 <- mbg_results[[i]]*pop_list[[i]]
  num2 <- extract(num1, ad0_reg, fun = sum, df = T, na.rm = T)[,2]
  
  if (i == 1) {
    mod <- zonal_pop[[1]]    
  }
  if (i > 1 & i < 7) {
    mod <- zonal_pop[[2]]    
  }
  if (i > 6 & i < 12) {
    mod <- zonal_pop[[3]]    
  }
  if (i > 11) {
    mod <- zonal_pop[[4]]
  }
  
  est <- (num2)/mod
  country <- 
  year <- rep(2000 + (i-1), length())
  mbg_agg[[i]] <- data_frame(year = year, mbg = est, country = country)
}
mbg_agg_plot <- do.call(rbind, mbg_agg)
mbg_agg_plot <- rename(mbg_agg_plot, GAUL = country)
mbg_agg_plot <- left_join(mbg_agg_plot, iso_gaul, by = 'GAUL')

# Aggregate covariates
cov_list_agg <- list()
for (j in 1:5) {
  cov <- cov_list[[j]]
  cov_agg <- list()
  for (i in 1:dim(cov)[3]) {
    message(paste('layer', i, 'out of' ,dim(cov)[3], ';',
                  j,'covariate out of',length(cov_list)))
    
    num1 <- cov[[i]]*pop_list[[i]]
    num2 <- extract(num1, ad0_reg, fun = sum, df = T, na.rm = T)[,2]
  
    if (i == 1) {
      mod <- zonal_pop[[1]]    
    }
  
    if (i > 1 & i < 7) {
      mod <- zonal_pop[[2]]    
    }
  
    if (i > 6 & i < 12) {
    mod <- zonal_pop[[3]]    
    }
  
    if (i > 11) {
      mod <- zonal_pop[[4]]
    }
  
    est <- (num2)/mod
    country <- 
    year <- rep(2000 + (i-1), length())
    cov_agg[[i]] <- data_frame(year = year, mbg = est, country = country)
  }
  cov_list_agg[[j]] <- do.call(rbind, cov_agg)
  names(cov_list_agg)[j] <- names(cov_list)[j]
}

 stackers_list <- cov_list_agg
# Plot
for (i in 1:5) {
  stackers_list[[i]] <- rename(stackers_list[[i]], GAUL = country)
  stackers_list[[i]] <- left_join(stackers_list[[i]], iso_gaul, by = 'GAUL')
  
}

pdf('peru_run1_no_input.pdf')
for (i in iso_list) {

  id_reg2 <- filter(id_reg, country == i)
  cntry_dat_reg2 <- filter(cntry_dat_reg, iso3 == i)
  
  stackers_list2 <- stackers_list
  for (j in 1:5) {
    stackers_list2[[j]] <- filter(stackers_list2[[j]], ISO3 == i)
  }
  
  mbg_agg_plot2 <- filter(mbg_agg_plot, ISO3 == i)
  
  print(
  ggplot() + 
  #input data
  # geom_point(data = id_reg2, aes(x = year, y = MBG_raw, size = MBG_raw_dat), col = 'blue') + 
  
  # country estimates
  geom_point(data = cntry_dat_reg2, aes(x = year_start, y = cntry_est, col = 'cntry_est'), 
             shape = 17, size = 3) +
  
  
  # mbg preds
  geom_line(data = mbg_agg_plot2, aes(x = year, y = mbg, col = 'mbg'), show.legend = T) +
  # all stackers
  geom_line(data = stackers_list2[[1]], aes(x = year, y = mbg, col = 'stacking'), show.legend = T) +
    
  # gam
  geom_line(data = stackers_list2[[2]], aes(x = year, y = mbg, col = 'gam'), show.legend = T) +
  # brt
  geom_line(data = stackers_list2[[3]], aes(x = year, y = mbg, col = 'brt'), show.legend = T) +
  # lasso
  geom_line(data = stackers_list2[[4]], aes(x = year, y = mbg, col = 'lasso'), show.legend = T) +
  # ridge
  geom_line(data = stackers_list2[[5]], aes(x = year, y = mbg, col = 'ridge'), show.legend = T) +
  
  # making it clean
  ylab('Prevalence') + xlab('Year') + 
  ggtitle(paste0('Piped Water Prevalence, ',i)) + 
  theme_bw()
  )
}
dev.off()