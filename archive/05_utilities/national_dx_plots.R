rm(list = ls())
indi <- 's_imp'
setwd(paste0('/share/geospatial/mbg/wash/',indi,'/output/2018_03_18_02_05_11'))
library(raster)
library(dplyr)
library(ggplot2)
library(tidyr)

draws <- list.files(pattern = 'admin_draws')


inla <- list.files(pattern = 'inla_model_fit_pre_preds_')

for (i in 1:length(inla)) {
	model <- readRDS(inla[i])
	assign(paste0(gsub('inla_model_fit_pre_preds_', '',
				gsub('_holdout_0_agebin_0.RDS', '', inla[i])), '_model'),
		   model) 
    rm(model)
}


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

regions <- c('cssa','wssa','essa_hilo','sssa_hi')

if (indi != 'w_imp') {
  regions <- c(regions, 'name_hi')
}

gaul_iso3 <- read.csv("/home/j/WORK/11_geospatial/10_mbg/gaul_to_loc_id.csv")
gaul_iso3 <- dplyr::select(gaul_iso3, GAUL_CODE, ihme_lc_id) %>%
  rename(ADM0_CODE = GAUL_CODE,
         country = ihme_lc_id)

for (reg in regions) {
  ad0 <- get(reg)
  admin0 <- select(ad0, year, ADM0_CODE)
  admin0$median <- apply(ad0[,3:252], 1, median)
  admin0$uci <- apply(ad0[,3:252], 1, quantile, 0.975)
  admin0$lci <- apply(ad0[,3:252], 1, quantile, 0.025)
  
  ad0_key <- distinct(select(get(paste0(reg, '_key')),
                             ADM0_CODE, ADM0_NAME))
  ad0_key$ADM0_NAME <- as.character(ad0_key$ADM0_NAME)
  admin0 <- left_join(admin0, ad0_key)
  assign(reg, admin0)
  
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

rm(ad0, input, admin0, admin_0, admin_1, admin_2, ad0_key)


setwd('/share/geospatial/mbg/wash/w_imp/model_image_history')
for (reg in regions) {
	load(paste0('2018_03_18_02_05_11_bin0_',reg, '_0.RData'))
	assign(paste0(reg, '_covs'), cov_list)
}
rm(all_fixed_effects, child_model_names, df, draws, mesh_s, mesh_t, period_map,	simple_raster)
gc()

cov_list <- list()
for (i in 2:length(cssa_covs)) {
	covs <- do.call(raster::merge, list(cssa_covs[[i]],
										wssa_covs[[i]],
										name_hi_covs[[i]],
										sssa_hi_covs[[i]],
										essa_hilo_covs[[i]]))
	cov_list[[i-1]] <- covs
}
rm(covs)
names(cov_list) <- names <- names(cssa_covs)[2:length(cssa_covs)]
rm(cssa_covs, wssa_covs, name_hi_covs, sssa_hi_covs, essa_hilo_covs)
gc()
cov_list <- cov_list[which(lapply(cov_list, nlayers) > 1)]

pop <- raster('/home/j/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/worldpop/total/1y/worldpop_total_1y_2015_00_00.tif')
shp <- shapefile('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/africa_ad0.shp')

cov_pop <- list()
for (i in 1:length(cov_list)) {
	print(names(cov_list)[i])
	cov_pop[[i]] <- cov_list[[i]] * pop
	cov_list <- cov_list[2:length(cov_list)]
	print(length(cov_list))
}


extract <- list()
for (i in 1:length(cov_pop)) {
	print(names[i])
	extract[[i]] <- raster::extract(cov_pop[[i]], shp, fun = sum, na.rm = T)
}

load('/homes/adesh/covariate_agg_ad0.RData')

ad0_code <- shp@data$ADM0_CODE
ad0_code[12] <- 'XXX_1'
ad0_code[13] <- 'XXX_2'
ad0_code[14] <- 'XXX_3'
ad0_code[19] <- 'XXX_4'
ad0_code[31] <- 'XXX_5'
ad0_code[33] <- 'XXX_6'
ad0_code[36] <- 'XXX_7'
ad0_code[39] <- 'XXX_8'

pop_ad0 <- raster::extract(pop, shp, fun = sum, na.rm = T)
agg_df <- as.data.frame(cbind(ad0_code, pop_ad0))
names(agg_df) <- c('ADM0_CODE','population')

extract2 <- list()
for (i in 1:length(extract)) {
	test <- as.data.frame(extract[[i]])
	test$ADM0_CODE <- ad0_code
	test <- gather(test, "year", names(extract)[i], 1:16)
	names(test)[3] <- names(extract)[i]
	extract2[[i]] <- test
}

test <- left_join(extract2[[1]], extract2[[2]])
for (i in 3:length(extract2)) {
	test <- left_join(test, extract2[[i]])
}

agg_df <- left_join(agg_df, test)
agg_df$population <- as.numeric(as.character(agg_df$population))

denom <- agg_df$population
for (i in 4:ncol(agg_df)) {
	agg_df[,i] <- agg_df[,i]/denom
	
}

for (i in 8:ncol(agg_df)) {
	agg_df[,i] <- (agg_df[,i]-min(agg_df[,i], na.rm = T))/(max(agg_df[,i], na.rm = T)-min(agg_df[,i], na.rm = T))
}


agg_df$year <- as.numeric(gsub('layer.', '', agg_df$year)) + 1999

pdf(paste0('/homes/adesh/',indi,'_iso3_diag_03.19_v4.pdf'))
for (reg in regions) {
  plotdat <- get(reg)
  input <- get(paste0(reg, '_input'))
  
  for (i in unique(plotdat$ADM0_NAME)) {
    print(i)
    input2  <- filter(input, ADM0_NAME == i)
    plotdat2 <- filter(plotdat, ADM0_NAME == i)

    agg_plot <- filter(agg_df, ADM0_CODE == unique(plotdat2$ADM0_CODE))
    agg_plot <- gather(agg_plot, 'model', 'prev', 4:7)
    agg_plot <- gather(agg_plot, 'covariate','value', 4:17)

    gg1 <- ggplot(plotdat2) +
            geom_point(aes(x = year, y = median, col = ADM0_NAME), col = 'coral3') +
            geom_line(aes(x = year, y = median, col = ADM0_NAME), col = 'coral3') +
        
            geom_line(aes(x = year, y = uci, col = ADM0_NAME),
                      linetype = 'dashed', col = 'coral3') +
            geom_line(aes(x = year, y = lci, col = ADM0_NAME),
                      linetype = 'dashed', col = 'coral3') +
            theme_bw() +
            ggtitle(paste(i, reg, indi)) +
            ylim(0,1)
    if (nrow(input2) > 0) {
        gg1 <- gg1 + geom_point(data = input2, aes(x = year, y = prev, size = ss,
                                     col = 'data', shape = as.factor(point)), col = 'deepskyblue') 
    }
    print(gg1)

    mod_plot <- get(paste0(reg, '_model'))
    means <- round(data.frame(summary(mod_plot)$fixed)$mean, 2)
    gg2 <- ggplot(agg_plot) +
    		geom_line(aes(x = year, y = prev, col = model)) +
    		theme_bw() +
            ggtitle(paste(i, reg, indi)) +
            annotate("text", seq(2001, 2012, by = 2), 1, label = c('int', 'gam', 'gbm',
            											'lasso','ridge','year_cov')) +
            annotate("text", seq(2001, 2012, by = 2), 0.9, label = means) +
            ylim(0,1)
    print(gg2)

    agg_plot2 <- filter(agg_plot, covariate %in% unique(agg_plot$covariate)[1:7]) 
    					#filter(!(covariate %in% c('worldpop', 'crutswet')))
    gg3 <- ggplot(agg_plot2) +
    		geom_line(aes(x = year, y = value, col = covariate)) +
    		theme_bw() +
            ggtitle(paste(i, reg, indi)) 
            #ylim(0,1)

    print(gg3)

    agg_plot2 <- filter(agg_plot, covariate %in% unique(agg_plot$covariate)[8:14]) 
    					#filter(!(covariate %in% c('worldpop', 'crutswet')))

    gg4 <- ggplot(agg_plot2) +
    		geom_line(aes(x = year, y = value, col = covariate)) +
    		theme_bw() +
            ggtitle(paste(i, reg, indi)) 
            #ylim(0,1)

    print(gg4)

  }  		
}
dev.off()


