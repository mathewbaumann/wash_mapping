library(dplyr)


indicators <- c('w_imp','w_unimp_calc','w_piped_calc','w_surface_calc','s_imp','s_unimp_calc','s_od_calc')
for (indi in indicators) {
	print(indi)
	setwd(paste0('/share/geospatial/mbg/wash/', indi,'/output/2018_03_22_20_13_25'))

	results <- list()
	for (i in 1:5) {
		load(list.files()[grep('admin_draws',list.files())][i])
		geo <- sp_hierarchy_list
		ad1 <- admin_1
		geo$ADM0_NAME <- as.character(geo$ADM0_NAME)
		geo$ADM1_NAME <- as.character(geo$ADM1_NAME)

		lci <- apply(ad1[,3:252], 1, quantile, 0.025, na.rm = T)
		uci <- apply(ad1[,3:252], 1, quantile, 0.975, na.rm = T)
		median <- apply(ad1[,3:252], 1, median, na.rm = T)

		ad1 <- cbind(ad1[,c(1:2,253)], lci, median, uci)
		print(nrow(ad1))
		ad1 <- left_join(ad1, distinct(select(geo, region, ADM0_CODE, ADM0_NAME, ADM1_NAME, ADM1_CODE)))
		print(nrow(ad1))
		results[[i]] <- ad1
	}

	ad1 <- do.call(rbind, results)
	names(ad1)[4] <- paste0(indi, '_lci')
	names(ad1)[5] <- paste0(indi, '_median')
	names(ad1)[6] <- paste0(indi, '_uci')
	assign(indi, ad1)	

}


nrow(w_imp)
master_ad1 <- left_join(w_imp, w_surface_calc)
nrow(master_ad1)
master_ad1 <- left_join(master_ad1, w_unimp_calc)
master_ad1 <- left_join(master_ad1, w_piped_calc)
master_ad1 <- left_join(master_ad1, s_imp)
master_ad1 <- left_join(master_ad1, s_unimp_calc)
master_ad1 <- left_join(master_ad1, s_od_calc)

###
indicators <- c('w_imp','w_unimp_calc','w_piped_calc','w_surface_calc','s_imp','s_unimp_calc','s_od_calc')
for (indi in indicators) {
	print(indi)
	setwd(paste0('/share/geospatial/mbg/wash/', indi,'/output/2018_03_22_20_13_25'))

	results <- list()
	for (i in 1:5) {
		load(list.files()[grep('admin_draws',list.files())][i])
		geo <- sp_hierarchy_list
		ad2 <- admin_2
		geo$ADM0_NAME <- as.character(geo$ADM0_NAME)
		geo$ADM1_NAME <- as.character(geo$ADM1_NAME)
		geo$ADM2_NAME <- as.character(geo$ADM2_NAME)

		lci <- apply(ad2[,3:252], 1, quantile, 0.025, na.rm = T)
		uci <- apply(ad2[,3:252], 1, quantile, 0.975, na.rm = T)
		median <- apply(ad2[,3:252], 1, median, na.rm = T)

		ad2 <- cbind(ad2[,1:2], lci, median, uci)
		ad2 <- left_join(ad2, distinct(select(geo, region, ADM0_CODE, ADM0_NAME, ADM1_NAME, ADM1_CODE, ADM2_NAME, ADM2_CODE)))
		results[[i]] <- ad2
	}

	ad2 <- do.call(rbind, results)
	names(ad2)[3] <- paste0(indi, '_lci')
	names(ad2)[4] <- paste0(indi, '_uci')
	names(ad2)[5] <- paste0(indi, '_median')
	assign(indi, ad2)	

}



master_ad2<- left_join(w_imp, w_surface_calc)
master_ad2 <- left_join(master_ad2, w_unimp_calc)
master_ad2 <- left_join(master_ad2, w_piped_calc)
master_ad2 <- left_join(master_ad2, s_imp)
master_ad2 <- left_join(master_ad2, s_unimp_calc)
master_ad2 <- left_join(master_ad2, s_od_calc)

###
indicators <- c('w_imp','w_unimp_calc','w_piped_calc','w_surface_calc','s_imp','s_unimp_calc','s_od_calc')
for (indi in indicators) {
	print(indi)
	setwd(paste0('/share/geospatial/mbg/wash/', indi,'/output/2018_03_22_20_13_25'))

	results <- list()
	for (i in 1:5) {
		load(list.files()[grep('admin_draws',list.files())][i])
		geo <- sp_hierarchy_list
		ad0 <- admin_0
		geo$ADM0_NAME <- as.character(geo$ADM0_NAME)

		lci <- apply(ad0[,3:252], 1, quantile, 0.025, na.rm = T)
		uci <- apply(ad0[,3:252], 1, quantile, 0.975, na.rm = T)
		median <- apply(ad0[,3:252], 1, median, na.rm = T)

		ad0 <- cbind(ad0[,1:2], lci, median, uci)
		print(nrow(ad0))
		ad0 <- left_join(ad0, distinct(select(geo, region, ADM0_CODE, ADM0_NAME)))
		print(nrow(ad0))
		results[[i]] <- ad0
	}

	ad0 <- do.call(rbind, results)
	names(ad0)[3] <- paste0(indi, '_lci')
	names(ad0)[4] <- paste0(indi, '_uci')
	names(ad0)[5] <- paste0(indi, '_median')
	assign(indi, ad0)	

}


nrow(w_imp)
master_ad0 <- left_join(w_imp, w_surface_calc)
master_ad0 <- left_join(master_ad0, w_unimp_calc)
nrow(master_ad0)
master_ad0 <- left_join(master_ad0, w_piped_calc)
master_ad0 <- left_join(master_ad0, s_imp)
master_ad0 <- left_join(master_ad0, s_unimp_calc)
master_ad0 <- left_join(master_ad0, s_od_calc)


write.csv(master_ad1, file = '/homes/adesh/final_results_ad1_03.27.18.csv')
write.csv(master_ad2, file = '/homes/adesh/final_results_ad2_03.27.18.csv')
write.csv(master_ad0, file = '/homes/adesh/final_results_ad0_03.27.18.csv')