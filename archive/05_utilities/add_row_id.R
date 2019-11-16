setwd('/home/j/WORK/11_geospatial/10_mbg/input_data')
rm(list = ls())
library(dplyr)

w_imp <- read.csv('w_imp.csv', stringsAsFactors = F)

w_imp_unique <- w_imp
w_imp_unique$latitude <- ifelse(w_imp$point == 0, NA, w_imp_unique$latitude)
w_imp_unique$longitude <- ifelse(w_imp$point == 0, NA, w_imp_unique$longitude)
w_imp_unique <- w_imp_unique %>%
				select(latitude, longitude, shapefile, location_code, year, nid) %>%
				distinct()

w_imp_key_poly <- filter(w_imp_unique, is.na(latitude))
w_imp_key_pt <- filter(w_imp_unique, !is.na(latitude))

w_imp_key_pt$cluster_id <- 1:nrow(w_imp_key_pt)
w_imp_key_pt$point <- 1

w_imp_key_poly$cluster_id <- 1:nrow(w_imp_key_poly)
w_imp_key_poly$point <- 0
w_imp_key_poly <- select(w_imp_key_poly, -latitude, -longitude)


w_imp_pt <- filter(w_imp, point == 1)
w_imp_poly <- filter(w_imp, point == 0)

w_imp_pt <- left_join(w_imp_pt, w_imp_key_pt)
w_imp_poly <- left_join(w_imp_poly, w_imp_key_poly)

results <- list()
pb <- txtProgressBar(min = 0, max = length(w_imp_key_pt$cluster_id), style = 3)
for (i in 1:length(w_imp_key_pt$cluster_id)) {
	mydat <- filter(w_imp_pt, cluster_id == w_imp_key_pt$cluster_id[i])
	mydat$row_id <- 1:nrow(mydat)
	results[[length(results) + 1]] <- mydat
	setTxtProgressBar(pb, i)
}
close(pb)
w_imp_pt <- do.call(bind_rows, results)

results <- list()
pb <- txtProgressBar(min = 0, max = length(w_imp_key_poly$cluster_id), style = 3)
for (i in 1:length(w_imp_key_poly$cluster_id)) {
	#print(i)
	mydat <- filter(w_imp_poly, cluster_id == w_imp_key_poly$cluster_id[i])
	mydat$row_id <- 1:nrow(mydat)
	results[[length(results) + 1]] <- mydat
	#setTxtProgressBar(pb, i)
}
close(pb)
w_imp_poly <- do.call(bind_rows, results)

w_imp2 <- bind_rows(w_imp_pt, w_imp_poly)
w_imp2 <- mutate(w_imp2, master_id = paste(point,cluster_id, row_id, sep = '_'))

#w_imp2 <- read.csv('s_imp.csv')

for (set in c('w_unimp_calc','w_surface_calc','w_piped_calc')) {
	w_imp <- read.csv(paste0(set,'.csv'), stringsAsFactors = F)
	w_imp_pt <- filter(w_imp, point == 1)
	w_imp_poly <- filter(w_imp, point == 0)

	w_imp_pt <- left_join(w_imp_pt, w_imp_key_pt)
	w_imp_poly <- left_join(w_imp_poly, w_imp_key_poly)

	results <- list()
	pb <- txtProgressBar(min = 0, max = length(w_imp_key_pt$cluster_id), style = 3)
	for (i in 1:length(w_imp_key_pt$cluster_id)) {
		mydat <- filter(w_imp_pt, cluster_id == w_imp_key_pt$cluster_id[i])
		if (nrow(mydat) > 0) {
			mydat$row_id <- 1:nrow(mydat)
			results[[length(results) + 1]] <- mydat	
		}
		setTxtProgressBar(pb, i)
	}
	close(pb)
	w_imp_pt <- do.call(bind_rows, results)

	results <- list()
	pb <- txtProgressBar(min = 0, max = length(w_imp_key_poly$cluster_id), style = 3)
	for (i in 1:length(w_imp_key_poly$cluster_id)) {
		#print(i)
		mydat <- filter(w_imp_poly, cluster_id == w_imp_key_poly$cluster_id[i])
		if (nrow(mydat) > 0) {
			mydat$row_id <- 1:nrow(mydat)
			results[[length(results) + 1]] <- mydat	
		}
		
		setTxtProgressBar(pb, i)
	}
	close(pb)
	w_imp_poly <- do.call(bind_rows, results)

	w_imp <- bind_rows(w_imp_pt, w_imp_poly)
	w_imp <- mutate(w_imp, master_id = paste(point,cluster_id, row_id, sep = '_'))

	assign(set, w_imp)
	
}

#write.csv(w_imp2, file = 's_imp.csv')
write.csv(w_unimp_calc, file = 'w_unimp_calc.csv')
write.csv(w_surface_calc, file = 'w_surface_calc.csv')
write.csv(w_piped_calc, file = 'w_piped_calc.csv')
