cw_water <- function(mydat) {

	cw_dat <- read.csv('/home/j/WORK/11_geospatial/wash/definitions/cw_water_2.csv')

	results <- list()
	for (i in unique(mydat$iso3)) {
		message(i)
		cw_sub <- filter(cw_dat, iso3 == i)
		if (nrow(cw_sub) == 0) {
			cw_sub[1,] <- 0
		}
		attach(cw_sub)
		reg <- (sources < 5) |
			   (piped + piped_imp == 0) |
			   (well_imp + well_unimp == 0) |
			   (spring_imp + spring_unimp == 0) 
		detach(cw_sub)

		if (!reg) {
			attach(cw_sub)
			iwell_pct <- well_imp/(well_imp + well_unimp)
			ispring_pct <- spring_imp/(spring_imp + spring_unimp)
			ipiped_pct <- piped_imp/(piped + piped_imp)
			detach(cw_sub)
		} else {
  		  regions <- read.csv('/home/j/WORK/11_geospatial/diarrhea/01_data/00_identifiers/geoid_stg2.csv')
  		  regions <- regions[,c('iso3', 'region')]
  		  regions$iso3 <- as.character(regions$iso3)
  		  regions$region <- as.character(regions$region)
  		  regions <- regions[complete.cases(regions),]
  		  
  		  region_list <- unique(regions$region)
  		  
  		  region_list <- region_list[region_list != 'excluded']
  		  region <- NA
  		  
  		  for(regi in region_list){
  		    sub_reg <- subset(regions, region == regi)
  		    list <- as.vector(sub_reg$iso3)
  		    assign(regi, list)
  		    if (i %in% get(regi)){
  		      region <- regi
  		    }
  		  }

			  cw_reg <- filter(cw_dat, reg == region)
			  cw_reg <- cw_reg %>% 
			  			group_by(reg) %>%
			  			summarize(well_imp = sum(well_imp),
			  					  well_unimp = sum(well_unimp),
			  					  spring_imp = sum(spring_imp),
			  					  spring_unimp = sum(spring_unimp),
			  					  piped = sum(piped),
			  					  piped_imp = sum(piped_imp))
			  attach(cw_reg)
			  iwell_pct <- well_imp/(well_imp + well_unimp)
			  ispring_pct <- spring_imp/(spring_imp + spring_unimp)
			  ipiped_pct <- piped_imp/(piped + piped_imp)
			  detach(cw_reg)

		}
		
		subdat <- mydat %>%
				 filter(iso3 == i)
		if(length(ipiped_pct) == 0){
		  ipiped_pct <- NA
		}
		if(length(iwell_pct) == 0){
		  iwell_pct <- NA
		}
		if(length(ispring_pct) == 0){
		  ispring_pct <- NA
		}

		if (is.na(ipiped_pct)) {
			ipiped_pct <- 1
			subdat$piped_cw <- 0
		}

		if (is.na(iwell_pct)) {
			iwell_pct <- 1
			subdat$well_cw <- 0
		}

		if (is.na(ispring_pct)) {
			ispring_pct <- 1
			subdat$spring_cw <- 0
		}

		subdat <- subdat %>%
				 mutate(network = piped + (ipiped_pct*piped_cw),
				   piped = piped + piped_cw + piped_imp,
				 		unimp = unimp + well_unimp + 
				 				(well_cw * (1 - iwell_pct)) +
				 				spring_unimp + (spring_cw * (1 - ispring_pct)),
				 		surface = surface) %>%
				 mutate(imp = well_imp + (well_cw * iwell_pct) +
				 			  spring_imp + (spring_cw * ispring_pct) +
				 			  imp) %>%
				 rename(N = total_hh) %>%
				 select(nid, iso3, survey_series, 
				 		lat, long, shapefile, location_code,
				 		year_start, int_year, year_median, sum_old_N,
				 		N, sum_of_sample_weights,
				 		network, piped, imp, unimp, surface, row_id)
		results[[length(results) + 1]] <- subdat

	}

	results <- do.call(rbind, results)

	return(results)
}

cw_sani <- function(mydat) {

	cw_dat <- read.csv('/home/j/WORK/11_geospatial/wash/definitions/cw_sani_2.csv')

	results <- list()
	for (i in unique(mydat$iso3)) {
		message(i)
		cw_sub <- filter(cw_dat, iso3 == i)
		if (nrow(cw_sub) == 0) {
			cw_sub[1,] <- 0
		}
		attach(cw_sub)
		reg <- (sources < 5) |
			   (latrine_imp + latrine_unimp == 0) |
			   (flush_imp + flush_unimp == 0)
		detach(cw_sub)

		if (!reg) {
			attach(cw_sub)
			ilatrine_pct <- latrine_imp/(latrine_imp + latrine_unimp)
			iflush_pct <- flush_imp/(flush_imp + flush_unimp)
			detach(cw_sub)
		} else {
  		  regions <- read.csv('/home/j/WORK/11_geospatial/diarrhea/01_data/00_identifiers/geoid_stg2.csv')
  		  regions <- regions[,c('iso3', 'region')]
  		  regions$iso3 <- as.character(regions$iso3)
  		  regions$region <- as.character(regions$region)
  		  regions <- regions[complete.cases(regions),]
  		  
  		  region_list <- unique(regions$region)
  		  
  		  region_list <- region_list[region_list != 'excluded']
  		  region <- NA
  		  
  		  for(regi in region_list){
  		    sub_reg <- subset(regions, region == regi)
  		    list <- as.vector(sub_reg$iso3)
  		    assign(regi, list)
  		    if (i %in% get(regi)){
  		      region <- regi
  		    }
  		  }

			  cw_reg <- filter(cw_dat, reg == region)
			  cw_reg <- cw_reg %>% 
			  			group_by(reg) %>%
			  			summarize(latrine_imp = sum(latrine_imp),
			  					  latrine_unimp = sum(latrine_unimp),
			  					  flush_imp = sum(flush_imp),
			  					  flush_unimp = sum(flush_unimp))
			  attach(cw_reg)
   			  ilatrine_pct <- latrine_imp/(latrine_imp + latrine_unimp)
			    iflush_pct <- flush_imp/(flush_imp + flush_unimp)
			  detach(cw_reg)

		}
		

		subdat <- mydat %>%
				 filter(iso3 == i)
		if(length(ilatrine_pct) == 0){
		  ilatrine_pct <- NA
		}
		if(length(iflush_pct) == 0){
		  iflush_pct <- NA
		}

		if (is.na(ilatrine_pct)) {
			ilatrine_pct <- 1
			subdat$latrine_cw <- 0
		}

		if (is.na(iflush_pct)) {
			iflush_pct <- 1
			subdat$flush_cw <- 0
		}

		subdat <- subdat %>%
		  mutate(piped = (iflush_pct * flush_cw) + flush_imp + s_piped,
		         imp = imp + 
		           (ilatrine_pct * latrine_cw) + latrine_imp,
		         unimp = unimp + 
		           (latrine_cw * (1 - ilatrine_pct)) + latrine_unimp +
		           (flush_cw * (1 - iflush_pct)) + flush_unimp,
		         od = od) %>%
		  rename(N = total_hh) %>%
		  select(nid, iso3, survey_series, 
		         lat, long, shapefile, location_code,
		         year_start, int_year, year_median, sum_old_N,
		         N, sum_of_sample_weights, network, piped,
		         imp, unimp, od, row_id)
		results[[length(results) + 1]] <- subdat

	}

	results <- do.call(rbind, results)

	return(results)
}