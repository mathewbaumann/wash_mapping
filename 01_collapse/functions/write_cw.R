# This functions outputs crosswalking data for indicator crosswalking

write_cw_ratio <- function(mydat = ptdat, dt = data_type, census = ipums,
						   var_family = indi_fam) {
  
  if (census) {
  	dt <- 'ipums'
  }
  regions <- read.csv('/home/j/WORK/11_geospatial/diarrhea/01_data/00_identifiers/geoid_stg2.csv')
  regions <- regions[,c('iso3', 'region')]
  regions$iso3 <- as.character(regions$iso3)
  regions$region <- as.character(regions$region)
  regions <- regions[complete.cases(regions),]
  
  region_list <- unique(regions$region)
  
  region_list <- region_list[region_list != 'excluded']
  
  for(reg in region_list){
    sub_reg <- subset(regions, region == reg)
    list <- as.vector(sub_reg$iso3)
    assign(reg, list)
  }
  
  mydat$reg <- NA
  mydat$reg <- ifelse(mydat$iso3 %in% essa, 'essa', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% afr_horn, 'afr_horn', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% cssa, 'cssa', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% wssa, 'wssa', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% name, 'name', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% sssa, 'sssa', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% mcacaf, 'mcacaf', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% s_america, 's_america', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% central_asia, 'central_asia', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% chn_mng, 'chn_mng', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% se_asia, 'se_asia', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% malay, 'malay', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% south_asia, 'south_asia', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% mid_east, 'mid_east', mydat$reg)
  mydat$reg <- ifelse(mydat$iso3 %in% oceania, 'oceania', mydat$reg)
  

  # Summarize number of observations by each indicator level and write to cw .csv
  if (var_family == 'sani') {

  	  # Collapse data to number of observations
	  mydat <- mydat %>%
		  		   mutate(imp = imp * total_hh,
		  		   		  unimp = unimp * total_hh,
		  		   		  od = od * total_hh,
		  		   		  latrine_imp = latrine_imp * total_hh,
		  		   		  latrine_unimp = latrine_unimp * total_hh,
		  		   		  latrine_cw = latrine_cw * total_hh,
		  		   		  flush_imp = (flush_imp + flush_imp_sewer + flush_imp_septic),
		  		   		  flush_unimp = flush_unimp * total_hh,
		  		   		  flush_cw = flush_cw * total_hh) %>%
		  		   group_by(iso3, reg) %>% 
		  		   summarize(N = sum(total_hh),
		  		   			 imp = sum(imp),
		  		   			 unimp = sum(unimp),
		  		   		  	 od = sum(od),
		  		   		  	 latrine_imp = sum(latrine_imp),
		  		   		  	 latrine_unimp = sum(latrine_unimp),
		  		   		  	 latrine_cw = sum(latrine_cw),
		  		   		  	 flush_imp = sum(flush_imp),
		  		   		  	 flush_unimp = sum(flush_unimp),
		  		   		  	 flush_cw = sum(flush_cw),
		  		   		  	 sources = length(unique(nid)),
		  		   		  	 data = dt)

	# Read in original cw file if it exists
	original <- try(read.csv('/home/j/WORK/11_geospatial/wash/definitions/cw_sani_2.csv', stringsAsFactors = F),
					silent = T)
	
	if (class(original) == 'try-error') {
		rm(original)
	}

	# Create indicator variable indicating data sources represented in the cw csv
	if (exists('original')) {
		data_present <- unlist(strsplit(unique(as.character(original$data)), ','))
		data_present <- unique(gsub(' ', '', data_present))
		original <- select(original, -X)
	} else {
		data_present <- ''
	}
				
	
	# if current data type is in the cw csv overwrite the csv with fresh run
	if ((dt %in% data_present) & dt != 'ipums') {
		write.csv(mydat, '/home/j/WORK/11_geospatial/wash/definitions/cw_sani_2.csv')
	} else {
		if (data_present != '') {
			mydat <- bind_rows(mydat, original)
			if (dt != 'ipums') {
				mydat$data <- paste(dt, ',', paste(data_present, collapse = ', '))
			} else {
				if (dt %in% data_present) {
				  mydat$data <- paste(data_present, collapse = ', ')
				} else {
					mydat$data <- paste(dt, ',', paste(data_present, collapse = ', '))
				}
			}
		} else {
			mydat$data <- dt
		}

		mydat <- mydat %>%
    	  		 group_by(iso3, reg, data) %>% 
	  		     summarize(N = sum(N),
	  		   	     	   imp = sum(imp),
	  		   			   unimp = sum(unimp),
	  		   		  	   od = sum(od),
	  		   		  	   latrine_imp = sum(latrine_imp),
	  		   		  	   latrine_unimp = sum(latrine_unimp),
	  		   		  	   latrine_cw = sum(latrine_cw),
	  		   		  	   flush_imp = sum(flush_imp),
	  		   		  	   flush_unimp = sum(flush_unimp),
	  		   		  	   flush_cw = sum(flush_cw),
	  		   		  	   sources = sum(sources))

	  	write.csv(mydat, '/home/j/WORK/11_geospatial/wash/definitions/cw_sani_2.csv')

	}

  }

  if (var_family == 'water') {
	  mydat <- mydat %>%
		  		   mutate(imp = imp * total_hh,
		  		   		  unimp = unimp * total_hh,
		  		   		  surface = surface * total_hh,
		  		   		  spring_imp = spring_imp * total_hh,
		  		   		  spring_unimp = spring_unimp * total_hh,
		  		   		  spring_cw = spring_cw * total_hh,
		  		   		  well_imp = well_imp * total_hh,
		  		   		  well_unimp = well_unimp * total_hh,
		  		   		  well_cw = well_cw * total_hh,
		  		   		  piped_imp = piped_imp * total_hh,
		  		   		  piped = piped * total_hh,
		  		   		  piped_cw = piped_cw * total_hh) %>%
		  		   group_by(iso3, reg) %>% 
		  		   summarize(N = sum(total_hh),
		  		   			 imp = sum(imp),
			  		   		 unimp = sum(unimp),
			  		   		 surface = sum(surface),
			  		   		 spring_imp = sum(spring_imp),
			  		   		 spring_unimp = sum(spring_unimp),
			  		   		 spring_cw = sum(spring_cw),
			  		   		 well_imp = sum(well_imp),
			  		   		 well_unimp = sum(well_unimp),
			  		   		 well_cw = sum(well_cw),
			  		   		 piped_imp = sum(piped_imp),
			  		   		 piped = sum(piped),
			  		   		 piped_cw = sum(piped_cw),
		  		   		  	 sources = length(unique(nid)),
		  		   		  	 data = dt)
	
	# Read in original cw file if it exists
	original <- try(read.csv('/home/j/WORK/11_geospatial/wash/definitions/cw_water_2.csv'),
					silent = T)
	
	if (class(original) == 'try-error') {
		rm(original)
	}
	
	# Create indicator variable indicating data sources represented in the cw csv
	if (exists('original')) {
		data_present <- unlist(strsplit(unique(as.character(original$data)), ','))
		data_present <- gsub(' ', '', data_present)
		original <- select(original, -X)
	} else {
		data_present <- ''
	}
				
	
	# if current data type is in the cw csv overwrite the csv with fresh run
	if ((dt %in% data_present) & dt != 'ipums') {
		write.csv(mydat, '/home/j/WORK/11_geospatial/wash/definitions/cw_water_2.csv')
	} else {
		if (data_present != '') {
			mydat <- bind_rows(mydat, original)
			if (dt != 'ipums') {
				mydat$data <- paste(dt, ',', paste(data_present, collapse = ', '))
			} else {
				if (dt %in% data_present) {
				  mydat$data <- paste(data_present, collapse = ', ')
				} else {
					mydat$data <- paste(dt, ',', paste(data_present, collapse = ', '))
				}
			}
		} else {
			mydat$data <- dt
		}

		mydat <- mydat %>%
    	  		 group_by(iso3, reg, data) %>% 
	  		     summarize(N = sum(N),
	  		   			   imp = sum(imp),
		  		   		   unimp = sum(unimp),
		  		   		   surface = sum(surface),
		  		   		   spring_imp = sum(spring_imp),
		  		   		   spring_unimp = sum(spring_unimp),
		  		   		   spring_cw = sum(spring_cw),
		  		   		   well_imp = sum(well_imp),
		  		   		   well_unimp = sum(well_unimp),
		  		   		   well_cw = sum(well_cw),
		  		   		   piped_imp = sum(piped_imp),
		  		   		   piped = sum(piped),
		  		   		   piped_cw = sum(piped_cw),
	  		   		  	   sources = sum(sources))

	  	write.csv(mydat, '/home/j/WORK/11_geospatial/wash/definitions/cw_water_2.csv')

		}

	}
 
	return(mydat)
}