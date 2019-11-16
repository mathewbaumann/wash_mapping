# Clear Environment
rm(list = ls())

# Load necessary libraries
package_list <- c('dplyr', 'ggrepel')
library(googlesheets)
library(httpuv)
library(data.table)
library(ggplot2)
library(openxlsx)

source("/share/code/geospatial/lbd_core/mbg_central/setup.R")
mbg_setup(package_list = package_list, repos="/share/code/geospatial/lbd_core/mbg_central")

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
}

codebook <- fread('/home/j/temp/baumannm/downloaded_vetting_sheet_3.csv')
codebook2 <- fread('/home/j/temp/baumannm/downloaded_vetting_sheet_2.csv')

library(dplyr)
library(feather)

indicators <- c('s_piped', 's_imp_cr', 's_unimp_cr','w_imp_cr','w_unimp_cr','w_piped')
for(indi in indicators){
  if(indi %in% c('s_piped','s_imp_cr','s_unimp_cr')){
    indi_fam <- 's'
  }else{
    indi_fam <- 'w'
  }
  temp <- fread(paste0('/share/geospatial/mbg/input_data/', indi, '.csv'))

  mydat3 <- temp %>%
    group_by(nid, surv_year, country, survey_series) %>%
    dplyr::summarize(prev = weighted.mean(x = prop, w = sum_of_sample_weights, na.rm = T),
                     N = sum(sum_of_sample_weights, na.rm = T))

  colnames(mydat3) <- c('nid','year','country','survey_series',indi,paste0('N_', indi_fam))
  if (indi == 's_piped'){
    weighted_means <- mydat3
  }else{
    weighted_means <- merge(weighted_means, mydat3, by = c('nid','year','country','survey_series'), all.x = T)
  }
}
weighted_means <- as.data.table(weighted_means)
N_w <- weighted_means$N_w.x
N_s <- weighted_means$N_s.x
weighted_means <- weighted_means[,-c('N_w.x','N_w.y','N_s.x','N_s.y')]
weighted_means$N_w <- N_w
weighted_means$N_s <- N_s




codebook <- codebook[,c('nid', 'ihme_loc_id', 'year_start', 'decision_s_imp', 'decision_w_piped')]
codebook2 <- codebook2[,c('nid', 'ihme_loc_id', 'year_start', 'National_Subnational')]
codebook$ihme_loc_id <- substr(codebook$ihme_loc_id, 1, 3)
codebook2$ihme_loc_id <- substr(codebook2$ihme_loc_id, 1, 3)
codebook <- unique(codebook)
codebook2 <- unique(codebook2)
codebook <- merge(codebook, codebook2, by = c('nid', 'ihme_loc_id', 'year_start'))
codebook <- as.data.table(codebook)
codebook$decision_s_imp <- as.character(codebook$decision_s_imp)
codebook$decision_w_piped <- as.character(codebook$decision_w_piped)
codebook[,s_outlier := ifelse(decision_s_imp == 'outlier', 1, 0)]
codebook[,w_outlier := ifelse(decision_w_piped == 'outlier', 1, 0)]
codebook <- codebook[,-c('decision_s_imp','decision_w_piped')]


weighted_means <- as.data.table(unique(merge(weighted_means, codebook, by.x = c('nid', 'country'), by.y = c('nid', 'ihme_loc_id'), all.x = T)))
weighted_means <- weighted_means[,c('nid','year','country','survey_series','s_piped','s_imp_cr','s_unimp_cr','w_imp_cr','w_unimp_cr','w_piped','N_w','N_s','National_Subnational','s_outlier','w_outlier')]
colnames(weighted_means) <- c('nid', 'year', 'country', 'survey_series', 's_piped', 's_imp_cr', 's_unimp_cr', 'w_imp_cr', 'w_unimp_cr', 'w_piped', 'N_w',
                              'N_s', 'subnat', 's_outlier', 'w_outlier')
weighted_means[is.na(s_outlier), s_outlier := 2]
weighted_means[is.na(w_outlier), w_outlier := 2]
weighted_means[is.na(subnat), subnat := 'no info']
weighted_means[subnat %like% 'Subnat',subnat := 'subnational']
weighted_means[subnat %like% 'Nat',subnat := 'national']
weighted_means[subnat != 'national' & subnat != 'subnational' & subnat != 'no info',subnat := 'subnational']
weighted_means$subnat <- as.character(weighted_means$subnat)

#SANI JMP NUMBERS
jmp_sani <- read.xlsx('/homes/baumannm/JMP_WASH_2017.xlsx', sheet = 3)
iso3 <- jmp_sani$X2[4:3715]
year <- as.numeric(jmp_sani$X3[4:3715])
jmp_sani_basic <- as.numeric(jmp_sani$X6[4:3715])
jmp_sani_limited <- as.numeric(jmp_sani$X7[4:3715])
imp <- jmp_sani_basic + jmp_sani_limited
unimp <- as.numeric(jmp_sani$X8[4:3715])
od <- as.numeric(jmp_sani$X9[4:3715])
jmp_sani <- data.table(iso3, year, imp, unimp, od)

#WATER JMP NUMBERS
jmp_water <- read.xlsx('/homes/baumannm/JMP_WASH_2017.xlsx', sheet = 2)
iso3 <- jmp_water$X2[4:3715]
year <- as.numeric(jmp_water$X3[4:3715])
jmp_water_basic <- as.numeric(jmp_water$X6[4:3715])
jmp_water_limited <- as.numeric(jmp_water$X7[4:3715])
imp <- jmp_water_basic + jmp_water_limited
unimp <- as.numeric(jmp_water$X8[4:3715])
surface <- as.numeric(jmp_water$X9[4:3715])
jmp_water <- data.table(iso3, year, imp, unimp, surface)

gbd_sani_imp <- fread('/home/j/temp/baumannm/wash_sanitation_imp_prop.csv')
gbd_sani_piped <- fread('/home/j/temp/baumannm/wash_sanitation_piped.csv')
gbd_water_imp <- fread('/home/j/temp/baumannm/wash_water_imp_prop.csv')
gbd_water_piped <- fread('/home/j/temp/baumannm/wash_water_piped.csv')

gbd_sani <- merge(gbd_sani_imp, gbd_sani_piped, by = c('nid','location_id','year_id'))
gbd_sani[,val := data.y + ((1 - data.y)*data.x)]

gbd_water <- merge(gbd_water_imp, gbd_water_piped, by = c('nid','location_id','year_id'))
gbd_water[,val := data.y + ((1 - data.y)*data.x)]

indi_fam <- c('water','sani')
today <- Sys.Date()
dir.create(file.path(paste0('/home/j/WORK/11_geospatial/wash/national_ts_plots'), today), showWarnings = FALSE)

for (indi in indi_fam){
  if (indi == 'water'){
    gbd <- copy(gbd_water)
    levels <- c('w_piped', 'w_unimp_cr')
    temp <- 'w_outlier'
    temp_N <- 'N_w'
    jmp <- copy(jmp_water)
  }else{
    gbd <- copy(gbd_sani)
    levels <- c('s_piped', 's_unimp_cr')
    temp <- 's_outlier'
    temp_N <- 'N_s'
    jmp <- copy(jmp_sani)
  }
  for (lvl in levels){
    # Read in input data
    mydat <- weighted_means[,c('nid', 'year', 'country', 'survey_series', lvl, temp, 'subnat', temp_N), with = FALSE]
    colnames(mydat) <- c('nid', 'year', 'country', 'survey_series', 'prop', 'outlier', 'subnat', 'N')

    # Subset data to modeling period
    mydat <- filter(mydat, year >= 2000)

    # Calculate weighted means by country-year-nid-source as well as weighted sum of
    # sample size post-processing
    mydat <- mutate(mydat, country = substr(country, 1, 3))
    mydat <- mutate(mydat, survey_series = substr(survey_series, 1, 16))

    if(lvl %in% c('w_piped', 's_piped')){
      if(lvl == 'w_piped'){
        added_lvl <- 'w_imp_cr'
      }else{
        added_lvl <- 's_imp_cr'
      }
      mydat3 <- mydat %>%
        group_by(nid, year, country, survey_series) %>%
        dplyr::summarize(prev = weighted.mean(x = prop, w = N, na.rm = T),
                         N = sum(N, na.rm = T), outlier = sum(outlier, na.rm = T), subnat = ifelse(length(unique(subnat)) > 1, 'subnat',unique(subnat)))

      mydat <- weighted_means[,c('nid', 'year', 'country', 'survey_series', added_lvl, temp, 'subnat', temp_N), with = FALSE]
      colnames(mydat) <- c('nid', 'year', 'country', 'survey_series', 'prop2', 'outlier', 'subnat', 'N')

      # Subset data to modeling period
      mydat <- filter(mydat, year >= 2000)

      # Calculate weighted means by country-year-nid-source as well as weighted sum of
      # sample size post-processing
      mydat <- mutate(mydat, country = substr(country, 1, 3))
      mydat <- mutate(mydat, survey_series = substr(survey_series, 1, 16))

      mydat4 <- mydat %>%
        group_by(nid, year, country, survey_series) %>%
        dplyr::summarize(prev2 = weighted.mean(x = prop2, w = N, na.rm = T),
                         N = sum(N, na.rm = T), outlier = sum(outlier, na.rm = T), subnat = ifelse(length(unique(subnat)) > 1, 'subnat',unique(subnat)))

      mydat3 <- merge(mydat3, mydat4, by = c('nid','year','country','survey_series', 'outlier','subnat'), all.x = TRUE, all.y = TRUE)
      mydat3 <- as.data.table(mydat3)
      #improved = piped + ((1 - piped)*(imp_cr))
      mydat3[,prev := prev + ((1 - prev)*(prev2))]
      mydat3[,N := N.x + N.y]
    }else{
      mydat3 <- mydat %>%
        group_by(nid, year, country, survey_series) %>%
        dplyr::summarize(prev = weighted.mean(x = prop, w = N, na.rm = T),
                         N = sum(N, na.rm = T), outlier = sum(outlier, na.rm = T), subnat = ifelse(length(unique(subnat)) > 1, 'subnat',unique(subnat)))
    }


    for (region in region_list){
      message(paste0('STARTING REGION ', region))
      dir.create(file.path(paste0('/home/j/WORK/11_geospatial/wash/national_ts_plots/', today), lvl), showWarnings = FALSE)
      pdf(paste0('/home/j/WORK/11_geospatial/wash/national_ts_plots/', today, '/', lvl, '/', region, '_national_ts.pdf'))
      for (j in get(region)) {
        message(j)
        ############################
        #sani
        jmp_temp <- jmp[iso3 == j,]
        if(lvl == 's_piped' | lvl == 'w_piped'){
          setnames(jmp_temp,"imp", "temp")
        }else{
          setnames(jmp_temp,"unimp", "temp")
        }
        jmp_temp$temp <- jmp_temp$temp/100
        test <- filter(mydat3, country == j)

        gbd_test <- copy(gbd)
        gbd_test[,country := ihme_loc_id.x]
        gbd_test <- subset(gbd_test, country == j)
        gbd_test <- subset(gbd_test, nid %in% test$nid)
        if (nrow(test) > 0) {
          print(
            p1 <-  ggplot(test, aes(x = year, y = prev)) +
            geom_point(aes(x = year, y = prev, size = N, shape = as.factor(subnat),
                           color = as.factor(outlier))) +
            geom_text_repel(aes(x = year, y = prev, label = nid), size = 2) +
            #geom_smooth(method = 'gam', se = FALSE, size = .5) +
            geom_point(aes(x = year_id, y = val), shape = 1, size = 2, data = gbd_test) +
            geom_line(aes(x = year, y = temp), data = jmp_temp) +
            xlim(2000, 2017) + ylim(0, 1) +
            ggtitle(paste0(lvl, ' in ', j)) +
            theme_bw() +
            labs(shape = 'Spatial Representation') +
            labs(color = 'Outliered Surveys')
          )
        }
      }
      dev.off()
    }
  }
}
