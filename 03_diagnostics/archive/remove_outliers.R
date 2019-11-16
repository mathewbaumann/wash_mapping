rm(list = ls())
setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')

library(feather)
library(dplyr)
library(ggplot2)

ptdat <- read_feather('ptdat_sani_unconditional_country_2017_09_29.feather')
polydat <- read_feather('polydat_sani_unconditional_country_2017_09_29.feather')

data_vet <- read.csv('/home/adesh/Documents/wash/documents/data_vetting_water.csv')

ptdat <- filter(ptdat, !(nid %in% data_vet$nid))
polydat <- filter(polydat, !(nid %in% data_vet$nid))

write_feather(ptdat, 'ptdat_sani_unconditional_country_clean_2017_09_29.feather')
write_feather(polydat, 'polydat_sani_unconditional_country_clean_2017_09_29.feather')
save(ptdat, file = 'ptdat_sani_unconditional_country_clean_2017_09_29.RData')
save(polydat, file = 'polydat_sani_unconditional_country_clean_2017_09_29.RData')