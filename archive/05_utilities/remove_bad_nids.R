rm(list = ls())

library(dplyr)

setwd('/home/j/WORK/11_geospatial/10_mbg/input_data')

w_imp <- read.csv('w_surface.csv')
bad_nids <- read.csv('/home/adesh/Documents/wash/problem_nids.csv')

w_imp <- filter(w_imp, !(nid %in% bad_nids$NID))
write.csv(w_imp, file = 'w_surface.csv')