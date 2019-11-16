.libPaths('/share/code/geospatial/adesh/r_packages')
rm(list = ls())

library(feather)
library(dplyr)

setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')

indi_fam <- 'water'
if (indi_fam == 'water') {
  ptdat <- read_feather('/home/j/WORK/11_geospatial/wash/data/cwed/water_2018_03_08_clean.feather')
  ptdat <- filter(ptdat, !(is.na(lat)))
  
  w_piped_calc <- ptdat
  w_piped_calc <- dplyr::select(w_piped_calc, -unimp, -surface, -imp)
  w_piped_calc <- mutate(w_piped_calc, point = 1, weight = 1, w_piped_calc = (piped*N))
  w_piped_calc <- rename(w_piped_calc, country = iso3, year = year_start, N = N, latitude = lat,
                  longitude = long)
  w_piped_calc <- mutate(w_piped_calc, prop = w_piped_calc/N) %>% 
                select(-piped) %>%
                filter(N > 0)
  write.csv(w_piped_calc, file = '/home/j/WORK/11_geospatial/10_mbg/input_data/w_piped_calc.csv')

  w_unimp_calc <- ptdat
  w_unimp_calc <- select(w_unimp_calc, -surface, -imp, -piped)
  w_unimp_calc <- mutate(w_unimp_calc, point = 1, weight = 1, w_unimp_calc = (unimp*N))
  w_unimp_calc <- rename(w_unimp_calc, country = iso3, year = year_start, N = N, latitude = lat,
                  longitude = long)
  w_unimp_calc <- mutate(w_unimp_calc, prop = w_unimp_calc/N) %>%
                select(-unimp) %>%
                filter(N > 0)
  write.csv(w_unimp_calc, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_unimp_calc.csv')

  w_surface_calc <- ptdat
  w_surface_calc <- select(w_surface_calc, -unimp, -imp, -piped)
  w_surface_calc <- mutate(w_surface_calc, point = 1, weight = 1, w_surface_calc = (surface*N))
  w_surface_calc <- rename(w_surface_calc, country = iso3, year = year_start, N = N, latitude = lat,
                  longitude = long)
  w_surface_calc <- mutate(w_surface_calc, prop = w_surface_calc/N) %>%
                select(-surface) %>%
                filter(N > 0)
  write.csv(w_surface_calc, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_surface_calc.csv')

}

rm(list = ls())
indi_fam <- 'sani'
if (indi_fam == 'sani') {
  ptdat <- read_feather('/home/j/WORK/11_geospatial/wash/data/cwed/sani_2018_03_08_clean.feather')
  ptdat <- filter(ptdat, !(is.na(lat)))

  s_od_calc <- ptdat
  s_od_calc <- select(s_od_calc, -imp, -unimp)
  s_od_calc <- mutate(s_od_calc, point = 1, weight = 1, s_od_calc = (od*N))
  s_od_calc <- rename(s_od_calc, country = iso3, year = year_start, prop = od, N = N, latitude = lat,
                  longitude = long)
  s_od_calc <- mutate(s_od_calc, N = (N))
  write.csv(s_od_calc, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_od_calc.csv')

  s_unimp_calc <- ptdat
  s_unimp_calc <- select(s_unimp_calc, -od, -imp)
  s_unimp_calc <- mutate(s_unimp_calc, point = 1, weight = 1, s_unimp_calc = (unimp*N))
  s_unimp_calc <- rename(s_unimp_calc, country = iso3, year = year_start, N = N, latitude = lat,
                  longitude = long)
  s_unimp_calc <- mutate(s_unimp_calc, N = N, prop = s_unimp_calc/N) %>% 
                  select(-unimp) %>% 
                  filter(N > 0)
  write.csv(s_unimp_calc, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp_calc.csv')

}
rm(list = ls())