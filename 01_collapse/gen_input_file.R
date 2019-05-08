.libPaths('/share/code/geospatial/adesh/r_packages')
rm(list = ls())

library(feather)
library(dplyr)

setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')

indi_fam <- 'water'
if (indi_fam == 'water') {
  ptdat <- read_feather('/home/j/WORK/11_geospatial/wash/data/cwed/water_2019_03_29.feather')
  ptdat <- filter(ptdat, !(is.na(lat)))
  
  w_piped <- ptdat
  w_piped <- dplyr::select(w_piped, -unimp, -surface)
  w_piped <- mutate(w_piped, point = 1, weight = 1, w_piped = (piped*N))
  w_piped <- rename(w_piped, country = iso3, year = year_start, prop = piped, N = N, latitude = lat,
                  longitude = long) %>% select(-imp) %>%
                  filter(N > 0)
  write.csv(w_piped, file = '/home/j/WORK/11_geospatial/10_mbg/input_data/w_piped.csv')

  w_imp_cr <- ptdat
  w_imp_cr <- select(w_imp_cr, -surface, -unimp)
  w_imp_cr <- mutate(w_imp_cr, point = 1, weight = 1, w_imp_cr = (imp*N), w_piped = (piped*N))
  w_imp_cr <- rename(w_imp_cr, country = iso3, year = year_start, N = N, latitude = lat,
                  longitude = long)
  w_imp_cr <- mutate(w_imp_cr, N = ((N)) - (w_piped)) %>% mutate(prop = w_imp_cr/N) %>%
                  select(-piped, -w_piped, -imp) %>%
                  filter(N > 0)
  write.csv(w_imp_cr, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_imp_cr.csv')

  w_unimp_cr <- ptdat
  w_unimp_cr <- select(w_unimp_cr, -surface)
  w_unimp_cr <- mutate(w_unimp_cr, point = 1, weight = 1, w_unimp_cr = (unimp*N), w_imp = (imp*N), w_piped = (piped*N))
  w_unimp_cr <- rename(w_unimp_cr, country = iso3, year = year_start, N = N, latitude = lat,
                  longitude = long)
  w_unimp_cr <- mutate(w_unimp_cr, N = ((N)) - (w_imp) - w_piped) %>% mutate(prop = w_unimp_cr/N) %>%
                select(-imp, -w_imp, -unimp, w_piped, piped) %>%
                filter(N > 0)
  write.csv(w_unimp_cr, '/home/j/WORK/11_geospatial/10_mbg/input_data/w_unimp_cr.csv')

}

rm(list = ls())
indi_fam <- 'sani'
if (indi_fam == 'sani') {
  ptdat <- read_feather('/home/j/WORK/11_geospatial/wash/data/cwed/sani_2019_03_29.feather')
  ptdat <- filter(ptdat, !(is.na(lat)))

  s_piped <- ptdat
  s_piped <- select(s_piped, -od, -unimp,-imp)
  s_piped <- mutate(s_piped, point = 1, weight = 1, s_piped = (piped*N))
  s_piped <- rename(s_piped, country = iso3, year = year_start, prop = piped, N = N, latitude = lat,
                  longitude = long)
  s_piped <- mutate(s_piped, N = (N))
  write.csv(s_piped, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_piped.csv')
  # s_imp <- ptdat
  # s_imp <- select(s_imp, -od, -unimp)
  # s_imp <- mutate(s_imp, point = 1, weight = 1, s_imp = (imp*N))
  # s_imp <- rename(s_imp, country = iso3, year = year_start, prop = imp, N = N, latitude = lat,
  #                 longitude = long)
  # s_imp <- mutate(s_imp, N = (N))
  # write.csv(s_imp, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_imp.csv')
  
  s_imp_cr <- ptdat
  s_imp_cr <- select(s_imp_cr, -od, -unimp)
  s_imp_cr <- mutate(s_imp_cr, point = 1, weight = 1, s_imp = (imp*N), s_piped = (piped*N))
  s_imp_cr <- rename(s_imp_cr, country = iso3, year = year_start, prop = imp, N = N, latitude = lat,
                  longitude = long)
  s_imp_cr <- mutate(s_imp_cr, N = (N) - s_piped) %>% 
    select(-s_piped,-piped)
  write.csv(s_imp_cr, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_imp_cr.csv')

  s_unimp_cr <- ptdat
  s_unimp_cr <- select(s_unimp_cr, -od)
  s_unimp_cr <- mutate(s_unimp_cr, point = 1, weight = 1, s_unimp_cr = (unimp*N), s_imp = (imp*N))
  s_unimp_cr <- rename(s_unimp_cr, country = iso3, year = year_start, N = N, latitude = lat,
                  longitude = long)
  s_unimp_cr <- mutate(s_unimp_cr, N = ((N) - s_imp)) %>% mutate(prop = s_unimp_cr/N) %>% 
                  select(-s_imp, -imp, -unimp) %>% 
                  filter(N > 0)
  write.csv(s_unimp_cr, '/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp_cr.csv')

}
rm(list = ls())
