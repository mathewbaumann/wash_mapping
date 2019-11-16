library(tidyverse)
library(sp)
library(tmap)

mydat <- read_csv('J:/WORK/11_geospatial/10_mbg/input_data/w_piped.csv',
                  col_types = 'diicddciciiididi')

zaf <- filter(mydat, country == 'ZAF' & year > 1999 & year < 2016 &
                (!(nid %in% c(unmatched_nids, 12146, 11826))))
zaf <- filter(mydat, country == 'ZAF' & point == 1)
write.csv(zaf, 'J:/WORK/11_geospatial/10_mbg/input_data/w_piped.csv')
for (i in unique(zaf$nid)) {
  zaf_plot <- filter(zaf, nid == i)
  zaf_sp <- SpatialPointsDataFrame(coords = data.frame(zaf_plot$longitude,
                                                       zaf_plot$latitude),
                                   data = zaf_plot)

  data(World)
  zaf_poly <- World[which(World$iso_a3 == 'ZAF'),]
  
  print(
    tm_shape(zaf_poly) + tm_polygons() +
      tm_shape(zaf_sp) + tm_dots(col = 'prop', title = i)
  )
}

zaf$year <- as.character(zaf$year)
ggplot(zaf) + geom_point(aes(x = N, y = prop, col = year))

zaf_test <- filter(mydat, country == 'ZAF' &
                          nid %in% c(12105, 12106, 11826, 12146, 25100, 106686,
                                     280803))

load('J:/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_2017_09_06_clean.RData')

collapse <- filter(polydat, nid %in% c(12105, 12106, 11826, 12146, 25100, 106686,
                                       280803))
collapse_test <- filter(collapse, nid == 11826)

load('J:/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/points_2017_09_06.Rdata')
load('J:/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/poly_2017_09_06.Rdata')
definitions <- read.csv('J:/WORK/11_geospatial/wash/definitions/t_type_defined_updated_2017_09_07.csv',
                        encoding="windows-1252")
# extract <- filter(poly_collapse, nid %in% c(12105, 12106, 11826, 12146, 25100, 106686,
#                                             280803))
# extract_test <- filter(extract, nid == 11826 & location_code == 1013701)
# poly_collapse_original <- poly_collapse 
poly_collapse <- filter(poly_collapse, !is.na(t_type))
poly_collapse <- filter(poly_collapse, t_type != "")
Encoding(poly_collapse$t_type)  <- "windows-1252"
length(unique(poly_collapse$t_type)[!(unique(poly_collapse$t_type) %in% unique(definitions$string))])
unique(poly_collapse$iso3[which(!((poly_collapse$t_type) %in% unique(definitions$string)))])

length(unique(pt_collapse$w_source_drink)[!(unique(pt_collapse$w_source_drink) %in% unique(definitions$string))])
unique(pt_collapse$nid[which(!((pt_collapse$w_source_drink) %in% unique(definitions$string)))])

unmatched_nids <- c(unique(poly_collapse$nid[which(!((poly_collapse$w_source_drink) %in% unique(definitions$string)))]),
                    unique(pt_collapse$nid[which(!((pt_collapse$w_source_drink) %in% unique(definitions$string)))]))
