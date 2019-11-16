library(tidyverse)
library(ggpubr)
library(grid)
library(gridExtra)

setwd('J:/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash')

# read in extraction
load('points_collapsed_2017_07_31.Rdata')
load('polys_collapsed_2017_07_31.Rdata')

# bind rows to create master dataset
alldat <- bind_rows(pt_collapse, poly_collapse)
alldat$point <- ifelse(is.na(alldat$lat)|is.na(alldat$long), 'poly','point')

# read in post-collapse data
mydat <- read_csv(file = 'J:/WORK/11_geospatial/10_mbg/input_data/w_imp.csv', col_types = 'ddddcddcdcddddddd')

# define regions
cssa <- c('CAR','COD','GNQ','COG', 'GAB','STP','AGO')
sssa_hi <- c('NAM','ZAF','BWA')
name_hi <- c('MAR','DZA','TUN','LBY','EGY')
essa_hilo <- c('SDN','SSD','ETH','ERI','DJI','SOM','KEN','UGA','RWA',
               'BDI','TZA','MWI','ZMB','ZWE','LSO','SWZ','MDG')
wssa <- c('MRT','MLI','NER','TCD','SEN','GMB','GIN','SLE','LBR','CIV',
          'GHA','BFA','TGO','BEN','NGA','CMR', 'CPV')
reg_list <- list(cssa, sssa_hi, name_hi, essa_hilo)
reg_vec <- unlist(reg_list)

# plot for 5 countries at a time violin plots of hh_size by point vs. poly
# for pre-collapse data
setwd('C:/Users/adesh/Desktop')
pdf('pre_collapse_hhsize_dx.pdf')
for (i in seq(1, length(reg_vec), by = 5)) {
  sub_vec <- reg_vec[i:ifelse(i+4>length(reg_vec),length(reg_vec),i+4)]
  plot_dat <- filter(alldat, iso3 %in% sub_vec)

  print(
    ggplot(plot_dat) + geom_violin(fill = 'red', aes(x = iso3, y = hh_size)) + theme_bw() +
    facet_grid(point ~ ., scales = 'free_y')
  )
}
dev.off()

# plot for 5 countries at a time violin plots of hh_size by point vs. poly
# for post-collapse data
setwd('C:/Users/adesh/Desktop')
pdf('post_collapse_hhsize_dx.pdf')
for (i in seq(1, length(reg_vec), by = 5)) {
  sub_vec <- reg_vec[i:ifelse(i+4>length(reg_vec),length(reg_vec),i+4)]
  plot_dat <- filter(mydat, country %in% sub_vec)
  
  print(
    ggplot(plot_dat) + geom_violin(fill = 'red', aes(x = country, y = N)) + theme_bw() +
      facet_grid(point ~ ., scales = 'free_y')
  )
}
dev.off()

# ggplot scatter of indicator vs. N after running collapse script
setwd('C:/Users/adesh/Desktop')
pdf('post_collapse_hhsize_dx_indi_pt.pdf')
for (i in 1:length(reg_vec)) {
  sub_vec <- reg_vec[i]
  plot_dat <- filter(ptdat, iso3 %in% sub_vec)
  if (nrow(plot_dat) > 0) {
  g1 <- ggplot(plot_dat, aes(total_hh)) + geom_freqpoly() + ggtitle(sub_vec) + theme_bw() 
  g2 <- ggplot(plot_dat, aes(piped)) + geom_freqpoly() + theme_bw()
  g3 <- ggplot(plot_dat) + geom_point(aes(x = total_hh, y = piped, col = survey_series)) + theme_bw() 
  g4 <- ggplot(plot_dat, aes(imp)) + geom_freqpoly() + theme_bw()
  g5 <- ggplot(plot_dat) + geom_point(aes(x = total_hh, y = imp, col = survey_series)) + theme_bw() 
  
  print(g1, labels = sub_vec)
  print(ggarrange(g2,g3, labels = sub_vec, ncol = 1, nrow = 2))
  print(ggarrange(g4,g5, labels = sub_vec, ncol = 1, nrow = 2))
  grid.newpage()
  print(grid.table(plot_dat %>% group_by(nid, survey_series, year_start) %>% 
          summarize(total_people = sum(total_hh, na.rm = T))))
  }
}
dev.off()


plot_dat <- filter(alldat, iso3 %in% c('LSO','MWI','TZA'))
ggplot(plot_dat) + geom_boxplot(fill = 'red', aes(x = iso3, y = hh_size)) + theme_bw() +
  facet_grid(point ~ ., scales = 'free_y')


test <- filter(alldat, point == 'poly') %>% group_by(iso3) %>% summarize(hh_75 = quantile(hh_size, 0.75, na.rm = T))

# repeat for peru data
peru_dat <- filter(alldat, iso3 %in% 'PER')
ggplot(peru_dat) + geom_violin(fill = 'red', aes(x = iso3, y = hh_size)) + theme_bw() +
  facet_grid(point ~ ., scales = 'free_y')


