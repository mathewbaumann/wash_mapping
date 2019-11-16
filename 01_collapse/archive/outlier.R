library(ggrepel)
library(feather)
library(dplyr)
library(ggplot2)
setwd('/home/j/WORK/11_geospatial/wash/data/cwed')

sani <- read_feather('sani_2018_03_08.feather')
water <- read_feather('water_2018_03_08.feather')

sani <- filter(sani, !(nid %in%
							c(214640, 30394, 22114, 21970, 235215,
								286657, 11774, 106512, 81004, 142934, 55973,
								157065, 285893, 26930, 58185, 31831, 21331,
								77395, 280228, 7919, 21393, 77387, 157059,
								161662, 218581, 157058, 30777, 283013, 3935, 
								34279, 26433, 56241, 1927, 12896, 206075,
								2039, 2063, 11516, 11540, 4818)))

water <- filter(water, !(nid %in%
							c(30394, 151568, 22114, 32189, 19557,
								235215, 11774, 142934, 20722, 58185,
								31831, 280228, 30777, 9439, 56148, 21173, 286788,
								19088, 1927, 257045, 206075, 24890, 9522,
								24915, 256267, 31797)))

sani2 <- water
sani2$point <- ifelse(is.na(sani2$lat), 0, 1)

sani2 <- sani2 %>%
			group_by(nid, year_start, point, iso3) %>%
			summarize(n_total = sum(N),
					  imp = weighted.mean(x = imp, w = N))

  sssa_hi <- c('NAM','BWA','ZAF')
  cssa <- c('CAF','GAB','GNQ','COD','COG','AGO','STP')
  name_hi <- c('MAR','DZA','TUN','LBY','EGY')
  essa_hilo <- c('SDN','ERI','DJI','SOM','ETH','SSD',
                 'SSD','UGA','KEN','RWA','BDI','TZA',
                 'MWI','MOZ','ZMB','MDG','ZWE','SWZ','LSO',
                 'COM')
  wssa <- c('CPV','SEN','GMB','GIN','GNB','SLE','MLI','LBR',
            'CIV','GHA','TGO','BEN','NGA','NER','TCD','CMR',
            'BFA','MRT')
  africa <- c(sssa_hi, cssa, name_hi, essa_hilo, wssa)

sani2 <- filter(sani2, iso3 %in% africa)

pdf('/home/adesh/Pictures/water_test.pdf')
for (country in unique(sani2$iso3)) {
	print(		ggplot(filter(sani2, iso3 == country)) +
			geom_point(aes(x = year_start, y = imp, size = n_total)) +
			geom_text_repel(aes(x = year_start, y = imp, label = nid)) +
			theme_bw() + ylim(0,1) + xlim(2000,2016) +
			ggtitle(paste('water', country))
	)
}
dev.off()

write_feather(sani, 'sani_2018_03_08_clean.feather')
write_feather(water, 'water_2018_03_08_clean.feather')