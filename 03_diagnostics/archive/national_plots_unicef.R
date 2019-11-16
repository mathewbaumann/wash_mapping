rm(list = ls())
setwd('/home/j/WORK/11_geospatial/wash/data/cwed')

library(feather)
library(dplyr)
library(ggplot2)
library(ggrepel)

alldat <- read_feather('sani_2018_02_07.feather')
alldat$point <- as.character(ifelse(is.na(alldat$lat), 0, 1))
alldat <- alldat %>%
		  group_by(nid, iso3, survey_series, year_start, point) %>%
		  summarize(imp = weighted.mean(imp, N),
		  			N = sum(N))

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
alldat <- filter(alldat, iso3 %in% africa)

pdf('/home/adesh/Documents/wash/plots/wash_dx_s_imp_problems_02_07.pdf')
for (i in unique(alldat$iso3)) {
	message(i)
	plotdat <- filter(alldat, iso3 == i)
	if (nrow(plotdat) > 0) {
	print(
		ggplot(plotdat) + 
			geom_point(aes(x = year_start, y = imp, col = point, size = N)) +
			geom_smooth(aes(x = year_start, y = imp, col = 'imp', weight = N),
							method = 'glm', size = 0.5, se = F, fullrange = F) +
			geom_text_repel(aes(x = year_start, y = imp, label = nid)) +
			#geom_point(aes(x = year_start, y = imp, shape = point, size = total_hh,
			#			   col = 'MBG Imp.')) +
			#geom_smooth(aes(x = year_start, y = imp, col = 'MBG Imp.', weight = total_hh),
			#	            method = 'glm', size = 0.5, se = F, fullrange = F) +
			#geom_line(data = filter(unicef, iso3 == i), aes(x = year, y = imp, col = 'UN Imp')) +
			#geom_line(data = filter(unicef, iso3 == i), aes(x = year, y = improved, col = 'UN Imp.')) +
			ggtitle(i) + ylim(0,1) + xlim(1998,2015) + 
			xlab('Year') + ylab('Prevalence') + theme_bw()
		)
	}
}
dev.off()	