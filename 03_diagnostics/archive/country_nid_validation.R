rm(list = ls())
setwd('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash')

library(feather)
library(dplyr)
library(ggplot2)

ptdat <- read_feather('ptdat_hw_unconditional__2017_09_29.feather')
ptdat$point <- 'pt'
polydat <- read_feather('polydat_hw_unconditional__2017_09_29.feather')
polydat$point <- 'poly'
mydat3 <- rbind(ptdat, polydat)

pdf('/home/adesh/Documents/wash/plots/iso3_nid_plots_hw.pdf')
for (j in unique(mydat3$iso3)) {
  message(j)
  test <- filter(mydat3, iso3 == j)

  indices <- seq(1, length(unique(test$nid)), by = 5)
  nids <- unique(test$nid)

  for (i in 0:round(((length(nids))/5))) {
    plotdat <- filter(test, nid %in% nids[(5*(i)):min((5*(i)+4),length(nids))])
    plotdat$nid <- as.character(plotdat$nid)
    print(
          ggplot(plotdat) +
            geom_point(aes(x = total_hh, y = hw_station, col = nid)) + 
            ylim(0,1) + xlim(0,NA) +
            ggtitle(paste0(j,'_',i))
    
    )
  }
}
dev.off()

