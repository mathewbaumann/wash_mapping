root <- ifelse(Sys.info()[1]=="Windows", "J:/", "/home/j/")
if(Sys.info()[1]=="Windows") {
  for(package in package_list) {
    library(package, character.only = T)
  }
} else {
  package_lib <- ifelse(grepl("geos", Sys.info()[4]),
                        paste0(root,'temp/geospatial/geos_packages'),                      		
                        paste0(root,'temp/geospatial/packages'))
  .libPaths(package_lib)     
}
library(dplyr); library(ggplot2)

setwd('/home/j/WORK/11_geospatial/wash/data/resamp/error_log/2017-09-13/')
mydat <- lapply(list.files()[grep("loc",list.files())], read.csv, stringsAsFactors = F)
mydat <- do.call(rbind, mydat)
mydat <- filter(mydat, loc != 'present')

load('/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_2017_09_13.RData')

polydat2 <- polydat
for (i in 1:nrow(mydat)) {
  polydat2 <- filter(polydat2, !(shapefile == mydat$shp[i] &
                                   location_code == mydat$loc[i]))
}

shp_error <- read.csv(list.files()[grep('shapefile',list.files())], stringsAsFactors = F)
polydat2 <- filter(polydat2, !(shapefile %in% unique(shp_error$shp)))

# tempdat_list <- list()
# for (i in unique(polydat2$iso3)) {
#   message(i)
#   tempdat <- filter(polydat2, iso3 == i)
#   cutoff <- quantile(tempdat$total_hh, 0.75) + (1.5*IQR(tempdat$total_hh)) 
#   tempdat <- filter(tempdat, total_hh < cutoff)
#   tempdat_list[[length(tempdat_list)+1]] <- tempdat
# }
# 
# tempdat <- do.call(rbind, tempdat_list)
# polydat2 <- tempdat
polydat <- polydat2

save(polydat, file = '/home/j/LIMITED_USE/LU_GEOSPATIAL/collapsed/wash/polydat_2017_09_13_clean.RData')

for (i in unique(polydat$iso3)) {
  message(i)
  tempdat <- filter(polydat, iso3 == i)
  print(
    ggplot(tempdat) + geom_point(aes(x = total_hh, y = piped, col = survey_series)) +
      ggtitle(i) + theme_bw()
  )
}
