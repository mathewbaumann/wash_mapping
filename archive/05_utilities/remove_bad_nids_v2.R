# Define if you are running code loally
local <- F

# Set repo & library path 
if(Sys.info()[1]!="Windows") {
  if(!local) {
    root <- "/home/j/"
    package_lib <- ifelse(grepl("geos", Sys.info()[4]),
                          paste0(root,'temp/geospatial/geos_packages'),
                          paste0(root,'temp/geospatial/packages'))
    .libPaths(package_lib)
  } else {
    package_lib <- .libPaths()
    root <- '/home/j/'
  }
} else {
  package_lib <- .libPaths()
  root <- 'J:/'
}


library(dplyr)

setwd('/home/j/WORK/11_geospatial/10_mbg/input_data/wash')

mydat <- lapply(list.files(), read.csv, stringsAsFactors = F)
indi <- gsub('_dirty.csv','',list.files())

for (i in 1:4) {
    subdat <- mydat[[i]]
    subdat_2 <- filter(subdat, !(nid %in% c(30394, 151568, 151805, 26661)),
                        year <= 2015)
    write.csv(subdat, paste0(indi[i],'_dirty.csv'))
    write.csv(subdat_2, paste0(indi[i],'.csv'))
}