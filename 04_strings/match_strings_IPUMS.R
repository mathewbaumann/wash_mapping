library(data.table)
library(feather)
library(parallel)
library(doParallel)
files <- list.files('/home/j/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/IPUMS_feathers', pattern = '.feather', full.names = TRUE)

dt <- data.table()

# for (file in files){
#   message(file)
#   test <- read_feather(paste0('/home/j/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/IPUMS_feathers/', file))
#   test <- unique(test[,c('nid', 'iso3', 't_type', 'sewage')])
#   dt <- rbind(dt, test)
# }


message("Make cluster")
cl <- makeCluster(50)
clusterEvalQ(cl, .libPaths('/home/j/temp/geospatial/singularity_packages/3.5.0'))
message("Register cluster")
registerDoParallel(cl)
message("Start foreach")
#Read in each .dta file in parallel - returns a list of data frames
top <- foreach(i=1:length(files), .packages = c('feather')) %dopar% {
  dta <- read_feather(files[i])
  dta <- unique(dta[,c('nid', 'iso3', 't_type', 'sewage')])
  return(dta)
}
message("Foreach finished")
message("Closing cluster")
stopCluster(cl)

topics <- rbindlist(top, fill=T, use.names=T)
colnames(topics)[colnames(topics)=="t_type"] <- "toilet"
write.csv(topics, '/home/j/temp/baumannm/IPUMS_def_temp.csv', row.names = FALSE)


defs <- read.csv('/home/j/WORK/11_geospatial/wash/definitions/IPUMS_sani_defs.csv')

test <- as.data.table(topics)
test <- test[!(nid %in% defs$nid)]


defs <- rbind(defs, test, fill = TRUE)
write.csv(defs, '/home/j/WORK/11_geospatial/wash/definitions/IPUMS_sani_defs.csv', row.names = FALSE)
