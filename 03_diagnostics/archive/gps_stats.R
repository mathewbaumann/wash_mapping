setwd('J:/WORK/11_geospatial/wash/results/water/2017_08_08_14_56_43')

# root           <- ifelse(Sys.info()[1]=='Windows', 'J:/', '/home/j/')
# package_lib    <- ifelse(grepl('geos', Sys.info()[4]),
#                          sprintf('%stemp/geospatial/geos_packages',root),
#                          sprintf('%stemp/geospatial/packages',root))
## Load libraries and  MBG project functions.
# .libPaths(package_lib)
library(raster)
library(tidyverse)

results <- brick('w_piped_prediction_eb_bin0_195_0.grd')
plot(results[[1]])

lima <- SpatialPoints(coords = data.frame(-77.042545, -12.041210))

lima_values <- c()
for (i in 1:16) {
  
  lima_values[i] <- raster::extract(results[[i]], lima)
  
}

iquitos <- SpatialPoints(coords = data.frame(-73.254874, -3.749624))
iquitos_values <- c()
for (i in 1:16) {
  
  iquitos_values[i] <- raster::extract(results[[i]], iquitos)
  
}
plot(iquitos_values, type = 'l')


results_nona <- list()
for(i in 1:16) {
  
  results_nona[[i]] <- results[[i]][!is.na(results[[i]])]
  
}

maxn <- length(results_nona[[1]])
rand_pix <- runif(4, 1, maxn)

temp <- list()
for (i in 1:16) {
  temp[[i]] <- results_nona[[i]][rand_pix]
}

values <- do.call(rbind, temp)

for (i in 1:4) {
print(plot(values[,i], main = i, type = 'l'))
}

pix_ts <- cbind(values, lima_values, iquitos_values)
pix_ts <- as.data.frame(pix_ts)
names(pix_ts) <- c('rand1', 'rand2', 'rand3', 'rand4', 'lima', 'iquitos')
pix_ts$time <- 2000:2015
pix_ts <- gather(pix_ts, 'pixel', 'prop', 1:6)

load('w_piped_cell_draws_eb_bin0_195_0.RData')
load('2017_08_08_14_56_43_bin0_195_0.RData')


gps_stats <- function(lat = latitude, long = longitude, simple_raster = simple_ras, cell_pred = pred_obj) {
 
  point <- SpatialPoints(coords = data.frame(long, lat))
  simple_ras_idx <- raster::extract(simple_raster, point, cellnumbers = T)[1]
  num_nas <- length(simple_raster[1:simple_ras_idx][is.na(simple_raster[1:simple_ras_idx])])
  row_num <- simple_ras_idx - num_nas
  all_rows <- seq(row_num, nrow(cell_pred), length(simple_raster[!is.na(simple_raster)]))
  
  sum_stats <- list()
  for (i in 1:length(all_rows)) {
    quants <- stats::quantile(cell_pred[all_rows[i],], c(0.05, 0.25, 0.50, 0.75, .95))
    mean <- mean(cell_pred[all_rows[i],]); names(mean) <- 'mean'
    sum_stats[[i]] <- c(quants, mean)
  }
  
  results <- as.data.frame(do.call(rbind, sum_stats))
  results$lat <- lat; results$long <- long;
  results$time <- 1:nrow(results)
  
  return(results)
}

simple_ras <- simple_raster
pred_obj <- cell_pred

lima <- gps_stats(lat = -12.041210, long = -77.042545)
iquitos <- gps_stats(-3.749624,-73.254874)

rand_pts <- sample_n(as.data.frame(rasterToPoints(simple_ras))[,1:2], 4)
rand_data <- list()
for(i in 1:nrow(rand_pts)) {
  rand_data[[i]] <-  gps_stats(rand_pts[i,2], rand_pts[i,1])
  rand_data[[i]]$name <- paste0('rand',i)
}
rand_data <- do.call(rbind, rand_data)

lima$name <- 'lima'; iquitos$name <- 'iquitos'
alldat <- rbind(rand_data, lima, iquitos)
alldat <- gather(alldat, 'measure','value',1:6)
alldat$mtype <- 'mean'
alldat$mtype[which(alldat$measure %in% c('50%'))] <- 'median'
alldat$mtype[which(alldat$measure %in% c('25%','75%'))] <- 'iqr'
alldat$mtype[which(alldat$measure %in% c('5%','95%'))] <- 'ci'

for (i in unique(alldat$name)) {
plot <- ggplot(filter(alldat, name == i)) +
  geom_line(aes(x = time, y = value, group = measure, col = mtype)) +
  ggtitle(aes(name)) + ggtitle(i)
  print(plot)
}



