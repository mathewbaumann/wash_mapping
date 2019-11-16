## Load libraries
library(sp)
library(seegSDM)
library(raster)
library(rgeos)
library(rgdal)
library(dplyr)
library(ROCR)
set.seed(1)
## Load in covariates and shapefile
img_path <- ('J:/WORK/11_geospatial/01_covariates/00_MBG_STANDARD/')

elevation <- raster(paste0(img_path,'elevation/mean/synoptic/elevation_mean_synoptic','.tif'))
evi <- raster(paste0(img_path,'evi/median/5y/evi_median_5y_2010_00_00','.tif'))
urban <- raster(paste0(img_path,'ghslurbanicity/mean/1y/ghslurbanicity_mean_1y_2015_00_00','.tif')) 
worldpop <- raster(paste0(img_path,'worldpop/total/5y/worldpop_total_5y_2010_00_00','.tif')) 
irrigation <- raster(paste0(img_path,'irrigation/mean/synoptic/irrigation_mean_synoptic','.tif'))
landcover_maj <- raster(paste0(img_path,'landcover/majority/1y/landcover_majority_1y_2010_00_00','.tif'))
landcover_prop <- raster(paste0(img_path,'landcover/proportionalclass13/5y/landcover_proportionalclass13_5y_2010_00_00','.tif'))
nexndvi <- raster(paste0(img_path,'nexndvi/mean/1y/nexndvi_mean_1y_2010_00_00','.tif')) 
reservoir <- raster(paste0(img_path,'reservoirs/boolean/synoptic/reservoirs_boolean_synoptic','.tif'))
tcb <- raster(paste0(img_path,'tcb/median/5y/tcb_median_5y_2010_00_00','.tif'))
tcw <- raster(paste0(img_path,'tcw/median/5y/tcw_median_5y_2010_00_00','.tif')) 
viirsntl <- raster(paste0(img_path,'viirsntl/mean/1m/viirsntl_mean_1m_2014_01_00','.tif'))

world <- shapefile('J:/WORK/11_geospatial/06_original shapefiles/GAUL_admin/admin0/g2015_2014_0/g2015_2014_0.shp')

# read in ID covnerter
id_convert <- read.csv("J:/WORK/11_geospatial/pandemic_indicator/data/raw/geographies/id_convert_ammended.csv", stringsAsFactors  = F)

# read in urban data
all_pts <- read.csv("J:/WORK/11_geospatial/10_mbg/urban_rural_project/all_input_data.csv")
pts <- filter(all_pts, !is.na(lat))

# Create a spatial data frame of ISO3s and urban/rural classification
pts <- SpatialPointsDataFrame(coords = pts[,c('long','lat')], data = as.data.frame(pts[,c("iso3","urban")]))

# brick together covariates
covs <- brick(elevation, evi, urban, worldpop, irrigation, landcover_maj, landcover_prop, nexndvi, reservoir, tcb, tcw, viirsntl)

# add covariate data and lat/long to urban/rural data
ur_data <- extract(covs, pts)
ur_data <- cbind(pts@data, ur_data, pts@coords)
ur_data <- as.data.frame(ur_data)
ur_data$iso3 <- as.character(ur_data$iso3)
#### Mozambique Test ####
# Subset country shapefile, covariate rasters, and data points to country
pred_list <- list()
dich_list <- list()
cv_list <- list()
for (i in 1:n_distinct(ur_data$iso3)) {
print(paste(i, "out of",n_distinct(ur_data$iso3)))
iso <- unique(ur_data$iso3)[i]
ur_moz <- filter(ur_data, iso3 == iso)
moz <- world[which(world$ADM0_CODE == id_convert$GAUL[which(id_convert$ISO3 == iso)]),]
cov_moz <- raster::mask(crop(covs, extent(moz)), moz)

# Plot country shapefile and covariates to verify subset 
#plot(cov_moz)
#plot(moz, add = T)
par(mfrow = c(1,2))

# Run single BRT and plot prediction
ur_moz$id <- 1:nrow(ur_moz)
ur_moz_80 <- sample_frac(ur_moz, size = 0.8, replace = F)
ur_moz_20 <- ur_moz[!(ur_moz$id %in% ur_moz_80$id),]
test <- runBRT(ur_moz_80, gbm.x = 3:14, gbm.y = 2, pred.raster = cov_moz,
       gbm.coords = 15:16, wt = NULL, tree.complexity = 4)
pred_list[[i]] <- test$pred
print(plot(test$pred, zlim = c(0, 1), main = iso))

ur_pred <- extract(test$pred, pts[which(pts$iso3 == iso),])
ur_pred <- cbind(ur_pred, pts[which(pts$iso3 == iso),]@data)
pred <- prediction(ur_pred$ur_pred, ur_pred$urban)
perf <- performance(pred, measure = "tpr",x.measure = "fpr")
#plot(perf, main = "MOZ")

cutoff <- data.frame(spec = 1 - perf@x.values[[1]], sens = perf@y.values[[1]])
cutoff <- mutate(cutoff, youden = sens + spec - 1)
cutoff_y <- max(cutoff$youden)
ur_pred_dich <- test$pred
ur_pred_dich[ur_pred_dich < cutoff_y] <- 0
ur_pred_dich[ur_pred_dich >= cutoff_y] <- 1
dich_list[[i]] <- ur_pred_dich
print(plot(ur_pred_dich, main = iso))
ur_moz_20 <- SpatialPointsDataFrame(coords = ur_moz_20[,c('long','lat')], data = (ur_moz_20[,c("iso3","urban","id")]))
ur_moz_cv <- as.data.frame(cbind(ur_moz_20@data$urban, extract(ur_pred_dich,ur_moz_20)))
ur_moz_cv_comp <- as.data.frame(cbind(ur_moz_20@data$urban, extract(urban,ur_moz_20)))
names(ur_moz_cv) <- c('true','model')

model_sens <- table(ur_moz_cv)[2,2]/(table(ur_moz_cv)[2,1] + table(ur_moz_cv)[2,2])
model_spec <- table(ur_moz_cv)[1,1]/(table(ur_moz_cv)[1,1] + table(ur_moz_cv)[1,2])

ghsl_sens <- table(ur_moz_cv_comp)[2,2]/(table(ur_moz_cv_comp)[2,1] + table(ur_moz_cv_comp)[2,2])
ghsl_spec <- table(ur_moz_cv_comp)[1,1]/(table(ur_moz_cv_comp)[1,1] + table(ur_moz_cv_comp)[1,2])


cv_stats <- c(iso, model_sens, model_spec, ghsl_sens, ghsl_spec)
message("iso3: ",iso,"; sens: ",round(model_sens, digits = 2), "; spec: ",round(model_spec, digits = 2))
cv_list[[i]] <- cv_stats
} 
ur_brt_results <- list(pred_list, dich_list, cv_list)
save(ur_brt_results, file = "H:/urban_brt_results.RData")
# 
# ur_moz_cv_std <- as.data.frame(cbind(ur_moz_20@data$urban, extract(cov_moz[[3]],ur_moz_20)))
# names(ur_moz_cv_std) <- c('true','model')
# 
# ur_moz_80_sp <- SpatialPointsDataFrame(coords = ur_moz_80[,c('long','lat')], data = (ur_moz_80[,c("iso3","urban","id")]))
# ur_moz_sp <- SpatialPointsDataFrame(coords = ur_moz[,c('long','lat')], data = (ur_moz[,c("iso3","urban","id")]))

#tm_shape(moz) + tm_borders() + tm_shape(ur_moz_sp) + tm_dots(col = "urban")
#tm_shape(moz) + tm_borders() + tm_shape(ur_moz_80_sp) + tm_dots(col = "blue") + tm_shape(ur_moz_20) + tm_dots(col = "red")
#### Run Loop to for countries ####
# Subset shapefile and rasters by country
for (i in 1:length(afro)) {
  cntry <- world[which(world$ADM0_CODE == afro[i]),]
  pts_cntry <- pts[cntry,]
  cov_cntry <- raster::mask(crop(covs, extent(cntry)), cntry)
  
}

