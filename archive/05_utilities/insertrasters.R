rm(list = ls())

indi <- as.character(commandArgs()[6])
run_date <- '2018_06_05_10_31_32'
indicator_group <- 'wash'
holdout <- 0
user <- 'adesh'
commondir      <- sprintf('/share/geospatial/mbg/common_inputs')
root           <- ifelse(Sys.info()[1]=='Windows', 'J:/', '/home/j/')
## load packages and custom functions
## drive locations
root           <- ifelse(Sys.info()[1]=='Windows', 'J:/', '/home/j/')
sharedir       <- sprintf('/share/geospatial/mbg/%s/%s',indicator_group,indi)
commondir      <- sprintf('/share/geospatial/mbg/common_inputs')
package_list <- c(t(read.csv(sprintf('%s/package_list.csv',commondir),header=FALSE)))
repo            <- sprintf('/share/code/geospatial/%s/mbg/',user)

# TBD: Remve all 'setwd()'
setwd(repo)
core_repo <- repo

for (p in package_list) {
  try(library(p, character.only = T))
}

library(seegSDM, lib.loc = '/share/code/geospatial/adesh/r_packages_hf_sing/')
library(seegMBG, lib.loc = '/share/code/geospatial/adesh/r_packages_hf_sing/')
library(mgcv)
library(matrixStats)

# Load MBG packages and functions
message('Loading in required R packages and MBG functions')
source(paste0(repo, '/mbg_central/setup.R'))
mbg_setup(package_list = package_list, repos = repo)

# s_imp
#for (indi in c('s_imp', 's_od_calc','s_unimp_calc','w_imp','w_piped_calc','w_unimp_calc','w_surface_calc')) {
	print(indi)
	setwd(paste0('/share/geospatial/mbg/wash/',indi,'/output/2018_06_05_10_31_32'))

	regions <- c('name_hi3', 'egy','cssa','sssa_hi','wssa','essa_hilo')

	mean_results <- list()
	uci_results <- list()
	lci_results <- list()

	for (i in 1:length(regions)) {
		reg <- regions[i]
		print(reg)
		## Load simple polygon template to model over
		gaul_list           <- get_gaul_codes(reg)
		simple_polygon_list <- load_simple_polygon(gaul_list = gaul_list, buffer = 0.4, tolerance = 0.2, use_premade = T)
		subset_shape        <- simple_polygon_list[[1]]
		simple_polygon      <- simple_polygon_list[[2]]

		print('check1')
		## Load list of raster inputs (pop and simple)
  		raster_list        <- build_simple_raster_pop(subset_shape)
  		simple_raster      <- raster_list[['simple_raster']]

		period_map <- 2000:2015
		
		print('check3')
		## Make cell preds and a mean raster
		test <- load(paste0(indi,'_cell_draws_eb_bin0_',reg,'_0.RData'))
		pred <- get(test)
		
		mean_ras  <- insertRaster(simple_raster,matrix(rowMeans(pred),ncol = 16))
		uci_ras <- insertRaster(simple_raster,matrix(rowQuantiles(pred, probs = 0.975),ncol = 16))
		lci_ras <- insertRaster(simple_raster,matrix(rowQuantiles(pred, probs = 0.025),ncol = 16))
		
		mean_results[[i]] <- mean_ras
		uci_results[[i]] <- uci_ras
		lci_results[[i]] <- lci_ras
	}

	mean_ras <- do.call(raster::merge, mean_results)
	uci_ras <- do.call(raster::merge, uci_results)
	lci_ras <- do.call(raster::merge, lci_results)
	
	writeRaster(mean_ras, format = 'GTiff', filename = paste0(indi,'_mean.tif'), overwrite = T)	
	writeRaster(uci_ras, format = 'GTiff', filename = paste0(indi,'_uci.tif'), overwrite = T)	
	writeRaster(lci_ras, format = 'GTiff', filename = paste0(indi,'_lci.tif'), overwrite = T)	
#}
