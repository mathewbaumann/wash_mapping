rm(list = ls())
library(ggplot2)
library(ggpubr)
library(raster)
library(dplyr)

setwd('/home/adesh/Documents/wash/data/models/2017-11-01')
piped <- brick('w_piped.tif')
imp <- brick('w_imp.tif')
unimp <- brick('w_unimp.tif')
surface <- brick('w_surface.tif')

denom <- piped + imp + unimp + surface

writeRaster(denom, 'denom.tif', format = 'GTiff')

piped_rescaled <- piped/denom
imp_rescaled <- imp/denom
unimp_rescaled <- unimp/denom
surface_rescaled <- surface/denom

basic <- piped_rescaled + imp_rescaled

writeRaster(piped_rescaled, 'piped_rescaled.tif', format = 'GTiff')
writeRaster(imp_rescaled, 'imp_rescaled.tif', format = 'GTiff')
writeRaster(unimp_rescaled, 'unimp_rescaled.tif', format = 'GTiff')
writeRaster(surface_rescaled, 'surface_rescaled.tif', format = 'GTiff')
writeRaster(basic, 'basic.tif', format = 'GTiff')