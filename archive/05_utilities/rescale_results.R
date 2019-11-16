setwd('/homes/adesh/results/water')
.libPaths('/share/code/geospatial/adesh/r_packages')

library(raster)

piped <- brick('w_piped_mean.tif')
imp <- brick('w_imp_mean.tif')
unimp <- brick('w_unimp_mean.tif')
surface <- brick('w_surface_mean.tif')

total <- piped + imp + unimp + surface

new_piped <- piped/total
new_imp <- imp/total
new_unimp <- unimp/total
new_surface <- surface/total

writeRaster(new_piped, file = 'w_piped_mean_rescale.tif',
    format = 'GTiff', overwrite = T)

writeRaster(new_imp, file = 'w_imp_mean_rescale.tif',
    format = 'GTiff', overwrite = T)

writeRaster(new_unimp, file = 'w_unimp_mean_rescale.tif',
    format = 'GTiff', overwrite = T)

writeRaster(new_surface, file = 'w_surface_mean_rescale.tif',
    format = 'GTiff', overwrite = T)
