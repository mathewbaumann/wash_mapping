setwd('/homes/adesh/results/')
.libPaths('/share/code/geospatial/adesh/r_packages')

library(raster)

rm(list = ls())

piped_median <- brick('water/w_piped_median.tif')
piped_ci <- brick('water/w_piped_cirange.tif')
w_imp_median <- brick('water/w_imp_median.tif')
w_imp_ci <- brick('water/w_imp_cirange.tif')
w_imp_greater_median <- brick('water/w_imp_greater_median.tif')
w_imp_greater_ci <- brick('water/w_imp_greater_cirange.tif')

s_imp_median <- brick('sani/s_imp_median.tif')
s_imp_ci <- brick('sani/s_imp_cirange.tif')
s_od_median <- brick('sani/s_od_median.tif')
s_od_ci <- brick('sani/s_od_cirange.tif')

reclass_uncertainty <- function(x) {

    for (i in 1:nlayers(x)) {
        vals <- quantile(x[[i]], c(0.25,0.5,0.75))
        rclass <- as.matrix(data.frame(from = c(0,vals), to = c(vals, 1), new = 1:4))
        x[[i]] <- reclassify(x[[i]], rclass)
    }
    return(x)
}

reclass_median <- function(x) {

    for (i in 1:nlayers(x)) {
        vals <- quantile(x[[i]], c(0.25,0.5,0.75))
        rclass <- as.matrix(data.frame(from = c(0,vals), to = c(vals, 1), 
            new = c(10,20,30,40)))
        x[[i]] <- reclassify(x[[i]], rclass)
    }
    return(x)
}

piped_median_reclass <- reclass_median(piped_median)
w_imp_median_reclass <- reclass_median(w_imp_median)
w_imp_greater_median_reclass <- reclass_median(w_imp_greater_median)
s_imp_median_reclass <- reclass_median(s_imp_median)
s_od_median_reclass <- reclass_median(s_od_median)


piped_ci_reclass <- reclass_uncertainty(piped_ci)
w_imp_ci_reclass <- reclass_uncertainty(w_imp_ci)
w_imp_greater_ci_reclass <- reclass_uncertainty(w_imp_greater_ci)
s_imp_ci_reclass <- reclass_uncertainty(s_imp_ci)
s_od_ci_reclass <- reclass_uncertainty(s_od_ci)

piped_median_ci_hybrid <- piped_median_reclass + piped_ci_reclass
w_imp_median_ci_hybrid  <- w_imp_median_reclass + w_imp_ci_reclass
w_imp_greater_ci_hybrid <- w_imp_greater_median_reclass + w_imp_greater_ci_reclass
s_imp_median_ci_hybrid  <- s_imp_median_reclass + s_imp_ci_reclass
s_od_median_ci_hybrid  <- s_od_median_reclass + s_od_ci_reclass

writeRaster(piped_median_reclass, file = 'water/w_piped_median_reclass.tif',
    format = 'GTiff', overwrite = T)

writeRaster(w_imp_median_reclass, file = 'water/w_imp_median_reclass.tif',
    format = 'GTiff', overwrite = T)

writeRaster(w_imp_greater_median_reclass, file = 'water/w_imp_greater_median_reclass.tif',
    format = 'GTiff', overwrite = T)

writeRaster(s_imp_median_reclass, file = 'sani/s_imp_median_reclass.tif',
    format = 'GTiff', overwrite = T)

writeRaster(s_od_median_reclass, file = 'sani/s_od_median_reclass.tif',
    format = 'GTiff', overwrite = T)

writeRaster(piped_ci_reclass, file = 'water/w_piped_ci_reclass.tif',
    format = 'GTiff', overwrite = T)

writeRaster(w_imp_ci_reclass, file = 'water/w_imp_ci_reclass.tif',
    format = 'GTiff', overwrite = T)

writeRaster(w_imp_greater_ci_reclass, file = 'water/w_imp_greater_ci_reclass.tif',
    format = 'GTiff', overwrite = T)

writeRaster(s_imp_ci_reclass, file = 'sani/s_imp_ci_reclass.tif',
    format = 'GTiff', overwrite = T)

writeRaster(s_od_ci_reclass, file = 'sani/s_od_ci_reclass.tif',
    format = 'GTiff', overwrite = T)

writeRaster(piped_median_ci_hybrid, file = 'water/w_piped_median_ci_hybrid.tif',
    format = 'GTiff', overwrite = T)

writeRaster(w_imp_median_ci_hybrid, file = 'water/w_imp_median_ci_hybrid.tif',
    format = 'GTiff', overwrite = T)

writeRaster(w_imp_greater_ci_hybrid, file = 'water/w_imp_greater_median_ci_hybrid.tif',
    format = 'GTiff', overwrite = T)

writeRaster(s_imp_median_ci_hybrid, file = 'sani/s_imp_median_ci_hybrid.tif',
    format = 'GTiff', overwrite = T)

writeRaster(s_od_median_ci_hybrid, file = 'sani/s_od_median_ci_hybrid.tif',
    format = 'GTiff', overwrite = T)
