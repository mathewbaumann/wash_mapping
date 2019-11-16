rm(list = ls())

library(raster)

setwd('/share/geospatial/mbg/wash/w_piped/output/2017_11_01_16_42_03')
piped <- lapply(list.files()[grep('.gri', list.files())], brick)
piped <- do.call(raster::merge, piped)

pdf('/home/adesh/Desktop/piped_2017.11.03.pdf')
print(
	print(spplot(piped[[c(1,6,11,16)]]))
	)
dev.off()


setwd('/share/geospatial/mbg/wash/w_imp/output/2017_11_01_16_35_53/')
imp <- lapply(list.files()[grep('.gri', list.files())], brick)
imp <- do.call(raster::merge, imp)

pdf('/home/adesh/Desktop/imp_2017.11.01.pdf')
print(
    print(spplot(imp[[c(1,6,11,16)]]))
    )
dev.off()

setwd('/share/geospatial/mbg/wash/w_unimp/output/2017_11_01_16_38_54')
unimp <- lapply(list.files()[grep('.gri', list.files())], brick)
unimp <- do.call(raster::merge, unimp)
pdf('/home/adesh/Desktop/unimp_2017.11.01.pdf')
print(
    print(spplot(unimp[[c(1,6,11,16)]]))
    )
dev.off()



setwd('/share/geospatial/mbg/wash/w_surface/output/2017_11_01_13_06_37')
surface <- lapply(list.files()[grep('.gri', list.files())], brick)
surface <- do.call(raster::merge, surface)
pdf('/home/adesh/Desktop/surface_2017.11.01.pdf')
print(
    print(spplot(surface[[c(1,6,11,16)]]))
    )
dev.off()