.libPaths('/share/code/geospatial/adesh/r_packages')
rm(list = ls())
library(rgeos); library(rgdal); library(maptools)
library(ggplot2)
library(ggpubr)
library(raster)
library(dplyr)

rm(list = ls())


# load in population and shapefile
pop <- raster('/home/j/WORK/11_geospatial/01_covariates/09_MBG_covariates/WorldPop_total_global_stack.tif', band = 4)
ad1 <- shapefile('/home/j/WORK/11_geospatial/09_MBG_maps/misc_files/africa_ad1.shp')


# read in indicators
piped <- raster('/homes/adesh/results/water/w_piped_median.tif', band = 16)
w_imp <- raster('/homes/adesh/results/water/w_imp_median.tif', band = 16)
s_imp <- raster('/homes/adesh/results/sani/s_imp_median.tif', band = 16)
s_od <- raster('/homes/adesh/results/sani/s_od_median.tif', band = 16)


piped_pop <- piped*pop
w_imp_pop <- w_imp*pop
s_imp_pop <- s_imp*pop
s_od_pop <- s_od*pop

pop_table <- extract(pop, ad1, df = T, fun = sum, na.rm = T)
piped_table <- extract(piped_pop, ad1, df = T, fun = sum, na.rm = T)
w_imp_table <- extract(w_imp_pop, ad1, df = T, fun = sum, na.rm = T)
s_imp_table <- extract(s_imp_pop, ad1, df = T, fun = sum, na.rm = T)
s_od_table <- extract(s_od_pop, ad1, df = T, fun = sum, na.rm = T)

names(pop_table)[2] <- 'pop'
names(piped_table)[2] <- 'piped'
names(w_imp_table)[2] <- 'w_imp`'
names(s_imp_table)[2] <- 's_imp'
names(s_od_table)[2] <- 's_od'

ad1_results <- ad1
ad1_results@data$ID <- 1:nrow(ad1_results@data)

int1 <- left_join(pop_table, piped_table, by = 'ID')
int1 <- left_join(int1, w_imp_table, by = 'ID')
int1 <- left_join(int1, s_imp_table, by = 'ID')
int1 <- left_join(int1, s_od_table, by = 'ID')

ad1_results@data <- left_join(ad1_results@data, int1, by = 'ID')
ad1_results@data <- rename(ad1_results@data, join_index = ID)
ad1_results_original <- ad1_results

ad1_results@data$id = rownames(ad1_results@data)
ad1_results.points = fortify(ad1_results, region="id")
ad1_results.df = left_join(ad1_results.points, ad1_results@data, by="id")

names(ad1_results.df)[21] <- 'w_imp'
ad1_results.df <- mutate(ad1_results.df,
                            piped_prev = piped/pop,
                            w_imp_prev = w_imp/pop,
                            s_imp_prev = s_imp/pop,
                            s_od_prev = s_od/pop)

png('/homes/adesh/results/water/w_piped_median_ad1.png',width = 1200, height = 1200)
print(
    ggplot(ad1_results.df) + 
      aes(long,lat,group=group,fill=s_od_prev) + 
      geom_polygon() +
      geom_path(color="black") +
      coord_equal() +
      theme_classic() +
        theme(axis.line = element_blank(), axis.text = element_blank(),
                      axis.ticks = element_blank()) +
        xlab('') + ylab('') + ggtitle(2015) +
      scale_fill_gradientn(colours = (c('#fff7ec','#fee8c8','#fdd49e',
            '#fdbb84','#fc8d59','#ef6548','#d7301f','#b30000','#7f0000')),
                             na.value = "#C0C0C0", limits = c(0,1),
                           name = 'Piped on Premises')
)
dev.off()