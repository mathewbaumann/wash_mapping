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
w_imp_brick <- brick('/homes/adesh/results/water/w_imp_greater_median.tif')
w_unimp_brick <- brick('/homes/adesh/results/water/w_unimp_median.tif')
w_surface_brick <-  brick('/homes/adesh/results/water/w_surface_median.tif')

s_imp_brick <- brick('/homes/adesh/results/sani/s_imp_median.tif')
s_unimp_brick <- brick('/homes/adesh/results/sani/s_unimp_median.tif')
s_od_brick <- brick('/homes/adesh/results/sani/s_od_median.tif')

year_list <- c(2000, 2015)

for (i in year_list) {
  i <- i - 1999

  w_imp <- w_imp_brick[[i]]
  w_unimp <- w_unimp_brick[[i]]
  w_surface <- w_surface_brick[[i]]
  s_imp <- s_imp_brick[[i]]
  s_unimp <- s_unimp_brick[[i]]
  s_od <- s_od_brick[[i]]

  w_imp_pop <- w_imp*pop
  w_unimp_pop <- w_unimp*pop
  w_surface_pop <- w_surface*pop

  s_imp_pop <- s_imp*pop
  s_unimp_pop <- s_unimp*pop
  s_od_pop <- s_od*pop

  pop_table <- extract(pop, ad1, df = T, fun = sum, na.rm = T)
  w_imp_table <- extract(w_imp_pop, ad1, df = T, fun = sum, na.rm = T)
  w_unimp_table <- extract(w_unimp_pop, ad1, df = T, fun = sum, na.rm = T)
  w_surface_table <- extract(w_surface_pop, ad1, df = T, fun = sum, na.rm = T)

  s_imp_table <- extract(s_imp_pop, ad1, df = T, fun = sum, na.rm = T)
  s_unimp_table <- extract(s_unimp_pop, ad1, df = T, fun = sum, na.rm = T)
  s_od_table <- extract(s_od_pop, ad1, df = T, fun = sum, na.rm = T)

  names(pop_table)[2] <- 'pop'
  names(w_imp_table)[2] <- 'w_imp`'
  names(w_unimp_table)[2] <- 'w_unimp'
  names(w_surface_table)[2] <- 'w_surface'

  names(s_imp_table)[2] <- 's_imp'
  names(s_unimp_table)[2] <- 's_unimp'
  names(s_od_table)[2] <- 's_od'

  ad1_results <- ad1
  ad1_results@data$ID <- 1:nrow(ad1_results@data)

  int1 <- left_join(pop_table, w_imp_table, by = 'ID')
  int1 <- left_join(int1, w_unimp_table, by = 'ID')
  int1 <- left_join(int1, w_surface_table, by = 'ID')
  int1 <- left_join(int1, s_imp_table, by = 'ID')
  int1 <- left_join(int1, s_unimp_table, by = 'ID')
  int1 <- left_join(int1, s_od_table, by = 'ID')

  names(int1)[grep('w_imp', names(int1))] <- 'w_imp'

  ad1_results@data <- left_join(ad1_results@data, int1, by = 'ID')
  ad1_results@data <- rename(ad1_results@data, join_index = ID)
  ad1_results_original <- ad1_results

  ad1_results@data$id = rownames(ad1_results@data)
  ad1_results.points = fortify(ad1_results, region="id")
  ad1_results.df = left_join(ad1_results.points, ad1_results@data, by="id")

  ad1_results.df <- mutate(ad1_results.df,
                              w_imp_prev = w_imp/pop,
                              w_unimp_prev = w_unimp/pop,
                              w_surface_prev = w_surface/pop,
                              s_imp_prev = s_imp/pop,
                              s_unimp_prev = s_unimp/pop,
                              s_od_prev = s_od/pop)
  
  ad1_results@data <- mutate(ad1_results@data,
                              w_imp_prev = w_imp/pop,
                              w_unimp_prev = w_unimp/pop,
                              w_surface_prev = w_surface/pop,
                              s_imp_prev = s_imp/pop,
                              s_unimp_prev = s_unimp/pop,
                              s_od_prev = s_od/pop)

  writeOGR(ad1_results, dsn = '/homes/adesh/results/water/', layer = paste0('ws_adm1_results_',i+1999),
           driver = 'ESRI Shapefile', overwrite_layer = T)
  write.csv(ad1_results@data, paste0('/homes/adesh/results/water/ws_adm1_results_',i+1999,'.csv'))

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
}

library(dplyr); library(ggplot2); library(ggtern); 
tern_df <- read.csv('/homes/adesh/results/water/ws_adm1_results_2015.csv')
loc_id <- read.csv('/home/adesh/Downloads/loc_metadata.csv', stringsAsFactors = F)
sdi <- read.csv('/home/adesh/Downloads/sdi.csv', stringsAsFactors = F)
sdi_locs <- left_join(sdi, loc_id[,c('location_name','location_id','location_type','ihme_loc_id')])
sdi_locs <- filter(sdi_locs, year_id == 2015) %>% rename(ISO3 = ihme_loc_id)
iso_gaul <- read.csv('/home/j/WORK/11_geospatial/pandemic_indicator/data/raw/geographies/id_convert_ammended.csv')
iso_gaul <- rename(iso_gaul, ADM0_CODE = GAUL)
tern_df <- left_join(tern_df, iso_gaul[,c('ADM0_CODE', 'ISO3')])
tern_df <- left_join(tern_df, sdi_locs[,c('ISO3','mean_value')], by = 'ISO3')

head(tern_df)

pop_dist <- ecdf(tern_df$pop)
tern_df$pop_q <- pop_dist(tern_df$pop)

sdi_dist <- ecdf(tern_df$mean_value)
tern_df$sdi_q <- sdi_dist(tern_df$mean_value)
tern_df$sdi_q <- ifelse(tern_df$sdi_q <= 0.33, "Low",
                  ifelse(tern_df$sdi_q > 0.33 & tern_df$sdi_q <= 0.66 , "Middle", "High"))

sssa <- c('NAM','BWA','LSO','SWZ','ZWE','ZAF')
cssa <- c('CAF','GAB','GNQ','COD','COG','AGO')
name <- c('MAR','DZA','TUN','LBY','EGY','SDN')
essa_n <- c('ERI','DJI','SOM','ETH','SSD')
essa_s <- c('UGA','KEN','RWA','BDI','TZA',
               'MWI','MOZ','ZMB','MDG','COM')
wssa_w <- c('CPV','SEN','GMB','GIN','GNB','SLE','MLI','MRT','LBR','CIV')
wssa_e <- c('GHA','TGO','BEN','NGA','NER','TCD','CMR','BFA','STP')

tern_df$Region <- ifelse(tern_df$ISO3 %in% sssa, 'SSSA',
                    ifelse(tern_df$ISO3 %in% cssa, 'CSSA',
                      ifelse(tern_df$ISO3 %in% wssa_w, 'WWSSA',
                        ifelse(tern_df$ISO3 %in% wssa_e, 'EWSSA',
                          ifelse(tern_df$ISO3 %in% essa_n, 'NESSA',
                            ifelse(tern_df$ISO3 %in% essa_s, 'SESSA',
                              ifelse(tern_df$ISO3 %in% name, 'NAME', 'NA')))))))
tern_df <- filter(tern_df, Region != 'NA')
tern_df_plot <- filter(tern_df, Region == 'CSSA')
tern_df_water <- select(tern_df, w_imp_prev, w_unimp_prev, w_surface_prev, pop, pop_q, ISO3,
                        Region) %>%
                 rename(basic = w_imp_prev, unimp = w_unimp_prev, worst = w_surface_prev) %>%
                 mutate(family = 'water')
tern_df_sani <- select(tern_df, s_imp_prev, s_unimp_prev, s_od_prev, pop, pop_q, ISO3,
                        Region) %>%
                 rename(basic = s_imp_prev, unimp = s_unimp_prev, worst = s_od_prev) %>%
                 mutate(family = 'sani')
tern_df_master <- rbind(tern_df_sani, tern_df_water)
tern_df_master <- mutate(tern_df_master, fam_reg = paste0(family, Region))

gg_s <- ggtern(tern_df, aes(x=s_imp_prev*100, y = s_unimp_prev*100, z = s_od_prev*100)) +
  theme_classic() + Rlab('Open\n Defecation') +
  Llab('At Least \nBasic') + Tlab('Unimproved') +

  #geom_point(aes(size = pop, alpha = pop_q, color = ISO3, 
  # fill = ISO3)) + ggtitle('Water, 2015') +
  # scale_shape_manual(values=c(21, 22, 23, 24, 25)) +

 #geom_point(aes(size = pop, alpha = pop_q, shape = sdi_q, color = Region, 
 #  fill = Region)) + ggtitle('Water, 2015') +
 #  scale_shape_manual(values=c(21, 22, 23, 24, 25)) +

  geom_point(aes(size = pop, alpha = pop_q, color = sdi_q, shape = Region,
    fill = sdi_q)) +
    scale_shape_manual(values=c(21, 22, 23, 24, 25)) +
    scale_color_manual(values = c('#023858','#74a9cf','#0570b0')) +
    scale_fill_manual(values = c('#023858','#74a9cf','#0570b0')) +
  
  ggtitle('Sanitation, 2015') +
  theme(
    tern.axis.line.T = element_line(color = '#1b9e77', size = 0.75),
    tern.axis.text.T = element_text(color = '#1b9e77', face = 'bold'),
    tern.axis.title.T = element_text(color = '#1b9e77', face = 'bold',
                                     size = 15),
    
    tern.axis.line.L = element_line(color = '#d95f02', size = 0.75),
    tern.axis.text.L = element_text(color = '#d95f02', face = 'bold'),
    tern.axis.title.L = element_text(color = '#d95f02', face = 'bold',
                                     size = 15),
    
    tern.axis.line.R = element_line(color = '#7570b3', size = 0.75),
    tern.axis.text.R = element_text(color = '#7570b3', face = 'bold'), 
    tern.axis.title.R = element_text(color = '#7570b3', face = 'bold', 
                                     size = 15)
    ) 
gg_s 

gg_w <- ggtern(tern_df, aes(x=w_imp_prev*100, y = w_unimp_prev*100, z = w_surface_prev*100)) +
  theme_classic() + Rlab('Surface') +
  Llab('At Least \nBasic') + Tlab('Unimproved') +
 
geom_point(aes(size = pop, alpha = pop_q, color = ISO3, 
   fill = ISO3), alpha = 0.5) + 
  scale_size(guide = 'none') +
  scale_alpha(guide = 'none') +
 #geom_point(aes(size = pop, alpha = pop_q, shape = sdi_q, color = Region, 
 #  fill = Region)) + ggtitle('Water, 2015') +
 # scale_shape_manual(values=c(21, 22, 23, 24, 25)) +

 # geom_point(aes(size = pop, alpha = pop_q, shape = Region, color = sdi_q, 
 #   fill = sdi_q)) +
 #   scale_shape_manual(values=c(21, 22, 23, 24, 25)) +
 #   scale_color_manual(values = c('#023858','#74a9cf','#0570b0')) +
 #   scale_fill_manual(values = c('#023858','#74a9cf','#0570b0')) +
  
  #ggtitle('Water, 2015') +
  theme(
    tern.axis.line.T = element_line(color = '#1b9e77', size = 0.75),
    tern.axis.text.T = element_text(color = '#1b9e77', face = 'bold'),
    tern.axis.title.T = element_text(color = '#1b9e77', face = 'bold',
                                     size = 15),
    
    tern.axis.line.L = element_line(color = '#d95f02', size = 0.75),
    tern.axis.text.L = element_text(color = '#d95f02', face = 'bold'),
    tern.axis.title.L = element_text(color = '#d95f02', face = 'bold',
                                     size = 15),
    
    tern.axis.line.R = element_line(color = '#7570b3', size = 0.75),
    tern.axis.text.R = element_text(color = '#7570b3', face = 'bold'), 
    tern.axis.title.R = element_text(color = '#7570b3', face = 'bold', 
                                     size = 15)
    ) 
  gg_w + theme(legend.position= c(0.1,0.85)) + facet_wrap(~Region)

gg_all <- ggtern(tern_df_master, aes(x=basic*100, y = unimp*100, z = worst*100)) +
  theme_classic() + Rlab('OD/\nSurface') +
  Llab('Basic') + Tlab('Unimproved') +

  geom_point(aes(size = pop, color = ISO3, fill = ISO3),
    alpha = 0.5) +
  scale_size(guide = 'none') +
  scale_alpha(guide = 'none') +
   theme(
    tern.axis.line.T = element_line(color = '#1b9e77'),
    tern.axis.text.T = element_text(color = '#1b9e77', face = 'bold'),
    tern.axis.title.T = element_text(color = '#1b9e77', face = 'bold'),
    
    tern.axis.line.L = element_line(color = '#d95f02'),
    tern.axis.text.L = element_text(color = '#d95f02', face = 'bold'),
    tern.axis.title.L = element_text(color = '#d95f02', face = 'bold'),
    
    tern.axis.line.R = element_line(color = '#7570b3'),
    tern.axis.text.R = element_text(color = '#7570b3', face = 'bold'), 
    tern.axis.title.R = element_text(color = '#7570b3', face = 'bold')
    )
gg_all +  theme(legend.position= c(0.1,0.85)) + facet_wrap(~fam_reg)

plot_list <- list()
for (i in c('water','sani')) {
  for (j in c('NESSA','SSSA','CSSA','NAME','SESSA','EWSSA','WWSSA')) {
    message(paste(i,j))
    tern_df_loop <- filter(tern_df_master, family == i, Region == j)
    gg_plot <- ggtern(tern_df_loop, aes(x=basic*100, y = unimp*100, z = worst*100)) +
      theme_classic() + Rlab(ifelse(i == 'water', 'S', 'OD')) +
      Llab('I') + Tlab('U') +

      geom_point(aes(size = pop, color = ISO3, fill = ISO3),
        alpha = 0.5) +
      scale_size(guide = 'none') +
      scale_alpha(guide = 'none') +
      scale_color_manual(values = c('#7F3C8D','#11A579','#3969AC','#F2B701',
        '#E73F74','#80BA5A','#E68310','#008695','#CF1C90','#f97b72','#4b4b8f','#A5AA99')) +
       theme(
        tern.axis.line.T = element_line(color = '#1b9e77'),
        tern.axis.text.T = element_text(color = '#1b9e77', face = 'bold'),
        tern.axis.title.T = element_text(color = '#1b9e77', face = 'bold'),
        
        tern.axis.line.L = element_line(color = '#d95f02'),
        tern.axis.text.L = element_text(color = '#d95f02', face = 'bold'),
        tern.axis.title.L = element_text(color = '#d95f02', face = 'bold'),
        
        tern.axis.line.R = element_line(color = '#7570b3'),
        tern.axis.text.R = element_text(color = '#7570b3', face = 'bold'), 
        tern.axis.title.R = element_text(color = '#7570b3', face = 'bold')
        ) + ggtitle(j) +
       theme(legend.position= c(0.1,0.75), plot.margin = margin(0,0,0,0),
             plot.title = element_text(hjust = 0.5))
      plot_list[[length(plot_list)+1]] <- gg_plot
  }
}


ggtern::grid.arrange(arrangeGrob(
                         plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], top = 'Water',
                         nrow = 4, ncol = 1
                         ),
             arrangeGrob(
                         plot_list[[8]], plot_list[[9]], plot_list[[10]], plot_list[[11]], top = 'Sanitation',
                         nrow = 4, ncol = 1),
             arrangeGrob(
                         plot_list[[5]], plot_list[[6]], plot_list[[7]], top = 'Water',
                         nrow = 3, ncol = 1),
             arrangeGrob(
                         plot_list[[12]], plot_list[[13]], plot_list[[14]], top = 'Sanitation',
                         nrow = 3, ncol = 1), ncol = 4)

png('/homes/adesh/results/water/ws_ternary.png', width = 1200, height = 1200)
print(
ggtern::grid.arrange(arrangeGrob(
                         plot_list[[1]], plot_list[[2]], plot_list[[3]], plot_list[[4]], top = 'Water',
                         nrow = 4, ncol = 1
                         ),
             arrangeGrob(
                         plot_list[[8]], plot_list[[9]], plot_list[[10]], plot_list[[11]], top = 'Sanitation',
                         nrow = 4, ncol = 1),
             arrangeGrob(
                         plot_list[[5]], plot_list[[6]], plot_list[[7]], top = 'Water',
                         nrow = 3, ncol = 1),
             arrangeGrob(
                         plot_list[[12]], plot_list[[13]], plot_list[[14]], top = 'Sanitation',
                         nrow = 3, ncol = 1), ncol = 4)
)
dev.off()

png('/home/adesh/Pictures/ternary_water_2015_v1.png', width = 1200, height = 1200)
print(gg_w)
dev.off()

png('/home/adesh/Pictures/ternary_sani_2015_v2.png', width = 1200, height = 1200)
print(gg_s)
dev.off()

sampfile = data.frame(Basic = 40, Unimproved = 45, Surface = 15)
gg <- ggtern(sampfile, aes(x=Basic, y = Unimproved, z = Surface)) +
  theme_classic() + Rlab('Surface or\nOpen\n Defecation') +
  Llab('At Least \nBasic') +
  geom_Tmark(linetype = 'dashed', color = '#1b9e77') +
  geom_Lmark(linetype = 'dashed', color = '#d95f02') +
  geom_Rmark(linetype = 'dashed', color = '#7570b3') +
  geom_point(size = 2) + 
  theme(
    tern.axis.line.T = element_line(color = '#1b9e77', size = 0.75),
    tern.axis.text.T = element_text(color = '#1b9e77', face = 'bold'),
    tern.axis.title.T = element_text(color = '#1b9e77', face = 'bold',
                                     size = 15),
    
    tern.axis.line.L = element_line(color = '#d95f02', size = 0.75),
    tern.axis.text.L = element_text(color = '#d95f02', face = 'bold'),
    tern.axis.title.L = element_text(color = '#d95f02', face = 'bold',
                                     size = 15),
    
    tern.axis.line.R = element_line(color = '#7570b3', size = 0.75),
    tern.axis.text.R = element_text(color = '#7570b3', face = 'bold'), 
    tern.axis.title.R = element_text(color = '#7570b3', face = 'bold', 
                                     size = 15)
    ) + geom_text(label = 'Location', hjust = -0.20, vjust = 1.3, size = 7)
   
png(file = '/homes/adesh/results/water/conceptual_ternary.png',
    height = 1200, width = 1200)
print(gg)
dev.off()