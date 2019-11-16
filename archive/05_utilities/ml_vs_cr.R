rm(list = ls()); library(raster); library(tidyverse); library(ggtern)

w_piped_df <-read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_piped.csv')
w_imp_df <-read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_imp.csv')
w_unimp_df <-read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_unimp.csv')
w_surface_df <-read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/w_surface.csv')

#for (i in c('w_piped', 'w_imp','w_unimp','w_surface')) {
#    assign(paste0(i,'_df'),
#        dplyr::select(get(paste0(i,'_df')), -prop, -X, -sdg_imp))
#}

#for (i in c('w_imp','w_unimp','w_surface')) {
#    assign(paste0(i,'_df'),
#        dplyr::select(get(paste0(i,'_df')),-latitude, -longitude, -weight))
#}

w_piped_df <- mutate(w_piped_df, w_piped = w_piped/N)
w_imp_df <- mutate(w_imp_df, w_imp = w_imp/N)
w_unimp_df <- mutate(w_unimp_df, w_unimp = w_unimp/N)
w_surface_df <- mutate(w_surface_df, w_surface = w_surface/N)

w_all_df <- w_piped_df
w_all_df$w_imp <- w_imp_df$w_imp; rm(w_imp_df)
w_all_df$w_unimp <- w_unimp_df$w_unimp; rm(w_unimp_df)
w_all_df$w_surface <- w_surface_df$w_surface; rm(w_surface_df)

#w_all_df <- left_join(w_piped_df, w_imp_df); #rm(list = c('w_piped_df', 'w_imp_df'))
#w_all_df <- left_join(w_all_df, w_unimp_df); rm(list = c('w_unimp_df'))
#w_all_df <- left_join(w_all_df, w_surface_df); rm(list = c('w_surface_df'))
w_all_df <- filter(w_all_df, year >= 2000 & year <= 2015)
w_all_df$key <- 1:nrow(w_all_df)

#### Model 1 Extraction ###
run_date <- '2017_12_14_11_01_03'
for (indicator in c('w_piped', 'w_imp_cr','w_unimp_cr')) {
    message(indicator); 

    message('loading data...')
    setwd(paste0('/share/geospatial/mbg/wash/',indicator,'/output/',run_date))
    files <- list.files()[grep(pattern = '.grd', x = list.files())]
    preds <- lapply(files, brick)
    preds <- do.call(raster::merge, preds)
    assign(indicator, preds); rm(preds)
    rm(files)
}

message('calculating...')
w_imp_calc <- (1 - w_piped) * w_imp_cr; rm(w_imp_cr)
w_unimp_calc <- (1 - w_piped - w_imp_calc) * w_unimp_cr; rm(w_unimp_cr)
w_surface_calc <- 1 - w_piped - w_imp_calc - w_unimp_calc

for (map in c('w_piped','w_imp_calc','w_unimp_calc','w_surface_calc')) {
    x_list <- list()
    for (i in unique(w_all_df$year)) {
        x_df <- select(filter(w_all_df, year == i), longitude, latitude, key)
        x_df$ID <- 1:nrow(x_df)
        xtract <- raster::extract(get(map)[[i - 1999]], 
                    select(x_df, longitude, latitude), df = T)
        names(xtract)[2] <- paste0(map,'_m1')
        x_list[[length(x_list)+1]] <- select(left_join(x_df, xtract, by = 'ID'), -ID)
    }
    xtract <- do.call(rbind, x_list)
    w_all_df <- left_join(w_all_df, xtract)
}
rm(list = c('w_imp_calc','w_unimp_calc','w_surface_calc'))

#### Model 2 Extraction ###
run_date <- '2017_12_22_14_12_39'
for (indicator in c('w_piped', 'w_imp','w_unimp', 'w_surface')) {
    message(indicator); 

    message('loading data...')
    setwd(paste0('/share/geospatial/mbg/wash/',indicator,'/output/',run_date))
    files <- list.files()[grep(pattern = '.grd', x = list.files())]
    preds <- lapply(files, brick)
    preds <- do.call(raster::merge, preds)
    assign(indicator, preds); rm(preds)
    rm(files)
}

message('calculating...')
total <- w_piped + w_imp + w_unimp + w_surface
w_piped_rescale <- w_piped/total; rm(w_piped)
w_imp_rescale <- w_imp/total; rm(w_imp)
w_unimp_rescale <- w_unimp/total; rm(w_unimp)
w_surface_rescale <- w_surface/total; rm(w_surface); rm(total)

for (map in c('w_piped_rescale', 'w_imp_rescale','w_unimp_rescale', 'w_surface_rescale')) {
    x_list <- list()
    for (i in unique(w_all_df$year)) {
        x_df <- select(filter(w_all_df, year == i), longitude, latitude, key)
        x_df$ID <- 1:nrow(x_df)
        xtract <- raster::extract(get(map)[[i - 1999]], 
                    select(x_df, longitude, latitude), df = T)
        names(xtract)[2] <- paste0(map,'_m2')
        x_list[[length(x_list)+1]] <- select(left_join(x_df, xtract, by = 'ID'), -ID)
    }
    xtract <- do.call(rbind, x_list)
    w_all_df <- left_join(w_all_df, xtract)
}
rm(list = c('w_piped_rescale', 'w_imp_rescale','w_unimp_rescale', 'w_surface_rescale'))

###
w_all_df <- mutate(w_all_df, w_basic_m1 = w_piped_m1 + w_imp_calc_m1,
                w_basic_m2 = w_piped_rescale_m2 + w_imp_rescale_m2, 
                w_basic = w_piped + w_imp)

w_all_df <- mutate(w_all_df, w_basic_wm = (w_basic_m1 + w_basic_m2)/2,
                    w_unimp_wm = (w_unimp_calc_m1 + w_unimp_rescale_m2)/2,
                    w_surface_wm = (w_surface_calc_m1 + w_surface_rescale_m2)/2)

w_all_df$period <- ifelse(w_all_df$year < 2005, 2000,
                        ifelse(w_all_df$year < 2010, 2005,
                            ifelse(w_all_df$year < 2015, 2010, 2015)))

sssa_hi <- c('NAM','BWA','ZAF')
cssa <- c('CAF','GAB','GNQ','COD','COG','AGO','STP')
name_hi <- c('MAR','DZA','TUN','LBY','EGY')
essa_hilo <- c('SDN','ERI','DJI','SOM','ETH','SSD',
             'SSD','UGA','KEN','RWA','BDI','TZA',
             'MWI','MOZ','ZMB','MDG','ZWE','SWZ','LSO',
             'COM')
wssa <- c('CPV','SEN','GMB','GIN','GNB','SLE','MLI','LBR',
        'CIV','GHA','TGO','BEN','NGA','NER','TCD','CMR',
        'BFA','MRT')

w_all_df$region <- ifelse(w_all_df$country %in% sssa_hi, 'sssa_hi',
                        ifelse(w_all_df$country %in% cssa, 'cssa',
                            ifelse(w_all_df$country %in% essa_hilo, 'essa_hilo',
                                ifelse(w_all_df$country %in% name_hi, 'name_hi','wssa'))))

w_all_df <- mutate(w_all_df, basic_ae_m1 = abs(w_basic_m1 - w_basic),
                             basic_ae_m2 = abs(w_basic_m2 - w_basic),
                             basic_ae_wn = abs(w_basic_wm - w_basic),
                             unimp_ae_m1 = abs(w_unimp_calc_m1 - w_unimp),
                             unimp_ae_m2 = abs(w_unimp_rescale_m2 - w_unimp),
                             unimp_ae_wm = abs(w_unimp_wm - w_unimp),
                             surface_ae_m1 = abs(w_surface_calc_m1 - w_surface),
                             surface_ae_m2 = abs(w_surface_rescale_m2 - w_surface),
                             surface_ae_wm = abs(w_surface_wm - w_surface)
                             )

w_mae <- w_all_df %>% group_by(country, period, year, region) %>%
            summarize(basic_mae_m1 = mean(basic_ae_m1, na.rm = T),
                        basic_mae_m2 = mean(basic_ae_m2, na.rm = T),
                        basic_mae_wn = mean(basic_ae_wn, na.rm = T),
                        unimp_mae_m1 = mean(unimp_ae_m1, na.rm = T),
                        unimp_mae_m2 = mean(unimp_ae_m2, na.rm = T),
                        unimp_mae_wm = mean(unimp_ae_wm, na.rm = T),
                        surface_mae_m1 = mean(surface_ae_m1, na.rm = T),
                        surface_mae_m2 = mean(surface_ae_m2, na.rm = T),
                        surface_mae_wm = mean(surface_ae_wm, na.rm = T)
                )

w_mae <- data.frame(
            model = c(rep('CR',nrow(w_mae)), rep('ML',nrow(w_mae)), 
                    rep('WM',nrow(w_mae))),
            basic_mae = c(w_mae$basic_mae_m1, w_mae$basic_mae_m2, w_mae$basic_mae_wn),
            unimp_mae = c(w_mae$unimp_mae_m1, w_mae$unimp_mae_m2, w_mae$unimp_mae_wm),
            surface_mae = c(w_mae$surface_mae_m1, w_mae$surface_mae_m2, w_mae$surface_mae_wm),
            country = rep(w_mae$country, 3),
            period = rep(w_mae$period, 3),
            region = rep(w_mae$region, 3)
            )

plot_mae <- filter(w_mae, model != 'WM')
png('/home/adesh/Documents/wash/presentations/unimp_surface_comparison.png', width = 1200, height = 1200)
ggplot(plot_mae) +
    geom_point(aes(x = unimp_mae, y = surface_mae, color = model)) + 
    facet_grid(period ~ region) + theme_bw()
dev.off()

w_mae2 <- w_all_df %>% group_by(country, period, region) %>%
            summarize(basic_mae_m1 = mean(basic_ae_m1, na.rm = T),
                        basic_mae_m2 = mean(basic_ae_m2, na.rm = T),
                        basic_mae_wn = mean(basic_ae_wn, na.rm = T),
                        unimp_mae_m1 = mean(unimp_ae_m1, na.rm = T),
                        unimp_mae_m2 = mean(unimp_ae_m2, na.rm = T),
                        unimp_mae_wm = mean(unimp_ae_wm, na.rm = T),
                        surface_mae_m1 = mean(surface_ae_m1, na.rm = T),
                        surface_mae_m2 = mean(surface_ae_m2, na.rm = T),
                        surface_mae_wm = mean(surface_ae_wm, na.rm = T)
                )

w_mae2 <- data.frame(
            model = c(rep('CR',nrow(w_mae2)), rep('ML',nrow(w_mae2)), 
                    rep('WM',nrow(w_mae2))),
            basic_mae = c(w_mae2$basic_mae_m1, w_mae2$basic_mae_m2, w_mae2$basic_mae_wn),
            unimp_mae = c(w_mae2$unimp_mae_m1, w_mae2$unimp_mae_m2, w_mae2$unimp_mae_wm),
            surface_mae = c(w_mae2$surface_mae_m1, w_mae2$surface_mae_m2, w_mae2$surface_mae_wm),
            country = rep(w_mae2$country, 3),
            #year = rep(w_mae2$year, 3),
            period = rep(w_mae2$period, 3),
            region = rep(w_mae2$region, 3)
            )

plot_mae2 <- filter(w_mae2, model != 'WM')
png('/home/adesh/Documents/wash/presentations/mae_time.png', width = 1200, height = 1200)
ggplot(plot_mae2) + 
    geom_line(aes(x = period, y = basic_mae, color = country), alpha = 0.7) + 
        facet_grid(model ~ region) + theme_bw()
dev.off()

w_corr <- w_all_df %>% mutate(weighted_N = weight*N) %>%
            group_by(country, year, period, region) %>%
            summarize(basic_m1 = weighted.mean(w_basic_m1, weighted_N, na.rm = T),
                        basic_m2 = weighted.mean(w_basic_m2, weighted_N, na.rm = T),
                        basic_input = weighted.mean(w_basic, weighted_N, na.rm = T),
                        unimp_m1 = weighted.mean(w_unimp_calc_m1, weighted_N, na.rm = T),
                        unimp_m2 = weighted.mean(w_unimp_rescale_m2, weighted_N, na.rm = T),
                        unimp_input = weighted.mean(w_unimp, weighted_N, na.rm = T),
                        surface_m1 = weighted.mean(w_surface_calc_m1, weighted_N, na.rm = T),
                        surface_m2 = weighted.mean(w_surface_rescale_m2, weighted_N, na.rm = T),
                        surface_input = weighted.mean(w_surface, weighted_N, na.rm = T)
                        )
plot_corr <- w_corr

png('/home/adesh/Documents/wash/presentations/basic_cr_corr.png', width = 1200, height = 1200)
ggplot(plot_corr) +
    geom_point(aes(x = basic_input, y = basic_m1)) + 
    geom_abline(aes(slope = 1, intercept = 0), color = 'red') + 
    theme_bw() + xlim(0,1) + ylim(0,1) +
    facet_grid(period ~ region) + ggtitle('Basic (CR)')
dev.off()

png('/home/adesh/Documents/wash/presentations/unimp_cr_corr.png', width = 1200, height = 1200)
ggplot(plot_corr) +
    geom_point(aes(x = unimp_input, y = unimp_m1)) + 
    geom_abline(aes(slope = 1, intercept = 0), color = 'red') + 
    theme_bw() + xlim(0,1) + ylim(0,1) +
    facet_grid(period ~ region) + ggtitle('Unimproved (CR)')
dev.off()

png('/home/adesh/Documents/wash/presentations/surface_cr_corr.png', width = 1200, height = 1200)
ggplot(plot_corr) +
    geom_point(aes(x = surface_input, y = surface_m1)) + 
    geom_abline(aes(slope = 1, intercept = 0), color = 'red') + 
    theme_bw() + xlim(0,1) + ylim(0,1) +
    facet_grid(period ~ region) + ggtitle('Surface (CR)')
dev.off()

png('/home/adesh/Documents/wash/presentations/basic_ml_corr.png', width = 1200, height = 1200)
ggplot(plot_corr) +
    geom_point(aes(x = basic_input, y = basic_m2)) + 
    geom_abline(aes(slope = 1, intercept = 0), color = 'red') + 
    theme_bw() + xlim(0,1) + ylim(0,1) +
    facet_grid(period ~ region) + ggtitle('Basic (ML)')
dev.off()

png('/home/adesh/Documents/wash/presentations/unimp_ml_corr.png', width = 1200, height = 1200)
ggplot(plot_corr) +
    geom_point(aes(x = unimp_input, y = unimp_m2)) + 
    geom_abline(aes(slope = 1, intercept = 0), color = 'red') + 
    theme_bw() + xlim(0,1) + ylim(0,1) +
    facet_grid(period ~ region) + ggtitle('Unimproved (ML)')
dev.off()

png('/home/adesh/Documents/wash/presentations/surface_ml_corr.png', width = 1200, height = 1200)
ggplot(plot_corr) +
    geom_point(aes(x = surface_input, y = surface_m2)) + 
    geom_abline(aes(slope = 1, intercept = 0), color = 'red') + 
    theme_bw() + xlim(0,1) + ylim(0,1) +
    facet_grid(period ~ region) + ggtitle('Surface (ML)')
dev.off()

w_tern <- data.frame(model = c(rep('input',nrow(w_all_df)), rep('CR',nrow(w_all_df)), 
                        rep('ML',nrow(w_all_df)), rep('WM',nrow(w_all_df))),
            basic = c(w_all_df$w_basic, w_all_df$w_basic_m1, w_all_df$w_basic_m2, w_all_df$w_basic_wm),
            unimp = c(w_all_df$w_unimp, w_all_df$w_unimp_calc_m1, w_all_df$w_unimp_rescale_m2, 
                w_all_df$w_unimp_wm),
            surface = c(w_all_df$w_surface, w_all_df$w_surface_calc_m1, w_all_df$w_surface_rescale_m2,
                w_all_df$w_surface_wm),
            year = rep(w_all_df$year, 4),
            country = rep(w_all_df$country, 4),
            N = rep(w_all_df$N, 4),
            weight = rep(w_all_df$weight, 4),
            period = rep(w_all_df$period, 4),
            region = rep(w_all_df$region, 4))
w_tern <- mutate(w_tern, weighted_N = weight*N)

w_tern_sum  <- w_tern %>% group_by(country, period, region, model) %>% 
                #summarize(basic = mean(basic, na.rm = T),
                #            unimp = mean(unimp, na.rm = T),
                #            surface = mean(surface, na.rm = T))
                summarize(basic = weighted.mean(basic, weighted_N, na.rm = T),
                            unimp = weighted.mean(unimp, weighted_N, na.rm = T),
                            surface = weighted.mean(surface, weighted_N, na.rm = T))

png('/home/adesh/Documents/wash/presentations/tern_essa_model_comparison_nowm.png', height = 1920)
    #plotdat <- filter(w_tern_sum, model != 'WM')
    plotdat <- filter(w_tern_sum, region == 'essa_hilo' & model != 'WM')
    print(
        ggtern(plotdat, 
                aes(x = basic, y = unimp, z = surface, color = model)) +
            facet_grid(period ~ region) +
            geom_point(alpha = 0.7) + 
            theme_bw() +
            theme(
                tern.axis.line.T = element_line(color = '#e7298a'),
                tern.axis.text.T = element_text(color = '#e7298a'),
                tern.axis.title.T = element_text(color = '#e7298a'),
                
                tern.axis.line.L = element_line(color = '#e6ab02'),
                tern.axis.text.L = element_text(color = '#e6ab02'),
                tern.axis.title.L = element_text(color = '#e6ab02'),
                
                tern.axis.line.R = element_line(color = '#d95f02'),
                tern.axis.text.R = element_text(color = '#d95f02'), 
                tern.axis.title.R = element_text(color = '#d95f02')
            ) +
            Rlab('S') + Llab('B') + Tlab('U')
        )
        #for (i in unique(plotdat$year)) {
        #message(paste(i,j))
        #print(
        #        ggtern(filter(plotdat, year == i),
        #            aes(x = basic, y = unimp, z = surface, color = model)) +
        #        geom_point(alpha = 0.7) + ggtitle(paste(j,i)) +
        #        theme_classic()
dev.off()

w_all_df <- mutate(w_all_df, piped_ae = abs(model - (w_piped/N)))
ggplot(data.frame(prev = c(w_all_df$model, w_all_df$w_piped/w_all_df$N),
        model = c(rep('yes',nrow(w_all_df)), rep('no',nrow(w_all_df))))) + 
        geom_violin(aes(x = model, y  = prev))