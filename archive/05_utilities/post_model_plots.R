rm(list = ls()); library(raster); library(tidyverse); library(ggtern)

s_imp_df <-read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_imp.csv')
s_unimp_df <-read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp.csv')
s_od_df <-read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_od.csv')

s_imp_df <- mutate(s_imp_df, s_imp = s_imp/N)
s_unimp_df <- mutate(s_unimp_df, s_unimp = s_unimp/N)
s_od_df <- mutate(s_od_df, s_od = s_od/N)

s_all_df <- s_imp_df
s_all_df$s_unimp <- s_unimp_df$s_unimp; rm(s_unimp_df)
s_all_df$s_od <- s_od_df$s_od; rm(s_od_df)

s_all_df <- filter(s_all_df, year >= 2000 & year <= 2015)
s_all_df$key <- 1:nrow(s_all_df)

#### Model 1 Extraction ###
rds <- c('2018_01_23_10_44_09', '2018_01_23_10_44_10', '2018_01_23_10_44_11', 
         '2018_01_23_10_44_12', '2018_01_23_10_44_13', '2018_01_23_10_44_14',
         '2018_01_23_10_44_15', '2018_01_23_10_44_16', '2018_01_23_10_44_17',
         '2018_01_23_10_44_18', '2018_01_23_10_44_19', '2018_01_23_10_44_20',
         '2018_01_23_10_44_21', '2018_01_23_10_44_22', '2018_01_23_10_44_23')
models <- c(9:23)

for (k in 1:length(rds)) {
    print(rds[k])

    s_imp_df <-read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_imp.csv')
    #s_unimp_df <-read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_unimp.csv')
    #s_od_df <-read.csv('/home/j/WORK/11_geospatial/10_mbg/input_data/s_od.csv')

    s_imp_df <- mutate(s_imp_df, s_imp = s_imp/N)
    #s_unimp_df <- mutate(s_unimp_df, s_unimp = s_unimp/N)
    #s_od_df <- mutate(s_od_df, s_od = s_od/N)

    s_all_df <- s_imp_df
    #s_all_df$s_unimp <- s_unimp_df$s_unimp; rm(s_unimp_df)
    #s_all_df$s_od <- s_od_df$s_od; rm(s_od_df)

    s_all_df <- filter(s_all_df, year >= 2000 & year <= 2015)
    s_all_df$key <- 1:nrow(s_all_df)

    run_date <- rds[k]
    for (indicator in c('s_imp')) { #,'s_unimp_cr')) {
        message(indicator); 

        message('loading data...')
        setwd(paste0('/share/geospatial/mbg/wash/',indicator,'/output/',run_date))
        files <- list.files()[grep(pattern = '.grd', x = list.files())]
        preds <- lapply(files, brick)
        preds <- do.call(raster::merge, preds)
        assign(indicator, preds); rm(preds)
        rm(files)
    }

    #message('calculating...')
    #s_unimp_calc <- (1 - s_imp) * s_unimp_cr; rm(s_unimp_cr)
    #s_od_calc <- 1 - s_imp - s_unimp_calc

    for (map in c('s_imp')) { #,'s_unimp_calc','s_od_calc')) {
        x_list <- list()
        for (i in unique(s_all_df$year)) {
            x_df <- select(filter(s_all_df, year == i), longitude, latitude, key)
            x_df$ID <- 1:nrow(x_df)
            xtract <- raster::extract(get(map)[[i - 1999]], 
                        select(x_df, longitude, latitude), df = T)
            names(xtract)[2] <- paste0(map,'_m1')
            x_list[[length(x_list)+1]] <- select(left_join(x_df, xtract, by = 'ID'), -ID)
        }
        xtract <- do.call(rbind, x_list)
        s_all_df <- left_join(s_all_df, xtract)
    }
    rm(list = c('s_imp','s_unimp_calc','s_od_calc'))


    ###
    s_all_df <- mutate(s_all_df, s_basic_m1 = s_imp_m1,
                                 s_basic = s_imp)

    s_all_df$period <- ifelse(s_all_df$year < 2005, 2000,
                            ifelse(s_all_df$year < 2010, 2005,
                                ifelse(s_all_df$year < 2015, 2010, 2015)))

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

    s_all_df$region <- ifelse(s_all_df$country %in% sssa_hi, 'sssa_hi',
                            ifelse(s_all_df$country %in% cssa, 'cssa',
                                ifelse(s_all_df$country %in% essa_hilo, 'essa_hilo',
                                    ifelse(s_all_df$country %in% name_hi, 'name_hi','wssa'))))

    s_all_df <- mutate(s_all_df, basic_ae_m1 = abs(s_basic_m1 - s_basic),
                                 # unimp_ae_m1 = abs(s_unimp_calc_m1 - s_unimp),
                                 # od_ae_m1 = abs(s_od_calc_m1 - s_od),
                                 weighted_N = weight*N)

    model_mae <- s_all_df %>% group_by(region) %>%
                    summarize(basic_mae_m1 = weighted.mean(basic_ae_m1, weighted_N, na.rm = T)#,
                              # unimp_mae_m1 = weighted.mean(unimp_ae_m1, weighted_N, na.rm = T),
                              # od_mae_m1 = weighted.mean(od_ae_m1, weighted_N, na.rm = T))
                             )
             
    write.csv(model_mae, paste0('/home/adesh/Documents/wash/documents/mae_', models[k],'.csv'))

    s_corr <- s_all_df %>% 
            group_by(country, year, period, region) %>%
            summarize(basic_m1 = weighted.mean(s_basic_m1, weighted_N, na.rm = T),
                        basic_input = weighted.mean(s_basic, weighted_N, na.rm = T) #,
                        #unimp_m1 = weighted.mean(s_unimp_calc_m1, weighted_N, na.rm = T),
                        #unimp_input = weighted.mean(s_unimp, weighted_N, na.rm = T),
                        #od_m1 = weighted.mean(s_od_calc_m1, weighted_N, na.rm = T),
                        #od_input = weighted.mean(s_od, weighted_N, na.rm = T)
                        )
                        
    plot_corr <- s_corr

    png(paste0('/home/adesh/Documents/wash/presentations/basic_cr_corr_m',
                models[k],'.png'), width = 1200, height = 1200)
    print(
        ggplot(plot_corr) +
            geom_point(aes(x = basic_input, y = basic_m1)) + 
            geom_abline(aes(slope = 1, intercept = 0), color = 'red') + 
            theme_bw() + xlim(0,1) + ylim(0,1) +
            facet_grid(period ~ region) + ggtitle(paste0('Basic, Model ',models[k]))
        )
    dev.off()


}

s_mae <- s_all_df %>% group_by(country, period, year, region) %>%
            summarize(basic_mae_m1 = mean(basic_ae_m1, na.rm = T),
                        unimp_mae_m1 = mean(unimp_ae_m1, na.rm = T),
                        od_mae_m1 = mean(od_ae_m1, na.rm = T),
                )



s_mae <- data.frame(
            model = c(rep('CR',nrow(s_mae))),
            basic_mae = c(s_mae$basic_mae_m1),
            unimp_mae = c(s_mae$unimp_mae_m1),
            od_mae = c(s_mae$od_mae_m1),
            country = rep(s_mae$country, 1),
            period = rep(s_mae$period, 1),
            region = rep(s_mae$region, 1)
            )

plot_mae <- filter(s_mae, model != 'WM')
png('/home/adesh/Documents/wash/presentations/unimp_od_comparison.png', width = 1200, height = 1200)
ggplot(plot_mae) +
    geom_point(aes(x = unimp_mae, y = od_mae, color = model)) + 
    facet_grid(period ~ region) + theme_bw()
dev.off()

s_corr <- s_all_df %>% mutate(weighted_N = weight*N) %>%
            group_by(country, year, period, region) %>%
            summarize(basic_m1 = weighted.mean(s_basic_m1, weighted_N, na.rm = T),
                        basic_input = weighted.mean(s_basic, weighted_N, na.rm = T),
                        unimp_m1 = weighted.mean(s_unimp_calc_m1, weighted_N, na.rm = T),
                        unimp_input = weighted.mean(s_unimp, weighted_N, na.rm = T),
                        od_m1 = weighted.mean(s_od_calc_m1, weighted_N, na.rm = T),
                        od_input = weighted.mean(s_od, weighted_N, na.rm = T)
                        )
plot_corr <- s_corr

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

png('/home/adesh/Documents/wash/presentations/od_cr_corr.png', width = 1200, height = 1200)
ggplot(plot_corr) +
    geom_point(aes(x = od_input, y = od_m1)) + 
    geom_abline(aes(slope = 1, intercept = 0), color = 'red') + 
    theme_bw() + xlim(0,1) + ylim(0,1) +
    facet_grid(period ~ region) + ggtitle('OD (CR)')
dev.off()

s_tern <- data.frame(model = c(rep('input',nrow(s_all_df)), rep('CR',nrow(s_all_df))),
            basic = c(s_all_df$s_basic, s_all_df$s_basic_m1),
            unimp = c(s_all_df$s_unimp, s_all_df$s_unimp_calc_m1),
            surface = c(s_all_df$s_od, s_all_df$s_od_calc_m1),
            year = rep(s_all_df$year, 2),
            country = rep(s_all_df$country, 2),
            N = rep(s_all_df$N, 2),
            weight = rep(s_all_df$weight, 2),
            period = rep(s_all_df$period, 2),
            region = rep(s_all_df$region, 2))
s_tern <- mutate(s_tern, weighted_N = weight*N)

s_tern_sum  <- s_tern %>% group_by(country, period, region, model) %>% 
                summarize(basic = weighted.mean(basic, weighted_N, na.rm = T),
                            unimp = weighted.mean(unimp, weighted_N, na.rm = T),
                            surface = weighted.mean(surface, weighted_N, na.rm = T))

png('/home/adesh/Documents/wash/presentations/tern_essa_model_comparison_nowm.png', height = 1920)
    #plotdat <- filter(s_tern_sum, model != 'WM')
    plotdat <- filter(s_tern_sum, model != 'WM')
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
            Rlab('O') + Llab('B') + Tlab('U')
        )
dev.off()
