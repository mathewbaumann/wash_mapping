rm(list = ls())
# load libraries
libs <- c('ggplot2', 'data.table', 'dplyr', 'raster')
lapply(libs, library, character.only = TRUE)

# define indicator groups
#indi_group <- 'water'
for(indi_group in c('water','sani')){
  if (indi_group == 'water') {
    indi <- c('w_network_cr','w_piped', 'w_imp_cr', 'w_unimp_cr')
  } else {
    indi <- c('s_network_cr','s_piped', 's_imp_cr', 's_unimp_cr')
  }

  outputdir <- file.path('/share/geospatial/mbg/input_data/')
  setwd(outputdir)
  repo <- '/share/code/geospatial/adesh/mbg'

  indicator <- indi[1]
  id <- list.files(pattern = paste0(indi[1], '.csv'))
  input_data <- fread(id)
  network <- input_data %>%
    group_by(nid, surv_year, country) %>%
    summarize(mean = weighted.mean(x = prop, w = sum_of_sample_weights*weight),
              ss = sum(weight*N)) %>%
    dplyr:::select(country, nid, surv_year, mean) %>%
    rename(network = mean)

  indicator <- indi[2]
  id <- list.files(pattern = paste0(indi[2], '.csv'))
  input_data <- fread(id)
  piped <- input_data %>%
    group_by(nid, surv_year, country) %>%
    summarize(mean = weighted.mean(x = prop, w = sum_of_sample_weights*weight),
              ss = sum(weight*N)) %>%
    dplyr:::select(country, nid, surv_year, mean) %>%
    rename(piped = mean)

  indicator <- indi[3]
  id <- list.files(pattern = paste0(indi[3], '.csv'))
  input_data <- fread(id)
  imp <- input_data %>%
    group_by(nid, surv_year, country) %>%
    summarize(mean = weighted.mean(x = prop, w = sum_of_sample_weights*weight),
              ss = sum(weight*N)) %>%
    dplyr:::select(country, nid, surv_year, mean) %>%
    rename(imp = mean)

  indicator <- indi[4]
  id <- list.files(pattern = paste0(indi[4], '.csv'))
  input_data <- fread(id)
  unimp <- input_data %>%
    group_by(nid, surv_year, country) %>%
    summarize(mean = weighted.mean(x = prop, w = sum_of_sample_weights*weight),
              ss = sum(weight*N)) %>%
    dplyr:::select(country, nid, surv_year, mean) %>%
    rename(unimp = mean)

  #fix surveys missing only network
  test <- subset(piped, !nid %in% network$nid) %>%
    rename(network = piped) %>%
    mutate(network = 0)
  network <- rbind(network, test)

  mydat <- left_join(network, piped)
  mydat <- left_join(mydat, imp)
  mydat <- left_join(mydat, unimp)
  mydat$imp <- ifelse(is.na(mydat$imp), 0, mydat$imp)
  mydat$unimp <- ifelse(is.na(mydat$unimp), 0, mydat$unimp)
  mydat$surface <- 0
  mydat <- mydat %>%
    mutate(piped_on_premises = piped*network) %>%
    mutate(piped_public = (1 - network)*piped) %>%
    mutate(imp = (1 - piped)*imp) %>%
    mutate(unimp = (1 - piped - imp)*unimp) %>%
    mutate(surface = 1 - piped - imp - unimp)

  #remove tracking sheet duplicates
  mydat <- unique(mydat)

  exc <- fread('/snfs1/WORK/11_geospatial/wash/stg2_pub/data/2019-10-28/data_tracking/nids_included.csv')
  if(indi_group == 'water'){
    exc <- exc$w_include
  }else{
    exc <- exc$s_include
  }

  if(!all(network$nid %in% mydat$nid)){
    message('NETWORK')
  }
  if(!all(piped$nid %in% mydat$nid)){
    message('PIPED')
  }
  if(!all(imp$nid %in% mydat$nid)){
    message('IMP')
  }
  if(!all(unimp$nid %in% mydat$nid)){
    message('UNIMP')
  }
  mydat <- subset(mydat, nid %in% exc)

  library(ggplot2)
  library(ggrepel)
  date <- gsub("-", "_", Sys.Date())

  dir.create(paste0('/home/j/WORK/11_geospatial/wash/national_ts_plots/data_vetting_pt2/', date))
  pdf(paste0('/home/j/WORK/11_geospatial/wash/national_ts_plots/data_vetting_pt2/',date,'/', indi_group,'.pdf'),
      8.5, 8.5)
  for (i in unique(mydat$country)) {
    print(i)
    print(
      ggplot(data = filter(mydat, country == i)) +
        geom_point(aes(x= surv_year, y = piped_on_premises, col = 'piped_on_premises'), size =3) +
        geom_point(aes(x= surv_year, y = piped_public, col = 'piped_public'), size =3) +
        geom_point(aes(x= surv_year, y = imp, col = 'imp'), size =3) +
        geom_point(aes(x= surv_year, y = unimp, col = 'unimp'), size =3) +
        geom_point(aes(x= surv_year, y = surface, col = 'surface'), size =3) +

        geom_line(aes(x= surv_year, y = piped_on_premises, col = 'piped_on_premises'), linetype = 'dashed') +
        geom_line(aes(x= surv_year, y = piped_public, col = 'piped_public'), linetype = 'dashed') +
        geom_line(aes(x= surv_year, y = imp, col = 'imp'), linetype = 'dashed') +
        geom_line(aes(x= surv_year, y = unimp, col = 'unimp'), linetype = 'dashed') +
        geom_line(aes(x= surv_year, y = surface, col = 'surface'), linetype = 'dashed') +

        geom_text_repel(aes(x= surv_year, y = piped_on_premises, label = nid, col = 'piped_on_premises'), size =3) +
        geom_text_repel(aes(x= surv_year, y = piped_public, label = nid, col = 'piped_public'), size =3) +
        geom_text_repel(aes(x= surv_year, y = imp, label = nid, col = 'imp'), size =3) +
        geom_text_repel(aes(x= surv_year, y = unimp, label = nid, col = 'unimp'), size =3) +
        geom_text_repel(aes(x= surv_year, y = surface, label = nid, col = 'surface'), size =3) +
        labs(color = 'Indicator', x = 'Year', y = 'Prevalence') +
        scale_color_manual(values = c('#fa9fb5','#e41a1c', '#377eb8', '#4daf4a', '#984ea3')) +
        ylim(0, 1) + xlim(2000, NA) +
        theme_bw() +
        ggtitle(i)
    )
  }
  dev.off()
}

