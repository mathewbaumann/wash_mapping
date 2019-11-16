library(dplyr)
bad_nids <- read.csv('/home/adesh/Documents/wash/documents/wash_data_vetting_12_4.csv',
                     stringsAsFactors = F)

setwd('/home/j/WORK/11_geospatial/10_mbg/input_data')

indicators <- list(c('w_imp',"w_piped",'w_unimp','w_surface','w_imp_cr', 'w_unimp_cr'),
                c('s_imp','s_unimp','s_od','s_unimp_cr'))
indi_fam <- c('water','sani')
for (j in 1:2) {
    message(j)
    for (i in indicators[[j]]) {
        message(i)
        mydat <- read.csv(paste0(i,'.csv'))
        nids <- unique(filter(bad_nids, family ==  indi_fam[j])$nid)
        mydat <- filter(mydat, !(nid %in% nids))
        write.csv(paste0(i,'.csv'))
    }
}