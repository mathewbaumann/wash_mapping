setwd('J:/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/')

load('washpoints_collapsed_2017_05_24.Rdata')
load('washpolys_collapsed_2017_05_24.Rdata')

pts <- select(pt_collapse, survey_name, ihme_loc_id, year_end, hhweight,
              t_type, shared_san, hh_size)
polys <- select(poly_collapse,survey_name, ihme_loc_id, year_end, hhweight,
                t_type, shared_san, hh_size)
master <- rbind(pts, polys, stringsAsFactors = F)
master <- filter(master, !is.na(hhweight))



master <- master %>% group_by(survey_name, ihme_loc_id, year_end) %>% mutate(denom = sum(hhweight*hh_size))
t_def <- read.csv('J:/WORK/11_geospatial/wash/definitions/2nd_t_type_defined_updated_2017_05_18.csv',
                  stringsAsFactors = F)

master$svy_short <- ifelse(master$survey_name %in% unique(master$survey_name[grepl("/",master$survey_name)]), "other",
                      master$survey_name)

master$sdg <- NA
for (i in unique(master$t_type)) {
  tryCatch({
    master$sdg[which(master$t_type == i)] <- t_def$sdg[which(t_def$string == i)]
  }, error = function(e){print(i)})
}

master$latrine_imp <- ifelse(master$sdg == "latrine_imp", 1, 0)
master$latrine_unimp <- ifelse(master$sdg == "latrine_unimp", 1, 0)
master$latrine_cw <- ifelse(master$sdg == "latrine_cw"|
                              master$sdg == "latrine_ cw", 1, 0)

master$open <- ifelse(master$sdg == "open", 1, 0)
master$imp <- ifelse((master$sdg == "imp"|
                       master$sdg == "imo"), 1, 0)
master$shared <- ifelse(master$imp == 1 & master$shared_san == 1, 1, 0)
master$unimp <- ifelse(master$sdg == "unimp", 1, 0)
master$resid <- ifelse(master$sdg %in% c('',NA),
                       1, 0)

master <- master %>% mutate(li = latrine_imp*hhweight*hh_size, lu = latrine_unimp*hhweight*hh_size,
                            lc = latrine_cw*hhweight*hh_size, od = open*hhweight*hh_size,
                            ip = imp*hhweight*hh_size, up = unimp*hhweight*hh_size,
                            rd = resid*hhweight*hh_size, sd = shared*hhweight*hh_size)
master_sum <- master %>% group_by(survey_name, ihme_loc_id, year_end) %>%
              summarize(od_sum = sum(od),
                        li_sum = sum(li),
                        lc_sum = sum(lc),
                        lu_sum = sum(lu),
                        ip_sum = sum(ip),
                        up_sum = sum(up),
                        rd_sum = sum(rd),
                        sd_sum = sum(sd),
                        denom_sum = unique(denom),
                        svy_short = unique(svy_short))
master_prev <- mutate(master_sum, od_prev = od_sum/denom_sum,
                      li_prev = li_sum/denom_sum,
                      lc_prev = lc_sum/denom_sum,
                      lu_prev = lu_sum/denom_sum,
                      ip_prev = ip_sum/denom_sum,
                      up_prev = up_sum/denom_sum,
                      rd_prev = rd_sum/denom_sum,
                      sd_prev = sd_sum/denom_sum)



master_prev$region <- "essa"
 master_prev$region[which(master_prev$ihme_loc_id %in% c('BEN','BFA','CMR','CPV','TCD','CIV','GMB','GHA','GIN','GNB','LBR','MLI',
                                                         'MRT','NER','NGA','STP','SEN','SLE','TGO'))] <- "wssa"
 master_prev$region[which(master_prev$ihme_loc_id %in% c("ZAF","BWA","NAM"))] <- "sssa"
 master_prev$region[which(master_prev$ihme_loc_id %in% c('TUN','EGY','LBY','DZA','MAR'))] <- "name"
 master_prev$region[which(master_prev$ihme_loc_id %in% c('AGO','CAF','COG','DRC','GNQ','GAB'))] <- "cssa"
 master_prev$region[which(master_prev$ihme_loc_id %in%  c(133,270,43,58,70,77,79,150,152,170,205,226,74,257,253,6,142,235,271))] <- "essa"
 
ggplot(data = master_prev, aes(x = year_end, y = od_prev, color = ihme_loc_id, label = ihme_loc_id)) + 
  geom_line() + facet_grid(region ~ .) + ggtitle("Open Defecation") + geom_text(size = 2) +  
  theme_classic() + theme(legend.position="none")

ggplot(data = master_prev, aes(x = year_end, y = li_prev, color = ihme_loc_id, label = ihme_loc_id)) + 
  geom_line() + facet_grid(region ~ .) + ggtitle("Latrine Improved") + geom_text(size = 2) +  
  theme_classic() + theme(legend.position="none")

ggplot(data = master_prev, aes(x = year_end, y = lc_prev, color = ihme_loc_id, label = ihme_loc_id)) + 
  geom_line() + facet_grid(region ~ .) + ggtitle("Latrine CW") + geom_text(size = 2) +  
  theme_classic() + theme(legend.position="none")


ggplot(data = master_prev, aes(x = year_end, y = lu_prev, color = ihme_loc_id, label = ihme_loc_id)) + 
  geom_line() + facet_grid(region ~ .) + ggtitle("Latrine Unimproved") + geom_text(size = 2) +  
  theme_classic() + theme(legend.position="none")


ggplot(data = master_prev, aes(x = year_end, y = ip_prev, color = ihme_loc_id, label = ihme_loc_id)) + 
  geom_line() + facet_grid(region ~ .) + ggtitle("Improved") + geom_text(size = 2) +  
  theme_classic() + theme(legend.position="none")


ggplot(data = master_prev, aes(x = year_end, y = up_prev, color = ihme_loc_id, label = ihme_loc_id)) + 
  geom_line() + facet_grid(region ~ .) + ggtitle("Unimproved") + geom_text(size = 2) +  
  theme_classic() + theme(legend.position="none")

ggplot(data = master_prev, aes(x = year_end, y = rd_prev, color = ihme_loc_id, label = ihme_loc_id)) + 
  geom_line() + facet_grid(region ~ .) + ggtitle("Residual") + geom_text(size = 2) +  
  theme_classic() + theme(legend.position="none")

ggplot(data = master_prev, aes(x = year_end, y = sd_prev, color = ihme_loc_id, label = ihme_loc_id)) + 
  geom_line() + facet_grid(region ~ .) + ggtitle("Shared") + geom_text(size = 2) +  
  theme_classic() + theme(legend.position="none")

shared_tabs <- filter(master, sdg %in% c('latrin_imp','imp','imo','latrine_cw'))
table(shared_tabs$shared_san, shared_tabs$sdg, useNA = "always")

