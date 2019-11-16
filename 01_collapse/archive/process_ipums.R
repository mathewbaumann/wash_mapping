library(dplyr)
library(data.table)

setwd('J:/LIMITED_USE/LU_GEOSPATIAL/geo_matched/wash/')
ipums <- list.files(pattern = "IPUMS")

if (!("id_ref" %in% ls())) {
  id_ref <- read.csv("J:/WORK/11_geospatial/pandemic_indicator/data/raw/geographies/id_convert_ammended.csv")
}

afro_gaul <- c(4,6,8,29,35,42,43,45,47,49,50,58,59,66,68,70,
               74,76,77,79,89,90,94,95,105,106,142,144,145,150,
               152,155,159,169,170,172,181,182,205,214,217,221,
               226,235,243,248,253,268,270,271,40764,40765,
               227,257,133)

process_ipums <- function(ip_1) {
  if(!(unique(ip_1$ihme_loc_id %in% (id_ref$ISO3[which(id_ref$GAUL %in% afro_gaul)]))))
  {vars <- names(ip_1); rm(ip_1); ip_sani_poly_agg <- NULL} 
  
  else {
    
    if(!all(c("year_start", "survey_name", "ihme_loc_id", "shapefile", "location_code",
              "t_type", "nid","pweight","hh_id") %in% names(ip_1)))
    {vars <- names(ip_1); rm(ip_1); ip_sani_poly_agg <- NULL}
    
    else {
      if (any(duplicated(names(ip_1)))) {
        vars <- names(ip_1); rm(ip_1); ip_sani_poly_agg <- NULL
      }
      else {
        vars <- names(ip_1)
        ip_sani_poly <- as.data.frame(ip_1)
        ip_sani_poly$t_type <- 1
        ip_sani_poly <- mutate(ip_sani_poly, poly_id = paste0(shapefile, location_code, sep = "_"))
        ip_sani_poly <- ip_sani_poly %>% group_by(hh_id) %>% mutate(hh_size = length(t_type))
        ip_sani_poly <- ip_sani_poly %>% group_by(poly_id) %>% mutate(weight_total = sum(pweight*hh_size))
        ip_sani_poly <- mutate(ip_sani_poly, wt_indic = (hh_size*pweight*t_type)/weight_total)
        ip_sani_poly_agg <- ip_sani_poly %>% group_by(year_start, survey_name, ihme_loc_id, shapefile, location_code,
                                                      t_type, nid) %>% summarize(sani = sum(wt_indic))
      }
    }
  }
  result <- list(ip_sani_poly_agg, vars)
  return(result)
}



ipums_list <- list()
ipums_vars <- list()
for (i in ipums) {
  message(i)
  ip_data <- fread(i, stringsAsFactors = F)
  ipums_list[[length(ipums_list)+1]] <- process_ipums(ip_1 = ip_data)[[1]]
  ipums_vars[[length(ipums_vars)+1]] <- process_ipums(ip_data)[[2]]
}