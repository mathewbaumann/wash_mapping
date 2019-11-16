pacman::p_load(haven, plyr, data.table, magrittr)

water <- read.csv("J:/WORK/11_geospatial/wash/definitions/w_source_defined_2017_12_18.csv", stringsAsFactors = F, encoding = "windows-1252")
water <- water %>% data.table

water <- water[sdg == "imp", jmp := "basic"]
water <- water[sdg == "bottled", jmp := "basic"]
water <- water[sdg == "piped", jmp := "basic"]
water <- water[grepl("spring_*|well_*", sdg), jmp := sdg]
water <- water[sdg == "surface", jmp := sdg]

write.csv(water, "J:/WORK/11_geospatial/wash/definitions/w_source_defined_2017_12_18.csv", row.names=F, na="")
