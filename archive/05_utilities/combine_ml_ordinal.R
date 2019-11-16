.libPaths('/share/code/geospatial/adesh/r_packages')
rm(list = ls())
indic <- as.character(commandArgs()[4])


if (indic == 'water') {
    run_date <- as.character(commandArgs()[5])
    for (region in c('cssa','essa_hilo','name_hi','sssa_hi','wssa')) {
            for (indicator in c('w_piped', 'w_imp','w_unimp','w_surface')) {
                message(region); message(indicator); 
                
                message('loading data...')
                setwd(paste0('/share/geospatial/mbg/wash/',indicator,'/output/',run_date))
                files <- list.files()[grep(pattern = 'cell_draws', x = list.files())]
                preds <- load(files[grep(x = files, pattern = region)])
                del_obj <- preds
                preds <- get(preds)
                assign(indicator, preds); rm(preds)
                rm(list = paste(del_obj)); rm(del_obj);
            }

        message('calculating...')
        total <- w_piped + w_imp + w_unimp + w_surface
        
        w_piped_rescale <- w_piped/total
        w_imp_rescale <- w_imp/total
        w_unimp_rescale <- w_unimp/total
        w_surface_rescale <- w_surface/total

        rm(list = c('w_piped','w_imp','w_unimp','w_surface','total'))

        message('saving w_piped_rescale...')
        dir.create(paste0('/share/geospatial/mbg/wash/w_piped_rescale/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/w_piped_rescale/output/',run_date))
        save(w_piped_rescale, file = paste0("w_piped_rescale_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(w_piped_rescale)

        message('saving w_imp_rescale...')
        dir.create(paste0('/share/geospatial/mbg/wash/w_imp_rescale/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/w_imp_rescale/output/',run_date))
        save(w_imp_rescale, file = paste0("w_imp_rescale_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(w_imp_rescale)

        message('saving w_unimp_rescale...')
        dir.create(paste0('/share/geospatial/mbg/wash/w_unimp_rescale/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/w_unimp_rescale/output/',run_date))
        save(w_unimp_rescale, file = paste0("w_unimp_rescale_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(w_unimp_rescale)

        message('saving w_surface_rescale...')
        dir.create(paste0('/share/geospatial/mbg/wash/w_surface_rescale/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/w_surface_rescale/output/',run_date))
        save(w_surface_rescale, file = paste0("w_surface_rescale_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(w_surface_rescale)
    
    }
}


if (indic == 'sani') {
    run_date <- as.character(commandArgs()[5])
    for (region in c('cssa','essa_hilo','name_hi','sssa_hi','wssa')) {
        for (indicator in c('s_imp','s_unimp','s_od')) {
            message(region); message(indicator); 
            
            message('loading data...')
            setwd(paste0('/share/geospatial/mbg/wash/',indicator,'/output/',run_date))
            files <- list.files()[grep(pattern = 'cell_draws', x = list.files())]
            preds <- load(files[grep(x = files, pattern = region)])
            del_obj <- preds
            preds <- get(preds)
            assign(indicator, preds); rm(preds)
            rm(list = paste(del_obj)); rm(del_obj);
        }

        message('calculating...')
        total <- s_imp + s_unimp + s_od
        
        s_imp_rescale <- s_imp/total
        s_unimp_rescale <- s_unimp/total
        s_od_rescale <- s_od/total

        rm(list = c('s_imp','s_unimp','s_od','total'))

        message('saving s_imp_rescale...')
        dir.create(paste0('/share/geospatial/mbg/wash/s_imp_rescale/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/s_imp_rescale/output/',run_date))
        save(s_imp_rescale, file = paste0("s_imp_rescale_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(s_imp_rescale)

        message('saving s_unimp_rescale...')
        dir.create(paste0('/share/geospatial/mbg/wash/s_unimp_rescale/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/s_unimp_rescale/output/',run_date))
        save(s_unimp_rescale, file = paste0("s_unimp_rescale_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(s_unimp_rescale)

        message('saving s_surface_rescale...')
        dir.create(paste0('/share/geospatial/mbg/wash/s_surface_rescale/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/s_surface_rescale/output/',run_date))
        save(s_surface_rescale, file = paste0("s_surface_rescale_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(s_surface_rescale)
    
    }        
}

if (indic == 'both') {

    run_date <- as.character(commandArgs()[5])
    for (region in c('cssa','essa_hilo','name_hi','sssa_hi','wssa')) {
            for (indicator in c('w_piped', 'w_imp','w_unimp','w_surface')) {
                message(region); message(indicator); 
                
                message('loading data...')
                setwd(paste0('/share/geospatial/mbg/wash/',indicator,'/output/',run_date))
                files <- list.files()[grep(pattern = 'cell_draws', x = list.files())]
                preds <- load(files[grep(x = files, pattern = region)])
                del_obj <- preds
                preds <- get(preds)
                assign(indicator, preds); rm(preds)
                rm(list = paste(del_obj)); rm(del_obj);
            }

        message('calculating...')
        total <- w_piped + w_imp + w_unimp + w_surface
        
        w_piped_rescale <- w_piped/total
        w_imp_rescale <- w_imp/total
        w_unimp_rescale <- w_unimp/total
        w_surface_rescale <- w_surface/total

        rm(list = c('w_piped','w_imp','w_unimp','w_surface','total'))

        message('saving w_piped_rescale...')
        dir.create(paste0('/share/geospatial/mbg/wash/w_piped_rescale/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/w_piped_rescale/output/',run_date))
        save(w_piped_rescale, file = paste0("w_piped_rescale_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(w_piped_rescale)

        message('saving w_imp_rescale...')
        dir.create(paste0('/share/geospatial/mbg/wash/w_imp_rescale/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/w_imp_rescale/output/',run_date))
        save(w_imp_rescale, file = paste0("w_imp_rescale_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(w_imp_rescale)

        message('saving w_unimp_rescale...')
        dir.create(paste0('/share/geospatial/mbg/wash/w_unimp_rescale/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/w_unimp_rescale/output/',run_date))
        save(w_unimp_rescale, file = paste0("w_unimp_rescale_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(w_unimp_rescale)

        message('saving w_surface_rescale...')
        dir.create(paste0('/share/geospatial/mbg/wash/w_surface_rescale/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/w_surface_rescale/output/',run_date))
        save(w_surface_rescale, file = paste0("w_surface_rescale_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(w_surface_rescale)
    
    }

    run_date <- as.character(commandArgs()[5])
    for (region in c('cssa','essa_hilo','name_hi','sssa_hi','wssa')) {
        for (indicator in c('s_imp','s_unimp','s_od')) {
            message(region); message(indicator); 
            
            message('loading data...')
            setwd(paste0('/share/geospatial/mbg/wash/',indicator,'/output/',run_date))
            files <- list.files()[grep(pattern = 'cell_draws', x = list.files())]
            preds <- load(files[grep(x = files, pattern = region)])
            del_obj <- preds
            preds <- get(preds)
            assign(indicator, preds); rm(preds)
            rm(list = paste(del_obj)); rm(del_obj);
        }

        message('calculating...')
        total <- s_imp + s_unimp + s_od
        
        s_imp_rescale <- s_imp/total
        s_unimp_rescale <- s_unimp/total
        s_od_rescale <- s_od/total

        rm(list = c('s_imp','s_unimp','s_od','total'))

        message('saving s_imp_rescale...')
        dir.create(paste0('/share/geospatial/mbg/wash/s_imp_rescale/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/s_imp_rescale/output/',run_date))
        save(s_imp_rescale, file = paste0("s_imp_rescale_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(s_imp_rescale)

        message('saving s_unimp_rescale...')
        dir.create(paste0('/share/geospatial/mbg/wash/s_unimp_rescale/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/s_unimp_rescale/output/',run_date))
        save(s_unimp_rescale, file = paste0("s_unimp_rescale_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(s_unimp_rescale)

        message('saving s_surface_rescale...')
        dir.create(paste0('/share/geospatial/mbg/wash/s_surface_rescale/output/',run_date))
        setwd(paste0('/share/geospatial/mbg/wash/s_surface_rescale/output/',run_date))
        save(s_surface_rescale, file = paste0("s_surface_rescale_cell_draws_eb_bin0_",
                                    region,"_0.RData"))
        rm(s_surface_rescale)
    
    }        
}