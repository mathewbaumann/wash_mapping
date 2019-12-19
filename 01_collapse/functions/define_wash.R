define_indi <- function(mydat = ptdat, var_family = indi_fam, define = definitions,
                        define2 = definitions2,debug = F, sdg_indi = T, census) {
  if (debug) {browser()}

  if ("data.table" %in% class(mydat)){
    message('ALERT: YOU ARE PASSING DATA.TABLES INTO THE MERGE. LARGE DATA.TABLES DROP STINGS. PLEASE MAKE THEM DATA.FRAMES.')
  }

  if (var_family == 'hw') {
    message('Nothing to define! HW is numeric!')
  }

  if (var_family == 'water') {
    # Rename string to indicator name to merge on and merge
    # definition file to mydatset
    if (sdg_indi) {
      define$sdg <- ifelse(define$jmp == 'basic' & !(define$sdg %in% c('piped', 'piped_cw',
                                                                       'piped_imp','imp')),
                      'imp', define$sdg)
    }
    #define <- rename(define, w_source_drink = string)
    mydat <- left_join(mydat, define, by = c("nid","iso3", "year_start","w_source_drink"))

    if (!sdg_indi) {
      define2 <- rename(define2, w_source_other = string)
      mydat <- left_join(mydat, define2, by = "w_source_other")
    }
  }

  if (var_family == 'sani') {
    if (census) {
      define <- rename(definitions, t_type = toilet, sdg = sani)
      mydat <- left_join(mydat, define, by = c("t_type", "sewage", "nid"))
    } else {
      mydat <- left_join(mydat, define, by = c("nid","iso3", "year_start","t_type"))
    }
  }

  if (var_family == 'water') {
    mydat <- mydat %>%
            mutate(# Define straightforward water ladder
                   surface = ifelse(mydat$sdg == "surface", 1, ifelse(is.na(mydat$sdg), NA, 0)),
                   imp = ifelse(mydat$sdg == "imp", 1, ifelse(is.na(mydat$sdg), NA, 0)),
                   unimp = ifelse(mydat$sdg == "unimp", 1, ifelse(is.na(mydat$sdg), NA, 0)),

                   # Define crosswalking indicators
                   well_cw = ifelse(mydat$sdg == "well_cw", 1, ifelse(is.na(mydat$sdg), NA, 0)),
                   well_imp = ifelse(mydat$sdg == "well_imp", 1, ifelse(is.na(mydat$sdg), NA, 0)),
                   well_unimp = ifelse(mydat$sdg == "well_unimp", 1, ifelse(is.na(mydat$sdg), NA, 0)),

                   spring_cw = ifelse(mydat$sdg == "spring_cw", 1, ifelse(is.na(mydat$sdg), NA, 0)),
                   spring_imp = ifelse(mydat$sdg == "spring_imp", 1, ifelse(is.na(mydat$sdg), NA, 0)),
                   spring_unimp = ifelse(mydat$sdg == "spring_unimp", 1, ifelse(is.na(mydat$sdg), NA, 0)),

                   piped_cw = ifelse(mydat$sdg == "piped_cw", 1, ifelse(is.na(mydat$sdg), NA, 0)),
                   piped_imp = ifelse(mydat$sdg == "piped_imp", 1, ifelse(is.na(mydat$sdg), NA, 0)),
                   piped = ifelse(mydat$sdg == "piped", 1, ifelse(is.na(mydat$sdg), NA, 0))
                   )
  }

  #not currently used
  if (var_family == 'hw') {
    mydat <- mydat %>%
              mutate(hw_unimp = ifelse(is.na(mydat$hw_soap) & is.na(mydat$hw_water), NA,
                ifelse((is.na(mydat$hw_soap)|is.na(mydat$hw_water)),
                ifelse(mydat$hw_soap == 1|mydat$hw_water == 1, 1, NA),
                ifelse(mydat$hw_soap == 1|mydat$hw_water == 1, 1, 0)
                )
              ),
                hw_basic = ifelse(is.na(mydat$hw_soap) & is.na(mydat$hw_water), NA,
                  ifelse((is.na(mydat$hw_soap)|is.na(mydat$hw_water)),
                  ifelse(mydat$hw_soap == 0|mydat$hw_water == 0, 0, NA),
                  ifelse(mydat$hw_soap == 0|mydat$hw_water == 0, 0, 1)
                  )
              ))
  }

  if (var_family == 'sani') {
    mydat <- mydat %>%
    mutate(imp = ifelse(mydat$sdg == "imp", 1, ifelse(is.na(mydat$sdg), NA, 0)),
           unimp = ifelse(mydat$sdg == "unimp", 1, ifelse(is.na(mydat$sdg), NA, 0)),
           od = ifelse(mydat$sdg == "open", 1, ifelse(is.na(mydat$sdg), NA, 0)),
           shared = ifelse((mydat$sdg %in% c('imp','latrine_imp')),
                      ifelse(mydat$shared_san == 1, 1, ifelse(is.na(mydat$shared_san), NA, 0)), 0),

           # Define crosswalking indicators for latrines
           latrine_cw = ifelse(mydat$sdg == "latrine_cw", 1, ifelse(is.na(mydat$sdg), NA, 0)),
           latrine_imp = ifelse(mydat$sdg == "latrine_imp", 1, ifelse(is.na(mydat$sdg), NA, 0)),
           latrine_unimp = ifelse(mydat$sdg == "latrine_unimp", 1, ifelse(is.na(mydat$sdg), NA, 0)),

           # Define crosswalking indicators for flush toilets
           flush_cw = ifelse(mydat$sdg == "flush_cw", 1, ifelse(is.na(mydat$sdg), NA, 0)),
           flush_imp = ifelse(mydat$sdg %in% c("flush_imp"), 1, ifelse(is.na(mydat$sdg), NA, 0)),
           flush_unimp = ifelse(mydat$sdg == "flush_unimp", 1, ifelse(is.na(mydat$sdg), NA, 0)),
           flush_imp_sewer = ifelse(mydat$sdg == "flush_imp_sewer", 1, ifelse(is.na(mydat$sdg), NA, 0)),
           flush_imp_septic = ifelse(mydat$sdg == "flush_imp_septic", 1, ifelse(is.na(mydat$sdg), NA, 0)),
           s_piped = ifelse(mydat$sdg %in% c('sewer','septic','flush_imp_sewer','flush_imp_septic'), 1, ifelse(is.na(mydat$sdg), NA, 0)),
           network = ifelse(mydat$sdg %in% c('sewer','flush_imp_sewer'), 1, ifelse(is.na(mydat$sdg), NA, 0))
           )
  }
  return(mydat)
}
