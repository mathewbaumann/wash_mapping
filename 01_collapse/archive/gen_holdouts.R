rm(list=ls())

library(dplyr)
library(data.table)

nid_folds <- function(nids, yr, ss, n_folds, pts) {
  pts <- rep(1, length(ss))
  # calculate the total sample size by NID
  #ss_by_nid <- tapply(ss, nids, sum)
  ss_by_nid <- tapply(pts, nids, sum)
  
  # randomly sort the NIDs
  ss_by_nid <- ss_by_nid[sample(1:length(ss_by_nid))]
  
  # calculate a running total sample size in the new sort order
  cumulative_ss <- Reduce(sum, ss_by_nid, accumulate = T)
  
  # identify five roughly equal folds based on the cumulative sample size
  target_fold_ss <- sum(pts) / n_folds
  brks <- sapply(1:5, function(x) which.min(abs(cumulative_ss - x * target_fold_ss)))
  if(any(duplicated(brks))){
    message('failed splitting by number of points, trying by sample size')
    ss_by_nid <- tapply(ss, nids, sum)
    # randomly sort the NIDs
    ss_by_nid <- ss_by_nid[sample(1:length(ss_by_nid))]
    # calculate a running total sample size in the new sort order
    cumulative_ss <- Reduce(sum, ss_by_nid, accumulate = T)
    target_fold_ss <- sum(ss) / n_folds
    brks <- sapply(1:5, function(x) which.min(abs(cumulative_ss - x * target_fold_ss)))
  }
  folds <- cut(cumulative_ss, breaks = c(0, cumulative_ss[brks]), labels = F)
  
  # return in the proper format
  fold_list <- lapply(1:n_folds, function(x) {
    which(nids %in% names(ss_by_nid)[folds == x])
  })
  return(fold_list)
}

filepath <- '/share/geospatial/mbg/input_data/'
indis <- c('w_imp_cr','w_unimp_cr','w_piped','s_imp','s_unimp_cr')
for (indi in indis){
  message(paste0('Starting ',indi))
  mydat0 <- fread(paste0(filepath, indi, '.csv'))
  if(indi %in% c('w_imp_cr','s_imp')){
    message(paste0('Getting holdouts for ', indi))
    mydat <- copy(mydat0)
    mydat[,ss := sum(N), by = c('nid', 'country', 'year')]
    mydat[,pts := .N, by = c('nid', 'country', 'year')]
    mydat <- unique(mydat[,c('nid','year','ss','pts')])
    nids <- mydat$nid
    fold_list <- nid_folds(nids = nids, yr = mydat$year, ss = mydat$ss, n_folds = 5, pts = mydat$pts)
    for (i in 1:5){
      assign(paste0('fold_',i), nids[fold_list[[i]]])
    }
    rm(mydat)
  }
  write.csv(mydat0, paste0(filepath, indi, '_00.csv'))
  for (i in 1:5){
    hold_out <- copy(mydat0)
    hold_out <- filter(hold_out, !(nid %in% get(paste0('fold_', i))))
    write.csv(hold_out, paste0(filepath, indi, '_0', i, '.csv'))
  }
  
}
