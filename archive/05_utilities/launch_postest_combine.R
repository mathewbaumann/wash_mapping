#### Normal Run Launcher ####
# set repo
setwd('/share/code/geospatial/adesh/wash_mapping/05_utilities')

# set node preference
nodes <- 'geos'
if (nodes == 'geos') {
  proj <- "-P proj_geo_nodes -l gn=TRUE"
  r_shell <- 'shell_geos.sh'
} else {
  proj <- "-P proj_geospatial -q all.q"
  r_shell <- 'shell_prod.sh'
}

# ihme username
user <- "adesh"

# Use hf_inla
hf_inla_shell <- '/share/singularity-images/health_fin/forecasting/shells/health_fin_forecasting_shell_mkl_singularity.sh'

r_shell <- hf_inla_shell
mkl <- 1

# run dates to be combind
run_dates <- c('2018_06_05_10_31_32')
holdout <- 0:5
regions <- c('egy', 'cssa', 'sssa_hi', 'name_hi3', 'wssa', 'essa_hilo')
for (reg in regions) {
  for (rd in run_dates) {
    for (ho in holdout) {
      jname <- paste0(paste0("cr_",rd))
      mycores <- 10
      sys.sub <- paste0("qsub ",proj,paste0(" -e /homes/adesh/cluster_errors"," -o /homes/adesh/cluster_output/ "),
                        "-cwd -N ", jname, " ", "-pe multi_slot ", mycores)
      # launch script name to qsub
      script <- "combine_cr_ordinal.R"
      indic <- 'both'
      region <- reg
      args <- paste(indic, rd, ho, region)
      system(paste(sys.sub, r_shell, mkl, script, args))   
    }
    
  }
}