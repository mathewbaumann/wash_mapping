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

# run dates to be combind
# Use hf_inla
hf_inla_shell <- '/share/singularity-images/health_fin/forecasting/shells/health_fin_forecasting_shell_mkl_singularity.sh'

r_shell <- hf_inla_shell
mkl <- 1

for (indi in c('s_imp', 's_od_calc','s_unimp_calc','w_imp','w_piped_calc','w_unimp_calc','w_surface_calc')) {
    jname <- paste0(indi, '_raster')
    mycores <- 10
    sys.sub <- paste0("qsub ",proj,paste0(" -e /homes/adesh/cluster_errors"," -o /homes/adesh/cluster_output/ "),
                      "-cwd -N ", jname, " ", "-pe multi_slot ", mycores, " ")
    # launch script name to qsub
    script <- "insertrasters.R"
    indic <- 'both'
    args <- paste(indi)
    system(paste(sys.sub, r_shell, mkl, script, args))   
 }
