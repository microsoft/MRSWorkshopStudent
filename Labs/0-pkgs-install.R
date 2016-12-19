
# The following packages are required for the Intro to Parallel Computing section of 3_MRS_Analysis.R lab

pkgs_to_install <- c("dplr", 
                     "microbenchmark", 
                     "snow", 
                     "robust"
)

pks_missing <- pkgs_to_install[!(pkgs_to_install %in% installed.packages()[, 1])]

install.packages(c(pks_missing))


