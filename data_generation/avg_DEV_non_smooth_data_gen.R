library(rEDM)
library(compiler)


source('non_smooth_model_functions.R')
source('univariate_DEV.R')
source('multivariate_DEV.R')


replicates <- 100

# Generate time series
for(i in 1:replicates){
  
  assign(paste0('no_bif_ts', i), non_smooth_general(x0 = -2.5, a = 0.6, b = 0.2))
  assign(paste0('ns_fold_ts', i), non_smooth_general(x0 = 1.25, a = 1.5, b = 0.2, mu_min = 1, mu_max = -1))
  assign(paste0('ns_pd_ts', i), non_smooth_general(x0 = -0.67, a = -0.5, b = -1.5))
  assign(paste0('ns_chaos_ts', i), non_smooth_general(x0 = -2.5, a = 0.6, b = -4))
  
  assign(paste0('ns_nm_ts', i), ns_neimark_sacker())
  
}

# Calculate DEV
for(i in 1:replicates){
  print(i)
  
  assign(paste0('no_bif_DEV', i), DEV(get(paste0('no_bif_ts', i)), E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                                      winsize = 250, stepsize = 50))
  
  assign(paste0('ns_fold_DEV', i), DEV(get(paste0('ns_fold_ts', i))[1:4800], E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                                       winsize = 250, stepsize = 50))
  
  assign(paste0('ns_pd_DEV', i), DEV(get(paste0('ns_pd_ts', i)), E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                                      winsize = 300, stepsize = 60))
  
  assign(paste0('ns_chaos_DEV', i), DEV(get(paste0('ns_chaos_ts', i)), E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                                     winsize = 250, stepsize = 50))
  
  
  assign(paste0('ns_nm_DEV', i), DEV(get(paste0('ns_nm_ts', i))[,2], E = 4, tau = -1, theta = seq(0, 2.5, 0.5),
                                         winsize = 250, stepsize = 50))
         
  assign(paste0('ns_nm_mult_DEV', i), multivariate_DEV(get(paste0('ns_nm_ts', i)), E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                                                winsize = 250, stepsize = 50))
}


# Calculate average DEV and save the values
no_bif_DEV_sum <- 0
#ns_fold_DEV_sum <- 0
ns_pd_DEV_sum <- 0
ns_chaos_DEV_sum <- 0
ns_nm_DEV_sum <- 0
ns_nm_mult_DEV_sum <- 0

for(i in 1:replicates){

  no_bif_DEV_sum <- no_bif_DEV_sum + get(paste0('no_bif_DEV', i))
  ns_fold_DEV_sum <- ns_fold_DEV_sum + get(paste0('ns_fold_DEV', i))
  ns_pd_DEV_sum <- ns_pd_DEV_sum + get(paste0('ns_pd_DEV', i))
  ns_chaos_DEV_sum <- ns_chaos_DEV_sum + get(paste0('ns_chaos_DEV', i))
  ns_nm_DEV_sum <- ns_nm_DEV_sum + get(paste0('ns_nm_DEV', i))
  ns_nm_mult_DEV_sum <- ns_nm_mult_DEV_sum + get(paste0('ns_nm_mult_DEV', i))
}

no_bif_mean_DEV <- no_bif_DEV_sum/replicates
ns_fold_mean_DEV <- ns_fold_DEV_sum/replicates
ns_pd_mean_DEV <- ns_pd_DEV_sum/replicates
ns_chaos_mean_DEV <- ns_chaos_DEV_sum/replicates
ns_nm_mean_DEV <- ns_nm_DEV_sum/replicates
ns_nm_mean_mult_DEV <- ns_nm_mult_DEV_sum/replicates


write.csv(no_bif_mean_DEV, 'data/avg_DEV_non_smooth/no_bif.csv')
write.csv(ns_fold_mean_DEV, 'data/avg_DEV_non_smooth/ns_fold.csv')
write.csv(ns_pd_mean_DEV, 'data/avg_DEV_non_smooth/ns_pd.csv')
write.csv(ns_chaos_mean_DEV, 'data/avg_DEV_non_smooth/ns_chaos.csv')
write.csv(ns_nm_mean_DEV, 'data/avg_DEV_non_smooth/ns_nm.csv')
write.csv(ns_nm_mean_mult_DEV, 'data/avg_DEV_non_smooth/ns_nm_mult.csv')
