library(rEDM)
library(compiler)


source('red_noise_functions.R')
source('univariate_DEV.R')


replicates <- 100
correlations <- seq(0, 0.9, by = 0.1)

for(correlation in correlations){
  
  print(correlation)

  # Generate the time series
  for(i in 1:replicates){
    assign(paste0('rn_fold_ts', i), nm_fold(corr = correlation)[1:9000])
    assign(paste0('rn_pd_ts', i), henon_pd(corr = correlation)[1:8500])
    assign(paste0('rn_ns_ts', i), rma_neimark_sacker(corr = correlation)[1:8500])
  }
  
  
  for(i in 1:replicates){
    print(i)
    
    assign(paste0('rn_fold_DEV', i), DEV(get(paste0('rn_fold_ts', i))[1:9000],
                                               E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                                               winsize = 250, stepsize = 50))
    
    assign(paste0('rn_pd_DEV', i), DEV(get(paste0('rn_pd_ts', i))[1:8500],
                                                        E = 3, tau = -1, theta = seq(0, 2.5, 0.5),
                                                        winsize = 100, stepsize = 20))
    
    assign(paste0('rn_ns_DEV', i), DEV(get(paste0('rn_ns_ts', i))[1:8500],
                                             E = 6, tau = -1, theta = seq(0, 2.5, 0.5),
                                             winsize = 100, stepsize = 20))
    
  
    write.csv(get(paste0('rn_fold_DEV', i)), paste0('data/avg_DEV_red_noise/rn_fold/rn_fold', correlation, '-', i, '.csv'))
    write.csv(get(paste0('rn_pd_DEV', i)), paste0('data/avg_DEV_red_noise/rn_pd/rn_pd.csv', correlation, '-', i, '.csv'))
    write.csv(get(paste0('rn_ns_DEV', i)), paste0('data/avg_DEV_red_noise/rn_ns/rn_ns.csv', correlation, '-', i, '.csv'))
  
  }
  
  
  rn_fold_DEV_sum <- 0
  rn_pd_DEV_sum <- 0
  rn_ns_DEV_sum <- 0
  
  
  for(i in 1:replicates){
    
    rn_fold_DEV_sum <- rn_fold_DEV_sum + get(paste0('rn_fold_DEV', i))
    rn_pd_DEV_sum <- rn_pd_DEV_sum + get(paste0('rn_pd_DEV', i))
    rn_ns_DEV_sum <- rn_ns_DEV_sum + get(paste0('rn_ns_DEV', i))
  }
  
  
  assign(paste0('rn_fold_mean_DEV', correlation), rn_fold_DEV_sum/replicates)
  assign(paste0('rn_pd_mean_DEV', correlation), rn_pd_DEV_sum/replicates)
  assign(paste0('rn_ns_mean_DEV', correlation), rn_ns_DEV_sum/replicates)
  
  
  write.csv(get(paste0('rn_fold_mean_DEV', correlation)), 'data/avg_DEV_red_noise/rn_fold/rn_fold', correlation, '.csv')
  write.csv(get(paste0('rn_pd_mean_DEV', correlation)), 'data/avg_DEV_red_noise/rn_pd/rn_pd', correlation, '.csv')
  write.csv(get(paste0('rn_ns_mean_DEV', correlation)), 'data/avg_DEV_red_noise/rn_ns/rn_ns', correlation, '.csv')

}