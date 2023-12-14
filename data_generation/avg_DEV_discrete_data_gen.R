library(rEDM)
library(compiler)


source('discrete_model_functions.R')
source('univariate_DEV.R')
source('multivariate_DEV.R')


replicates <- 100
# generate the time series
for(i in 1:replicates){
  assign(paste0('discrete_fold_ts', i), nm_fold())
  assign(paste0('discrete_transcritical_ts', i), lv_transcritical())
  assign(paste0('discrete_pf_ts', i), lorenz_pitchfork())
}


for(i in 1:replicates){
  print(i)
  
  assign(paste0('discrete_fold_DEV', i), DEV(get(paste0('discrete_fold_ts', i))[1:9200],
                                                      E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                                                      winsize = 250, stepsize = 50))
  
  assign(paste0('discrete_transcritical_DEV', i), DEV(get(paste0('discrete_transcritical_ts', i))[1:5000, 2],
                                                      E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                                                      winsize = 250, stepsize = 50))
  
  assign(paste0('discrete_pf_DEV', i), DEV(get(paste0('discrete_pf_ts', i))[1:5000, 2],
                                                      E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                                                      winsize = 250, stepsize = 50))
  

}


discrete_fold_DEV_sum <- 0
discrete_transcritical_DEV_sum <- 0
discrete_pitchfork_DEV_sum <- 0


for(i in 1:replicates){
  
  discrete_fold_DEV_sum <- discrete_fold_DEV_sum + get(paste0('discrete_fold_DEV', i))
  discrete_transcritical_DEV_sum <- discrete_transcritical_DEV_sum + get(paste0('discrete_transcritical_DEV', i))
  discrete_pitchfork_DEV_sum <- discrete_pitchfork_DEV_sum + get(paste0('discrete_pf_DEV', i))
}


discrete_fold_mean_DEV <- discrete_fold_DEV_sum/replicates
discrete_transcritical_mean_DEV <- discrete_transcritical_DEV_sum/replicates
discrete_pitchfork_mean_DEV <- discrete_pitchfork_DEV_sum/replicates


write.csv(discrete_fold_mean_DEV, 'data/avg_DEV_discrete/discrete_fold.csv')
write.csv(discrete_transcritical_mean_DEV, 'data/avg_DEV_discrete/discrete_transcritical.csv')
write.csv(discrete_pitchfork_mean_DEV, 'data/avg_DEV_discrete/discrete_pitchfork.csv')
